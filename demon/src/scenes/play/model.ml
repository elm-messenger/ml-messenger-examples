(* Playing one level. The scene owns the board, the HUD and the end-of-level
   banner, and routes their messages: the board reports every change, the
   scene forwards it to the HUD and opens or closes the banner, and button
   presses come back as board commands. *)

open Ml_regl_core
open Messenger
module Board = Board.Model
module Hud = Hud.Model
module Banner = Banner.Model

type msg = Board of Board.msg | Hud of Hud.msg | Banner of Banner.msg

let board = Component.port (fun m -> Board m) (function Board m -> Some m | _ -> None)
let hud = Component.port (fun m -> Hud m) (function Hud m -> Some m | _ -> None)
let banner = Component.port (fun m -> Banner m) (function Banner m -> Some m | _ -> None)

type children = (unit, User_data.t, string, msg, unit) Component.t list

type data = {
  index : int;
  children : children option;  (** once the level file has loaded *)
  error : string option;
}

let make_children runtime env index (lv : Rules.level) : children =
  let world = Levels.world_of index in
  let ids = if world = 1 then Levels.world1 else Levels.world2 in
  let first = if world = 1 then 0 else List.length Levels.world1 in
  let glass = Array.exists (fun (o : Rules.obj) -> o.kind = Rules.Glass) lv.start in
  let fixed = List.exists (Board_view.bolted lv) (List.init (lv.w * lv.h) Fun.id) in
  [
    Component.make hud Hud.component
      {
        Hud.level_id = Levels.ids.(index);
        world;
        number = index - first + 1;
        of_count = List.length ids;
        glass;
        fixed;
      }
      runtime env;
    Component.make board Board.component { Board.level = lv } runtime env;
    (* last: it sees input first and blocks it while shown *)
    Component.make banner Banner.component () runtime env;
  ]

let try_load runtime env data =
  if data.children <> None || data.error <> None then data
  else
    match Levels.load runtime data.index with
    | Some lv -> { data with children = Some (make_children runtime env data.index lv) }
    | None -> data
    | exception Failure e -> { data with error = Some e }

let init runtime env (params : Play_params.t option) =
  let index = match params with Some p -> p.index | None -> 0 in
  let index = max 0 (min (Levels.count - 1) index) in
  try_load runtime env { index; children = None; error = None }

let record_solve env id moves =
  let u = env.Base.global_data.user_data in
  let u' = User_data.record u id moves in
  ( { env with global_data = { env.global_data with user_data = u' } },
    [ Scene.SOMSaveValue (User_data.storage_key, User_data.serialize u') ] )

(* What the scene does with one message from a child: messages to send to
   children, scene output messages, and the new env. *)
let react data env msg =
  let id = Levels.ids.(data.index) in
  match msg with
  | Board (Board.Changed r) ->
      let to_hud = ("hud", Hud (Hud.Show r)) in
      begin match r.status with
      | Rules.Solved ->
          let best = User_data.best env.Base.global_data.user_data id in
          let env, soms = record_solve env id r.moves in
          let outcome =
            { Banner.solved = true; moves = r.moves; best; last = data.index = Levels.count - 1 }
          in
          ([ to_hud; ("banner", Banner (Banner.Open outcome)) ], soms, env)
      | Rules.Dead ->
          let outcome = { Banner.solved = false; moves = r.moves; best = None; last = false } in
          ([ to_hud; ("banner", Banner (Banner.Open outcome)) ], [], env)
      | Rules.Playing -> ([ to_hud; ("banner", Banner Banner.Close) ], [], env)
      end
  | Hud (Hud.Pressed a) -> (
      match a with
      | Hud.Undo -> ([ ("board", Board Board.Undo) ], [], env)
      | Hud.Restart -> ([ ("board", Board Board.Restart) ], [], env)
      | Hud.Thermal -> ([ ("board", Board Board.Toggle_thermal) ], [], env)
      | Hud.Levels -> ([], [ Nav.select () ], env))
  | Banner (Banner.Chose c) -> (
      match c with
      | Banner.Next ->
          let next = data.index + 1 in
          ([], [ (if next < Levels.count then Nav.level next else Nav.select ()) ], env)
      | Banner.Retry -> ([ ("board", Board Board.Restart) ], [], env)
      | Banner.Undo -> ([ ("board", Board Board.Undo) ], [], env)
      | Banner.Levels -> ([], [ Nav.select () ], env))
  | _ -> ([], [], env)

(* Handle messages until the children stop answering. *)
let rec settle runtime env data children msgs soms =
  match msgs with
  | [] -> (children, List.rev soms, env)
  | msg :: rest ->
      let sends, new_soms, env = react data env msg in
      let children, replies, child_soms, env = Component.send runtime env sends children in
      settle runtime env data children (rest @ replies)
        (List.rev_append child_soms (List.rev_append new_soms soms))

let update runtime env evnt data =
  let data = try_load runtime env data in
  match data.children with
  | None -> (data, [], env)
  | Some children ->
      let children, msgs, soms, (env, block) =
        Component.update_children runtime env evnt children
      in
      let children, more, env = settle runtime env data children msgs [] in
      let keys =
        match evnt with
        | Regl_proto.KeyDown "Escape" when not block -> [ Nav.select () ]
        | _ -> []
      in
      ({ data with children = Some children }, soms @ more @ keys, env)

let view runtime env data =
  Regl_common.group []
    [
      Regl_builtin_programs.clear Pal.ink;
      (match data.children with
      | Some children -> Component.view_components runtime env children
      | None ->
          let text = match data.error with Some e -> e | None -> "Loading…" in
          Widgets.mono_centered (640., 360.) 18. text Pal.cream_dim);
    ]

let scene params runtime env = Scene.abstract { init; update; view } params runtime env
