(* Choosing a level: a grid of tiles for both worlds and a preview of the
   focused level, drawn with the real board renderer. Arrow keys or the mouse
   move the focus; Enter or a click starts the level. *)

open Ml_regl_core
open Messenger
open Regl_builtin_programs
module Tile = Tile.Model

type msg = Tile of Tile.msg

let tile = Component.port (fun m -> Tile m) (fun (Tile m) -> Some m)

type data = {
  focus : int;
  tiles : (Common.t, User_data.t, int, msg, unit) Component.t list;
  levels : Rules.level option array;  (** parsed once their files load *)
  clock : Clock.t;
  now : float;
  focused_at : float;
}

let init runtime env _params =
  (* start on the first unsolved level *)
  let focus = Levels.first_unsolved env.Base.global_data.user_data in
  let cenv = Base.add_common_data { Common.focus } env in
  {
    focus;
    tiles =
      List.init Levels.count (fun index ->
          Component.make tile Tile.component { Tile.index } runtime cenv);
    levels = Array.make Levels.count None;
    clock = Clock.start;
    now = 0.;
    focused_at = 0.;
  }

let load_levels runtime d =
  Array.iteri
    (fun i l ->
      if l = None then
        match Levels.load runtime i with
        | Some lv -> d.levels.(i) <- Some lv
        | None | (exception Failure _) -> ())
    d.levels

(* The tile above or below [i]: same column, in the neighbouring row (which may
   be in the other world, or shorter). *)
let vertical i dir =
  let x, y = Common.tile_pos i in
  let candidates =
    List.init Levels.count Fun.id
    |> List.filter (fun j ->
        let _, yj = Common.tile_pos j in
        if dir < 0 then yj < y else yj > y)
  in
  match candidates with
  | [] -> i
  | _ ->
      let ys = List.map (fun j -> snd (Common.tile_pos j)) candidates in
      let target_y = if dir < 0 then List.fold_left Float.max (-1.) ys else List.fold_left Float.min 1e9 ys in
      let row = List.filter (fun j -> snd (Common.tile_pos j) = target_y) candidates in
      List.fold_left
        (fun best j ->
          let dx j = Float.abs (fst (Common.tile_pos j) -. x) in
          if dx j < dx best then j else best)
        (List.hd row) row

let set_focus d f =
  if f = d.focus then (d, [])
  else ({ d with focus = f; focused_at = d.now }, [ Sfx.play "select" ])

let update runtime env evnt d =
  load_levels runtime d;
  let cenv = Base.add_common_data { Common.focus = d.focus } env in
  let tiles, msgs, soms, (cenv, block) =
    Component.update_children runtime cenv evnt d.tiles
  in
  let env = Base.remove_common_data cenv in
  let d = { d with tiles } in
  let d =
    match evnt with
    | Regl_proto.UpdateTick t ->
        let clock = Clock.tick d.clock t in
        { d with clock; now = clock.elapsed }
    | _ -> d
  in
  (* the tiles' reports *)
  let d, picked, sounds =
    List.fold_left
      (fun (d, picked, sounds) (Tile m) ->
        match m with
        | Tile.Hovered i ->
            let d, s = set_focus d i in
            (d, picked, sounds @ s)
        | Tile.Picked i -> (d, Some i, sounds))
      (d, None, []) msgs
  in
  let d, keys =
    if block || picked <> None then (d, [])
    else
      match evnt with
      | KeyDown ("Left" | "A") -> set_focus d (max 0 (d.focus - 1))
      | KeyDown ("Right" | "D") -> set_focus d (min (Levels.count - 1) (d.focus + 1))
      | KeyDown ("Up" | "W") -> set_focus d (vertical d.focus (-1))
      | KeyDown ("Down" | "S") -> set_focus d (vertical d.focus 1)
      | KeyDown ("Return" | "Space" | "KP Enter") -> (d, [ Nav.level d.focus ])
      | KeyDown "Escape" -> (d, [ Nav.title () ])
      | _ -> (d, [])
  in
  let start = match picked with Some i -> [ Sfx.play "select"; Nav.level i ] | None -> [] in
  (d, soms @ sounds @ keys @ start, env)

let preview d (u : User_data.t) =
  let id = Levels.ids.(d.focus) in
  let px = 784. and py = 128. and pw = 456. and ph = 472. in
  let panel =
    [
      rounded_rect (px +. (pw /. 2.), py +. (ph /. 2.)) (pw, ph) 18. Pal.panel;
      textbox (px +. 24., py +. 18.) 44. id Widgets.display Pal.cream;
      Widgets.mono_text (px +. 26., py +. 84.) 12.
        (if Levels.world_of d.focus = 1 then "WORLD I  ·  CONDUCTION" else "WORLD II  ·  GLASS")
        Pal.cream_dim;
      Widgets.mono_text (px +. 26., py +. ph -. 30.) 15.
        (match User_data.best u id with
         | Some b -> Printf.sprintf "Solved  ·  best %d moves" b
         | None -> "Not yet solved")
        (if User_data.solved u id then Pal.gold else Pal.cream_dim);
    ]
  in
  let board =
    match d.levels.(d.focus) with
    | Some lv ->
        let layout = Board_view.fit lv ~area:(px +. 20., py +. 104., pw -. 40., ph -. 156.) ~max_cell:44. in
        let objs = Rules.initial lv in
        let fade = Float.min 1. ((d.now -. d.focused_at) /. 180.) in
        Regl_common.group [ Regl_effects.alpha_mult fade ]
          [ Board_view.draw lv objs layout (Board_view.still objs d.now) ]
    | None -> empty
  in
  Regl_common.group [] (panel @ [ board ])

let view runtime env d =
  let u = env.Base.global_data.user_data in
  let solved = Array.fold_left (fun n id -> if User_data.solved u id then n + 1 else n) 0 Levels.ids in
  let cenv = Base.add_common_data { Common.focus = d.focus } env in
  let header (y : float) roman title =
    Regl_common.group []
      [
        Widgets.mono_text ~font:Widgets.mono_bold (48., y) 14. roman Pal.gold;
        Widgets.mono_text (48. +. Widgets.mono_width 14. roman +. 14., y) 14. title Pal.cream_dim;
      ]
  in
  Regl_common.group []
    [
      clear Pal.ink;
      textbox (44., 26.) 50. "Choose an experiment" Widgets.display Pal.cream;
      Widgets.mono_text (1240. -. Widgets.mono_width 14. (Printf.sprintf "SOLVED %d / %d" solved Levels.count), 58.)
        14. (Printf.sprintf "SOLVED %d / %d" solved Levels.count) Pal.gold;
      header 132. "WORLD I" "Conduction  ·  27 experiments";
      header 356. "WORLD II" "Glass  ·  30 experiments";
      Component.view_components runtime cenv d.tiles;
      preview d u;
      Widgets.mono_centered (640., 690.) 13.
        "ARROWS choose  ·  ENTER start  ·  ESC title" Pal.cream_dim;
    ]

let scene params runtime env = Scene.abstract { init; update; view } params runtime env
