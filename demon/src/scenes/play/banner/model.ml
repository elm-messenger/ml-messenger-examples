(* The card shown when a level ends: solved (go on) or overheated (undo). While
   it is up it takes every key and click, so the board below gets none. *)

open Ml_regl_core
open Messenger
open Regl_builtin_programs

type choice = Next | Retry | Undo | Levels

type outcome = { solved : bool; moves : int; best : int option; last : bool }

type msg =
  | Open of outcome  (** from the parent *)
  | Close
  | Chose of choice  (** to the parent *)

type data = {
  shown : (outcome * float) option;  (** what, and from when *)
  clock : Clock.t;
  now : float;
  mouse : float * float;
  focus : int;
}

let init _runtime _env () =
  { shown = None; clock = Clock.start; now = 0.; mouse = (-1., -1.); focus = 0 }

(* Wait a moment before covering the board, so the last move can be seen. *)
let delay o = if o.solved then 450. else 650.

let visible d =
  match d.shown with Some (o, at) -> d.now >= at +. delay o | None -> false

let buttons o =
  let labels =
    if o.solved then
      [ ((if o.last then "Back to levels" else "Next experiment"), "ENTER", (if o.last then Levels else Next)); ("Replay", "R", Retry) ]
    else [ ("Undo last move", "Z", Undo); ("Restart", "R", Retry) ]
  in
  List.mapi
    (fun i (text, hint, choice) ->
      ( choice,
        { Widgets.text; hint; bx = 640. -. 290. +. (float i *. 300.); by = 420.; bw = 280.; bh = 58. } ))
    labels

let choose d c = (d, [ Component.Parent (Chose c) ], true)

let update _runtime env evnt d =
  let d, cmds, block =
    match (evnt, d.shown) with
    | Regl_proto.UpdateTick t, _ ->
        let clock = Clock.tick d.clock t in
        ({ d with clock; now = clock.elapsed }, [], false)
    | MouseMove { x; y }, _ -> ({ d with mouse = (x, y) }, [], visible d)
    | _, None -> (d, [], false)
    | _, Some _ when not (visible d) -> (d, [], false)
    | MouseDown { button = 1; x; y }, Some (o, _) -> (
        match List.find_opt (fun (_, b) -> Widgets.button_inside (x, y) b) (buttons o) with
        | Some (c, _) -> choose d c
        | None -> (d, [], true))
    | KeyDown ("Left" | "Right" | "A" | "D" | "Tab"), Some _ ->
        ({ d with focus = 1 - d.focus }, [ Component.Som (Sfx.play "select") ], true)
    | KeyDown ("Return" | "Space" | "KP Enter"), Some (o, _) ->
        choose d (fst (List.nth (buttons o) d.focus))
    | KeyDown ("Z" | "U" | "Backspace"), Some _ -> choose d Undo
    | KeyDown "R", Some _ -> choose d Retry
    | KeyDown "Escape", Some _ -> choose d Levels
    | _, Some _ -> (d, [], true)
  in
  (d, cmds, (env, block))

let updaterec _runtime env msg d =
  match msg with
  | Open o -> ({ d with shown = Some (o, d.now); focus = 0 }, [], env)
  | Close -> ({ d with shown = None }, [], env)
  | Chose _ -> (d, [], env)

let view _runtime _env d =
  match d.shown with
  | Some (o, at) when visible d ->
      let t = Float.min 1. ((d.now -. at -. delay o) /. 260.) in
      let e = 1. -. ((1. -. t) ** 3.) in
      let accent = if o.solved then Pal.gold else Pal.hot_text in
      let title = if o.solved then "Equilibrium." else "Overheated!" in
      let line =
        if o.solved then
          match o.best with
          | Some b when b < o.moves -> Printf.sprintf "Solved in %d moves  ·  your best is %d" o.moves b
          | Some b when b = o.moves -> Printf.sprintf "Solved in %d moves  ·  matches your best" o.moves
          | Some _ -> Printf.sprintf "Solved in %d moves  ·  a new best!" o.moves
          | None -> Printf.sprintf "Solved in %d moves" o.moves
        else "Heat flowed into the demon. Undo, or start over."
      in
      let dy = 30. *. (1. -. e) in
      let card =
        Regl_common.group []
          ([
             rounded_rect (640., 360. +. dy) (680., 300.) 22. Pal.panel_edge;
             rounded_rect (640., 360. +. dy) (672., 292.) 18. Pal.panel;
             rect (330., 232. +. dy) (620., 3.) accent;
             textbox_centered (640., 300. +. dy) 64. title Widgets.display accent;
             Widgets.mono_centered (640., 372. +. dy) 15. line Pal.cream_dim;
           ]
          @ List.mapi
              (fun i (_, (b : Widgets.button)) ->
                let b = { b with by = b.by +. dy } in
                Widgets.draw_button ~focus:(i = d.focus || Widgets.button_inside d.mouse b) ~accent b)
              (buttons o))
      in
      ( Regl_common.group [ Regl_effects.alpha_mult e ]
          [ rect (0., 74.) (1280., 588.) (Pal.with_alpha Pal.ink 0.72); card ],
        20 )
  | _ -> (empty, 20)

let component =
  { Component.init; update; updaterec; view; targets = (fun _ -> [ "banner" ]) }
