(* The board: holds the rules state and its undo history, turns key presses
   into moves, animates them and reports every change to its parent. *)

open Ml_regl_core
open Messenger
module R = Rules

type msg =
  | Undo  (** from the parent *)
  | Restart
  | Toggle_thermal
  | Changed of Common.report  (** to the parent *)

type init = { level : R.level }

type data = {
  lv : R.level;
  layout : Board_view.layout;
  objs : R.obj array;
  history : (R.obj array * int) list;  (** earlier states and move counts *)
  moves : int;
  from : R.obj array;  (** the state the animation starts from *)
  anim_at : float;
  anim_ms : float;
  bump : (int * float) option;  (** blocked direction, when *)
  facing_left : bool;
  clock : Clock.t;
  now : float;  (** [clock]'s elapsed time *)
  died_at : float option;
  thermal : bool;
  held : (string * int * float) option;  (** key, direction, next repeat *)
}

let repeat_delay = 210.
let repeat_every = 115.

let dir_of_key = function
  | "W" | "Up" -> Some R.up
  | "S" | "Down" -> Some R.down
  | "A" | "Left" -> Some R.left
  | "D" | "Right" -> Some R.right
  | _ -> None

let init _runtime _env (init : init) : data =
  let objs = R.initial init.level in
  {
    lv = init.level;
    layout = Board_view.fit init.level ~area:Common.board_area ~max_cell:72.;
    objs;
    history = [];
    moves = 0;
    from = objs;
    anim_at = 0.;
    anim_ms = 1.;
    bump = None;
    facing_left = false;
    clock = Clock.start;
    now = 0.;
    died_at = None;
    thermal = false;
    held = None;
  }

let status d = R.status d.lv d.objs

let report d =
  Component.Parent
    (Changed
       { moves = d.moves; status = status d; can_undo = d.history <> []; thermal = d.thermal })

(* Sounds for the change from [before] to [after]. *)
let sounds lv before after moved =
  let pos0 = R.positions before and heat0 = Hashtbl.create 64 in
  Array.iter (fun (o : R.obj) -> Hashtbl.replace heat0 o.id o.heat) before;
  let shifted = ref 0 and warmed = ref false and cooled = ref false in
  Array.iteri
    (fun i (o : R.obj) ->
      if o.kind <> R.Nothing then begin
        if Hashtbl.find_opt pos0 o.id <> Some i then incr shifted;
        match Hashtbl.find_opt heat0 o.id with
        | Some h when h <> o.heat ->
            if o.heat = R.Hot then warmed := true
            else if o.heat = R.Cold then cooled := true
        | _ -> ()
      end)
    after;
  let motion =
    if not moved then [ "bump" ] else if !shifted > 1 then [ "push" ] else [ "step" ]
  in
  let heat = (if !warmed then [ "heat" ] else []) @ if !cooled then [ "freeze" ] else [] in
  let ending =
    match R.status lv after with
    | R.Dead -> [ "die" ]
    | R.Solved -> [ "win" ]
    | R.Playing -> []
  in
  List.map (fun n -> Component.Som (Sfx.play n)) (motion @ heat @ ending)

let move d dir =
  if status d <> R.Playing then (d, [])
  else
    let after, moved = R.turn d.lv d.objs dir in
    let cmds = sounds d.lv d.objs after moved in
    if not moved then ({ d with objs = after; bump = Some (dir, d.now) }, cmds)
    else
      let d' =
        {
          d with
          objs = after;
          history = (d.objs, d.moves) :: d.history;
          moves = d.moves + 1;
          from = d.objs;
          anim_at = d.now;
          anim_ms = 120.;
          bump = None;
          facing_left =
            (if dir = R.left then true
             else if dir = R.right then false
             else d.facing_left);
          died_at = (if R.status d.lv after = R.Dead then Some d.now else None);
        }
      in
      (d', cmds @ [ report d' ])

(* Jump to another state, animating from the current one. *)
let jump d (objs, moves) history ms sound =
  let d' =
    {
      d with
      objs;
      moves;
      history;
      from = d.objs;
      anim_at = d.now;
      anim_ms = ms;
      bump = None;
      died_at = None;
    }
  in
  (d', [ Component.Som (Sfx.play sound); report d' ])

let undo d =
  match d.history with
  | prev :: rest -> jump d prev rest 90. "undo"
  | [] -> (d, [])

let restart d =
  if d.moves = 0 && d.history = [] then (d, [])
  else jump d (R.initial d.lv, 0) ((d.objs, d.moves) :: d.history) 260. "undo"

let toggle_thermal d =
  let d = { d with thermal = not d.thermal } in
  (d, [ report d ])

let update _runtime env evnt d =
  let d, cmds =
    match evnt with
    | Regl_proto.UpdateTick t -> (
        let clock = Clock.tick d.clock t in
        let now = clock.elapsed in
        let d = { d with clock; now } in
        match d.held with
        | Some (key, dir, next) when now >= next ->
            let d, cmds = move d dir in
            ({ d with held = Some (key, dir, now +. repeat_every) }, cmds)
        | _ -> (d, []))
    | KeyDown key -> (
        match (dir_of_key key, d.held) with
        | Some _, Some (k, _, _) when k = key -> (d, []) (* the OS's own repeat *)
        | Some dir, _ ->
            let d, cmds = move d dir in
            ({ d with held = Some (key, dir, d.now +. repeat_delay) }, cmds)
        | None, _ -> (
            match key with
            | "Z" | "U" | "Backspace" -> undo d
            | "R" -> restart d
            | "T" -> toggle_thermal d
            | _ -> (d, [])))
    | KeyUp key -> (
        match d.held with
        | Some (k, _, _) when k = key -> ({ d with held = None }, [])
        | _ -> (d, []))
    | _ -> (d, [])
  in
  (d, cmds, (env, false))

let updaterec _runtime env msg d =
  let d, cmds =
    match msg with
    | Undo -> undo d
    | Restart -> restart d
    | Toggle_thermal -> toggle_thermal d
    | Changed _ -> (d, [])
  in
  (d, cmds, env)

let clamp01 x = Float.max 0. (Float.min 1. x)

let view _runtime _env d =
  let t = clamp01 ((d.now -. d.anim_at) /. d.anim_ms) in
  (* heat follows once things have arrived *)
  let heat_t = clamp01 ((d.now -. d.anim_at -. (d.anim_ms *. 0.6)) /. 260.) in
  let bump =
    match d.bump with
    | Some (dir, at) when d.now -. at < 130. -> Some (dir, (d.now -. at) /. 130.)
    | _ -> None
  in
  let anim : Board_view.anim =
    {
      from = d.from;
      t;
      heat_t;
      bump;
      facing_left = d.facing_left;
      now = d.now;
      dead_for = Option.map (fun at -> d.now -. at) d.died_at;
      thermal = d.thermal;
    }
  in
  (Board_view.draw d.lv d.objs d.layout anim, 0)

let component =
  { Component.init; update; updaterec; view; targets = (fun _ -> [ "board" ]) }
