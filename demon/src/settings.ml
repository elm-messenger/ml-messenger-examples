(* The settings panel: a global component above every scene. The gear in the
   top-right corner, O anywhere, or Esc on the title opens it; it sets the
   master volume and fullscreen (also F11 anywhere), erases the progress, and
   quits. While it is open it takes every key press, click and mouse move;
   ticks and releases still reach the scene, so a key held when it opened
   does not stay held there. The settings are saved under [storage_key] as
   "volume=80;fullscreen=0". *)

open Ml_regl_core
open Messenger
open Regl_builtin_programs

type msg = Open | Close

let key : msg Global_component.key = Global_component.key "settings"
let storage_key = "maxwell.settings"

(* The browser host ignores fullscreen, and quitting there only stops the
   loop: both are offered on the desktop only. *)
let desktop = match Sys.backend_type with Native | Bytecode -> true | Other _ -> false

(* Scenes keep this corner free. *)
let gear = { Widgets.key = ""; label = ""; x = 1214.; y = 19.; w = 34.; h = 34. }

type item = Volume | Fullscreen | Reset | Resume | Quit

let rows = (Volume :: (if desktop then [ Fullscreen ] else [])) @ [ Reset ]
let actions = Resume :: (if desktop then [ Quit ] else [])
let items = rows @ actions

(* Layout of the card, centred on the screen. *)
let card_w = 600.
let row_h = 58.
let row_step = row_h +. 8.
let card_h = 112. +. (float (List.length rows) *. row_step) +. 16. +. 58. +. 64.
let card_x = 640. -. (card_w /. 2.)
let card_y = 360. -. (card_h /. 2.)
let row_x = card_x +. 28.
let row_w = card_w -. 56.
let row_y i = card_y +. 112. +. (float i *. row_step)
let row_boxes = List.mapi (fun i r -> (r, (row_x, row_y i, row_w, row_h))) rows
let in_box (mx, my) (x, y, w, h) = mx >= x && mx <= x +. w && my >= y && my <= y +. h

(* The volume slider's track. *)
let track_x0 = row_x +. 236.
let track_x1 = row_x +. row_w -. 92.

let action_buttons =
  let n = float (List.length actions) and bw = 262. and gap = 16. in
  let x0 = 640. -. (((n *. bw) +. ((n -. 1.) *. gap)) /. 2.) in
  let by = card_y +. card_h -. 64. -. 58. in
  List.mapi
    (fun i a ->
      let text, hint = match a with Quit -> ("Quit game", "") | _ -> ("Resume", "ESC") in
      (a, { Widgets.text; hint; bx = x0 +. (float i *. (bw +. gap)); by; bw; bh = 58. }))
    actions

let item_at p =
  match List.find_opt (fun (_, b) -> in_box p b) row_boxes with
  | Some (it, _) -> Some it
  | None -> Option.map fst (List.find_opt (fun (_, b) -> Widgets.button_inside p b) action_buttons)

type data = {
  shown : bool;
  opened_at : float;
  clock : Clock.t;
  focus : item;
  mouse : float * float;
  dragging : bool;  (** the volume knob is held *)
  volume : float;  (** 0 to 1 *)
  fullscreen : bool;
  armed : bool;  (** Reset was pressed once: the next press erases *)
  asked : bool;  (** the saved settings have been requested *)
}

let clamp01 x = Float.max 0. (Float.min 1. x)

let serialize d =
  Printf.sprintf "volume=%d;fullscreen=%d"
    (Float.to_int (Float.round (d.volume *. 100.)))
    (if d.fullscreen then 1 else 0)

let deserialize s d =
  List.fold_left
    (fun d e ->
      match String.split_on_char '=' e with
      | [ "volume"; v ] -> (
          match int_of_string_opt v with
          | Some v -> { d with volume = clamp01 (float v /. 100.) }
          | None -> d)
      | [ "fullscreen"; f ] -> { d with fullscreen = f = "1" }
      | _ -> d)
    d (String.split_on_char ';' s)

let save d = Scene.SOMSaveValue (storage_key, serialize d)

let window fullscreen =
  Scene.SOMConfigWindow { Regl_proto.default_window_config with fullscreen = Some fullscreen }

let open_panel d =
  { d with shown = true; opened_at = d.clock.elapsed; focus = Volume; armed = false; dragging = false }

let close d = { d with shown = false; armed = false; dragging = false }

(* While dragging: no sound until the knob is let go. *)
let drag_volume d x =
  let v = Float.round (clamp01 ((x -. track_x0) /. (track_x1 -. track_x0)) *. 100.) /. 100. in
  if v = d.volume then (d, []) else ({ d with volume = v }, [ Scene.SOMSetVolume v ])

(* The keys move by tenths, snapping a dragged value to the next tenth. *)
let step_volume d dir =
  let t = d.volume *. 10. in
  let t = if dir > 0 then Float.floor (t +. 1e-6) +. 1. else Float.ceil (t -. 1e-6) -. 1. in
  let v = clamp01 (t /. 10.) in
  if v = d.volume then (d, [])
  else
    let d = { d with volume = v } in
    (d, [ Scene.SOMSetVolume v; Sfx.play "select"; save d ])

let toggle_fullscreen d =
  if not desktop then (d, [])
  else
    let d = { d with fullscreen = not d.fullscreen } in
    (d, [ window d.fullscreen; Sfx.play "select"; save d ])

let reset env d =
  let u = env.Base.global_data.user_data in
  if u.User_data.best = [] then (d, [], env)
  else if not d.armed then ({ d with armed = true }, [ Sfx.play "select" ], env)
  else
    ( { d with armed = false },
      [ Sfx.play "undo"; Scene.SOMSaveValue (User_data.storage_key, User_data.serialize User_data.default) ],
      { env with global_data = { env.global_data with user_data = User_data.default } } )

let activate env d = function
  | Volume -> (d, [], env)
  | Fullscreen ->
      let d, soms = toggle_fullscreen d in
      (d, soms, env)
  | Reset -> reset env d
  | Resume -> (close d, [ Sfx.play "select" ], env)
  | Quit -> (d, [ Scene.SOMQuit ], env)

let focus_on d it = if it = d.focus then d else { d with focus = it; armed = false }

let move_focus d dir =
  let n = List.length items in
  let i = Option.get (List.find_index (( = ) d.focus) items) in
  (focus_on d (List.nth items ((i + dir + n) mod n)), [ Sfx.play "select" ])

let sideways env d dir =
  match d.focus with
  | Volume ->
      let d, soms = step_volume d dir in
      (d, soms, env)
  | Fullscreen when dir > 0 <> d.fullscreen ->
      let d, soms = toggle_fullscreen d in
      (d, soms, env)
  | Fullscreen | Reset -> (d, [], env)
  | (Resume | Quit) as a -> (
      let i = Option.get (List.find_index (( = ) a) actions) + dir in
      match List.nth_opt actions i with
      | Some b when i >= 0 -> (focus_on d b, [ Sfx.play "select" ], env)
      | _ -> (d, [], env))

let closed_input env evnt d =
  match evnt with
  | Regl_proto.MouseMove { x; y } -> ({ d with mouse = (x, y) }, [], env, false)
  | MouseDown { button = 1; x; y } when Widgets.inside (x, y) gear ->
      (open_panel d, [ Sfx.play "select" ], env, true)
  | KeyDown "O" -> (open_panel d, [ Sfx.play "select" ], env, true)
  | KeyDown "F11" ->
      let d, soms = toggle_fullscreen d in
      (d, soms, env, true)
  | _ -> (d, [], env, false)

let open_input env evnt d =
  let d, soms, env =
    match evnt with
    | Regl_proto.MouseMove { x; y } -> (
        let d = { d with mouse = (x, y) } in
        if d.dragging then
          let d, soms = drag_volume d x in
          (d, soms, env)
        else match item_at (x, y) with Some it -> (focus_on d it, [], env) | None -> (d, [], env))
    | MouseDown { button = 1; x; y } -> (
        match item_at (x, y) with
        | Some Volume when x >= track_x0 -. 14. && x <= track_x1 +. 14. ->
            let d, soms = drag_volume { (focus_on d Volume) with dragging = true } x in
            (d, soms, env)
        | Some Volume -> (focus_on d Volume, [], env)
        | Some it -> activate env (focus_on d it) it
        | None when not (in_box (x, y) (card_x, card_y, card_w, card_h)) ->
            (close d, [ Sfx.play "select" ], env)
        | None -> (d, [], env))
    | MouseUp { button = 1; _ } when d.dragging ->
        ({ d with dragging = false }, [ Sfx.play "select"; save d ], env)
    | KeyDown ("Escape" | "O") -> (close d, [ Sfx.play "select" ], env)
    | KeyDown ("Up" | "W") ->
        let d, soms = move_focus d (-1) in
        (d, soms, env)
    | KeyDown ("Down" | "S" | "Tab") ->
        let d, soms = move_focus d 1 in
        (d, soms, env)
    | KeyDown ("Left" | "A") -> sideways env d (-1)
    | KeyDown ("Right" | "D") -> sideways env d 1
    | KeyDown ("Return" | "Space" | "KP Enter") -> activate env d d.focus
    | KeyDown "F11" ->
        let d, soms = toggle_fullscreen d in
        (d, soms, env)
    | _ -> (d, [], env)
  in
  let block = match evnt with KeyDown _ | MouseDown _ | MouseMove _ -> true | _ -> false in
  (d, soms, env, block)

let update _runtime env evnt d bdata =
  let d, soms, env, block =
    match evnt with
    | Regl_proto.UpdateTick t -> ({ d with clock = Clock.tick d.clock t }, [], env, false)
    | ValueRead { key; value = Some v } when key = storage_key ->
        let d = deserialize v d in
        let fullscreen = if desktop && d.fullscreen then [ window true ] else [] in
        (d, Scene.SOMSetVolume d.volume :: fullscreen, env, false)
    | _ when d.shown -> open_input env evnt d
    | _ -> closed_input env evnt d
  in
  let d, soms =
    if d.asked then (d, soms) else ({ d with asked = true }, Scene.SOMReadValue storage_key :: soms)
  in
  ((d, bdata), soms, (env, block))

let updaterec _runtime env msg d bdata =
  match msg with
  | Open when not d.shown -> ((open_panel d, bdata), [ Sfx.play "select" ], env)
  | Close when d.shown -> ((close d, bdata), [], env)
  | Open | Close -> ((d, bdata), [], env)

(* A cog: eight teeth around a ring with a hole of the background colour. *)
let draw_gear (cx, cy) r color hole =
  Regl_common.group []
    (List.init 8 (fun k ->
         let a = float k *. Float.pi /. 4. in
         rect_centered (cx +. (r *. cos a), cy +. (r *. sin a)) (r *. 0.55, r *. 0.55) a color)
    @ [ circle (cx, cy) (r *. 0.84) color; circle (cx, cy) (r *. 0.36) hole ])

let gear_button d =
  let hover = Widgets.inside d.mouse gear in
  let bg = if hover then Pal.panel_edge else Pal.mix Pal.panel Pal.panel_edge 0.55 in
  let cx = gear.x +. (gear.w /. 2.) and cy = gear.y +. (gear.h /. 2.) in
  let tip = "SETTINGS  O" in
  let tw = Widgets.mono_width 11. tip +. 20. in
  Regl_common.group []
    [
      rounded_rect (cx, cy) (gear.w, gear.h) 8. bg;
      draw_gear (cx, cy) 9. (if hover then Pal.cream else Pal.mix Pal.cream Pal.cream_dim 0.4) bg;
      (if hover then
         Regl_common.group []
           [
             rounded_rect (gear.x +. gear.w -. (tw /. 2.), 70.) (tw, 22.) 6. Pal.panel_edge;
             Widgets.mono_text (gear.x +. gear.w -. tw +. 10., 70.) 11. tip Pal.cream;
           ]
       else empty);
    ]

(* A row's label and caption on the left; its control is drawn by the caller. *)
let row_frame d it (x, y, w, h) label caption =
  let focus = d.focus = it in
  let cy = y +. (h /. 2.) in
  Regl_common.group []
    [
      rounded_rect (x +. (w /. 2.), cy) (w, h) 10.
        (if focus then Pal.panel_edge else Pal.mix Pal.panel Pal.panel_edge 0.3);
      (if focus then rounded_rect (x +. 3., cy) (6., h -. 20.) 3. Pal.gold else empty);
      Widgets.mono_text ~font:Widgets.mono_bold (x +. 22., cy -. 8.) 15. label
        (if focus then Pal.cream else Pal.cream_dim);
      Widgets.mono_text (x +. 22., cy +. 13.) 11. caption (Pal.with_alpha Pal.cream_dim 0.8);
    ]

let volume_row d box =
  let _, y, _, h = box in
  let cy = y +. (h /. 2.) in
  let kx = track_x0 +. (d.volume *. (track_x1 -. track_x0)) in
  let focus = d.focus = Volume in
  let pct = if d.volume = 0. then "MUTED" else Printf.sprintf "%d%%" (Float.to_int (Float.round (d.volume *. 100.))) in
  let tw = track_x1 -. track_x0 in
  Regl_common.group []
    [
      row_frame d Volume box "VOLUME" "LEFT / RIGHT, or drag";
      rounded_rect (track_x0 +. (tw /. 2.), cy) (tw, 6.) 3. Pal.ink;
      (if d.volume > 0. then rounded_rect (track_x0 +. ((kx -. track_x0) /. 2.), cy) (kx -. track_x0, 6.) 3. Pal.gold
       else empty);
      (if focus then circle (kx, cy) 13. (Pal.with_alpha Pal.gold 0.35) else empty);
      circle (kx, cy) 9. (if focus || d.dragging then Pal.cream else Pal.cream_dim);
      Widgets.mono_text ~font:Widgets.mono_bold
        (row_x +. row_w -. 22. -. Widgets.mono_width 15. pct, cy)
        15. pct
        (if d.volume = 0. then Pal.hot_text else Pal.cream);
    ]

let fullscreen_row d box =
  let _, y, _, h = box in
  let cy = y +. (h /. 2.) in
  let sw = 60. and sh = 30. in
  let sx = row_x +. row_w -. 22. -. (sw /. 2.) in
  let on = d.fullscreen in
  let state = if on then "ON" else "OFF" in
  Regl_common.group []
    [
      row_frame d Fullscreen box "FULLSCREEN" "ENTER, or F11 anywhere";
      rounded_rect (sx, cy) (sw, sh) (sh /. 2.) (if on then Pal.gold else Pal.ink);
      circle ((if on then sx +. 15. else sx -. 15.), cy) 10. (if on then Pal.ink else Pal.cream_dim);
      Widgets.mono_text ~font:Widgets.mono_bold
        (sx -. (sw /. 2.) -. 14. -. Widgets.mono_width 15. state, cy)
        15. state
        (if on then Pal.gold else Pal.cream_dim);
    ]

let reset_row env d box =
  let _, y, _, h = box in
  let cy = y +. (h /. 2.) in
  let u = env.Base.global_data.user_data in
  let solved = List.length u.User_data.best in
  let caption =
    if d.armed then "This cannot be undone"
    else Printf.sprintf "%d of %d experiments solved" solved Levels.count
  in
  let right = row_x +. row_w -. 22. in
  let control =
    if solved = 0 then
      Widgets.mono_text (right -. Widgets.mono_width 13. "Nothing to erase", cy) 13. "Nothing to erase"
        (Pal.with_alpha Pal.cream_dim 0.7)
    else
      let text = if d.armed then "Press again to erase" else "Erase" in
      let bw = Widgets.mono_width 14. text +. 32. in
      Regl_common.group []
        [
          rounded_rect (right -. (bw /. 2.), cy) (bw, 34.) 8.
            (if d.armed then Pal.hot_text else Pal.with_alpha Pal.hot_text 0.25);
          Widgets.mono_text ~font:Widgets.mono_bold (right -. bw +. 16., cy) 14. text
            (if d.armed then Pal.ink else Pal.hot_text);
        ]
  in
  Regl_common.group [] [ row_frame d Reset box "PROGRESS" caption; control ]

let panel env d =
  let t = Float.min 1. ((d.clock.elapsed -. d.opened_at) /. 200.) in
  let e = 1. -. ((1. -. t) ** 3.) in
  let row (it, box) =
    match it with
    | Volume -> volume_row d box
    | Fullscreen -> fullscreen_row d box
    | _ -> reset_row env d box
  in
  let card =
    [
      rounded_rect (640., card_y +. (card_h /. 2.)) (card_w +. 8., card_h +. 8.) 22. Pal.panel_edge;
      rounded_rect (640., card_y +. (card_h /. 2.)) (card_w, card_h) 18. Pal.panel;
      rect (card_x +. 26., card_y +. 20.) (card_w -. 52., 3.) Pal.gold;
      textbox_centered (640., card_y +. 66.) 56. "Settings" Widgets.display Pal.cream;
    ]
    @ List.map row row_boxes
    @ List.map
        (fun (a, b) ->
          Widgets.draw_button ~focus:(d.focus = a) ~bg:(Pal.mix Pal.panel Pal.panel_edge 0.3) b)
        action_buttons
    @ [
        Widgets.mono_centered (640., card_y +. card_h -. 30.) 12.
          "ARROWS choose  ·  LEFT / RIGHT change  ·  ENTER select  ·  ESC close"
          (Pal.with_alpha Pal.cream_dim 0.8);
      ]
  in
  Regl_common.group [ Regl_effects.alpha_mult e ]
    (rect (0., 0.) (1280., 720.) (Pal.with_alpha Pal.ink 0.78) :: card)

let view _runtime env d _bdata = if d.shown then panel env d else gear_button d

let component : (data, msg, User_data.t) Scene.concrete_global_component =
  {
    init =
      (fun runtime _env ->
        ( {
            shown = false;
            opened_at = 0.;
            clock = Clock.start;
            focus = Volume;
            mouse = (-1., -1.);
            dragging = false;
            volume = Base.get_volume runtime;
            fullscreen = false;
            armed = false;
            asked = false;
          },
          { Scene.dead = false; post_processor = Fun.id } ));
    update;
    updaterec;
    view;
    key;
  }

let gc = Global_component.make component
