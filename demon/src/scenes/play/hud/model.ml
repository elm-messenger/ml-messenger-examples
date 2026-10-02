(* The top bar (level, move counter, clickable key hints) and the legend
   along the bottom. *)

open Ml_regl_core
open Messenger
open Regl_builtin_programs

type action = Undo | Restart | Thermal | Levels

type msg =
  | Pressed of action  (** to the parent *)
  | Show of Common.report  (** from the parent *)

type init = {
  level_id : string;
  world : int;
  number : int;
  of_count : int;
  glass : bool;  (** the level has glass: show it in the legend *)
  fixed : bool;  (** the level has fixed blocks or glass besides walls *)
}

type data = {
  info : init;
  report : Common.report;
  mouse : float * float;
  chips : (action * Widgets.chip) list;
}

let actions =
  [ (Undo, ("Z", "Undo")); (Restart, ("R", "Reset")); (Thermal, ("T", "Heat links")); (Levels, ("Esc", "Levels")) ]

let init _runtime _env info =
  let chips =
    Widgets.chips_from_right ~right:(Settings.gear.x -. 10.) ~y:19. ~h:34. (List.map snd actions)
  in
  {
    info;
    report = { moves = 0; status = Rules.Playing; can_undo = false; thermal = false };
    mouse = (-1., -1.);
    chips = List.map2 (fun (a, _) c -> (a, c)) actions chips;
  }

let update _runtime env evnt d =
  match evnt with
  | Regl_proto.MouseMove { x; y } -> ({ d with mouse = (x, y) }, [], (env, false))
  | MouseDown { button = 1; x; y } -> (
      match List.find_opt (fun (_, c) -> Widgets.inside (x, y) c) d.chips with
      | Some (a, _) -> (d, [ Component.Parent (Pressed a) ], (env, true))
      | None -> (d, [], (env, false)))
  | _ -> (d, [], (env, false))

let updaterec _runtime env msg d =
  match msg with
  | Show report -> ({ d with report }, [], env)
  | Pressed _ -> (d, [], env)

let roman = function 1 -> "I" | 2 -> "II" | n -> string_of_int n

(* One legend entry: a swatch drawn in [box] and a label after it. *)
let legend_items (info : init) =
  let sw x y = (x, y, 30., 18.) in
  let swatch kind (x, y, w, h) =
    match kind with
    | `Copper ->
        [ rect (x, y) (w, h) Pal.neutral_block; rect (x, y) (w, 5.) Pal.copper; rect (x, y) (w, 2.) Pal.copper_light ]
    | `Felt ->
        [ rect (x, y) (w, h) Pal.neutral_block; rect (x, y) (w, 7.) Pal.cream; rect (x +. 5., y +. 3.) (6., 1.5) Pal.stitch; rect (x +. 18., y +. 3.) (6., 1.5) Pal.stitch ]
    | `Glue ->
        rect (x, y +. 4.) (w, h -. 4.) Pal.neutral_block
        :: rect (x, y +. 4.) (w, 2.) Pal.glue
        :: List.init 4 (fun k ->
            let a = x +. (w *. (float k +. 0.5) /. 4.) in
            triangle (a -. 3.5, y +. 4.) (a +. 3.5, y +. 4.) (a, y -. 1.) Pal.glue)
    | `Block ->
        (* a movable crate with its raised panel *)
        [
          rect (x +. 4., y -. 2.) (22., 22.) Pal.neutral_block;
          rounded_rect (x +. 15., y +. 9.) (14., 14.) 2. Pal.neutral_light;
        ]
    | `Fixed ->
        (* flat stone, bolted in the corners *)
        rect (x +. 4., y -. 2.) (22., 22.) Pal.hot_stone
        :: rect (x +. 4., y -. 2.) (22., 2.) (Pal.mix Pal.hot_stone Pal.hot_block 0.35)
        :: List.map
             (fun (u, v) -> circle (x +. 4. +. u, y -. 2. +. v) 1.8 Pal.bolt)
             [ (4.5, 4.5); (17.5, 4.5); (4.5, 17.5); (17.5, 17.5) ]
    | `Wall ->
        [
          rect (x, y) (w, h) Pal.wall;
          rect (x, y) (w, 3.) Pal.wall_light;
          lineloop [ (x, y); (x +. w, y); (x +. w, y +. h); (x, y +. h) ] Pal.panel_edge;
        ]
    | `Goal ->
        [
          circle (x +. (w /. 2.), y +. (h /. 2.)) 9. Pal.gold;
          circle (x +. (w /. 2.), y +. (h /. 2.)) 6.5 Pal.floor;
          rect_centered (x +. (w /. 2.), y +. (h /. 2.)) (6., 6.) (Float.pi /. 4.) Pal.gold;
        ]
    | `Glass ->
        [ rect (x, y) (w, h) (Pal.hex ~a:0.3 "cfe6ff"); lineloop [ (x, y); (x +. w, y); (x +. w, y +. h); (x, y +. h) ] (Pal.hex ~a:0.7 "e8f4ff") ]
  in
  let items =
    [ (`Copper, "conducts"); (`Felt, "insulates"); (`Glue, "sticky"); (`Block, "block") ]
    @ (if info.fixed then [ (`Fixed, "fixed") ] else [])
    @ [ (`Wall, "wall (drains heat)"); (`Goal, "goal") ]
    @ if info.glass then [ (`Glass, "glass") ] else []
  in
  let size = 13. in
  let widths = List.map (fun (_, l) -> 30. +. 8. +. Widgets.mono_width size l) items in
  let gap = 24. in
  let total = List.fold_left ( +. ) 0. widths +. (gap *. float (List.length items - 1)) in
  let y = 682. in
  let _, out =
    List.fold_left2
      (fun (x, acc) (kind, label) w ->
        let pieces = swatch kind (sw x y) in
        ( x +. w +. gap,
          (Widgets.mono_text (x +. 38., y +. 9.) size label Pal.cream_dim :: pieces) @ acc ))
      ((640. -. (total /. 2.)), [])
      items widths
  in
  out

let view _runtime _env d =
  let r = d.report in
  let count_color =
    match r.status with
    | Rules.Solved -> Pal.gold
    | Rules.Dead -> Pal.hot_text
    | Rules.Playing -> Pal.cream
  in
  let top =
    [
      rect (0., 0.) (1280., 72.) Pal.panel;
      rect (0., 72.) (1280., 2.) Pal.panel_edge;
      Widgets.mono_text (36., 18.)
        12.
        (Printf.sprintf "WORLD %s  ·  EXPERIMENT %d OF %d" (roman d.info.world) d.info.number d.info.of_count)
        Pal.cream_dim;
      textbox (34., 22.) 40. d.info.level_id Widgets.display Pal.cream;
      Widgets.mono_centered (640., 20.) 12. "MOVES" Pal.cream_dim;
      Widgets.mono_centered ~font:Widgets.mono_bold (640., 46.) 30. (Printf.sprintf "%03d" r.moves) count_color;
    ]
  in
  let chips =
    List.map
      (fun (a, c) ->
        let hover = Widgets.inside d.mouse c in
        let dim = a = Undo && not r.can_undo in
        if dim then Regl_common.group [ Regl_effects.alpha_mult 0.45 ] [ Widgets.draw_chip c ]
        else if a = Thermal && r.thermal then Widgets.draw_chip ~hover:true ~accent:Pal.hot_light c
        else Widgets.draw_chip ~hover c)
      d.chips
  in
  let bottom = rect (0., 664.) (1280., 56.) Pal.panel :: rect (0., 662.) (1280., 2.) Pal.panel_edge :: legend_items d.info in
  (Regl_common.group [] (top @ chips @ bottom), 10)

let component =
  { Component.init; update; updaterec; view; targets = (fun _ -> [ "hud" ]) }
