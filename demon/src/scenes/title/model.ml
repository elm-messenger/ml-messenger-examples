(* The title: the demon guarding the door between a cold and a hot chamber,
   and two buttons (continue where you left off, or pick a level). *)

open Ml_regl_core
open Messenger
open Regl_builtin_programs

type data = { clock : Clock.t; now : float; focus : int; mouse : float * float }

let init _runtime _env _params = { clock = Clock.start; now = 0.; focus = 0; mouse = (-1., -1.) }

let buttons (u : User_data.t) =
  let next = Levels.first_unsolved u in
  let first =
    if u.best = [] then ("Start", "ENTER") else ("Continue " ^ Levels.ids.(next), "ENTER")
  in
  [
    ({ Widgets.text = fst first; hint = snd first; bx = 340.; by = 572.; bw = 290.; bh = 60. }, Nav.level next);
    ({ Widgets.text = "Levels"; hint = "L"; bx = 650.; by = 572.; bw = 290.; bh = 60. }, Nav.select ());
  ]

let update _runtime env evnt d =
  let u = env.Base.global_data.user_data in
  let pick i = [ Sfx.play "select"; snd (List.nth (buttons u) i) ] in
  match evnt with
  | Regl_proto.UpdateTick t ->
      let clock = Clock.tick d.clock t in
      ({ d with clock; now = clock.elapsed }, [], env)
  | MouseMove { x; y } ->
      let focus =
        match List.find_index (fun (b, _) -> Widgets.button_inside (x, y) b) (buttons u) with
        | Some i -> i
        | None -> d.focus
      in
      ({ d with mouse = (x, y); focus }, [], env)
  | MouseDown { button = 1; x; y } -> (
      match List.find_index (fun (b, _) -> Widgets.button_inside (x, y) b) (buttons u) with
      | Some i -> (d, pick i, env)
      | None -> (d, [], env))
  | KeyDown ("Left" | "Right" | "Up" | "Down" | "A" | "D" | "W" | "S" | "Tab") ->
      ({ d with focus = 1 - d.focus }, [ Sfx.play "select" ], env)
  | KeyDown ("Return" | "Space" | "KP Enter") -> (d, pick d.focus, env)
  | KeyDown "L" -> (d, pick 1, env)
  | _ -> (d, [], env)

(* Fold [u] into [0, w] by reflecting at both ends: a bouncing coordinate. *)
let bounce u w =
  let m = Float.rem u (2. *. w) in
  let m = if m < 0. then m +. (2. *. w) else m in
  if m < w then m else (2. *. w) -. m

let molecules now =
  let t = now /. 1000. in
  let one k =
    let h = float ((k * 7919) mod 1000) /. 1000. and g = float ((k * 104729) mod 997) /. 997. in
    let hot = k mod 2 = 0 in
    let speed = if hot then 170. else 45. in
    let left, width = if hot then (660., 300.) else (320., 300.) in
    let vx = speed *. (0.5 +. h) *. if k mod 3 = 0 then -1. else 1. in
    let vy = speed *. (0.4 +. g) *. if k mod 5 = 0 then -1. else 1. in
    let x = left +. 14. +. bounce ((h *. 900.) +. (vx *. t)) (width -. 28.) in
    let y = 300. +. 14. +. bounce ((g *. 700.) +. (vy *. t)) (200. -. 28.) in
    let col, core = if hot then (Pal.hot_block, Pal.hot_light) else (Pal.cold_block, Pal.cold_light) in
    [ circle (x, y) 8. col; circle (x -. 2., y -. 2.) 3.5 core ]
  in
  List.concat (List.init 28 one)

let felt x y w h = [ rect (x, y) (w, h) Pal.cream ]

let view _runtime env d =
  let u = env.Base.global_data.user_data in
  let bob = 5. *. sin (d.now /. 420.) in
  Regl_common.group []
    ([
       clear Pal.ink;
       Widgets.mono_centered (640., 64.) 15. "M A X W E L L ' S" Pal.cream_dim;
       textbox_centered (640., 140.) 104. "Puzzling Demon" Widgets.display Pal.cream;
       Widgets.mono_centered (640., 222.) 14.
         "push blocks onto goals  ·  keep the heat away from the demon" Pal.cream_dim;
       (* the chamber *)
       rect (306., 286.) (668., 228.) Pal.panel;
       rect (320., 300.) (300., 200.) (Pal.mix Pal.ink Pal.cold_block 0.12);
       rect (660., 300.) (300., 200.) (Pal.mix Pal.ink Pal.hot_block 0.12);
     ]
    @ molecules d.now
    @ felt 306. 286. 668. 14. @ felt 306. 500. 668. 14.
    @ felt 306. 286. 14. 228. @ felt 960. 286. 14. 228.
    @ [
        (* the partition, with the demon's door *)
        rect (620., 300.) (40., 70.) Pal.stone;
        rect (620., 440.) (40., 60.) Pal.stone;
        rect (620., 300.) (40., 4.) Pal.stone_light;
        rect (617., 366.) (46., 6.) Pal.copper;
        rect (617., 434.) (46., 6.) Pal.copper;
        centered_texture (640., 404. +. bob) (124., 124.) 0. "demon_big";
      ]
    @ List.mapi
        (fun i (b, _) -> Widgets.draw_button ~focus:(i = d.focus) b)
        (buttons u)
    @ [
        Widgets.mono_centered (640., 690.) 11.
          "Fonts: DM Serif Display & Space Mono (SIL Open Font License)  ·  rules after Maxwell's Puzzling Demon"
          (Pal.with_alpha Pal.cream_dim 0.7);
      ])

let scene params runtime env = Scene.abstract { init; update; view } params runtime env
