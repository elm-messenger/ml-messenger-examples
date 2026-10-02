(* One level on the select screen. Hovering reports it, clicking picks it;
   the focused level comes from the parent's common data. *)

open Ml_regl_core
open Messenger
open Regl_builtin_programs

type msg = Hovered of int | Picked of int
type init = { index : int }
type data = { index : int; x : float; y : float }

let init _runtime _env ({ index } : init) =
  let x, y = Common.tile_pos index in
  { index; x; y }

let hit d (mx, my) =
  mx >= d.x && mx <= d.x +. Common.tile_w && my >= d.y && my <= d.y +. Common.tile_h

let update _runtime env evnt d =
  match evnt with
  | Regl_proto.MouseMove { x; y } when hit d (x, y) ->
      (d, [ Component.Parent (Hovered d.index) ], (env, false))
  | MouseDown { button = 1; x; y } when hit d (x, y) ->
      (d, [ Component.Parent (Picked d.index) ], (env, true))
  | _ -> (d, [], (env, false))

let updaterec _runtime env _msg d = (d, [], env)

let view _runtime (env : (Common.t, User_data.t) Base.env) d =
  let id = Levels.ids.(d.index) in
  let solved = User_data.solved env.global_data.user_data id in
  let focus = env.common_data.focus = d.index in
  let cx = d.x +. (Common.tile_w /. 2.) and cy = d.y +. (Common.tile_h /. 2.) in
  let lift = if focus then -2. else 0. in
  let bg = if solved then Pal.mix Pal.panel Pal.gold 0.1 else Pal.panel in
  ( Regl_common.group []
      [
        (if focus then
           rounded_rect (cx, cy +. lift) (Common.tile_w +. 6., Common.tile_h +. 6.) 11. Pal.gold
         else empty);
        rounded_rect (cx, cy +. lift) (Common.tile_w, Common.tile_h) 9.
          (if focus then Pal.panel_edge else bg);
        Widgets.mono_centered ~font:Widgets.mono_bold (cx, cy +. lift) 16. id
          (if focus then Pal.cream else if solved then Pal.gold else Pal.cream_dim);
        (if solved then circle (d.x +. Common.tile_w -. 9., d.y +. 9. +. lift) 3.5 Pal.gold else empty);
      ],
    0 )

let component =
  {
    Component.init;
    update;
    updaterec;
    view;
    targets = (fun (d : data) -> [ d.index ]);
  }
