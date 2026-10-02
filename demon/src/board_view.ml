(* Draws a board: floor, goals, walls, blocks, glass and the demon, with the
   edges that decide how things stick and conduct.

   Visual language:
   - conducting edge: a copper strip;
   - insulating edge: a thick cream felt pad with stitches;
   - a side with no edge is sticky: flush against an edgeless neighbour the
     two bodies merge into one piece, otherwise it shows lime glue teeth;
   - walls (fixed neutral blocks) are dark stone, all alike, as in the CLI's
     '#': whether a wall stands on an outer cell is not drawn;
   - fixed hot/cold blocks and fixed glass have bolts in their corners;
     movable blocks are crates with a raised panel.

   Objects are drawn at interpolated positions and colours so a move or a
   heat change animates: [from] is the state before the last turn and [t]
   how far the animation is (0 to 1). *)

open Ml_regl_core
open Regl_builtin_programs
module R = Rules

type layout = { ox : float; oy : float; s : float }

(* The largest whole-pixel cell that fits the board in [area], centred. *)
let fit (lv : R.level) ~area:(ax, ay, aw, ah) ~max_cell =
  let s =
    Float.floor
      (Float.min max_cell
         (Float.min (aw /. float lv.w) (ah /. float lv.h)))
  in
  let bw = s *. float lv.w and bh = s *. float lv.h in
  {
    ox = Float.round (ax +. ((aw -. bw) /. 2.));
    oy = Float.round (ay +. ((ah -. bh) /. 2.));
    s;
  }

let cell_xy l i (lv : R.level) =
  (l.ox +. (float (i mod lv.w) *. l.s), l.oy +. (float (i / lv.w) *. l.s))

(* Animation state handed to [draw]. *)
type anim = {
  from : R.obj array;  (** the state before the last turn *)
  t : float;  (** movement progress, 0..1 *)
  heat_t : float;  (** heat colour progress, 0..1 *)
  bump : (int * float) option;  (** blocked move: direction and progress *)
  facing_left : bool;
  now : float;  (** ms, for idle motion *)
  dead_for : float option;  (** ms since the demon overheated *)
  thermal : bool;  (** show conducting contacts *)
}

let still objs now =
  {
    from = objs;
    t = 1.;
    heat_t = 1.;
    bump = None;
    facing_left = false;
    now;
    dead_for = None;
    thermal = false;
  }

let ease_out t = 1. -. ((1. -. t) ** 3.)

(* ---------- colours ---------- *)

let block_colors (info : R.info) heat =
  match (info.fixed, heat) with
  | true, R.Hot -> (Pal.hot_stone, Pal.mix Pal.hot_stone Pal.hot_block 0.35)
  | true, R.Cold -> (Pal.cold_stone, Pal.mix Pal.cold_stone Pal.cold_block 0.35)
  | true, R.Neutral -> (Pal.wall, Pal.wall_light)
  | false, R.Hot -> (Pal.hot_block, Pal.hot_light)
  | false, R.Cold -> (Pal.cold_block, Pal.cold_light)
  | false, R.Neutral -> (Pal.neutral_block, Pal.neutral_light)

(* Fixed objects that are not walls: fixed hot/cold blocks and fixed glass.
   Fixed objects never move, so the level's start says what each one is. *)
let bolted (lv : R.level) i =
  lv.info.(i).fixed
  &&
  match lv.start.(i) with
  | { kind = R.Glass; _ } | { kind = R.Block; heat = R.Hot | R.Cold; _ } -> true
  | _ -> false

(* Drawn over the edges, inside the widest (felt) strip. *)
let bolts (x, y) s =
  let k = s *. 0.27 and rad = Float.max 1.8 (s *. 0.055) in
  List.map
    (fun (u, v) -> circle (x +. u, y +. v) rad Pal.bolt)
    [ (k, k); (s -. k, k); (k, s -. k); (s -. k, s -. k) ]

let glass_tint = function
  | R.Hot -> Pal.hex ~a:0.42 "ff6a3a"
  | R.Cold -> Pal.hex ~a:0.42 "4aa8ff"
  | R.Neutral -> Pal.hex ~a:0.16 "cfe6ff"

let heat_text = function
  | R.Hot -> Pal.hot_text
  | R.Cold -> Pal.cold_text
  | R.Neutral -> Pal.cream

(* ---------- pieces ---------- *)

(* The strip of thickness [t] along side [d] of the box. *)
let side_rect (bx, by, bw, bh) d t =
  if d = R.up then (bx, by, bw, t)
  else if d = R.down then (bx, by +. bh -. t, bw, t)
  else if d = R.left then (bx, by, t, bh)
  else (bx +. bw -. t, by, t, bh)

let rect4 (x, y, w, h) c = rect (x, y) (w, h) c

let conduct_edge box d s =
  let t = Float.max 2. (s *. 0.11) in
  let hl = Float.max 1. (t *. 0.38) in
  (* the highlight sits on the outer side of the strip *)
  [ rect4 (side_rect box d t) Pal.copper; rect4 (side_rect box d hl) Pal.copper_light ]

let insulate_edge box d s =
  let t = Float.max 3. (s *. 0.17) in
  let x, y, w, h = side_rect box d t in
  let horizontal = d = R.up || d = R.down in
  let len = if horizontal then w else h in
  let n = 3 in
  let dash = len /. float ((2 * n) + 1) in
  let th = Float.max 1. (s *. 0.035) in
  let stitches =
    List.init n (fun k ->
        let a = dash *. float ((2 * k) + 1) in
        if horizontal then rect (x +. a, y +. ((t -. th) /. 2.)) (dash, th) Pal.stitch
        else rect (x +. ((t -. th) /. 2.), y +. a) (th, dash) Pal.stitch)
  in
  rect (x, y) (w, h) Pal.cream :: stitches

(* Glue teeth pointing out of side [d]. *)
let sticky_side box d s =
  let x, y, w, h = box in
  let n = 4 and depth = Float.max 2. (s *. 0.08) in
  let base = Float.max 1. (s *. 0.04) in
  let strip = rect4 (side_rect box d base) Pal.glue in
  let tooth k =
    let a = (float k +. 0.5) /. float n and hw = 0.5 /. float n in
    if d = R.up || d = R.down then
      let edge_y = if d = R.up then y else y +. h in
      let tip = if d = R.up then edge_y -. depth else edge_y +. depth in
      triangle
        (x +. (w *. (a -. hw)), edge_y)
        (x +. (w *. (a +. hw)), edge_y)
        (x +. (w *. a), tip) Pal.glue
    else
      let edge_x = if d = R.left then x else x +. w in
      let tip = if d = R.left then edge_x -. depth else edge_x +. depth in
      triangle
        (edge_x, y +. (h *. (a -. hw)))
        (edge_x, y +. (h *. (a +. hw)))
        (tip, y +. (h *. a)) Pal.glue
  in
  strip :: List.init n tooth

(* Flush sides: both touching sides edgeless and both movers (walls included),
   or a fixed object against the board's border. *)
let merged_sides (lv : R.level) objs i =
  let o = objs.(i) in
  let r = i / lv.w and c = i mod lv.w in
  Array.init 4 (fun d ->
      let nr = r + R.dr.(d) and nc = c + R.dc.(d) in
      if not (R.inside lv nr nc) then lv.info.(i).fixed
      else
        let nb = objs.(R.index lv nr nc) in
        R.is_mover o && R.is_mover nb
        && o.edges.(d) = R.No_edge
        && nb.edges.(R.opposite d) = R.No_edge)

let body_box (x, y) s merged =
  let g = Float.max 1.5 (s *. 0.05) in
  let inset d = if merged.(d) then 0. else g in
  ( x +. inset R.left,
    y +. inset R.up,
    s -. inset R.left -. inset R.right,
    s -. inset R.up -. inset R.down )

let edges_and_glue (lv : R.level) objs i box s =
  let o = objs.(i) in
  let r = i / lv.w and c = i mod lv.w in
  List.concat
    (List.init 4 (fun d ->
         match o.R.edges.(d) with
         | R.Conduct -> conduct_edge box d s
         | R.Insulate -> insulate_edge box d s
         | R.No_edge ->
             let nr = r + R.dr.(d) and nc = c + R.dc.(d) in
             let against_fixed =
               R.inside lv nr nc && lv.info.(R.index lv nr nc).fixed
             in
             (* glue between two fixed things never matters *)
             if
               R.shows_sticky lv objs r c d
               && not (lv.info.(i).fixed && against_fixed)
             then sticky_side box d s
             else []))

let heat_icon (x, y) s heat alpha =
  let name = match heat with R.Hot -> "flame" | R.Cold -> "snow" | R.Neutral -> "" in
  if name = "" || alpha <= 0.01 then empty
  else
    centered_texture_with_alpha
      (x +. (s /. 2.), y +. (s /. 2.))
      (s *. 0.46, s *. 0.46)
      0. alpha name

let draw_block lv objs i (x, y) s (base, light) ~heat ~heat_alpha =
  let info = lv.R.info.(i) in
  let merged = merged_sides lv objs i in
  let ((bx, by, bw, bh) as box) = body_box (x, y) s merged in
  let body =
    if info.fixed then
      (* stone: a bevel on the open top side only *)
      [
        rect (bx, by) (bw, bh) base;
        (if merged.(R.up) then empty
         else rect (bx, by) (bw, Float.max 1. (s *. 0.08)) light);
      ]
    else
      [
        rect (bx, by) (bw, bh) base;
        rounded_rect
          (x +. (s /. 2.), y +. (s /. 2.))
          (s *. 0.62, s *. 0.62)
          (s *. 0.08) light;
        rounded_rect
          (x +. (s /. 2.), y +. (s *. 0.53))
          (s *. 0.5, s *. 0.5)
          (s *. 0.06) (Pal.mix light base 0.45);
      ]
  in
  Regl_common.group []
    (body
    @ [ heat_icon (x, y) s heat heat_alpha ]
    @ edges_and_glue lv objs i box s
    @ if bolted lv i then bolts (x, y) s else [])

let draw_glass lv objs i (x, y) s tint =
  let merged = merged_sides lv objs i in
  let ((bx, by, bw, bh) as box) = body_box (x, y) s merged in
  let shine = Pal.hex ~a:0.35 "ffffff" in
  let fixed_marks = if lv.R.info.(i).fixed then bolts (x, y) s else [] in
  let glue = edges_and_glue lv objs i box s in
  Regl_common.group []
    ([
       rect (bx, by) (bw, bh) tint;
       quad
         (x +. (s *. 0.22), y +. (s *. 0.78))
         (x +. (s *. 0.34), y +. (s *. 0.78))
         (x +. (s *. 0.78), y +. (s *. 0.34))
         (x +. (s *. 0.78), y +. (s *. 0.22))
         shine;
       quad
         (x +. (s *. 0.46), y +. (s *. 0.8))
         (x +. (s *. 0.52), y +. (s *. 0.8))
         (x +. (s *. 0.8), y +. (s *. 0.52))
         (x +. (s *. 0.8), y +. (s *. 0.46))
         shine;
       lineloop
         [ (bx, by); (bx +. bw, by); (bx +. bw, by +. bh); (bx, by +. bh) ]
         (Pal.hex ~a:0.55 "e8f4ff");
     ]
    @ glue @ fixed_marks)

let draw_goal (x, y) s now =
  let cx = x +. (s /. 2.) and cy = y +. (s /. 2.) in
  let pulse = 0.75 +. (0.25 *. sin (now /. 380.)) in
  Regl_common.group []
    [
      circle (cx, cy) (s *. 0.34) (Pal.with_alpha Pal.gold (0.18 *. pulse));
      circle (cx, cy) (s *. 0.27) (Pal.with_alpha Pal.gold 0.9);
      circle (cx, cy) (s *. 0.21) Pal.floor;
      rect_centered (cx, cy) (s *. 0.2, s *. 0.2) (Float.pi /. 4.)
        (Pal.with_alpha Pal.gold pulse);
    ]

(* Gold corner brackets over a block that fills a goal. *)
let goal_brackets (x, y) s =
  let k = s *. 0.26 and t = Float.max 2. (s *. 0.07) and m = s *. 0.02 in
  let c = Pal.gold in
  Regl_common.group []
    [
      rect (x +. m, y +. m) (k, t) c;
      rect (x +. m, y +. m) (t, k) c;
      rect (x +. s -. m -. k, y +. m) (k, t) c;
      rect (x +. s -. m -. t, y +. m) (t, k) c;
      rect (x +. m, y +. s -. m -. t) (k, t) c;
      rect (x +. m, y +. s -. m -. k) (t, k) c;
      rect (x +. s -. m -. k, y +. s -. m -. t) (k, t) c;
      rect (x +. s -. m -. t, y +. s -. m -. k) (t, k) c;
    ]

let draw_player (x, y) s heat (a : anim) =
  let name =
    match heat with R.Hot -> "demon_hot" | R.Cold -> "demon_cold" | R.Neutral -> "demon"
  in
  let bob = if heat = R.Hot then 0. else s *. 0.03 *. sin (a.now /. 260.) in
  let shake =
    match a.dead_for with
    | Some ms when ms < 450. -> s *. 0.06 *. sin (ms /. 18.) *. (1. -. (ms /. 450.))
    | _ -> 0.
  in
  let size = s *. 1.0 in
  let cx = x +. (s /. 2.) +. shake and cy = y +. (s /. 2.) -. (s *. 0.04) +. bob in
  let l = cx -. (size /. 2.) and r = cx +. (size /. 2.) in
  let t = cy -. (size /. 2.) and b = cy +. (size /. 2.) in
  Regl_common.group []
    [
      (* shadow *)
      circle (x +. (s /. 2.), y +. (s *. 0.9)) (s *. 0.22) (Pal.hex ~a:0.35 "000000");
      (if a.facing_left then texture (r, t) (l, t) (l, b) (r, b) name
       else texture (l, t) (r, t) (r, b) (l, b) name);
    ]

(* A dot on every conducting contact that involves something that moves. *)
let thermal_links (lv : R.level) objs l =
  let s = l.s in
  let dots = ref [] in
  Array.iteri
    (fun i (o : R.obj) ->
      if o.kind <> R.Nothing then
        List.iter
          (fun d ->
            let r = i / lv.w and c = i mod lv.w in
            let nr = r + R.dr.(d) and nc = c + R.dc.(d) in
            if R.inside lv nr nc then begin
              let j = R.index lv nr nc in
              let nb = objs.(j) in
              if
                nb.R.kind <> R.Nothing
                && R.conducts objs i d j
                && not (lv.info.(i).fixed && lv.info.(j).fixed)
              then begin
                let x, y = cell_xy l i lv in
                let cx = x +. (s /. 2.) +. (float R.dc.(d) *. s /. 2.)
                and cy = y +. (s /. 2.) +. (float R.dr.(d) *. s /. 2.) in
                let col =
                  match o.heat with
                  | R.Hot -> Pal.hex "ffe08a"
                  | R.Cold -> Pal.hex "d8f3ff"
                  | R.Neutral -> Pal.cream
                in
                dots :=
                  circle (cx, cy) (s *. 0.08) col
                  :: circle (cx, cy) (s *. 0.125) Pal.ink
                  :: !dots
              end
            end)
          [ R.down; R.right ])
    objs;
  (* outlines (ink) first, fills on top *)
  let rec split outl fill = function
    | f :: o :: rest -> split (o :: outl) (f :: fill) rest
    | _ -> (outl, fill)
  in
  let outl, fill = split [] [] !dots in
  Regl_common.group [] (outl @ fill)

(* ---------- the whole board ---------- *)

let draw (lv : R.level) (objs : R.obj array) (l : layout) (a : anim) =
  let s = l.s in
  let n = lv.w * lv.h in
  let prev_pos = R.positions a.from in
  let prev_heat = Hashtbl.create 64 in
  Array.iter (fun (o : R.obj) -> if o.kind <> R.Nothing then Hashtbl.replace prev_heat o.id o.heat) a.from;
  let mt = ease_out a.t and ht = ease_out a.heat_t in
  (* display position of the object now at [i] *)
  let pos_of i (o : R.obj) =
    let x1, y1 = cell_xy l i lv in
    let x0, y0 =
      match Hashtbl.find_opt prev_pos o.id with
      | Some j -> cell_xy l j lv
      | None -> (x1, y1)
    in
    (x0 +. ((x1 -. x0) *. mt), y0 +. ((y1 -. y0) *. mt))
  in
  let old_heat (o : R.obj) =
    Option.value ~default:o.heat (Hashtbl.find_opt prev_heat o.id)
  in
  (* floor *)
  let floor =
    rect (l.ox, l.oy) (s *. float lv.w, s *. float lv.h) Pal.grout
    :: List.concat
         (List.init n (fun i ->
              if lv.info.(i).fixed && objs.(i).R.kind <> R.Nothing then []
              else
                let x, y = cell_xy l i lv in
                let r = i / lv.w and c = i mod lv.w in
                let col = if (r + c) mod 2 = 0 then Pal.floor else Pal.floor_alt in
                let tile = rect (x +. 1., y +. 1.) (s -. 2., s -. 2.) col in
                if lv.info.(i).goal then [ tile; draw_goal (x, y) s a.now ]
                else [ tile ]))
  in
  let fixed = ref [] and movers = ref [] and glow = ref [] and over = ref [] in
  let player = ref empty in
  Array.iteri
    (fun i (o : R.obj) ->
      let info = lv.info.(i) in
      match o.kind with
      | R.Nothing -> ()
      | R.Player ->
          let x, y = pos_of i o in
          let x, y =
            match a.bump with
            | Some (d, p) ->
                let k = s *. 0.14 *. sin (Float.pi *. p) in
                (x +. (float R.dc.(d) *. k), y +. (float R.dr.(d) *. k))
            | None -> (x, y)
          in
          player := draw_player (x, y) s o.heat a
      | R.Block | R.Glass ->
          let xy = pos_of i o in
          let h0 = old_heat o in
          let picture =
            if o.kind = R.Glass then
              draw_glass lv objs i xy s (Pal.mix (glass_tint h0) (glass_tint o.heat) ht)
            else
              let b0, l0 = block_colors info h0 and b1, l1 = block_colors info o.heat in
              let colors = (Pal.mix b0 b1 ht, Pal.mix l0 l1 ht) in
              let alpha = if h0 = o.heat then 1. else ht in
              draw_block lv objs i xy s colors ~heat:o.heat ~heat_alpha:alpha
          in
          if info.fixed then fixed := picture :: !fixed else movers := picture :: !movers;
          (* a pulse where heat just changed *)
          if h0 <> o.heat && a.heat_t < 1. && o.heat <> R.Neutral then begin
            let x, y = xy in
            glow :=
              circle
                (x +. (s /. 2.), y +. (s /. 2.))
                (s *. (0.5 +. (0.45 *. ht)))
                (Pal.with_alpha (heat_text o.heat) (0.55 *. (1. -. ht)))
              :: !glow
          end;
          if info.goal && o.kind = R.Block && a.t >= 1. then
            over := goal_brackets (cell_xy l i lv) s :: !over)
    objs;
  Regl_common.group []
    (floor @ !glow @ List.rev !fixed @ List.rev !movers @ !over
    @ [ !player; (if a.thermal then thermal_links lv objs l else empty) ])
