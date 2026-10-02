(* The palette: a night-time thermodynamics lab. *)

open Ml_regl_core

let hex ?(a = 1.) s =
  let c i = float (int_of_string ("0x" ^ String.sub s i 2)) /. 255. in
  Color.rgba (c 0) (c 2) (c 4) a

let ink = hex "15111c"        (* background *)
let panel = hex "1f1a29"
let panel_edge = hex "3a3150"
let floor = hex "2b2538"
let floor_alt = hex "2f2940"
let grout = hex "221d2c"
let wall = hex "1c1725"       (* walls: fixed neutral blocks *)
let wall_light = hex "2a2336"
let stone = hex "3b3549"      (* the title's partition *)
let stone_light = hex "4a4359"
let bolt = hex "d9cdb4"       (* marks fixed objects that are not walls *)
let cream = hex "f1e7d0"      (* text and insulating felt *)
let cream_dim = hex "a89f8c"
let stitch = hex "a8916a"
let copper = hex "dd8640"     (* conducting edge *)
let copper_light = hex "ffc07a"
let glue = hex "9be15d"       (* sticky side *)
let gold = hex "ffd166"       (* goals *)
let violet = hex "9a5cf5"
let neutral_block = hex "9e978b"
let neutral_light = hex "bdb6a9"
let hot_block = hex "f0552c"
let hot_light = hex "ffa047"
let cold_block = hex "2f8fe0"
let cold_light = hex "8fd0ff"
let hot_stone = hex "6e2a24"
let cold_stone = hex "24406e"
let hot_text = hex "ff7a45"
let cold_text = hex "6cc0ff"

let mix (a : Color.t) (b : Color.t) t =
  let la = Regl_common.to_rgba_list a and lb = Regl_common.to_rgba_list b in
  match List.map2 (fun x y -> x +. ((y -. x) *. t)) la lb with
  | [ r; g; b; a ] -> Color.rgba r g b a
  | _ -> a

let with_alpha (c : Color.t) alpha =
  match Regl_common.to_rgba_list c with
  | [ r; g; b; _ ] -> Color.rgba r g b alpha
  | _ -> c
