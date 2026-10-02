(* Shared with the tiles through [env.common_data]. *)

type t = { focus : int }

let tile_w = 62.
let tile_h = 48.
let columns = 10

(* Top-left corner of the tile for level [i]. *)
let tile_pos i =
  let n1 = List.length Levels.world1 in
  let world_y, k = if i < n1 then (152., i) else (376., i - n1) in
  (48. +. (float (k mod columns) *. 70.), world_y +. (float (k / columns) *. 56.))
