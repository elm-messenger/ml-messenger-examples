(* Types the Play scene's children share. Plain, so children may read it. *)

type report = { moves : int; status : Rules.status; can_undo : bool; thermal : bool }

(* The playing area, below the top bar and above the legend. *)
let board_area = (32., 84., 1216., 572.)
