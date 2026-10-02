(* The Play scene's parameters and key, in a plain module so any scene can
   start a level without depending on the Play scene itself. *)

type t = { index : int }  (** position in [Levels.ids] *)

let key : t Messenger.Scene.key = Messenger.Scene.key "Play"
