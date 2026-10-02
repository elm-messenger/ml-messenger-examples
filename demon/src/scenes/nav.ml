(* Scene changes, all through a short fade to the background colour. *)

open Messenger
module T = Messenger_extra.Transition_transitions

let go target : User_data.t Scene.scene_output_msg =
  Messenger_extra.Transition_model.gen_sequential_transition_som
    (T.fade_out_with_color Pal.ink, 220.)
    (T.fade_in_with_color Pal.ink, 220.)
    target

let title () = go (Scene.By_name "Title")
let select () = go (Scene.By_name "Select")
let level i = go (Scene.By_key (Play_params.key, { Play_params.index = i }))
