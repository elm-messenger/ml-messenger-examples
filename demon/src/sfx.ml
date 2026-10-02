(* Sound effects: names of the audio resources and the message that plays
   one. Each kind of sound has its own channel so a step does not cut off a
   sizzle. *)

open Messenger

let names = [ "step"; "push"; "bump"; "heat"; "freeze"; "select"; "undo"; "win"; "die" ]

let resources : Resources.resource_defs =
  List.map (fun n -> ("sfx:" ^ n, Resources.Audio_res ("assets/sfx/" ^ n ^ ".wav"))) names

let channel = function
  | "step" | "push" | "bump" -> 1
  | "heat" | "freeze" -> 2
  | "win" | "die" -> 3
  | _ -> 4

let play name : _ Scene.scene_output_msg =
  Scene.SOMPlayAudio (channel name, "sfx:" ^ name, Audio_base.A_once None)
