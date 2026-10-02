(* Plays the game through Ui.update: title -> level 1-1 -> solve it -> banner,
   progress recorded; a death and its undo on 1-15; the select screen; the
   settings panel (volume, keys kept from the board, erasing progress). Drawn
   text is checked in the encoded frame (textbox strings are verbatim). *)

open Ml_regl_core
open Messenger

let input = Game.App.input
let failures = ref 0

let check name ok =
  Printf.printf "%s %s\n%!" (if ok then "ok  " else "FAIL") name;
  if not ok then incr failures

let draws model text =
  let frame = Bytes.to_string (Regl_common.encode_frame_pb (Ui.view input model)) in
  let n = String.length frame and m = String.length text in
  let rec go i = i + m <= n && (String.sub frame i m = text || go (i + 1)) in
  go 0

let feed model inp =
  let model, _audio, _out = Ui.update input model inp in
  model

let event model e = feed model (Regl_proto.Event e)
let now = ref 0.

let ticks model ms =
  let rec go model left =
    if left <= 0. then model
    else begin
      now := !now +. 16.;
      go (event model (UpdateTick !now)) (left -. 16.)
    end
  in
  go model ms

let press model key = ticks (event (event model (KeyDown key)) (KeyUp key)) 16.

let play model moves =
  String.fold_left
    (fun m c ->
      let key = match c with 'w' -> "W" | 'a' -> "A" | 's' -> "S" | _ -> "D" in
      ticks (press m key) 150.)
    model moves

let scene model = Base.get_current_scene model.Model.runtime
let user model = model.Model.env.global_data.user_data

(* The host would answer each Data_res with the file's contents. *)
let load_levels model =
  Array.fold_left
    (fun m id ->
      let path = "assets/levels/" ^ id ^ ".txt" in
      let data = In_channel.with_open_bin ("../" ^ path) In_channel.input_all in
      feed m (Regl_proto.REGLRecvMsg (REGLFileLoaded { path; data })))
    model Game.Levels.ids

let () =
  let m, _ = Ui.init input () in
  let m = ticks (load_levels m) 100. in
  check "starts on the title" (scene m = "Title");
  check "title offers Start" (draws m "Start");
  let m = ticks (press m "Escape") 300. in
  check "Esc on the title opens the settings" (scene m = "Title" && draws m "VOLUME");
  let m = ticks (press m "Escape") 300. in
  check "Esc closes them again" (not (draws m "VOLUME"));
  let m = ticks (press m "Return") 800. in
  check "Enter starts a level" (scene m = "Play");
  check "level 1-1 is shown" (draws m "1-1" && draws m "000");
  let m = play m "sdddddwaas" in
  check "moves are counted" (draws m "010");
  let m = ticks (press m "Z") 300. in
  check "undo takes one back" (draws m "009");
  let m = play m "sawww" in
  let m = ticks m 1000. in
  check "solving shows the banner" (draws m "Equilibrium." && draws m "Solved in 14 moves");
  check "the solve is recorded" (Game.User_data.best (user m) "1-1" = Some 14);
  let m = ticks (press m "Return") 800. in
  check "Enter goes to the next level" (scene m = "Play" && draws m "1-2");
  let m = ticks (press m "Escape") 800. in
  check "Esc opens the select screen" (scene m = "Select" && draws m "SOLVED 1 / 57");
  (* 1-15 is tile 14: go right 13 times from 1-2 (focused, first unsolved) *)
  let m = List.fold_left (fun m _ -> press m "Right") m (List.init 13 Fun.id) in
  check "the preview follows the focus" (draws m "1-15");
  let m = ticks (press m "Return") 800. in
  check "Enter starts the focused level" (scene m = "Play" && draws m "1-15");
  let m = ticks (play m "dddddd") 1200. in
  check "touching heat kills the demon" (draws m "Overheated!");
  let m = ticks (press m "Z") 300. in
  check "undo from the banner revives it" ((not (draws m "Overheated!")) && draws m "005");
  let m = ticks (press m "R") 400. in
  check "reset goes back to the start" (draws m "000");
  (* the settings panel, over the level *)
  let saved m = Base.get_local_value Game.Settings.storage_key m.Model.runtime in
  let m = ticks (press m "O") 300. in
  check "O opens the settings over the level" (scene m = "Play" && draws m "VOLUME" && draws m "100%");
  let m = ticks (press m "Left") 300. in
  check "Left lowers the volume" (draws m "90%" && Base.get_volume m.Model.runtime = 0.9);
  check "the keys do not reach the board" (draws m "000");
  check "the volume is saved" (saved m = Some "volume=90;fullscreen=0");
  let m = ticks (press m "Escape") 300. in
  check "Esc closes the panel, not the level" (scene m = "Play" && not (draws m "VOLUME"));
  let m = ticks (press m "D") 300. in
  check "the board has the keys back" (draws m "001");
  let m = ticks (press m "O") 300. in
  let m = List.fold_left press m [ "Down"; "Down"; "Return" ] in
  let m = ticks m 100. in
  check "Erase asks first" (draws m "Press again to erase" && (user m).best <> []);
  let m = ticks (press m "Return") 100. in
  check "the second press erases the progress"
    ((user m).best = [] && draws m "Nothing to erase");
  let m = ticks (press m "Escape") 300. in
  let m = ticks (press m "Escape") 800. in
  check "the select screen shows the reset" (scene m = "Select" && draws m "SOLVED 0 / 57");
  if !failures > 0 then exit 1
