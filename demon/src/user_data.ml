(* Progress shared by every scene: the fewest moves each solved level took.
   Persisted under [storage_key] as "1-1=12;1-2=30". *)

type t = { best : (string * int) list }

let default = { best = [] }
let storage_key = "maxwell.progress"
let best t id = List.assoc_opt id t.best
let solved t id = List.mem_assoc id t.best

let record t id moves =
  match best t id with
  | Some b when b <= moves -> t
  | _ -> { best = (id, moves) :: List.remove_assoc id t.best }

let serialize t =
  String.concat ";" (List.map (fun (id, m) -> Printf.sprintf "%s=%d" id m) t.best)

let deserialize s =
  let entry e =
    match String.split_on_char '=' e with
    | [ id; m ] -> Option.map (fun m -> (id, m)) (int_of_string_opt m)
    | _ -> None
  in
  { best = List.filter_map entry (String.split_on_char ';' s) }
