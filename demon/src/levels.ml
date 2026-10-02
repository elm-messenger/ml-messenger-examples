(* The level catalogue: 57 levels in two worlds, each a text file loaded as a
   data resource. *)

open Messenger

let world1 =
  List.init 20 (fun i -> Printf.sprintf "1-%d" (i + 1))
  @ List.map (Printf.sprintf "1-%c") [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G' ]

let world2 =
  List.init 20 (fun i -> Printf.sprintf "2-%d" (i + 1))
  @ List.map (Printf.sprintf "2-%c")
      [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J' ]

let ids = Array.of_list (world1 @ world2)
let count = Array.length ids
let world_of i = if i < List.length world1 then 1 else 2
let resource_name id = "level:" ^ id

let resources : Resources.resource_defs =
  Array.to_list ids
  |> List.map (fun id ->
      (resource_name id, Resources.Data_res ("assets/levels/" ^ id ^ ".txt")))

(* The parsed level, once its file has loaded. *)
let load runtime i =
  Option.map
    (fun text -> Rules.parse ~fallback_name:ids.(i) text)
    (Base.get_config_data (resource_name ids.(i)) runtime)

let first_unsolved (u : User_data.t) =
  let rec go i =
    if i >= count then 0 else if User_data.solved u ids.(i) then go (i + 1) else i
  in
  go 0
