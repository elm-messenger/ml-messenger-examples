(* Usage: trace [--dump] <level-file> <moves>. Same output as sample/maxwell
   (compact or --dump), so the two can be diffed. *)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let dump = List.mem "--dump" args in
  match List.filter (fun a -> a <> "--dump") args with
  | [ path; moves ] ->
      let lv = Rules.parse ~fallback_name:path (read_file path) in
      let render = if dump then Rules.render_dump else Rules.render_compact in
      let moves =
        String.to_seq moves
        |> Seq.filter (fun c -> c <> ' ')
        |> Seq.map Char.lowercase_ascii
        |> List.of_seq
      in
      let dir = function
        | 'w' -> Rules.up
        | 's' -> Rules.down
        | 'a' -> Rules.left
        | 'd' -> Rules.right
        | c -> failwith (Printf.sprintf "invalid move '%c'" c)
      in
      Printf.printf "Level %s (%dx%d)\n\n" lv.name lv.h lv.w;
      let print header objs =
        Printf.printf "%s\n%sState: %s\n\n" header (render lv objs)
          (Rules.status_name (Rules.status lv objs))
      in
      let objs = Rules.initial lv in
      print "Step 0: start" objs;
      let total = List.length moves in
      let rec go objs i = function
        | [] -> ()
        | m :: rest ->
            let st = Rules.status lv objs in
            if st <> Rules.Playing then
              Printf.printf
                "Game over (%s): %d remaining move(s) ignored.\n"
                (Rules.status_name st) (total - i)
            else begin
              let objs, _ = Rules.turn lv objs (dir m) in
              print (Printf.sprintf "Step %d: %c" (i + 1) m) objs;
              go objs (i + 1) rest
            end
      in
      go objs 0 moves
  | _ ->
      prerr_endline "usage: trace [--dump] <level-file> <moves>";
      exit 2
