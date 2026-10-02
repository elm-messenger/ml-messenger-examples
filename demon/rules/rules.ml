(* The rules of Maxwell's Puzzling Demon, ported from sample/maxwell.cpp.

   A state is an array of objects, one per cell; [turn] returns a new array and
   never mutates its argument, so the game keeps an undo history for free.
   Every object carries the id it got at load time, so the view can follow an
   object from one state to the next to animate it. *)

type kind = Nothing | Player | Block | Glass
type heat = Neutral | Hot | Cold
type edge = No_edge | Conduct | Insulate

(* Directions index the [edges] arrays: up, down, left, right. *)
type dir = int

let up = 0
let down = 1
let left = 2
let right = 3
let dr = [| -1; 1; 0; 0 |]
let dc = [| 0; 0; -1; 1 |]
let opposite d = d lxor 1

type obj = { kind : kind; heat : heat; edges : edge array; id : int }

let no_edges = [| No_edge; No_edge; No_edge; No_edge |]
let nothing = { kind = Nothing; heat = Neutral; edges = no_edges; id = -1 }

(* What never changes in a cell: wall, goal and outer markers. *)
type info = { fixed : bool; goal : bool; outer : bool }

type level = {
  name : string;
  w : int;
  h : int;
  info : info array;
  start : obj array;  (** before the start-of-level heat update *)
}

type status = Playing | Solved | Dead

let index lv r c = (r * lv.w) + c
let inside lv r c = r >= 0 && r < lv.h && c >= 0 && c < lv.w

(* ---------- Parsing ---------- *)

let starts_with prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

(* Levels written like the LEVELS section of the PuzzleScript script: optional
   "section <name>" headers, grids separated into layers by "layer" lines,
   "(...)" comment lines, blank lines between levels. Returns (name, layers)
   with each layer a list of rows. *)
let parse_texts text =
  (* newest level first; each level's layers newest first, rows reversed *)
  let out : (string * string list list ref) list ref = ref [] in
  let closed = ref true in
  let current_layer_nonempty () =
    match !out with
    | (_, layers) :: _ -> ( match !layers with l :: _ -> l <> [] | [] -> false)
    | [] -> false
  in
  String.split_on_char '\n' text
  |> List.iter (fun raw ->
      let s = String.trim raw in
      if s = "" then (if current_layer_nonempty () then closed := true)
      else if s.[0] = '(' then ()
      else if starts_with "section" s then (
        out :=
          (String.trim (String.sub s 7 (String.length s - 7)), ref [ [] ])
          :: !out;
        closed := false)
      else if s = "layer" then (
        match !out with
        | [] -> failwith "'layer' before any level rows"
        | (_, layers) :: _ ->
            layers := [] :: !layers;
            closed := false)
      else begin
        if !closed && (!out = [] || current_layer_nonempty ()) then
          out :=
            (Printf.sprintf "level %d" (List.length !out + 1), ref [ [] ])
            :: !out;
        closed := false;
        match !out with
        | (_, layers) :: _ -> (
            match !layers with
            | cur :: rest -> layers := (s :: cur) :: rest
            | [] -> assert false)
        | [] -> assert false
      end);
  List.rev_map (fun (name, layers) -> (name, List.rev_map List.rev !layers)) !out

(* Builds a level from its layers, using the script's legend a..r. *)
let of_layers name layers =
  let base = match layers with l :: _ -> l | [] -> [] in
  if base = [] then failwith (Printf.sprintf "level '%s' is empty" name);
  let h = List.length base and w = String.length (List.hd base) in
  let n = h * w in
  let kind = Array.make n Nothing and heat = Array.make n Neutral in
  let edges = Array.init n (fun _ -> Array.make 4 No_edge) in
  let fixed = Array.make n false
  and goal = Array.make n false
  and outer = Array.make n false in
  let conflict r c ch =
    failwith
      (Printf.sprintf
         "level '%s': '%c' collides with another object at row %d, column %d"
         name ch r c)
  in
  let place r c ch =
    let i = (r * w) + c in
    let set_obj k ht =
      if kind.(i) <> Nothing then conflict r c ch;
      kind.(i) <- k;
      heat.(i) <- ht
    in
    let set_edge d e =
      if edges.(i).(d) <> No_edge then conflict r c ch;
      edges.(i).(d) <- e
    in
    match ch with
    | '.' -> ()
    | 'a' -> set_obj Player Neutral
    | 'b' -> fixed.(i) <- true
    | 'c' -> goal.(i) <- true
    | 'd' -> set_obj Block Hot
    | 'e' -> set_obj Block Cold
    | 'f' -> set_obj Block Neutral
    | 'i' -> set_obj Glass Neutral
    | 'j' -> set_edge up Conduct
    | 'k' -> set_edge down Conduct
    | 'l' -> set_edge left Conduct
    | 'm' -> set_edge right Conduct
    | 'n' -> set_edge up Insulate
    | 'o' -> set_edge down Insulate
    | 'p' -> set_edge left Insulate
    | 'q' -> set_edge right Insulate
    | 'r' -> outer.(i) <- true
    | ch ->
        failwith (Printf.sprintf "level '%s': unknown character '%c'" name ch)
  in
  List.iter
    (fun layer ->
      if List.length layer <> h then
        failwith (Printf.sprintf "level '%s': layers differ in height" name);
      List.iteri
        (fun r row ->
          if String.length row <> w then
            failwith (Printf.sprintf "level '%s': rows differ in width" name);
          String.iteri (fun c ch -> place r c ch) row)
        layer)
    layers;
  {
    name;
    w;
    h;
    info =
      Array.init n (fun i ->
          { fixed = fixed.(i); goal = goal.(i); outer = outer.(i) });
    start =
      Array.init n (fun i ->
          if kind.(i) = Nothing then { nothing with edges = edges.(i) }
          else { kind = kind.(i); heat = heat.(i); edges = edges.(i); id = i });
  }

(* A file holding exactly one level. *)
let parse ?(fallback_name = "level") text =
  match parse_texts text with
  | [] -> failwith "no level found"
  | [ (name, layers) ] ->
      let name = if starts_with "level " name then fallback_name else name in
      of_layers name layers
  | l -> failwith (Printf.sprintf "%d levels found; expected one" (List.length l))

(* ---------- Movement ---------- *)

let is_mover o = o.kind = Block || o.kind = Glass

(* Whether [a] and its neighbour [b] (in direction [d] from [a]) are glued. *)
let sticks a d b =
  is_mover a && is_mover b
  && (a.edges.(d) = No_edge || b.edges.(opposite d) = No_edge)

let alive_player objs =
  let found = ref None in
  Array.iteri
    (fun i o ->
      if !found = None && o.kind = Player && o.heat <> Hot then found := Some i)
    objs;
  !found

exception Blocked

(* The objects that move when the player tries to go [d], or [None] when
   something in the way is fixed. *)
let moving_set lv objs d =
  match alive_player objs with
  | None -> None
  | Some start -> (
      let moving = Array.make (lv.w * lv.h) false in
      let queue = Queue.create () and members = ref [] in
      let add i =
        moving.(i) <- true;
        Queue.add i queue;
        members := i :: !members
      in
      add start;
      try
        while not (Queue.is_empty queue) do
          let i = Queue.pop queue in
          let r = i / lv.w and c = i mod lv.w in
          if lv.info.(i).fixed then raise Blocked;
          let tr = r + dr.(d) and tc = c + dc.(d) in
          if not (inside lv tr tc) then raise Blocked;
          let t = index lv tr tc in
          if lv.info.(t).fixed then raise Blocked;
          (* push *)
          if objs.(t).kind <> Nothing && not moving.(t) then add t;
          (* glue *)
          for e = 0 to 3 do
            let nr = r + dr.(e) and nc = c + dc.(e) in
            if inside lv nr nc then begin
              let nb = index lv nr nc in
              if (not moving.(nb)) && sticks objs.(i) e objs.(nb) then add nb
            end
          done
        done;
        Some !members
      with Blocked -> None)

let try_move lv objs d =
  match moving_set lv objs d with
  | None -> None
  | Some members ->
      let next = Array.copy objs in
      List.iter (fun i -> next.(i) <- { nothing with edges = no_edges }) members;
      let shift = (dr.(d) * lv.w) + dc.(d) in
      List.iter (fun i -> next.(i + shift) <- objs.(i)) members;
      Some next

(* ---------- Heat ---------- *)

(* Heat passes between two touching objects unless either touching side is
   insulated. *)
let conducts objs a d b =
  objs.(a).edges.(d) <> Insulate && objs.(b).edges.(opposite d) <> Insulate

(* The conductive component reachable from [seeds] (already marked seen). *)
let flood lv objs seeds seen =
  let queue = Queue.create () and acc = ref [] in
  List.iter
    (fun i ->
      Queue.add i queue;
      acc := i :: !acc)
    seeds;
  while not (Queue.is_empty queue) do
    let i = Queue.pop queue in
    let r = i / lv.w and c = i mod lv.w in
    for d = 0 to 3 do
      let nr = r + dr.(d) and nc = c + dc.(d) in
      if inside lv nr nc then begin
        let nb = index lv nr nc in
        if
          (not seen.(nb))
          && objs.(nb).kind <> Nothing
          && conducts objs i d nb
        then begin
          seen.(nb) <- true;
          Queue.add nb queue;
          acc := nb :: !acc
        end
      end
    done
  done;
  !acc

(* Conductive components of all objects, as lists of cell indices. *)
let components lv objs =
  let n = lv.w * lv.h in
  let seen = Array.make n false and out = ref [] in
  for i = 0 to n - 1 do
    if (not seen.(i)) && objs.(i).kind <> Nothing then begin
      seen.(i) <- true;
      out := flood lv objs [ i ] seen :: !out
    end
  done;
  List.rev !out

let settle_heat lv objs =
  let objs = Array.copy objs in
  let n = lv.w * lv.h in
  let set_heat i heat = objs.(i) <- { (objs.(i)) with heat } in
  (* 1. Blocks conductively connected to an outer cell lose their heat. *)
  let seen = Array.make n false in
  let seeds = ref [] in
  for i = n - 1 downto 0 do
    if lv.info.(i).outer && objs.(i).kind <> Nothing then begin
      seen.(i) <- true;
      seeds := i :: !seeds
    end
  done;
  List.iter
    (fun i -> if objs.(i).kind = Block then set_heat i Neutral)
    (flood lv objs !seeds seen);
  (* 2. Glass and the player hold no heat of their own. *)
  Array.iteri
    (fun i o -> if o.kind = Glass || o.kind = Player then set_heat i Neutral)
    objs;
  (* 3. Hot and cold blocks cancel one for one; the surplus takes over. *)
  List.iter
    (fun comp ->
      let balance =
        List.fold_left
          (fun acc i ->
            match objs.(i) with
            | { kind = Block; heat = Hot; _ } -> acc + 1
            | { kind = Block; heat = Cold; _ } -> acc - 1
            | _ -> acc)
          0 comp
      in
      let result =
        if balance > 0 then Hot else if balance < 0 then Cold else Neutral
      in
      List.iter (fun i -> set_heat i result) comp)
    (components lv objs);
  objs

(* ---------- Turns ---------- *)

let status lv objs =
  let alive = ref false and all_goals = ref true in
  Array.iteri
    (fun i o ->
      if o.kind = Player && o.heat <> Hot then alive := true;
      if lv.info.(i).goal && o.kind <> Block then all_goals := false)
    objs
  ;
  if not !alive then Dead else if !all_goals then Solved else Playing

(* The state once the level starts (run_rules_on_level_start). *)
let initial lv = settle_heat lv lv.start

(* One turn: movement, then heat. Returns the new state and whether anything
   moved. *)
let turn lv objs d =
  match try_move lv objs d with
  | Some moved -> (settle_heat lv moved, true)
  | None -> (settle_heat lv objs, false)

(* Positions of objects by id. *)
let positions objs =
  let tbl = Hashtbl.create 64 in
  Array.iteri (fun i o -> if o.kind <> Nothing then Hashtbl.replace tbl o.id i) objs;
  tbl

(* ---------- Text renderings (the CLI's, for tests) ---------- *)

let glyph lv objs i =
  let o = objs.(i) and info = lv.info.(i) in
  if info.fixed then
    match o with
    | { kind = Glass; _ } -> '%'
    | { kind = Block; heat = Hot; _ } -> '^'
    | { kind = Block; heat = Cold; _ } -> '~'
    | _ -> '#'
  else
    let g =
      match o.kind with
      | Player -> if o.heat = Hot then 'X' else '@'
      | Block -> ( match o.heat with Hot -> 'H' | Cold -> 'C' | Neutral -> 'B')
      | Glass -> 'G'
      | Nothing -> if info.goal then '*' else '.'
    in
    if o.kind = Nothing || not info.goal then g
    else if g = '@' then '+'
    else Char.lowercase_ascii g

let render_compact lv objs =
  let b = Buffer.create 256 in
  for r = 0 to lv.h - 1 do
    for c = 0 to lv.w - 1 do
      Buffer.add_char b (glyph lv objs (index lv r c))
    done;
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

(* A side without an edge that is not glued flush to another mover's
   edgeless side: drawn as sticky. *)
let shows_sticky lv objs r c d =
  let o = objs.(index lv r c) in
  if (not (is_mover o)) || o.edges.(d) <> No_edge then false
  else
    let nr = r + dr.(d) and nc = c + dc.(d) in
    if not (inside lv nr nc) then false
    else
      let other = objs.(index lv nr nc) in
      other.edges.(opposite d) <> No_edge || not (is_mover other)

let render_dump lv objs =
  let heat_name = function Neutral -> "none" | Hot -> "hot" | Cold -> "cold" in
  let dir_name = [| "up"; "down"; "left"; "right" |] in
  let b = Buffer.create 1024 in
  for r = 0 to lv.h - 1 do
    for c = 0 to lv.w - 1 do
      let i = index lv r c in
      let o = objs.(i) and info = lv.info.(i) in
      let heat = heat_name o.heat in
      let names = ref [] in
      let add s = names := s :: !names in
      if info.fixed then add "Fixed";
      if info.goal then add "goal";
      if info.outer then add "outer";
      (match o.kind with
      | Player -> add ("player:" ^ heat)
      | Block -> add ("block:" ^ heat)
      | Glass -> add ("glass:" ^ heat)
      | Nothing -> ());
      for d = 0 to 3 do
        match o.edges.(d) with
        | Conduct -> add ("edge:conduct:" ^ heat ^ ":" ^ dir_name.(d))
        | Insulate -> add ("edge:insulate:" ^ dir_name.(d))
        | No_edge ->
            if shows_sticky lv objs r c d then
              add ("edge:sticky:" ^ heat ^ ":" ^ dir_name.(d))
      done;
      if c > 0 then Buffer.add_string b " | ";
      Buffer.add_string b (String.concat "," (List.sort compare !names))
    done;
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

let status_name = function
  | Playing -> "none"
  | Solved -> "accept"
  | Dead -> "die"
