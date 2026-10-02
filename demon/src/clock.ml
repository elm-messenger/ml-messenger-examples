(* A clock that only moves forward. [UpdateTick] carries the host's absolute
   time, which can jump (a stall, or the MCP server switching the game to its
   deterministic clock, which starts again from 0); animations run on the
   elapsed time built from the deltas instead, each clamped to [0, 100] ms. *)

type t = { last : float option; elapsed : float }

let start = { last = None; elapsed = 0. }

let tick c now =
  let dt =
    match c.last with
    | Some l when now >= l -> Float.min 100. (now -. l)
    | _ -> 0.
  in
  { last = Some now; elapsed = c.elapsed +. dt }
