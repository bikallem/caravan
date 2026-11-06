open Weft

(* [Clock.mono] has no [compare]. [Time.diff] saturates, thus its sign is
   correct also for two times that are far apart. *)
let compare a b = Span.compare (Time.diff Mono a b) Span.zero

let from_now span = Time.add Mono (Time.now Mono) span

let has_passed = function
  | None ->
      false
  | Some d ->
      compare (Time.now Mono) d >= 0

let at = function None -> never | Some d -> Time.at Mono d
