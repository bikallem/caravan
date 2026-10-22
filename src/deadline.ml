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

let equal a b =
  match (a, b) with
  | None, None ->
      true
  | Some a, Some b ->
      compare a b = 0
  | None, Some _ | Some _, None ->
      false

let earlier a b =
  match (a, b) with
  | None, d | d, None ->
      d
  | Some x, Some y ->
      if compare x y <= 0 then a else b

let at = function None -> never | Some d -> Time.at Mono d
