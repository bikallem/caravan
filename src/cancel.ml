open Weft

(* A sync first reads the cancel flags of the context of the fiber. A flag wins
   over a ready event (weft src/core/sched.ml, the self commit of a cancel). *)
let check () = sync (always ())

let is_cancelled () =
  match check () with () -> false | exception Fiber.Cancelled _ -> true

let is_own = function Fiber.Cancelled _ -> is_cancelled () | _ -> false

let scope_closed () = check () ; raise Fiber.Scope_closed
