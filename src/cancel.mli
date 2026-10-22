(* Internal. Cancellation points that do not wait. weft has no public form of
   them. *)

val check : unit -> unit
(* [check ()] raises [Weft.Fiber.Cancelled r] if the context of the fiber is
   cancelled with the reason [r]. Else it does nothing. It never waits. Outside
   a fiber, it raises [Weft.Fiber.Not_in_fiber].

   [check ()] is [Weft.sync (Weft.always ())]: a cancel wins at the start of a
   sync, also if the event is ready. In a [Fiber.shield], it does not see the
   cancels of the outer scopes. A cancel stays: a second call raises again. *)

val is_cancelled : unit -> bool
(* [is_cancelled ()] is [true] if [check ()] raises, else [false]. It does not
   remove the cancel. *)

val is_own : exn -> bool
(* [is_own e] is [true] if [e] is the cancel of this fiber: a
   [Weft.Fiber.Cancelled] when the context of the fiber is cancelled. The cancel
   of a different scope, for example of a scope that the fiber cancelled, is not
   its own cancel. weft gives it as a failure. For each other exception,
   [is_own] is [false].

   For a [Weft.Fiber.Cancelled], [is_own] raises and catches an exception
   inside. Thus get the backtrace of [e] before the call. *)

val scope_closed : unit -> 'a
(* [scope_closed ()] is the handler of a [Weft.Fiber.Scope_closed] from a fork
   into a scope that the caller made. Only a cancel of the caller closes such a
   scope. Thus [scope_closed ()] raises the cancel of the caller, as [check ()].
   If the context is not cancelled, it raises [Weft.Fiber.Scope_closed]. *)
