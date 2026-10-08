(* A fake monotonic time line for tests. Its time moves only when the test
   advances it. Its timers wake calls with cells, so its deadlines never reach
   the waits of affect. *)

module Mtime = Affect_unix.Mtime

type t

val make : unit -> t
(** [make ()] starts one day after the real [Mtime.now ()]. Then the time moves
    only with {!advance}. With the offset, a mix of fake and real time fails
    clearly: a real timer with a fake deadline waits about one day (the 2 s
    guard of the test fails), and a fake timer with a real deadline is enabled
    at once. [make] and [advance] work also outside [Unix.main]. *)

val mono : t -> Caravan.Time.mono
(** [mono t] is the time line of [t]. [make] builds it, so each call gives the
    same value. *)

val now : t -> Mtime.t
(** [now t] is the time of [t]. *)

val advance : t -> Mtime.Span.t -> unit
(** [advance t span] moves the time forward by [span]. Then each timer whose
    deadline is at or before the new time is enabled, in the order of the
    deadlines. Raises [Invalid_argument] if the time leaves the range of
    [Mtime.t]. *)

(** The type for a point in the log of the timers. *)
type mark

val mark : t -> mark
(** [mark t] is the point in the log of the timers at this time. *)

(** Raised by {!wait_mono_timer} when no timer for the deadline came in time.
    The string shows the deadline. *)
exception No_timer of string

val wait_mono_timer : ?guard:Mtime.Span.t -> t -> since:mark -> Mtime.t -> unit
(** [wait_mono_timer t ~since d] waits until the code makes a timer for the
    deadline [d] after [since]. It raises [No_timer] after [guard] of real time
    (default 2 s). Call it in [Unix.main]. *)
