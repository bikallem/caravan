(* A fake monotonic time line and a fake POSIX clock for tests, in one state.
   Their time moves only when the test advances it. The timers wake calls with
   cells, so their deadlines never reach the waits of affect. *)

module Mtime = Affect_unix.Mtime
module Ptime = Affect_unix.Ptime

type t

val make : unit -> t
(** [make ()] starts the monotonic time one day after the real [Mtime.now ()],
    and the POSIX time at 784111777 s (Sun, 06 Nov 1994 08:49:37 GMT). Then the
    two move only with {!advance}. With the offset, a real timer with a fake
    deadline waits about one day (the 2 s guard of the test fails), and a fake
    timer with a real deadline is enabled at once. [make] and [advance] work
    also outside [Unix.main]. *)

val mono : t -> Caravan.Time.mono
(** [mono t] is the monotonic time line of [t]. [make] builds it, so each call
    gives the same value. *)

val posix : t -> Caravan.Time.posix
(** [posix t] is the POSIX time line of [t]: only a clock. Its timer raises
    [Invalid_argument], because caravan only reads the POSIX time. *)

val mono_now : t -> Mtime.t
(** [mono_now t] is the monotonic time of [t]. *)

val advance : t -> Mtime.Span.t -> unit
(** [advance t span] moves the two time lines forward by [span]. Then each timer
    whose deadline is at or before the new time is enabled, in the order of the
    deadlines. The calls that wait for them run in the order of the scheduler.
    Raises [Invalid_argument] if a line leaves its range, and then nothing
    changes. *)

(** The type for a point in the log of the timers. *)
type mark

val mark : t -> mark
(** [mark t] is the point in the log of the timers at this time. *)

(** Raised by {!wait_mono_timer} when no timer for the deadline came in time.
    The string shows the deadline and the timers after the mark. *)
exception No_timer of string

val wait_mono_timer : ?guard:Mtime.Span.t -> t -> since:mark -> Mtime.t -> unit
(** [wait_mono_timer t ~since d] waits until the code makes a monotonic timer
    for the deadline [d] after [since]. It raises [No_timer] after [guard] of
    real time (default 2 s). Call it in [Unix.main]. *)

(** {1 Helpers of the tests} *)

val ( +: ) : Mtime.t -> Mtime.Span.t -> Mtime.t
(** [t +: span] is [t] plus [span]. Raises [Invalid_argument] if the result is
    not representable. *)

val within_50ms : _ Affect.Action.t -> bool
(** [within_50ms a] is [true] if [a] is enabled within 50 ms of real time. Call
    it in [Unix.main]. *)
