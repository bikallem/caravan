(* A fake monotonic time line and a fake POSIX time line for tests, in one
   state. Their time moves only when the test advances it. Their timers wake
   calls with cells, so their deadlines never reach the waits of affect. *)

module Mtime = Affect_unix.Mtime
module Ptime = Affect_unix.Ptime

type t

val make : unit -> t
(** [make ()] starts the monotonic time one day after the real [Mtime.now ()],
    and the POSIX time at 784111777 s (Sun, 06 Nov 1994 08:49:37 GMT). Then the
    two move only with {!advance} and {!jump_posix}. With the offsets, a mix of
    fake and real time fails clearly:
    - Monotonic: a real timer with a fake deadline waits about one day (the 2 s
      guard of the test fails), and a fake timer with a real deadline is enabled
      at once.
    - POSIX: a real timer with a fake deadline is enabled at once, and a fake
      timer with a real deadline is about 32 years away.

    [make] and [advance] work also outside [Unix.main]. *)

val mono : t -> Caravan.Time.mono
(** [mono t] is the monotonic time line of [t]. [make] builds it, so each call
    gives the same value. *)

val posix : t -> Caravan.Time.posix
(** [posix t] is the POSIX time line of [t]. [make] builds it, so each call
    gives the same value. *)

val mono_now : t -> Mtime.t
(** [mono_now t] is the monotonic time of [t]. *)

val posix_now : t -> Ptime.t
(** [posix_now t] is the POSIX time of [t]. *)

val advance : t -> Mtime.Span.t -> unit
(** [advance t span] moves the two time lines forward by [span]. Then each
    monotonic timer whose deadline is at or before the new time is enabled. A
    POSIX timer waits as in affect: when it blocks, it gets a monotonic deadline
    from its POSIX deadline. At that monotonic deadline, it is enabled if the
    POSIX time is at or after its deadline, and else it gets a new monotonic
    deadline. [advance] enables the timers in the order of the monotonic
    deadlines, but the calls that wait for them run in the order of the
    scheduler. Raises [Invalid_argument] if a line leaves its range, and then
    nothing changes. *)

val jump_posix : t -> Ptime.Span.t -> unit
(** [jump_posix t span] moves only the POSIX time, by the signed [span]: a step
    of the system clock. It enables no timer, as in affect: a POSIX timer that
    waits checks the POSIX time only at its monotonic deadline. Raises
    [Invalid_argument] if the time leaves the range of [Ptime.t], and then
    nothing changes. *)

(** The type for a point in the log of the timers. *)
type mark

val mark : t -> mark
(** [mark t] is the point in the log of the timers at this time. *)

(** Raised by {!wait_mono_timer} and {!wait_posix_timer} when no timer for the
    deadline came in time. The string shows the deadline and the timers after
    the mark. *)
exception No_timer of string

val wait_mono_timer : ?guard:Mtime.Span.t -> t -> since:mark -> Mtime.t -> unit
(** [wait_mono_timer t ~since d] waits until the code makes a monotonic timer
    for the deadline [d] after [since]. It raises [No_timer] after [guard] of
    real time (default 2 s). Call it in [Unix.main]. *)

val wait_posix_timer : ?guard:Mtime.Span.t -> t -> since:mark -> Ptime.t -> unit
(** [wait_posix_timer t ~since p] waits until the code makes a POSIX timer for
    the deadline [p] after [since]. It raises [No_timer] after [guard] of real
    time (default 2 s). Call it in [Unix.main]. *)
