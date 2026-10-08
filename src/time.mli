(** Time lines.

    A time line has a clock that reads the time, a timer that is an event source
    at a time, and the addition of spans to its times. [Time] does not know
    which clock it holds. Caravan uses {!mono} for all durations. *)

(** {1:seams Seams} *)

(** A clock. It only reads the time. *)
module type CLOCK = sig
  type t

  val now : unit -> t
  (** [now ()] is the current time of the clock. *)
end

(** A timer. It only gives an event source. The scheduler waits when a call
    invokes the action. *)
module type TIMER = sig
  type time

  val at : time -> 'tag -> 'tag Affect.Action.t
  (** [at d tag] is enabled with [tag] when the time line is at [d] or later, at
      once if it is already. It reads its time line at each invoke, not when the
      action is made. It can be enabled late. *)
end

(** The spans of a time line and their addition to its times. *)
module type SPAN = sig
  type time

  type t

  val add : time -> t -> time option
  (** [add time span] is [time] plus [span], [None] if the result is not
      representable. *)
end

(** {1:time_lines Time lines} *)

(** The type for a clock, a timer and the span addition of one time line. *)
type ('time, 'span) t

val make :
     (module CLOCK with type t = 'time)
  -> (module TIMER with type time = 'time)
  -> (module SPAN with type time = 'time and type t = 'span)
  -> ('time, 'span) t
(** [make clock timer span] is a time line. The timer must use the time line of
    the clock: nothing checks this. *)

val now : ('time, _) t -> 'time
(** [now t] is the current time of the clock of [t]. *)

val at : ('time, _) t -> 'time option -> 'tag -> 'tag Affect.Action.t
(** [at t d tag] is the timer of [t] at the deadline [d], enabled with [tag].
    [None] is no deadline: the action is never enabled. *)

val after : ('time, 'span) t -> 'span -> 'time option
(** [after t span] is [now t] plus [span], [None] if the result is not
    representable. *)

val sleep : (_, 'span) t -> 'span -> unit
(** [sleep t span] waits until the deadline [after t span]. It waits forever if
    the deadline is not representable, or if the timer of [t] never enables it
    (see {!mono}). *)

(** {1:system System time lines} *)

(** The type for monotonic time lines. *)
type mono = (Affect_unix.Mtime.t, Affect_unix.Mtime.Span.t) t

val mono : mono
(** The monotonic clock of the system and the timers of affect. Use it for
    durations: time-outs, retries and graces. A deadline 2{^ 63} ns or more
    after the invoke of its timer is never enabled. *)
