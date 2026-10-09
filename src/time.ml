open Affect
module Mtime = Affect_unix.Mtime
module Ptime = Affect_unix.Ptime

module type CLOCK = sig
  type t

  val now : unit -> t
end

module type TIMER = sig
  type time

  val at : time -> 'tag -> 'tag Action.t
end

module type SPAN = sig
  type time

  type t

  val add : time -> t -> time option
end

type ('time, 'span) t =
  { clock: (module CLOCK with type t = 'time)
  ; timer: (module TIMER with type time = 'time)
  ; span: (module SPAN with type time = 'time and type t = 'span) }

let make clock timer span = {clock; timer; span}

let now (type time span) (t : (time, span) t) =
  let module C = (val t.clock) in
  C.now ()

let at (type time span) (t : (time, span) t) d tag =
  match d with
  | None ->
      Action.never
  | Some d ->
      let module T = (val t.timer) in
      T.at d tag

let add (type time span) (t : (time, span) t) time span =
  let module S = (val t.span) in
  S.add time span

let after t span = add t (now t) span

let sleep t span = Action.invoke (at t (after t span) ())

type mono = (Mtime.t, Mtime.Span.t) t

module Mono_clock = struct
  type t = Mtime.t

  let now = Mtime.now
end

module Mono_timer = struct
  type time = Mtime.t

  let at d tag =
    Action.guard (fun () ->
        let now = Mtime.now () in
        if not (Mtime.is_earlier now ~than:d) then Action.always tag
        else
          let span = Mtime.span now d in
          (* affect ends a wait of 2^63 ns or more at once: it compares with a
             signed comparison. Such a deadline is 292 years away. *)
          if Int64.compare (Mtime.Span.to_uint64_ns span) 0L < 0 then
            Action.never
          else Mtime.wait_for' span tag )
end

module Mono_span = struct
  type time = Mtime.t

  type t = Mtime.Span.t

  let add = Mtime.add_span
end

let mono = make (module Mono_clock) (module Mono_timer) (module Mono_span)

type posix = (Ptime.t, Ptime.Span.t) t

module Posix_clock = struct
  type t = Ptime.t

  let now = Ptime.now
end

(* affect converts the deadline to a monotonic deadline when the call blocks,
   and reads the POSIX clock again at that deadline. *)
module Posix_timer = struct
  type time = Ptime.t

  let at = Ptime.wait_until'
end

module Posix_span = struct
  type time = Ptime.t

  type t = Ptime.Span.t

  let add = Ptime.add_span
end

let posix = make (module Posix_clock) (module Posix_timer) (module Posix_span)
