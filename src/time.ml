open Affect
module Mtime = Affect_unix.Mtime

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

let after (type time span) (t : (time, span) t) span =
  let module S = (val t.span) in
  S.add (now t) span

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
        if Mtime.is_earlier now ~than:d then
          Mtime.wait_for' (Mtime.span now d) tag
        else Action.always tag )
end

module Mono_span = struct
  type time = Mtime.t

  type t = Mtime.Span.t

  let add = Mtime.add_span
end

let mono = make (module Mono_clock) (module Mono_timer) (module Mono_span)
