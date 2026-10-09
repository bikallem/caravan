open Affect
open Caravan
module Mtime = Affect_unix.Mtime
module Ptime = Affect_unix.Ptime

exception No_timer of string

(* The state, under [lock]. Nothing under the lock performs an effect. *)
type state =
  { lock: Mutex.t
  ; mutable mono_now: Mtime.t
  ; mutable posix_now: Ptime.t
  ; mutable pending: (Mtime.t * unit Cell.Once.t) list
        (* The deadlines after [mono_now], one cell for each deadline. *)
  ; mutable log: (int * Mtime.t) list
        (* Each timer that the code made, with its number, the last first. *)
  ; mutable count: int }

type t = {state: state; mono: Time.mono; posix: Time.posix}

type mark = int

let locked st f = Mutex.protect st.lock f

(* A timer: it records the deadline in the log at each invoke. A deadline at or
   before [mono_now] is enabled at once. Timers at one deadline share one
   cell. *)
let mono_at st d tag =
  Action.guard (fun () ->
      let wait =
        locked st (fun () ->
            st.count <- st.count + 1 ;
            st.log <- (st.count, d) :: st.log ;
            if not (Mtime.is_earlier st.mono_now ~than:d) then None
            else
              match
                List.find_opt (fun (d', _) -> Mtime.equal d d') st.pending
              with
              | Some (_, cell) ->
                  Some cell
              | None ->
                  let cell = Cell.Once.make () in
                  st.pending <- (d, cell) :: st.pending ;
                  Some cell )
      in
      match wait with
      | None ->
          Action.always tag
      | Some cell ->
          Action.map (fun () -> tag) (Cell.Once.get' cell) )

(* Sun, 06 Nov 1994 08:49:37 GMT. *)
let posix_start = Option.get (Ptime.of_float_s 784111777.)

let make () =
  let one_day = Mtime.Span.(24 * hour) in
  let st =
    { lock= Mutex.create ()
    ; mono_now= Option.get (Mtime.add_span (Mtime.now ()) one_day)
    ; posix_now= posix_start
    ; pending= []
    ; log= []
    ; count= 0 }
  in
  let module Mono_clock = struct
    type t = Mtime.t

    let now () = locked st (fun () -> st.mono_now)
  end in
  let module Mono_timer = struct
    type time = Mtime.t

    let at d tag = mono_at st d tag
  end in
  let module Mono_span = struct
    type time = Mtime.t

    type t = Mtime.Span.t

    let add = Mtime.add_span
  end in
  let module Posix_clock = struct
    type t = Ptime.t

    let now () = locked st (fun () -> st.posix_now)
  end in
  let module Posix_timer = struct
    type time = Ptime.t

    let at _ _ = invalid_arg "Fake_time: the POSIX line has no timer"
  end in
  let module Posix_span = struct
    type time = Ptime.t

    type t = Ptime.Span.t

    let add = Ptime.add_span
  end in
  { state= st
  ; mono= Time.make (module Mono_clock) (module Mono_timer) (module Mono_span)
  ; posix=
      Time.make (module Posix_clock) (module Posix_timer) (module Posix_span) }

let mono t = t.mono

let posix t = t.posix

let mono_now t = locked t.state (fun () -> t.state.mono_now)

let advance t span =
  let st = t.state in
  let due =
    locked st (fun () ->
        match
          ( Mtime.add_span st.mono_now span
          , Ptime.add_span st.posix_now (Ptime.Span.of_mtime_span span) )
        with
        | Some mono_now, Some posix_now ->
            st.mono_now <- mono_now ;
            st.posix_now <- posix_now ;
            let due, later =
              List.partition
                (fun (d, _) -> not (Mtime.is_earlier mono_now ~than:d))
                st.pending
            in
            st.pending <- later ;
            due
        | _ ->
            invalid_arg "Fake_time.advance: the time leaves the range" )
  in
  let due = List.sort (fun (d, _) (d', _) -> Mtime.compare d d') due in
  List.iter (fun (_, cell) -> Cell.Once.try_set cell ()) due

let mark t = locked t.state (fun () -> t.state.count)

(* [d] as a span after the time of the line, for messages. *)
let pp_deadline st ppf d =
  if Mtime.is_earlier d ~than:st.mono_now then
    Format.fprintf ppf "now - %a" Mtime.Span.pp (Mtime.span d st.mono_now)
  else Format.fprintf ppf "now + %a" Mtime.Span.pp (Mtime.span st.mono_now d)

let no_timer st ~since d =
  locked st (fun () ->
      let after = List.filter (fun (n, _) -> n > since) st.log in
      Format.asprintf "no timer at %a; the timers after the mark: [%a]"
        (pp_deadline st) d
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
           (fun ppf (_, d) -> pp_deadline st ppf d) )
        (List.rev after) )

(* It examines the log each 1 ms of real time. *)
let wait_mono_timer ?(guard = Mtime.Span.(2 * s)) t ~since d =
  let st = t.state in
  let late = Option.get (Mtime.add_span (Mtime.now ()) guard) in
  let found () =
    locked st (fun () ->
        List.exists (fun (n, d') -> n > since && Mtime.equal d d') st.log )
  in
  while not (found ()) do
    if not (Mtime.is_earlier (Mtime.now ()) ~than:late) then
      raise (No_timer (no_timer st ~since d)) ;
    Mtime.wait_for Mtime.Span.ms
  done

let ( +: ) t span = Option.get (Mtime.add_span t span)

let within_50ms a =
  Action.invoke
    (Action.choose
       [Action.map (fun _ -> true) a; Mtime.wait_for' Mtime.Span.(50 * ms) false] )

let with_call_at_timer t d f test =
  let m = mark t in
  let c = Fun.Async.call f in
  Stdlib.Fun.protect
    ~finally:(fun () -> Fun.Async.cancel c)
    (fun () ->
      wait_mono_timer t ~since:m d ;
      test c )
