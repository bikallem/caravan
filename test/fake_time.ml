open Affect
open Caravan
module Mtime = Affect_unix.Mtime
module Ptime = Affect_unix.Ptime

exception No_timer of string

(* What a pending mono deadline does when it comes: it sets the cell of a mono
   timer, or it checks the POSIX time for a POSIX timer, as affect does. *)
type waiter =
  | Wake of unit Cell.Once.t
  | Posix_check of Ptime.t * unit Cell.Once.t

(* The deadline of a timer in the log. *)
type entry = Mono of Mtime.t | Posix of Ptime.t

(* The state, under [lock]. Nothing under the lock performs an effect. *)
type state =
  { lock: Mutex.t
  ; mutable mono_now: Mtime.t
  ; mutable posix_now: Ptime.t
  ; mutable pending: (Mtime.t * waiter) list
        (* The mono deadlines after [mono_now]: one [Wake] cell for each mono
           deadline, and one [Posix_check] for each POSIX timer that waits. *)
  ; mutable log: (int * entry) list
        (* Each timer that the code made, with its number, the last first. *)
  ; mutable count: int
  ; mutable changed: unit Cell.Once.t (* Set and replaced at each new timer. *)
  }

type t = {state: state; mono: Time.mono; posix: Time.posix}

type mark = int

let locked st f = Mutex.protect st.lock f

(* Adds [entry] to the log, and gives the [changed] cell to set outside the
   lock. Under the lock. *)
let log st entry =
  st.count <- st.count + 1 ;
  st.log <- (st.count, entry) :: st.log ;
  let changed = st.changed in
  st.changed <- Cell.Once.make () ;
  changed

(* The mono deadline at which affect checks the POSIX time again for the POSIX
   deadline [p], after [posix_now]: the span to [p], rounded up to the ns. The
   round up only keeps the deadline after [mono_now]: the one partition of
   [advance] already prevents a loop. Under the lock. *)
let check_at st p =
  let span = Ptime.diff p st.posix_now in
  let ns =
    match Ptime.Span.to_mtime_span span with
    | None ->
        None
    | Some ns ->
        let _, ps = Ptime.Span.to_d_ps span in
        if Int64.rem ps 1000L = 0L then Some ns
        else Some (Mtime.Span.add ns Mtime.Span.ns)
  in
  match Option.bind ns (Mtime.add_span st.mono_now) with
  | Some d ->
      d
  | None ->
      Mtime.max_stamp

(* A mono timer: it records the deadline in the log at each invoke. A deadline
   at or before [mono_now] is enabled at once. Mono timers at one deadline share
   one [Wake] cell. *)
let mono_at st d tag =
  Action.guard (fun () ->
      let changed, wait =
        locked st (fun () ->
            let changed = log st (Mono d) in
            if not (Mtime.is_earlier st.mono_now ~than:d) then (changed, None)
            else
              let same = function
                | d', Wake cell when Mtime.equal d d' ->
                    Some cell
                | _ ->
                    None
              in
              match List.find_map same st.pending with
              | Some cell ->
                  (changed, Some cell)
              | None ->
                  let cell = Cell.Once.make () in
                  st.pending <- (d, Wake cell) :: st.pending ;
                  (changed, Some cell) )
      in
      Cell.Once.try_set changed () ;
      match wait with
      | None ->
          Action.always tag
      | Some cell ->
          Action.map (fun () -> tag) (Cell.Once.get' cell) )

(* A POSIX timer, as affect does it: a deadline at or before [posix_now] is
   enabled at once. Else it waits until the mono deadline [check_at], and there
   [advance] checks the POSIX time again. It logs only its POSIX deadline. *)
let posix_at st p tag =
  Action.guard (fun () ->
      let changed, wait =
        locked st (fun () ->
            let changed = log st (Posix p) in
            if not (Ptime.is_earlier st.posix_now ~than:p) then (changed, None)
            else
              let cell = Cell.Once.make () in
              st.pending <- (check_at st p, Posix_check (p, cell)) :: st.pending ;
              (changed, Some cell) )
      in
      Cell.Once.try_set changed () ;
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
    ; count= 0
    ; changed= Cell.Once.make () }
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

    let at p tag = posix_at st p tag
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

let posix_now t = locked t.state (fun () -> t.state.posix_now)

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
            (* One partition: a POSIX check that waits again goes after the new
               [mono_now], and this advance does not examine it again. *)
            let due, later =
              List.partition
                (fun (d, _) -> not (Mtime.is_earlier mono_now ~than:d))
                st.pending
            in
            let due, again =
              List.partition_map
                (fun (d, w) ->
                  match w with
                  | Wake cell ->
                      Either.Left (d, cell)
                  | Posix_check (p, cell) ->
                      if not (Ptime.is_earlier posix_now ~than:p) then
                        Either.Left (d, cell)
                      else Either.Right (check_at st p, w) )
                due
            in
            st.pending <- again @ later ;
            due
        | _ ->
            invalid_arg "Fake_time.advance: the time leaves the range" )
  in
  let due = List.sort (fun (d, _) (d', _) -> Mtime.compare d d') due in
  List.iter (fun (_, cell) -> Cell.Once.try_set cell ()) due

let jump_posix t span =
  let st = t.state in
  locked st (fun () ->
      match Ptime.add_span st.posix_now span with
      | Some posix_now ->
          st.posix_now <- posix_now
      | None ->
          invalid_arg "Fake_time.jump_posix: the time leaves the range" )

let mark t = locked t.state (fun () -> t.state.count)

(* The deadline of [entry] as a span after the time of its line, for
   messages. *)
let pp_entry st ppf = function
  | Mono d ->
      if Mtime.is_earlier d ~than:st.mono_now then
        Format.fprintf ppf "now - %a" Mtime.Span.pp (Mtime.span d st.mono_now)
      else
        Format.fprintf ppf "now + %a" Mtime.Span.pp (Mtime.span st.mono_now d)
  | Posix p ->
      Format.fprintf ppf "posix now %+.9fs"
        (Ptime.Span.to_float_s (Ptime.diff p st.posix_now))

let no_timer st ~since entry =
  locked st (fun () ->
      let after = List.filter (fun (n, _) -> n > since) st.log in
      Format.asprintf "no timer at %a; the timers after the mark: [%a]"
        (pp_entry st) entry
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
           (fun ppf (_, e) -> pp_entry st ppf e) )
        (List.rev after) )

(* Waits until the log has an entry after [since] that [is] accepts. *)
let wait_timer ?(guard = Mtime.Span.(2 * s)) t ~since entry ~is =
  let st = t.state in
  let late = Time.after Time.mono guard in
  let rec loop () =
    let found, changed =
      locked st (fun () ->
          (List.exists (fun (n, e) -> n > since && is e) st.log, st.changed) )
    in
    if not found then
      (* [`Late] is first: the guard holds also if timers come all the time. *)
      match
        Action.invoke
          (Action.choose
             [ Time.at Time.mono late `Late
             ; Action.map (fun () -> `Changed) (Cell.Once.get' changed) ] )
      with
      | `Changed ->
          loop ()
      | `Late ->
          raise (No_timer (no_timer st ~since entry))
  in
  loop ()

let wait_mono_timer ?guard t ~since d =
  wait_timer ?guard t ~since (Mono d) ~is:(function
    | Mono d' ->
        Mtime.equal d d'
    | Posix _ ->
        false )

let wait_posix_timer ?guard t ~since p =
  wait_timer ?guard t ~since (Posix p) ~is:(function
    | Posix p' ->
        Ptime.equal p p'
    | Mono _ ->
        false )
