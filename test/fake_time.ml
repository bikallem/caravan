open Affect
open Caravan
module Mtime = Affect_unix.Mtime

exception No_timer of string

(* The state, under [lock]. Nothing under the lock performs an effect. *)
type state =
  { lock: Mutex.t
  ; mutable now: Mtime.t
  ; mutable pending: (Mtime.t * unit Cell.Once.t) list
        (* The cells of the deadlines after [now]: one cell for each
           deadline. *)
  ; mutable log: (int * Mtime.t) list
        (* Each timer that the code made, with its number, the last first. *)
  ; mutable count: int
  ; mutable changed: unit Cell.Once.t (* Set and replaced at each new timer. *)
  }

type t = {state: state; mono: Time.mono}

type mark = int

let locked st f = Mutex.protect st.lock f

(* A timer: it records the deadline in the log at each invoke. A deadline at or
   before [now] is enabled at once. *)
let at st d tag =
  Action.guard (fun () ->
      let changed, wait =
        locked st (fun () ->
            st.count <- st.count + 1 ;
            st.log <- (st.count, d) :: st.log ;
            let changed = st.changed in
            st.changed <- Cell.Once.make () ;
            if not (Mtime.is_earlier st.now ~than:d) then (changed, None)
            else
              match
                List.find_opt (fun (d', _) -> Mtime.equal d d') st.pending
              with
              | Some (_, cell) ->
                  (changed, Some cell)
              | None ->
                  let cell = Cell.Once.make () in
                  st.pending <- (d, cell) :: st.pending ;
                  (changed, Some cell) )
      in
      Cell.Once.try_set changed () ;
      match wait with
      | None ->
          Action.always tag
      | Some cell ->
          Action.map (fun () -> tag) (Cell.Once.get' cell) )

let make () =
  let one_day = Mtime.Span.(24 * hour) in
  let st =
    { lock= Mutex.create ()
    ; now= Option.get (Mtime.add_span (Mtime.now ()) one_day)
    ; pending= []
    ; log= []
    ; count= 0
    ; changed= Cell.Once.make () }
  in
  let module Clock = struct
    type t = Mtime.t

    let now () = locked st (fun () -> st.now)
  end in
  let module Timer = struct
    type time = Mtime.t

    let at d tag = at st d tag
  end in
  let module Span = struct
    type time = Mtime.t

    type t = Mtime.Span.t

    let add = Mtime.add_span
  end in
  {state= st; mono= Time.make (module Clock) (module Timer) (module Span)}

let mono t = t.mono

let now t = locked t.state (fun () -> t.state.now)

let advance t span =
  let st = t.state in
  let due =
    locked st (fun () ->
        match Mtime.add_span st.now span with
        | None ->
            invalid_arg "Fake_time.advance: the time leaves the range"
        | Some now ->
            st.now <- now ;
            let due, later =
              List.partition
                (fun (d, _) -> not (Mtime.is_earlier now ~than:d))
                st.pending
            in
            st.pending <- later ;
            due )
  in
  let due = List.sort (fun (d, _) (d', _) -> Mtime.compare d d') due in
  List.iter (fun (_, cell) -> Cell.Once.try_set cell ()) due

let mark t = locked t.state (fun () -> t.state.count)

(* The deadline [d] as a span after the fake time, for messages. *)
let pp_deadline st ppf d =
  if Mtime.is_earlier d ~than:st.now then
    Format.fprintf ppf "now - %a" Mtime.Span.pp (Mtime.span d st.now)
  else Format.fprintf ppf "now + %a" Mtime.Span.pp (Mtime.span st.now d)

let no_timer st ~since d =
  locked st (fun () ->
      let after = List.filter (fun (n, _) -> n > since) st.log in
      Format.asprintf "no timer at %a; the timers after the mark: [%a]"
        (pp_deadline st) d
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
           (fun ppf (_, d) -> pp_deadline st ppf d) )
        (List.rev after) )

let wait_mono_timer ?(guard = Mtime.Span.(2 * s)) t ~since d =
  let st = t.state in
  let late = Time.after Time.mono guard in
  let rec loop () =
    let found, changed =
      locked st (fun () ->
          ( List.exists (fun (n, d') -> n > since && Mtime.equal d d') st.log
          , st.changed ) )
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
          raise (No_timer (no_timer st ~since d))
  in
  loop ()
