module Cfg = Config
open Weft
open Caravan

(* The idle connections of each (host, port), the newest first, and the
   connections in use. A stdlib Mutex protects them. This is the only stdlib
   lock of caravan.

   The rule: no critical section holds the lock across a sync, a world effect,
   or a call into weft or into a Flow. Thus a critical section has only Hashtbl
   operations, list operations, and field reads and writes. The connect, the
   checks of an idle connection, the close of a flow and the clock reads occur
   outside the lock. [Time.now] is a world effect.

   In the simulator, all the virtual domains run on one OS thread, and a world
   effect can switch the domain. Thus a lock across a world effect gives
   [Sys_error] at the next lock of the same Mutex.

   A [Fiber.Mutex] is not a fit: its [lock] is a cancellation point, and a
   deferred release runs in a cancelled job fiber. *)
type t =
  { config: Cfg.t
  ; connect: host:string -> port:int -> Flow.t
  ; lock: Mutex.t
  ; idle: (string * int, Conn.t list) Hashtbl.t
  ; in_use: (int, Conn.t) Hashtbl.t
  ; mutable closed: bool }

let make ?connect config =
  let connect =
    match connect with
    | Some c ->
        c
    | None ->
        fun ~host ~port -> Connect.connect config ~host ~port
  in
  { config
  ; connect
  ; lock= Mutex.create ()
  ; idle= Hashtbl.create 8
  ; in_use= Hashtbl.create 8
  ; closed= false }

(* The newest idle connection of [key], now in use. *)
let take t key =
  Mutex.protect t.lock (fun () ->
      if t.closed then Error.refuse_closed () ;
      match Hashtbl.find_opt t.idle key with
      | Some (c :: rest) ->
          if rest = [] then Hashtbl.remove t.idle key
          else Hashtbl.replace t.idle key rest ;
          Hashtbl.replace t.in_use c.Conn.id c ;
          Some c
      | _ ->
          None )

(* An idle connection is stale when it is too old, or when its server closed it,
   reset it, or sent bytes that no request asked for. *)
let too_old t ~now (c : Conn.t) =
  match c.released_at with
  | Some r ->
      Span.compare (Time.diff Mono now r) t.config.idle_timeout >= 0
  | None ->
      false

let stale t c =
  too_old t ~now:(Time.now Mono) c
  ||
  match Flow.read_now c.flow (Bytes.create 1) 0 1 with
  | None ->
      false
  | Some _ ->
      true
  | exception Flow.Closed ->
      true

let discard t (c : Conn.t) =
  Mutex.protect t.lock (fun () -> Hashtbl.remove t.in_use c.id) ;
  Flow.close c.flow

let is_closed t = Mutex.protect t.lock (fun () -> t.closed)

let rec checkout ?(reuse = true) t ~host ~port =
  let key = (String.lowercase_ascii host, port) in
  match
    if reuse then take t key
    else if is_closed t then Error.refuse_closed ()
    else None
  with
  | Some c when stale t c ->
      discard t c ;
      checkout ~reuse t ~host ~port
  | Some c ->
      (c, `Reused)
  | None ->
      let c = Conn.make ~host:(fst key) ~port (t.connect ~host ~port) in
      let added =
        Mutex.protect t.lock (fun () ->
            if t.closed then false
            else (
              Hashtbl.replace t.in_use c.id c ;
              true ) )
      in
      if added then (c, `New) else (Flow.close c.flow ; Error.refuse_closed ())

let release t (c : Conn.t) ~reusable =
  let now = Time.now Mono in
  let kept =
    Mutex.protect t.lock (fun () ->
        Hashtbl.remove t.in_use c.id ;
        let key = (c.host, c.port) in
        let idle = Option.value ~default:[] (Hashtbl.find_opt t.idle key) in
        if
          reusable && (not t.closed)
          && List.length idle < t.config.max_idle_per_host
        then begin
          c.released_at <- Some now ;
          Hashtbl.replace t.idle key (c :: idle) ;
          true
        end
        else false )
  in
  if not kept then Flow.close c.flow

(* Closes the idle connections whose age is [idle_timeout] or more. Thus a host
   that the client does not use again keeps no socket open. *)
let reap t =
  let now = Time.now Mono and old = ref [] in
  Mutex.protect t.lock (fun () ->
      Hashtbl.filter_map_inplace
        (fun _ cs ->
          let o, keep = List.partition (too_old t ~now) cs in
          old := o @ !old ;
          if keep = [] then None else Some keep )
        t.idle ) ;
  List.iter (fun (c : Conn.t) -> Flow.close c.flow) !old

let idle_hosts t = Mutex.protect t.lock (fun () -> Hashtbl.length t.idle)

let in_use t = Mutex.protect t.lock (fun () -> Hashtbl.length t.in_use)

(* The connections in use close too: their exchanges then fail. *)
let close t =
  let all =
    Mutex.protect t.lock (fun () ->
        t.closed <- true ;
        let all =
          Hashtbl.fold
            (fun _ cs acc -> cs @ acc)
            t.idle
            (Hashtbl.fold (fun _ c acc -> c :: acc) t.in_use [])
        in
        Hashtbl.reset t.idle ; Hashtbl.reset t.in_use ; all )
  in
  List.iter (fun (c : Conn.t) -> Flow.close c.flow) all
