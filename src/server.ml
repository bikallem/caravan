open Weft

module type LISTENER = sig
  type t

  val addr : t -> Net.addr

  val accept : t -> Net.t * Net.addr

  val close : t -> unit
end

(* The reasons of the cancels of the connections. Nothing matches them: they
   only name the cancel in the reports. *)
exception Aborted

exception Grace_expired

(* [stop] is full when the server starts to stop. Each connection waits on it:
   an Ivar adds a waiter in amortized O(1) (#11). [abort] is full after [abort].
   Only the grace daemon reads it. *)
type t =
  { addr: Net.addr
  ; stop: unit Ivar.t
  ; abort: unit Ivar.t
  ; close_listener: unit -> unit
  ; open_connections: int Atomic.t
  ; server: unit Promise.t }

(* The pause after an error of accept for resources, for example EMFILE. *)
let pause = Span.ms 100

(* The fiber of one connection. It owns the socket [c] and one slot. The flow
   owns [c] from the start: [Tcp.flow] does not sync and does not raise.
   [finally] does not sync, so it runs also in a cancelled fiber. *)
let connection (config : Config.t) engine handler ~stop ~slots ~count ~id c peer
    () =
  let f = Tcp.flow c in
  Fun.protect
    ~finally:(fun () ->
      Flow.close f ;
      Atomic.decr count ;
      Fiber.Semaphore.release slots )
    (fun () ->
      let local = try Net.local_addr c with Net.Error _ -> peer in
      let conn =
        Conn_info.make ~id ~peer ~local ~stopping:(Ivar.read stop) ()
      in
      engine
        {Engine.config; conn; is_stopping= (fun () -> Ivar.peek stop <> None)}
        f handler )

(* The acceptor loop: it takes a free slot, accepts a connection and forks the
   fiber of the connection into [conns], until the stop. *)
let acceptor (config : Config.t) (module L : LISTENER) l ~conns ~slots ~stop
    ~count engine handler =
  (* The acceptor holds a slot and accepts until a connection comes or the stop.
     It does not report an error of a connection: it accepts again. It reports
     an error for resources, then it pauses for [pause] or until the stop.
     [reported] tells that it reported an error of this series: it reports only
     the first one: Linux and the simulator give EMFILE also when no client
     waits, thus with no flag the acceptor reports the same error after each
     pause. An error of the listener goes to the caller. *)
  let rec accept_conn ~reported =
    match L.accept l with
    | c, peer ->
        if Ivar.peek stop <> None then (Net.close c ; `Stop) else `Conn (c, peer)
    | exception Net.Error (Bad_fd, _) when Ivar.peek stop <> None ->
        `Stop
    | exception (Net.Error (e, _) as exn) -> (
        let bt = Printexc.get_raw_backtrace () in
        match Accept_error.classify e with
        | `Connection ->
            accept_conn ~reported
        | `Resources -> (
            if not reported then config.on_error exn bt ;
            match
              sync
                (choose
                   [ wrap (Time.after Mono pause) (fun () -> `Retry)
                   ; wrap (Ivar.read stop) (fun () -> `Stop) ] )
            with
            | `Retry ->
                accept_conn ~reported:true
            | `Stop ->
                `Stop )
        | `Listener ->
            Printexc.raise_with_backtrace exn bt )
  in
  (* One slot. The connection fiber owns it after the fork. On each other exit
     (a stop, a failure or a cancel), the guard releases it, one time only: a
     second release adds a permit with no error, and then the server serves more
     than [max_connections] connections. *)
  let with_slot id =
    let handed = ref false in
    Fun.protect
      ~finally:(fun () -> if not !handed then Fiber.Semaphore.release slots)
      (fun () ->
        match accept_conn ~reported:false with
        | `Stop ->
            `Stop
        | `Conn (c, peer) -> (
            Atomic.incr count ;
            match
              Fiber.fork_isolated ~name:"caravan.conn" conns
                ~on_error:config.on_error
                (connection config engine handler ~stop ~slots ~count ~id c peer)
            with
            | () ->
                handed := true ;
                (* The acceptor yields after each 16 accepts. weft does not
                   switch at an accept that finds a connection. A yield after
                   each accept made the accepts slow on a busy domain: each
                   yield can wait for a full round of the run queue. A cancel at
                   the yield leaves the slot with the connection. *)
                if id land 15 = 15 then Fiber.yield () ;
                `Next
            | exception Fiber.Scope_closed ->
                (* [conns] is cancelled. The next sync raises the cancel. *)
                Atomic.decr count ; Net.close c ; `Next ) )
  in
  let rec loop id =
    (* The peek before the choose: weft selects a ready branch at random. *)
    if Ivar.peek stop = None then
      match
        sync
          (choose
             [ wrap (Fiber.Semaphore.acquire slots) (fun () -> `Slot)
             ; wrap (Ivar.read stop) (fun () -> `Stop) ] )
      with
      | `Stop ->
          ()
      | `Slot -> (
        match with_slot id with `Next -> loop (id + 1) | `Stop -> () )
  in
  loop 0

(* The grace daemon. It starts at the end of the acceptor. It cancels the
   connections at the end of the grace or at [abort]. The scope cancels it when
   the last connection ends. A grace that overflows never ends. *)
let grace (config : Config.t) ~conns ~abort () =
  let reason =
    sync
      (choose
         [ wrap (Time.after Mono config.shutdown_grace) (fun () -> Grace_expired)
         ; wrap (Ivar.read abort) (fun () -> Aborted) ] )
  in
  Fiber.cancel conns reason

(* The fiber of the server. The acceptor loop is the body of [conns]. Its
   [finally] fills [stop] at each end of the acceptor: a stop, a failure of the
   listener and a cancel of the scope of the server. A failure of the listener
   goes to [wait] after the connections ended.

   A cancel of the scope of the server also cancels [conns]. Then the fork of
   the grace raises [Scope_closed], and the server fiber raises the cancel
   again: weft makes it a cancel of [server]. A [Cancelled] of a different scope
   from [L.accept] is a failure of the listener as each other exception is. *)
let server_body config engine handler (module L : LISTENER) l ~slots ~stop
    ~abort ~count () =
  let failure = ref None in
  Fiber.scope (fun conns ->
      ( match
          Fun.protect
            ~finally:(fun () ->
              ignore (Ivar.try_fill stop ()) ;
              L.close l )
            (fun () ->
              acceptor config
                (module L)
                l ~conns ~slots ~stop ~count engine handler )
        with
      | () ->
          ()
      | exception e ->
          failure := Some (e, Printexc.get_raw_backtrace ()) ) ;
      (* A cancel of the scope of the server before this fork: no grace is
         necessary. *)
      try
        Fiber.fork_daemon ~name:"caravan.grace" conns
          (grace config ~conns ~abort)
      with Fiber.Scope_closed -> () ) ;
  Option.iter (fun (e, bt) -> Printexc.raise_with_backtrace e bt) !failure

let http1 = H1_server.serve

let start_with ?(config = Config.default) ?(engine = http1) sw
    (module L : LISTENER) l handler =
  match
    let addr = L.addr l in
    let slots = Fiber.Semaphore.create config.max_connections in
    let stop = Ivar.create () and abort = Ivar.create () in
    let count = Atomic.make 0 in
    let server =
      Fiber.fork_promise ~name:"caravan.server" sw
        (server_body config engine handler
           (module L)
           l ~slots ~stop ~abort ~count )
    in
    { addr
    ; stop
    ; abort
    ; close_listener= (fun () -> L.close l)
    ; open_connections= count
    ; server }
  with
  | t ->
      t
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      L.close l ;
      Printexc.raise_with_backtrace e bt

let start ?(config = Config.default) ?engine sw handler =
  let l = Tcp.Listener.listen ~backlog:config.backlog config.addr in
  start_with ~config ?engine sw (module Tcp.Listener) l handler

let local_addr t = t.addr

let open_connections t = Atomic.get t.open_connections

(* The listener closes in the fiber of the caller. On one domain, the OS then
   refuses a connect right after [shutdown]. On more than one domain, weft can
   do the close(2) later, on the domain that registered the listener. A client
   that connects before that close gets a reset. *)
let shutdown t =
  ignore (Ivar.try_fill t.stop ()) ;
  t.close_listener ()

let abort t =
  shutdown t ;
  ignore (Ivar.try_fill t.abort ())

(* [Promise.Cancelled] comes only from a cancel of the scope of the server (or
   of an outer scope): a stop too. A cancel of the caller raises
   [Fiber.Cancelled]. A [Cancelled] of a different scope that the server fiber
   raised is a failure, and [wait] raises it. *)
let wait t =
  match Promise.await t.server with
  | () ->
      ()
  | exception Promise.Cancelled ->
      ()

let stop_on_signals t =
  let signal =
    choose [Signal.received Signal.SIGINT; Signal.received Signal.SIGTERM]
  in
  (* A commit can count more than one signal. *)
  if sync signal < 2 then (
    shutdown t ;
    ignore (sync signal) ) ;
  abort t ;
  Signal.unwatch Signal.SIGINT ;
  Signal.unwatch Signal.SIGTERM

let run ?config ?engine ?domains ?(on_start = ignore) os handler =
  Weft.run ?domains ~signals:[Signal.SIGINT; Signal.SIGTERM] os (fun () ->
      Fiber.scope (fun sw ->
          let s = start ?config ?engine sw handler in
          Fiber.fork_daemon ~name:"caravan.signals" sw (fun () ->
              stop_on_signals s ) ;
          on_start s ;
          wait s ) )
