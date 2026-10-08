open Affect
module Mtime = Affect_unix.Mtime

module type LISTENER = sig
  type t

  val addr : t -> Unix.sockaddr

  val wait_acceptable : t -> 'tag -> 'tag Action.t

  val try_accept : t -> (Unix.file_descr * Unix.sockaddr) option

  val close : t -> unit
end

type t =
  { addr: Unix.sockaddr
  ; acceptor: unit Fun.Async.t
  ; supervisor: unit Fun.Async.t
  ; grace: unit Fun.Async.t
  ; stopping: unit Cell.Once.t }

let report (config : Config.t) e =
  config.on_error e (Printexc.get_raw_backtrace ())

(* Accepts a pending connection, if there is one: readiness can be spurious.
   Errors that do not concern the listener are ridden out: a connection that
   failed while queued, or running out of file descriptors, which is reported
   and waited out. *)
let accept config (module L : LISTENER) listener =
  match L.try_accept listener with
  | conn ->
      conn
  | exception (Unix.Unix_error (err, _, _) as e) -> (
    match Accept_error.classify err with
    | `Connection ->
        None
    | `Resources ->
        report config e ;
        Time.sleep Time.mono Mtime.Span.(100 * ms) ;
        None
    | `Listener ->
        raise e )

let serve_connection (config : Config.t) engine handler ~stopping ~id flow
    (fd, peer) =
  let local = try Unix.getsockname fd with Unix.Unix_error _ -> peer in
  let conn =
    Conn_info.make ~id ~peer ~local ~stopping:(Cell.Once.get' stopping) ()
  in
  let ctx =
    { Engine.config
    ; conn
    ; stopping= Cell.Once.get' stopping
    ; is_stopping= (fun () -> Cell.Once.is_set stopping) }
  in
  match engine ctx flow handler with
  | () ->
      ()
  | exception Fun.Async.Cancelled ->
      raise Fun.Async.Cancelled
  | exception e ->
      report config e

(* Takes a free connection slot, accepts a connection and hands it to the
   supervisor, until [stopping] is set. Then closes the listener, so that new
   clients are refused. *)
let acceptor config (module L : LISTENER) listener ~stopping ~slots handoff =
  let wait action =
    Action.invoke
      (Action.choose
         [Action.map (fun () -> `Stop) (Cell.Once.get' stopping); action] )
  in
  let rec loop () =
    match wait (Semaphore.acquire' slots `Slot) with
    | `Stop ->
        ()
    | `Slot ->
        with_slot ()
  and with_slot () =
    match wait (L.wait_acceptable listener `Ready) with
    | `Stop ->
        Semaphore.release slots
    | `Ready -> (
      match accept config (module L) listener with
      | exception e ->
          Semaphore.release slots ; raise e
      | None ->
          with_slot ()
      | Some conn -> (
        match wait (Port.offer' handoff conn `Taken) with
        | `Taken ->
            loop ()
        | `Stop ->
            (* The supervisor saw [stopping] first and takes no more. *)
            Affect_unix.Unix.close_noerr (fst conn) ;
            Semaphore.release slots ) )
  in
  Fun.Async.protect ~finally:(fun () -> L.close listener) loop

(* Owns the connections: each one is an asynchronous call of the supervisor,
   which therefore returns only once they have all finished. *)
let supervisor config engine handler ~stopping ~slots handoff =
  let conn = Action.map (fun c -> `Conn c) (Port.take' handoff) in
  let stop = Action.map (fun () -> `Stop) (Cell.Once.get' stopping) in
  let rec loop id =
    match Action.invoke (Action.choose [stop; conn]) with
    | `Stop ->
        ()
    | `Conn conn ->
        Fun.Async.call_trap_exn (fun () ->
            let flow = Tcp.flow (fst conn) in
            Fun.Async.protect
              ~finally:(fun () -> Flow.close flow ; Semaphore.release slots)
              (fun () ->
                serve_connection config engine handler ~stopping ~id flow conn ) ) ;
        loop (id + 1)
  in
  loop 0

let start_with ?(config = Config.default) ?(engine = H1_server.serve)
    (module L : LISTENER) listener handler =
  let addr, slots =
    match (L.addr listener, Semaphore.make config.max_connections) with
    | setup ->
        setup
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        L.close listener ;
        Printexc.raise_with_backtrace e bt
  in
  let stopping = Cell.Once.make () in
  let handoff = Port.make () in
  let supervisor =
    Fun.Async.call (fun () ->
        supervisor config engine handler ~stopping ~slots handoff )
  in
  let acceptor =
    Fun.Async.call (fun () ->
        (* If accepting fails, the connections still finish. *)
        Fun.Async.protect
          ~finally:(fun () -> Cell.Once.try_set stopping ())
          (fun () ->
            acceptor config (module L) listener ~stopping ~slots handoff ) )
  in
  (* Once stopping, the connections get [shutdown_grace] to finish. *)
  let grace =
    Fun.Async.call (fun () ->
        Action.invoke (Cell.Once.get' stopping) ;
        let finished =
          Action.map (fun () -> `Finished) (Fun.Async.get' supervisor)
        in
        (* The grace starts at the stop. *)
        let expired =
          Time.at Time.mono
            (Time.after Time.mono config.shutdown_grace)
            `Expired
        in
        match Action.invoke (Action.choose [finished; expired]) with
        | `Finished ->
            ()
        | `Expired ->
            Fun.Async.cancel supervisor
        (* Aborted: the supervisor was cancelled, see [wait]. *)
        | exception Fun.Async.Cancelled
          when not (Fun.Async.is_current_cancelled ()) ->
            () )
  in
  {addr; acceptor; supervisor; grace; stopping}

let start ?(config = Config.default) ?engine handler =
  let listener = Tcp.Listener.listen ~backlog:config.backlog config.addr in
  start_with ~config ?engine (module Tcp.Listener) listener handler

let local_addr s = s.addr

let shutdown s = Cell.Once.try_set s.stopping ()

let abort s =
  shutdown s ;
  Fun.Async.cancel s.supervisor

(* The supervisor returns as soon as it sees [stopping], then waits for its
   connections. Cancelled before it sees it, by [abort] or a grace of zero, it
   ends with [Cancelled]: a stop too. *)
let wait s =
  ( match Fun.Async.get s.supervisor with
  | () ->
      ()
  | exception Fun.Async.Cancelled
    when Cell.Once.is_set s.stopping && not (Fun.Async.is_current_cancelled ())
    ->
      () ) ;
  Fun.Async.get s.grace ; Fun.Async.get s.acceptor

let run ?config ?engine ?domain_count ?(on_start = ignore) handler =
  let signals = Stop_signal.install () in
  Stdlib.Fun.protect ~finally:(fun () -> Stop_signal.release signals)
  @@ fun () ->
  Affect_unix.Unix.main ?domain_count
  @@ fun () ->
  let server = start ?config ?engine handler in
  on_start server ;
  let stopper =
    Fun.Async.call (fun () ->
        ignore (Stop_signal.wait signals) ;
        shutdown server ;
        ignore (Stop_signal.wait signals) ;
        abort server )
  in
  Fun.Async.protect
    ~finally:(fun () -> Fun.Async.cancel stopper)
    (fun () -> wait server)
