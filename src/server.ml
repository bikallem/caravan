open Affect
module Mtime = Affect_unix.Mtime

type t = {
  listener : Tcp.Listener.t;
  acceptor : unit Fun.Async.t;
  supervisor : unit Fun.Async.t;
  stopping : unit Cell.Once.t;
}

let report (config : Config.t) e = config.on_error e (Printexc.get_raw_backtrace ())

(* Accepts the next connection, riding out errors that do not concern the
   listener: a client that gave up while queued, or running out of file
   descriptors, which is reported and waited out. *)
let rec accept config listener =
  match Tcp.Listener.accept listener with
  | conn -> conn
  | exception Unix.Unix_error ((ECONNABORTED | EINTR), _, _) -> accept config listener
  | exception (Unix.Unix_error ((EMFILE | ENFILE | ENOBUFS | ENOMEM), _, _) as e) ->
      report config e;
      Mtime.wait_for Mtime.Span.(100 * ms);
      accept config listener

let serve_connection (config : Config.t) engine handler ~stopping ~id flow (fd, peer) =
  let local = try Unix.getsockname fd with Unix.Unix_error _ -> peer in
  let conn = Conn_info.make ~id ~peer ~local () in
  let ctx =
    { Engine.config; conn; stopping = Cell.Once.get' stopping;
      is_stopping = (fun () -> Cell.Once.is_set stopping) }
  in
  match engine ctx flow handler with
  | () -> ()
  | exception Fun.Async.Cancelled -> raise Fun.Async.Cancelled
  | exception e -> report config e

(* Takes a free connection slot, accepts a connection and hands it to the
   supervisor, until [stopping] is set. Then closes the listener, so that new
   clients are refused. *)
let acceptor config listener ~stopping ~slots handoff =
  let wait action =
    Action.invoke (Action.choose [ Action.map (fun () -> `Stop) (Cell.Once.get' stopping); action ])
  in
  let rec loop () =
    match wait (Semaphore.acquire' slots `Slot) with
    | `Stop -> ()
    | `Slot -> (
        match wait (Tcp.Listener.wait_acceptable listener `Ready) with
        | `Stop -> Semaphore.release slots
        | `Ready -> (
            let conn = try accept config listener with e -> Semaphore.release slots; raise e in
            match wait (Port.offer' handoff conn `Taken) with
            | `Taken -> loop ()
            | `Stop ->
                (* The supervisor saw [stopping] first and takes no more. *)
                Affect_unix.Unix.close_noerr (fst conn);
                Semaphore.release slots))
  in
  Fun.Async.protect ~finally:(fun () -> Tcp.Listener.close listener) loop

(* Owns the connections: each one is an asynchronous call of the supervisor,
   which therefore returns only once they have all finished. *)
let supervisor config engine handler ~stopping ~slots handoff =
  let conn = Action.map (fun c -> `Conn c) (Port.take' handoff) in
  let stop = Action.map (fun () -> `Stop) (Cell.Once.get' stopping) in
  let rec loop id =
    match Action.invoke (Action.choose [ stop; conn ]) with
    | `Stop -> ()
    | `Conn conn ->
        Fun.Async.call_trap_exn (fun () ->
            let flow = Tcp.flow (fst conn) in
            Fun.Async.protect
              ~finally:(fun () -> Flow.close flow; Semaphore.release slots)
              (fun () -> serve_connection config engine handler ~stopping ~id flow conn));
        loop (id + 1)
  in
  loop 0

let start ?(config = Config.default) ?(engine = H1_server.serve) handler =
  let listener = Tcp.Listener.listen ~backlog:config.backlog config.addr in
  let stopping = Cell.Once.make () and slots = Semaphore.make config.max_connections in
  let handoff = Port.make () in
  let supervisor =
    Fun.Async.call (fun () -> supervisor config engine handler ~stopping ~slots handoff)
  in
  let acceptor =
    Fun.Async.call (fun () ->
        (* If accepting fails, the connections still finish. *)
        Fun.Async.protect
          ~finally:(fun () -> Cell.Once.try_set stopping ())
          (fun () -> acceptor config listener ~stopping ~slots handoff))
  in
  { listener; acceptor; supervisor; stopping }

let local_addr s = Tcp.Listener.addr s.listener

let shutdown s =
  Cell.Once.try_set s.stopping ();
  Fun.Async.cancel s.supervisor

let wait s =
  (match Fun.Async.get s.supervisor with
  | () -> ()
  | exception Fun.Async.Cancelled when Cell.Once.is_set s.stopping -> ());
  Fun.Async.get s.acceptor
