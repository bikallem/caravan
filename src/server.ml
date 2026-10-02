open Affect
module Mtime = Affect_unix.Mtime

type t = {
  listener : Tcp.Listener.t;
  call : unit Fun.Async.t;
  stopped : bool Atomic.t;
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

let serve_connection (config : Config.t) engine handler ~id flow (fd, peer) =
  let local = try Unix.getsockname fd with Unix.Unix_error _ -> peer in
  let conn = Conn_info.make ~id ~peer ~local () in
  let ctx =
    { Engine.config; conn; stopping = Action.never; is_stopping = (fun () -> false) }
  in
  match engine ctx flow handler with
  | () -> ()
  | exception Fun.Async.Cancelled -> raise Fun.Async.Cancelled
  | exception e -> report config e

let start ?(config = Config.default) ?(engine = H1_server.serve) handler =
  let listener = Tcp.Listener.listen ~backlog:config.backlog config.addr in
  let slots = Semaphore.make config.max_connections in
  let next_id = Atomic.make 0 in
  let rec accept_loop () =
    Semaphore.acquire slots;
    let conn =
      try accept config listener with e -> Semaphore.release slots; raise e
    in
    let id = Atomic.fetch_and_add next_id 1 in
    Fun.Async.call_trap_exn (fun () ->
        let flow = Tcp.flow (fst conn) in
        Fun.Async.protect
          ~finally:(fun () -> Flow.close flow; Semaphore.release slots)
          (fun () -> serve_connection config engine handler ~id flow conn));
    accept_loop ()
  in
  let call =
    Fun.Async.call (fun () ->
        Fun.Async.protect ~finally:(fun () -> Tcp.Listener.close listener) accept_loop)
  in
  { listener; call; stopped = Atomic.make false }

let local_addr s = Tcp.Listener.addr s.listener

let shutdown s =
  Atomic.set s.stopped true;
  Fun.Async.cancel s.call

let wait s =
  match Fun.Async.get s.call with
  | () -> ()
  | exception Fun.Async.Cancelled when Atomic.get s.stopped -> ()
