(* Stdlib Unix for system calls; affect's for waiting. *)
open Affect
module Aunix = Affect_unix.Unix
module Mtime = Affect_unix.Mtime

(* Waits for [ready], or raises Flow.Timeout once [deadline] has passed. *)
let wait ready deadline =
  match deadline with
  | None -> Action.invoke ready
  | Some deadline ->
      let now = Mtime.now () in
      if not (Mtime.is_earlier now ~than:deadline) then raise Flow.Timeout;
      let timeout = Mtime.wait_for' (Mtime.span now deadline) () in
      match Action.invoke (Action.either ready timeout) with
      | Left () -> ()
      | Right () -> raise Flow.Timeout

let is_closed = function
| Unix.EPIPE | ECONNRESET | ENOTCONN | ECONNABORTED -> true
| _ -> false

let flow fd =
  let read_deadline = Atomic.make None and write_deadline = Atomic.make None in
  let rec read_now buf off len =
    match Unix.read fd buf off len with
    | n -> Some n
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> None
    | exception Unix.Unix_error (EINTR, _, _) -> read_now buf off len
    | exception Unix.Unix_error (e, _, _) when is_closed e -> raise Flow.Closed
  in
  let rec read buf off len =
    Fun.Async.check_cancellation ();
    match read_now buf off len with
    | Some n -> n
    | None ->
        wait (Aunix.wait_readable fd ()) (Atomic.get read_deadline);
        read buf off len
  in
  let rec write buf off len =
    if len > 0
    then begin
      Fun.Async.check_cancellation ();
      match Unix.single_write fd buf off len with
      | n -> write buf (off + n) (len - n)
      | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
          wait (Aunix.wait_writable fd ()) (Atomic.get write_deadline);
          write buf off len
      | exception Unix.Unix_error (EINTR, _, _) -> write buf off len
      | exception Unix.Unix_error (e, _, _) when is_closed e -> raise Flow.Closed
    end
  in
  let shutdown_send () =
    try Unix.shutdown fd Unix.SHUTDOWN_SEND
    with Unix.Unix_error (e, _, _) when is_closed e -> ()
  in
  Flow.make ~read ~read_now ~write ~readable:(Aunix.wait_readable fd ()) ~shutdown_send
    ~close:(fun () -> Aunix.close_noerr fd)
    ~reset:(fun () ->
        (* A zero linger time makes close send a reset. *)
        (try Unix.setsockopt_optint fd Unix.SO_LINGER (Some 0) with Unix.Unix_error _ -> ());
        Aunix.close_noerr fd)
    ~set_read_deadline:(Atomic.set read_deadline)
    ~set_write_deadline:(Atomic.set write_deadline) ()

let set_nodelay fd = function
| Unix.ADDR_INET _ -> Unix.setsockopt fd Unix.TCP_NODELAY true
| Unix.ADDR_UNIX _ -> ()

let connect addr =
  let fd = Aunix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  match Aunix.connect fd addr; set_nodelay fd addr with
  | () -> flow fd
  | exception e -> Aunix.close_noerr fd; raise e

module Listener = struct
  type t = { fd : Unix.file_descr; addr : Unix.sockaddr }

  let listen ?(backlog = 1024) addr =
    let fd = Aunix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
    match
      Unix.set_nonblock fd;
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      Unix.bind fd addr;
      Unix.listen fd backlog
    with
    | () -> { fd; addr = Unix.getsockname fd }
    | exception e -> Aunix.close_noerr fd; raise e

  let addr l = l.addr

  (* Accepted sockets are blocking, whatever the listener is. *)
  let setup (fd, peer) =
    match Unix.set_nonblock fd; set_nodelay fd peer with
    | () -> (fd, peer)
    | exception e -> Aunix.close_noerr fd; raise e

  let accept l = setup (Aunix.accept ~cloexec:true l.fd)

  let try_accept l =
    match Unix.accept ~cloexec:true l.fd with
    (* A socket that cannot be set up is already broken: drop it. *)
    | conn -> (try Some (setup conn) with Unix.Unix_error _ -> None)
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> None

  let wait_acceptable l tag = Aunix.wait_readable l.fd tag
  let close l = Aunix.close_noerr l.fd
end
