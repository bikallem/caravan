(* Stdlib Unix does the system calls, and affect does the waits. *)
open Affect
module Aunix = Affect_unix.Unix
module Mtime = Affect_unix.Mtime

(* A deadline, and a cell that wakes the waits when the deadline changes. *)
type deadline = {at: Mtime.t option; changed: unit Cell.Once.t}

let no_deadline () = Atomic.make {at= None; changed= Cell.Once.make ()}

(* A deadline that does not change keeps its record: the engines set the same
   read deadline again before many reads. *)
let set_deadline d at =
  if not (Option.equal Mtime.equal (Atomic.get d).at at) then begin
    let old = Atomic.exchange d {at; changed= Cell.Once.make ()} in
    Cell.Once.try_set old.changed ()
  end

(* A socket that fibers share. A read or a write, with its waits, uses the
   socket. [close] wakes the waits at once, but the fd closes only when no fiber
   uses it. Thus the operating system cannot give the number of the fd to a new
   socket while a fiber can use the number. *)
type socket =
  { fd: Unix.file_descr
  ; users: int Atomic.t
  ; closing: unit Cell.Once.t
  ; reset: bool Atomic.t (* [close] sends a reset *)
  ; freed: bool Atomic.t (* the fd is closed *) }

let free s =
  if Atomic.compare_and_set s.freed false true then begin
    (* A zero linger time makes close send a reset. *)
    ( if Atomic.get s.reset then
        try Unix.setsockopt_optint s.fd Unix.SO_LINGER (Some 0)
        with Unix.Unix_error _ -> () ) ;
    Aunix.close_noerr s.fd
  end

let release s =
  if Atomic.fetch_and_add s.users (-1) = 1 && Cell.Once.is_set s.closing then
    free s

(* Runs [f] while it uses [s]. *)
let use s f =
  Atomic.incr s.users ;
  if Cell.Once.is_set s.closing then (release s ; raise Flow.Closed) ;
  Fun.protect ~finally:(fun () -> release s) f

let close ~reset s =
  if reset then Atomic.set s.reset true ;
  Cell.Once.try_set s.closing () ;
  if Atomic.get s.users = 0 then free s

(* Waits for [ready]. Raises Flow.Timeout when the deadline in [d] passes, and
   Flow.Closed when [s] closes. A new deadline applies also to a wait that
   started before it. *)
let rec wait s ready d =
  let {at; changed} = Atomic.get d in
  let timeout =
    match at with
    | None ->
        Action.never
    | Some deadline ->
        (* A passed deadline wins over the other events. *)
        if not (Mtime.is_earlier (Time.now Time.mono) ~than:deadline) then
          raise Flow.Timeout ;
        Time.at Time.mono at `Timeout
  in
  let events =
    [ Action.map (fun () -> `Ready) ready
    ; Action.map (fun () -> `Changed) (Cell.Once.get' changed)
    ; Action.map (fun () -> `Closed) (Cell.Once.get' s.closing)
    ; timeout ]
  in
  match Action.invoke (Action.choose events) with
  | `Ready ->
      ()
  | `Changed ->
      wait s ready d
  | `Closed ->
      raise Flow.Closed
  | `Timeout ->
      raise Flow.Timeout

let is_closed = function
  | Unix.EPIPE | ECONNRESET | ENOTCONN | ECONNABORTED ->
      true
  | _ ->
      false

let flow fd =
  let s =
    { fd
    ; users= Atomic.make 0
    ; closing= Cell.Once.make ()
    ; reset= Atomic.make false
    ; freed= Atomic.make false }
  in
  let read_deadline = no_deadline () and write_deadline = no_deadline () in
  let rec try_read buf off len =
    match Unix.read fd buf off len with
    | n ->
        Some n
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
        None
    | exception Unix.Unix_error (EINTR, _, _) ->
        try_read buf off len
    | exception Unix.Unix_error (e, _, _) when is_closed e ->
        raise Flow.Closed
  in
  let rec read buf off len =
    Fun.Async.check_cancellation () ;
    match try_read buf off len with
    | Some n ->
        n
    | None ->
        wait s (Aunix.wait_readable fd ()) read_deadline ;
        read buf off len
  in
  let rec write buf off len =
    if len > 0 then begin
      Fun.Async.check_cancellation () ;
      match Unix.single_write fd buf off len with
      | n ->
          write buf (off + n) (len - n)
      | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
          wait s (Aunix.wait_writable fd ()) write_deadline ;
          write buf off len
      | exception Unix.Unix_error (EINTR, _, _) ->
          write buf off len
      | exception Unix.Unix_error (e, _, _) when is_closed e ->
          raise Flow.Closed
    end
  in
  let shutdown_send () =
    try Unix.shutdown fd Unix.SHUTDOWN_SEND
    with Unix.Unix_error (e, _, _) when is_closed e -> ()
  in
  (* The action is enabled also when the flow closes. Then a read raises
     Flow.Closed. *)
  let readable =
    Action.choose [Aunix.wait_readable fd (); Cell.Once.get' s.closing]
  in
  Flow.make
    ~read:(fun buf off len -> use s (fun () -> read buf off len))
    ~read_now:(fun buf off len -> use s (fun () -> try_read buf off len))
    ~write:(fun buf off len -> use s (fun () -> write buf off len))
    ~readable
    ~shutdown_send:(fun () ->
      (* A different fiber can close the flow after the check of Flow. *)
      try use s shutdown_send with Flow.Closed -> () )
    ~close:(fun () -> close ~reset:false s)
    ~reset:(fun () -> close ~reset:true s)
    ~set_read_deadline:(set_deadline read_deadline)
    ~set_write_deadline:(set_deadline write_deadline)
    ()

let set_nodelay fd = function
  | Unix.ADDR_INET _ ->
      Unix.setsockopt fd Unix.TCP_NODELAY true
  | Unix.ADDR_UNIX _ ->
      ()

let connect addr =
  let fd = Aunix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  match Aunix.connect fd addr ; set_nodelay fd addr with
  | () ->
      flow fd
  | exception e ->
      Aunix.close_noerr fd ; raise e

module Listener = struct
  type t = {fd: Unix.file_descr; addr: Unix.sockaddr}

  let listen ?(backlog = 1024) addr =
    let fd = Aunix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
    match
      Unix.set_nonblock fd ;
      Unix.setsockopt fd Unix.SO_REUSEADDR true ;
      Unix.bind fd addr ;
      Unix.listen fd backlog
    with
    | () ->
        {fd; addr= Unix.getsockname fd}
    | exception e ->
        Aunix.close_noerr fd ; raise e

  let addr l = l.addr

  (* Accepted sockets are blocking, whatever the listener is. *)
  let setup (fd, peer) =
    match Unix.set_nonblock fd ; set_nodelay fd peer with
    | () ->
        (fd, peer)
    | exception e ->
        Aunix.close_noerr fd ; raise e

  let accept l = setup (Aunix.accept ~cloexec:true l.fd)

  let try_accept l =
    match Unix.accept ~cloexec:true l.fd with
    (* A socket that cannot be set up is already broken: drop it. *)
    | conn -> (
      try Some (setup conn) with Unix.Unix_error _ -> None )
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
        None

  let wait_acceptable l tag = Aunix.wait_readable l.fd tag

  let close l = Aunix.close_noerr l.fd
end
