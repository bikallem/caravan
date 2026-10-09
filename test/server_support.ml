(* Shared by the server test suites: a handler, request kinds, and a small
   client that parses responses. *)

open Affect
open Caravan
module Mtime = Affect_unix.Mtime
module Buf_reader = Private.Buf_reader

let loopback = Unix.ADDR_INET (Unix.inet_addr_loopback, 0)

(* Copies [src] to [dst] until [src] ends, then ends [dst]. *)
let copy src dst =
  let buf = Bytes.create 4096 in
  let rec loop () =
    match Flow.read src buf 0 4096 with
    | 0 ->
        Flow.shutdown_send dst
    | n ->
        Flow.write dst buf 0 n ; loop ()
  in
  loop ()

(* A CONNECT tunnel to [authority], an "ip:port", bridging both ways. *)
let tunnel authority client =
  let i = String.rindex authority ':' in
  let host = String.sub authority 0 i in
  let port =
    int_of_string
      (String.sub authority (i + 1) (String.length authority - i - 1))
  in
  let upstream =
    Tcp.connect (Unix.ADDR_INET (Unix.inet_addr_of_string host, port))
  in
  Fun.Async.protect ~finally:(fun () -> Flow.close upstream)
  @@ fun () ->
  let up = Fun.Async.call (fun () -> copy client upstream) in
  copy upstream client ; Fun.Async.get up

(* For the disconnect tests: /wait sets [waiting] and then waits 10s, setting
   [cancelled] if it is cancelled. /fetch does the same while waiting for an
   answer from [upstream]. *)
let waiting = ref (Cell.Once.make ())

let cancelled = ref (Cell.Once.make ())

let upstream = ref loopback

let until_cancelled f =
  Cell.Once.try_set !waiting () ;
  match f () with
  | r ->
      r
  | exception Fun.Async.Cancelled ->
      Cell.Once.try_set !cancelled () ;
      raise Fun.Async.Cancelled

let fetch () =
  let flow = Tcp.connect !upstream in
  Fun.Async.protect ~finally:(fun () -> Flow.close flow)
  @@ fun () ->
  Flow.write_string flow "question" ;
  until_cancelled (fun () -> ignore (Flow.read flow (Bytes.create 1) 0 1)) ;
  Response.string "answer"

(* Handlers read request bodies, except /ignore, which leaves its body to be
   drained or lingered over. CONNECT opens a tunnel. *)
let handler req =
  let body = Request.body req in
  let read () = ignore (Body.to_string body) in
  if Method.equal (Request.meth req) Method.CONNECT then
    Response.upgrade ~status:Status.ok
      (tunnel (Option.get (Request.authority req)))
  else
    match Request.path_only req with
    | "/wait" ->
        until_cancelled (fun () -> Mtime.wait_for Mtime.Span.(10 * s)) ;
        Response.string "late"
    | "/fetch" ->
        fetch ()
    | "/stopping" ->
        (* Sets [waiting], then gives "stopping" when the server starts to stop,
           or "timeout" after 2 s. *)
        Cell.Once.try_set !waiting () ;
        let stopping = Conn_info.stopping (Option.get (Request.conn req)) in
        Response.string
          (Action.invoke
             (Action.choose
                [ Action.map (fun () -> "stopping") stopping
                ; Mtime.wait_for' Mtime.Span.(2 * s) "timeout" ] ) )
    | "/is-stopping" ->
        let stopping = Conn_info.stopping (Option.get (Request.conn req)) in
        Response.string
          (Action.invoke
             (Action.choose
                [Action.map (fun () -> "yes") stopping; Action.always "no"] ) )
    | "/partial" ->
        Response.stream (fun w ->
            Body.Writer.write_string w "partial" ;
            Body.Writer.flush w ;
            failwith "cut short" )
    | "/hello" ->
        read () ; Response.string "hello"
    | "/echo" ->
        Response.stream ?length:(Body.length body) (fun w ->
            Body.write_to body w )
    | "/empty" ->
        read () ; Response.empty ()
    | "/close" ->
        read () ;
        Response.string
          ~headers:(Headers.of_list [("Connection", "close")])
          "bye"
    | "/stream" ->
        read () ;
        Response.stream (fun w ->
            Body.Writer.write_string w "ab" ;
            Body.Writer.flush w ;
            Body.Writer.write_string w "cd" )
    | "/ignore" ->
        Response.string "ignored"
    | _ ->
        read () ;
        Response.string ~status:Status.not_found "not found"

let config = {Config.default with addr= loopback; date_header= false}

(* Requests *)

type body = No_body | Length of string | Chunked of string

type kind =
  { meth: string
  ; path: string
  ; v10: bool
  ; conn: string option (* the Connection field, if any *)
  ; body: body }

let pp_kind ppf k =
  Format.fprintf ppf "%s %s %s%s%s" k.meth k.path
    (if k.v10 then "1.0" else "1.1")
    (match k.conn with None -> "" | Some c -> " conn:" ^ c)
    ( match k.body with
    | No_body ->
        ""
    | Length s when String.length s > 16 ->
        Printf.sprintf " len:%d" (String.length s)
    | Length s ->
        Printf.sprintf " len:%S" s
    | Chunked s ->
        Printf.sprintf " chunked:%S" s )

let serialize k =
  let fields, body =
    match k.body with
    | No_body ->
        ("", "")
    | Length s ->
        (Printf.sprintf "Content-Length: %d\r\n" (String.length s), s)
    | Chunked s ->
        ( "Transfer-Encoding: chunked\r\n"
        , Printf.sprintf "%x\r\n%s\r\n0\r\n\r\n" (String.length s) s )
  in
  Printf.sprintf "%s %s %s\r\nHost: x\r\n%s%s\r\n%s" k.meth k.path
    (if k.v10 then "HTTP/1.0" else "HTTP/1.1")
    (match k.conn with None -> "" | Some c -> "Connection: " ^ c ^ "\r\n")
    fields body

(* Responses, summarised as "status body" and " close" when the server says it
   will close. *)

let summary code body close =
  Printf.sprintf "%d %S%s" code body (if close then " close" else "")

let read_response ~head r =
  let status = Buf_reader.read_line r ~max:1000 in
  let code = int_of_string (String.sub status 9 3) in
  let rec fields acc =
    match Buf_reader.read_line r ~max:1000 with
    | "" ->
        acc
    | line ->
        let i = String.index line ':' in
        let name = String.lowercase_ascii (String.sub line 0 i) in
        let value =
          String.trim (String.sub line (i + 1) (String.length line - i - 1))
        in
        fields ((name, String.lowercase_ascii value) :: acc)
  in
  let fields = fields [] in
  let find n = List.assoc_opt n fields in
  let close = find "connection" = Some "close" in
  let rec to_eof acc =
    let buf = Bytes.create 4096 in
    match Buf_reader.read_into r buf 0 4096 with
    | 0 ->
        acc
    | n ->
        to_eof (acc ^ Bytes.sub_string buf 0 n)
  in
  let body =
    if head || code = 204 || code = 304 then ""
    else if find "transfer-encoding" = Some "chunked" then
      Body.to_string
        (Private.H1_body.chunked
           ~limits:(Private.H1_server.limits Config.default)
           r )
    else
      match find "content-length" with
      | Some n ->
          let n = int_of_string n in
          let buf = Bytes.create n in
          let rec fill got =
            if got < n then fill (got + Buf_reader.read_into r buf got (n - got))
          in
          fill 0 ; Bytes.to_string buf
      | None ->
          to_eof ""
  in
  summary code body close

(* Runs [f server] with a server inside affect's scheduler. *)
(* Two domains unless TEST_DOMAINS says otherwise: mutation testing forks,
   which OCaml forbids once a domain was spawned. *)
let domain_count =
  Option.value ~default:2
    (Option.bind (Sys.getenv_opt "TEST_DOMAINS") int_of_string_opt)

let with_server ?(config = config) ?engine ?(handler = handler) f =
  Affect_unix.Unix.main ~domain_count
  @@ fun () ->
  let server = Server.start ~config ?engine handler in
  Fun.protect
    ~finally:(fun () -> Server.shutdown server ; Server.wait server)
    (fun () -> f server)

(* Runs [f] with the flows of the server and of the peer of a TCP connection.
   The flow of the server is on the time line [time]. *)
let with_flows ?time f =
  Affect_unix.Unix.main ~domain_count
  @@ fun () ->
  let l = Tcp.Listener.listen loopback in
  Stdlib.Fun.protect ~finally:(fun () -> Tcp.Listener.close l)
  @@ fun () ->
  let peer = Fun.Async.call (fun () -> Tcp.connect (Tcp.Listener.addr l)) in
  let fd, _ = Tcp.Listener.accept l in
  let server = Tcp.flow ?time fd and peer = Fun.Async.get peer in
  Stdlib.Fun.protect ~finally:(fun () -> Flow.close peer ; Flow.close server)
  @@ fun () -> f server peer

(* The result of [f], or a failure of the test if [f] takes more than [span] of
   real time: a bug gives a failure, not a hang. *)
let within span f =
  let call = Fun.Async.call f in
  let ended = Action.map (fun v -> `Ended v) (Fun.Async.get' call) in
  match Action.invoke (Action.choose [ended; Mtime.wait_for' span `Late]) with
  | `Ended v ->
      v
  | `Late ->
      Fun.Async.cancel call ;
      Windtrap.fail
        (Printf.sprintf "no end after %g s"
           (Mtime.Span.to_float_ns span /. 1e9) )

let within_2s f = within Mtime.Span.(2 * s) f

(* The result of the call [c], or its exception, within 2 s of real time. *)
let get_2s c = within_2s (fun () -> Fun.Async.get c)

(* Polls [p] each 1 ms until it is true, for at most 2 s of real time. If [p] is
   still false, the test fails with [msg]. *)
let eventually ~msg p =
  let late = Option.get (Mtime.add_span (Mtime.now ()) Mtime.Span.(2 * s)) in
  while (not (p ())) && Mtime.is_earlier (Mtime.now ()) ~than:late do
    Mtime.wait_for Mtime.Span.ms
  done ;
  Windtrap.is_true ~msg (p ())

(* [inner] with counts, as proof that the code took data or set a deadline: the
   bytes that reads gave, and the calls of [set_write_deadline]. [write]
   replaces the write of [inner], and [on_write_deadline] gets each write
   deadline. *)
type counted =
  {flow: Flow.t; read_bytes: int Atomic.t; write_deadlines: int Atomic.t}

let counted ?write ?(on_write_deadline = ignore) inner =
  let read_bytes = Atomic.make 0 and write_deadlines = Atomic.make 0 in
  let took n = ignore (Atomic.fetch_and_add read_bytes n) in
  let read buf off len =
    let n = Flow.read inner buf off len in
    took n ; n
  in
  let read_now buf off len =
    let r = Flow.read_now inner buf off len in
    Option.iter took r ; r
  in
  let set_write_deadline d =
    Atomic.incr write_deadlines ;
    on_write_deadline d ;
    Flow.set_write_deadline inner d
  in
  let write = Option.value write ~default:(Flow.write inner) in
  let flow =
    Flow.make ~time:(Flow.time inner) ~read ~read_now ~write
      ~readable:(Flow.readable' inner ())
      ~shutdown_send:(fun () -> Flow.shutdown_send inner)
      ~close:(fun () -> Flow.close inner)
      ~reset:(fun () -> Flow.reset inner)
      ~set_read_deadline:(Flow.set_read_deadline inner)
      ~set_write_deadline ()
  in
  {flow; read_bytes; write_deadlines}

(* The server on a fake time line *)

module Fake = Fake_time

let ( +: ) = Fake.( +: )

(* For a call [c] that only an advance can end: after the mark [since], the call
   made a timer for [span] after [t0]. It does not end 1 ns before that
   deadline, and it ends at it. The fake time must be at [t0]. *)
let ends_at f ~since ~t0 span c =
  Fake.wait_mono_timer f ~since (t0 +: span) ;
  Fake.advance f Mtime.Span.(span - ns) ;
  Windtrap.is_false ~msg:"ended 1 ns before" (Fake.ends_within_50ms c) ;
  Fake.advance f Mtime.Span.ns ;
  get_2s c

(* The spans of the server on a fake time line. A test advances only to
   deadlines that it proved with [Fake.wait_mono_timer]. *)
let header_h = Mtime.Span.(3 * s)

let idle_i = Mtime.Span.(5 * s)

let grace_g = Mtime.Span.(7 * s)

let linger_l = Mtime.Span.(11 * s)

let read_r = Mtime.Span.(13 * s)

let write_w = Mtime.Span.(17 * s)

let fake_config f =
  { config with
    mono_time= Fake.mono f
  ; posix_time= Fake.posix f
  ; header_timeout= header_h
  ; idle_timeout= idle_i
  ; read_timeout= read_r
  ; write_timeout= write_w
  ; shutdown_grace= grace_g
  ; linger_timeout= linger_l }

(* [f server] with a server on the fake time line [fake]. A failure must not
   hang: at the end, the server aborts, and then the fake time advances one day,
   so that each wait on it ends. The abort also ends the connections of clients
   that [f] left open. A shutdown in place of the abort can hang: the grace
   computes its deadline when it sees the stop, which can come after the
   advance. *)
let with_fake_time_server fake ?(config = fake_config fake) ?engine
    ?(handler = handler) f =
  Affect_unix.Unix.main ~domain_count
  @@ fun () ->
  let server = Server.start ~config ?engine handler in
  Fun.protect
    ~finally:(fun () ->
      Server.abort server ;
      Fake.advance fake Mtime.Span.day ;
      Server.wait server )
    (fun () -> f server)

(* The steps before a header timeout: the connection opened at t0 and waits for
   its first request until t0 + H. At t0 + 1 s, half a head comes, and the
   engine reads it: it waits for the rest until t0 + 1 s + H. *)
let half_head f c ~t0 ~since =
  Fake.wait_mono_timer f ~since (t0 +: header_h) ;
  Fake.advance f Mtime.Span.s ;
  let m = Fake.mark f in
  Flow.write_string c "GET / HTTP/1.1\r\n" ;
  Fake.wait_mono_timer f ~since:m (t0 +: Mtime.Span.s +: header_h)
