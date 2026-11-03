(* The helpers of the caravan tests on the simulator: the helpers for the core
   modules and the helpers for a server. *)

open Weft
open Caravan
module Buf_reader = Private.Buf_reader

(* [show_outcome o] is the text of the end of a body, for the tests of
   [Body.on_end]. *)
let show_outcome = function
  | Body.Ended ->
      "Ended"
  | Failed e ->
      "Failed " ^ Printexc.to_string e
  | Abandoned ->
      "Abandoned"

(* [on_end_log ()] is a hook that records each call as "<outcome> <bytes>", and
   the calls so far, in order. *)
let on_end_log () =
  let log = ref [] in
  ( (fun how ~bytes ->
      log := Printf.sprintf "%s %d" (show_outcome how) bytes :: !log )
  , fun () -> List.rev !log )

(* A numeric host, so that a client needs no DNS answer. Port 0 gets a free
   port. *)
let loopback = Net.Tcp ("127.0.0.1", 0)

(* More than the capacity of a socket of the simulator (64 KiB). Thus a write of
   [big] bytes that the peer does not read waits. *)
let big = 256 * 1024

(* [with_flows sim f] gives [f] the two sides of a TCP connection of the
   simulator as flows of [Tcp.flow]. The two sockets close when [f] ends, also
   when [f] raises. *)
let with_flows sim f =
  Sim_support.with_pair sim (fun client server ->
      f ~server:(Tcp.flow server) ~client:(Tcp.flow client) )

(* [read_all ~size flow] reads [flow] to the end, [size] bytes at a time
   (default 65536), and gives the bytes. *)
let read_all ?(size = 65536) flow =
  let buf = Bytes.create size and out = Buffer.create size in
  let rec loop () =
    match Flow.read flow buf 0 size with
    | 0 ->
        Buffer.contents out
    | n ->
        Buffer.add_subbytes out buf 0 n ;
        loop ()
  in
  loop ()

(* [read_exactly flow n] reads [n] bytes of [flow] and gives them. An end of the
   stream before the [n] bytes raises [Failure]. *)
let read_exactly flow n =
  let buf = Bytes.create n in
  let rec loop got =
    if got < n then
      match Flow.read flow buf got (n - got) with
      | 0 ->
          failwith "read_exactly: the end of the stream before the bytes"
      | k ->
          loop (got + k)
  in
  loop 0 ; Bytes.to_string buf

(* [read_one flow] reads at most 1 byte of [flow]. It gives 1, or 0 at the end
   of the stream. *)
let read_one flow = Flow.read flow (Bytes.create 1) 0 1

(* [span] compares two spans in a check, for example an end time. *)
let span = Windtrap.Testable.make ~pp:Span.pp ~equal:Span.equal

(* [inner] with a count of the bytes that reads gave, as proof that the code
   took data. [write] replaces the write of [inner], [on_write_deadline] gets
   each write deadline, and [on_write_timeout] gets each write timeout.
   [counted] forwards the deadlines and the timeouts, so that the flow keeps the
   limits of the code under test. *)
type counted = {flow: Flow.t; read_bytes: int Atomic.t}

let counted ?write ?(on_write_deadline = ignore) ?(on_write_timeout = ignore)
    inner =
  let read_bytes = Atomic.make 0 in
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
    on_write_deadline d ;
    Flow.set_write_deadline inner d
  in
  let write = Option.value write ~default:(Flow.write inner) in
  let flow =
    Flow.make ~read ~read_now ~write ~readable:(Flow.readable inner)
      ~shutdown_send:(fun () -> Flow.shutdown_send inner)
      ~close:(fun () -> Flow.close inner)
      ~reset:(fun () -> Flow.reset inner)
      ~set_read_deadline:(Flow.set_read_deadline inner)
      ~set_write_deadline
      ~set_read_timeout:(Flow.set_read_timeout inner)
      ~set_write_timeout:(fun t ->
        on_write_timeout t ;
        Flow.set_write_timeout inner t )
      ()
  in
  {flow; read_bytes}

(* The server helpers *)

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

(* [addr_of_authority "host:port"] is the address [Net.Tcp (host, port)]. *)
let addr_of_authority authority =
  let i = String.rindex authority ':' in
  let host = String.sub authority 0 i in
  let port =
    int_of_string
      (String.sub authority (i + 1) (String.length authority - i - 1))
  in
  Net.Tcp (host, port)

(* A CONNECT tunnel to [authority], a "host:port". It copies the bytes in the
   two directions. A fork into the new scope after a cancel of an outer scope
   raises [Scope_closed]: then the caller gets its cancel. *)
let tunnel authority client =
  let upstream = Tcp.connect (addr_of_authority authority) in
  Fun.protect ~finally:(fun () -> Flow.close upstream)
  @@ fun () ->
  Fiber.scope (fun s ->
      ( match
          Fiber.fork ~name:"tunnel.up" s (fun () -> copy client upstream)
        with
      | () ->
          ()
      | exception Fiber.Scope_closed ->
          Private.Cancel.scope_closed () ) ;
      copy upstream client )

(* For the disconnect and stop tests: /wait fills [waiting], then waits 10 s. A
   cancel fills [cancelled]. /fetch does the same while it waits for an answer
   from [upstream]. A test makes new Ivars before it starts. *)
let waiting : unit Ivar.t ref = ref (Ivar.create ())

let cancelled : unit Ivar.t ref = ref (Ivar.create ())

let upstream = ref loopback

(* [reset_ivars ()] gives [waiting] and [cancelled] new Ivars. *)
let reset_ivars () =
  waiting := Ivar.create () ;
  cancelled := Ivar.create ()

let until_cancelled f =
  ignore (Ivar.try_fill !waiting ()) ;
  match f () with
  | r ->
      r
  | exception (Fiber.Cancelled _ as e) ->
      ignore (Ivar.try_fill !cancelled ()) ;
      raise e

let fetch () =
  let flow = Tcp.connect !upstream in
  Fun.protect ~finally:(fun () -> Flow.close flow)
  @@ fun () ->
  Flow.write_string flow "question" ;
  until_cancelled (fun () -> ignore (Flow.read flow (Bytes.create 1) 0 1)) ;
  Response.string "answer"

(* The handlers read the request bodies, except /ignore, which leaves its body
   to the drain or to the linger. CONNECT opens a tunnel. *)
let handler req =
  let body = Request.body req in
  let read () = ignore (Body.to_string body) in
  if Method.equal (Request.meth req) Method.CONNECT then
    Response.upgrade ~status:Status.ok
      (tunnel (Option.get (Request.authority req)))
  else
    match Request.path_only req with
    | "/wait" ->
        until_cancelled (fun () -> Time.sleep Mono (Span.s 10)) ;
        Response.string "late"
    | "/fetch" ->
        fetch ()
    | "/stopping" ->
        (* Fills [waiting]. Then it gives "stopping" when the server starts to
           stop, or "timeout" after 2 s. *)
        ignore (Ivar.try_fill !waiting ()) ;
        let stopping = Conn_info.stopping (Option.get (Request.conn req)) in
        Response.string
          (sync
             (choose
                [ wrap stopping (fun () -> "stopping")
                ; wrap (Time.after Mono (Span.s 2)) (fun () -> "timeout") ] ) )
    | "/is-stopping" ->
        (* Only for a request before the stop: after the stop, the two branches
           are ready, and weft selects one at random. *)
        let stopping = Conn_info.stopping (Option.get (Request.conn req)) in
        Response.string
          (sync (choose [wrap stopping (fun () -> "yes"); always "no"]))
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

(* The spans of the server in the tests of exact time. *)
let header_h = Span.s 3

let idle_i = Span.s 5

let grace_g = Span.s 7

let linger_l = Span.s 11

let read_r = Span.s 13

let write_w = Span.s 17

let timed_config =
  { config with
    header_timeout= header_h
  ; idle_timeout= idle_i
  ; read_timeout= read_r
  ; write_timeout= write_w
  ; shutdown_grace= grace_g
  ; linger_timeout= linger_l }

(* A small engine for the tests of the server lifecycle. It does not use the
   HTTP/1.1 engine of caravan. Thus these tests examine only the server. For
   each request, it reads the head (a request line and fields, no body), calls
   the handler in the fiber of the connection, and writes the status, a
   Content-Length field and the body. It sends "Connection: close" and ends the
   connection when the request asks for it or when the server stops. Between two
   requests, it waits for input or for the stop. *)
module Fake_engine = struct
  let rec fields r acc =
    match Buf_reader.read_line r ~max:8192 with
    | "" ->
        acc
    | line ->
        fields r (String.lowercase_ascii line :: acc)

  let serve (ctx : Engine.ctx) flow handler =
    let r = Buf_reader.of_flow flow in
    let rec next () =
      if ctx.is_stopping () then ()
      else if Buf_reader.buffered r > 0 then request ()
      else
        match
          sync
            (choose
               [ wrap (Flow.readable flow) (fun () -> `Input)
               ; wrap (Conn_info.stopping ctx.conn) (fun () -> `Stop) ] )
        with
        | `Stop ->
            ()
        | `Input -> (
          (* Readiness is not input: a read with no wait. *)
          match Buf_reader.fill_now r with
          | `Bytes ->
              request ()
          | `Eof ->
              ()
          | `Would_block ->
              next () )
    and request () =
      let line = Buf_reader.read_line r ~max:8192 in
      let fields = fields r [] in
      match String.split_on_char ' ' line with
      | [meth; path; _] -> (
          let req = Request.make ~conn:ctx.conn (Method.of_string meth) path in
          let resp = handler req in
          match Response.kind resp with
          | Body b ->
              let body = Body.to_string b in
              let close =
                List.mem "connection: close" fields || ctx.is_stopping ()
              in
              let status = Response.status resp in
              Flow.write_string flow
                (Printf.sprintf
                   "HTTP/1.1 %d %s\r\nContent-Length: %d\r\n%s\r\n%s"
                   (Status.code status) (Status.reason status)
                   (String.length body)
                   (if close then "Connection: close\r\n" else "")
                   body ) ;
              if not close then next ()
          | Upgrade _ ->
              failwith "Fake_engine: no upgrade" )
      | _ ->
          failwith ("Fake_engine: a bad request line: " ^ line)
    in
    next ()
end

(* The requests *)

type body = No_body | Length of string | Chunked of string

type kind =
  { meth: string
  ; path: string
  ; v10: bool
  ; conn: string option (* The Connection field, if one. *)
  ; body: body }

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

(* [get ~conn path] is a GET request for [path], with the Connection field
   [conn] if one. *)
let get ?conn path =
  serialize {meth= "GET"; path; v10= false; conn; body= No_body}

(* The responses, as "status body", and " close" when the server tells that it
   closes. *)

let summary code body close =
  Printf.sprintf "%d %S%s" code body (if close then " close" else "")

(* [read_buffered_exactly r n] reads [n] bytes of the reader [r] and gives them.
   An end of the stream before the [n] bytes raises [Failure]. *)
let read_buffered_exactly r n =
  let buf = Bytes.create n in
  let rec fill got =
    if got < n then
      match Buf_reader.read_into r buf got (n - got) with
      | 0 ->
          failwith
            "read_buffered_exactly: the end of the stream before the bytes"
      | k ->
          fill (got + k)
  in
  fill 0 ; Bytes.to_string buf

(* [read_response ~head ~field r] reads one response of [r] and gives its
   summary. [head] tells that the request was a HEAD. [field] gets each field
   line as it came. *)
let read_response ?(head = false) ?(field = ignore) r =
  let status = Buf_reader.read_line r ~max:1000 in
  let code = int_of_string (String.sub status 9 3) in
  let rec fields acc =
    match Buf_reader.read_line r ~max:1000 with
    | "" ->
        acc
    | line ->
        field line ;
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
          read_buffered_exactly r (int_of_string n)
      | None ->
          to_eof ""
  in
  summary code body close

(* [connect server] connects to [server], and gives the flow and a reader of
   it. *)
let connect server =
  let flow = Tcp.connect (Server.local_addr server) in
  (flow, Buf_reader.of_flow flow)

(* [is_eof r] tells if [r] is at the end of the stream. It waits for a byte or
   the end. *)
let is_eof r = Buf_reader.read_into r (Bytes.create 1) 0 1 = 0

(* [with_server ~config ~engine ~handler f] runs [f server] with a server on
   [loopback]. Call it in the body of [Sim_support.run] or [Sim_support.manual].
   At the end, the server aborts, and [with_server] waits for it. A failure of
   [wait] after an exception of [f] does not hide that exception. A test of a
   graceful stop calls [Server.shutdown] itself. A test that expects a failure
   of the server does not use [with_server]: it calls [Server.wait] itself. *)
let with_server ?(config = config) ?engine ?(handler = handler) f =
  Fiber.scope (fun sw ->
      let server =
        Server.start ~config:{config with addr= loopback} ?engine sw handler
      in
      match f server with
      | v ->
          Server.abort server ; Server.wait server ; v
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          Server.abort server ;
          (try Server.wait server with _ -> ()) ;
          Printexc.raise_with_backtrace e bt )
