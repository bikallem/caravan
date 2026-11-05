(* The test_linux suite: the behavior of caravan on the OS, in Weft_linux.run.
   Each test runs one time on 1 domain and one time on 4 domains. A test waits
   for events, never for a time, except three items of real time: the failure
   limit of 10 s for each test, the linger deadline of 5 s of test 7, and the
   fallback of 2 s of /slow of serve_until_signal, which test 5 does not reach
   when the stop works. *)

open Windtrap
open Weft
open Caravan
module Buf_reader = Private.Buf_reader
module Buf_writer = Private.Buf_writer
module Frame = Caravan_websocket.Private.Frame

let loopback = Server_support.loopback

let config = Server_support.config

let read_response = Server_support.read_response

let get = Server_support.get

(* The failure limit stops a test that hangs. It is not a wait for an event. *)
let limit = Span.s 10

(* [in_world ~domains f] runs [f] in a new Linux world with [domains] domains.
   After the failure limit, the world cancels [f], and the test fails. *)
let in_world ~domains f =
  Weft_linux.run ~domains (fun () ->
      match Fiber.with_timeout Mono limit f with
      | v ->
          v
      | exception Fiber.Timeout ->
          fail "the test did not end before the failure limit of 10 s" )

(* The number of open socket fds of the process: the targets of /proc/self/fd
   that start with "socket:". The world has other fds (epoll, timerfd, eventfd)
   and its thread pool can open and close fds, so the tests count only the
   sockets. *)
let socket_fds () =
  let is_socket fd =
    match Unix.readlink ("/proc/self/fd/" ^ fd) with
    | target ->
        String.starts_with ~prefix:"socket:" target
    | exception Unix.Unix_error _ ->
        (* The fd of the readdir closed before the readlink. *)
        false
  in
  List.length
    (List.filter is_socket (Array.to_list (Sys.readdir "/proc/self/fd")))

(* [in_world_with_no_leak ~msg ~domains f] is [in_world ~domains f], and it
   makes sure that the process has the same socket fds after the world as before
   it. The count is outside the world: if a different domain has a registration
   of a socket, weft does the close(2) later, on the thread of that domain. The
   world ends only after each close(2) of the sockets that fibers closed. *)
let in_world_with_no_leak ~msg ~domains f =
  let before = socket_fds () in
  in_world ~domains f ;
  equal int ~msg before (socket_fds ())

(* [with_refused_port f] calls [f port] with a port that refuses connections: a
   socket that is bound and does not listen. The socket keeps the port, so a
   different process cannot take it during [f]. Call it outside a world: it uses
   [Unix]. *)
let with_refused_port f =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close s)
  @@ fun () ->
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) ;
  match Unix.getsockname s with
  | Unix.ADDR_INET (_, port) ->
      f port
  | Unix.ADDR_UNIX _ ->
      fail "the socket has no port"

let port_of server =
  match Server.local_addr server with
  | Net.Tcp (_, port) ->
      port
  | Net.Unix _ ->
      fail "the server has no port"

let url ?(host = "127.0.0.1") port path =
  Printf.sprintf "http://%s:%d%s" host port path

let read_one = Server_support.read_one

(* [find_sub sub s] is the index of the first [sub] in [s], if one. *)
let find_sub sub s =
  let n = String.length sub in
  let rec at i =
    if i + n > String.length s then None
    else if String.sub s i n = sub then Some i
    else at (i + 1)
  in
  at 0

(* [read_head r] reads a response head of [r]: the status line and the fields,
   with no change of case. *)
let read_head r =
  let status = Buf_reader.read_line r ~max:1000 in
  let rec fields acc =
    match Buf_reader.read_line r ~max:1000 with
    | "" ->
        List.rev acc
    | line ->
        let i = String.index line ':' in
        let value =
          String.trim (String.sub line (i + 1) (String.length line - i - 1))
        in
        fields ((String.lowercase_ascii (String.sub line 0 i), value) :: acc)
  in
  (status, fields [])

(* 1. EADDRINUSE *)

let in_use ~domains () =
  in_world_with_no_leak ~msg:"the socket fds after the two starts" ~domains
  @@ fun () ->
  Server_support.with_server (fun server ->
      let second =
        Fiber.scope (fun sw ->
            match
              Server.start
                ~config:{config with addr= Server.local_addr server}
                sw Server_support.handler
            with
            | s ->
                Server.abort s ; Server.wait s ; "a second server started"
            | exception Net.Error (In_use, _) ->
                "In_use" )
      in
      equal string ~msg:"the second start" "In_use" second ;
      let flow, r = Server_support.connect server in
      Flow.write_string flow (get "/hello") ;
      equal string ~msg:"the first server serves" {|200 "hello"|}
        (read_response r) ;
      Flow.close flow )

(* 2. ECONNREFUSED *)

let refused ~domains () =
  with_refused_port
  @@ fun port ->
  in_world ~domains
  @@ fun () ->
  let cause =
    Caravan_client.run (fun c ->
        match Caravan_client.get c (url port "/") with
        | _ ->
            "a response"
        | exception Caravan_client.Error (Connect cause) ->
            cause )
  in
  equal string ~msg:"the cause of Error (Connect _)" "Connection refused" cause

(* 3. Clients during a shutdown *)

(* [shutdown_client addr ~answered ~ten] connects to [addr], sends a request
   that closes the connection and reads the response. The tenth answered client
   fills [ten]. A client that comes during the shutdown can get a refused
   connect, a reset or the end of the stream. *)
let shutdown_client addr ~answered ~ten () =
  match Tcp.connect addr with
  | exception Net.Error ((Refused | Reset), _) ->
      ()
  | flow -> (
      Fun.protect ~finally:(fun () -> Flow.close flow)
      @@ fun () ->
      match
        Flow.write_string flow (get ~conn:"close" "/hello") ;
        read_response (Buf_reader.of_flow flow)
      with
      | response ->
          equal string {|200 "hello" close|} response ;
          if Atomic.fetch_and_add answered 1 = 9 then
            ignore (Ivar.try_fill ten ())
      | exception (Flow.Closed | Buf_reader.Eof) ->
          () )

let clients_during_shutdown ~domains () =
  in_world_with_no_leak ~msg:"the socket fds after the shutdown" ~domains
  @@ fun () ->
  Fiber.scope (fun sw ->
      let server =
        Server.start
          ~config:{config with max_connections= 8}
          sw Server_support.handler
      in
      let addr = Server.local_addr server in
      let answered = Atomic.make 0 and ten = Ivar.create () in
      Fiber.scope (fun clients ->
          for _ = 1 to 50 do
            Fiber.fork clients (shutdown_client addr ~answered ~ten)
          done ;
          (* The shutdown comes while the other clients connect, wait in the
             queue or get their responses. *)
          sync (Ivar.read ten) ;
          Server.shutdown server ;
          Server.wait server ) )

(* 4. The fds of the client *)

let client_fds ~domains () =
  with_refused_port
  @@ fun refused ->
  in_world_with_no_leak ~msg:"the socket fds after the client and the server"
    ~domains
  @@ fun () ->
  Server_support.with_server (fun server ->
      Caravan_client.run (fun c ->
          let resp = Caravan_client.get c (url (port_of server) "/hello") in
          equal string ~msg:"the GET" "hello"
            (Body.to_string (Caravan_client.body resp)) ;
          match Caravan_client.get c (url refused "/") with
          | _ ->
              fail "a response from a refused port"
          | exception Caravan_client.Error (Connect _) ->
              () ) )

(* 5 and 6. Server.run in a child process *)

(* The child program, serve_until_signal.exe. The rule of test/linux/dune gives
   its path. *)
let program () =
  match Sys.getenv_opt "CARAVAN_SERVE_UNTIL_SIGNAL" with
  | Some path ->
      path
  | None ->
      failwith
        "CARAVAN_SERVE_UNTIL_SIGNAL is not set: run the suite with dune test, \
         or set it to the path of serve_until_signal.exe"

let status =
  let pp ppf : Process.status -> unit = function
    | Exited n ->
        Format.fprintf ppf "Exited %d" n
    | Signaled SIGINT ->
        Format.pp_print_string ppf "Signaled SIGINT"
    | Signaled SIGTERM ->
        Format.pp_print_string ppf "Signaled SIGTERM"
    | Signaled SIGKILL ->
        Format.pp_print_string ppf "Signaled SIGKILL"
    | Signaled _ ->
        Format.pp_print_string ppf "Signaled (a different signal)"
  in
  Testable.make ~pp ~equal:( = )

(* [lines pipe] gives a function that reads the next CRLF line of [pipe]. *)
let lines pipe =
  let pending = Buffer.create 64 and chunk = Bytes.create 256 in
  let rec next () =
    let s = Buffer.contents pending in
    match String.index_opt s '\r' with
    | Some i when i + 1 < String.length s && s.[i + 1] = '\n' ->
        Buffer.clear pending ;
        Buffer.add_substring pending s (i + 2) (String.length s - i - 2) ;
        String.sub s 0 i
    | _ -> (
      match Pipe.read pipe chunk 0 (Bytes.length chunk) with
      | 0 ->
          fail "the output of the program ended"
      | n ->
          Buffer.add_subbytes pending chunk 0 n ;
          next () )
  in
  next

(* [with_program f] starts the child program and calls [f p line], with [line
   ()] the next line of the output of the child. [f] waits for the child. If [f]
   raises, also at the failure limit, the child gets SIGKILL: only this child,
   by its pidfd. *)
let with_program f =
  let p = Process.spawn ~stdout:Piped (program ()) [] in
  let out = Option.get (Process.stdout p) in
  let waited = ref false in
  let wait () =
    let s = Process.wait p in
    waited := true ;
    s
  in
  Fun.protect
    ~finally:(fun () ->
      Fiber.shield (fun () ->
          Pipe.close out ;
          if not !waited then begin
            Process.kill p SIGKILL ;
            ignore (Process.wait p)
          end ) )
    (fun () -> f p (lines out) wait)

(* The talk with the child uses plain [Net], not caravan: thus these tests reach
   no mutant of caravan. *)
let send_get port path =
  let sock = Net.connect (Net.Tcp ("127.0.0.1", port)) in
  Net.write sock (Printf.sprintf "GET %s HTTP/1.1\r\nHost: x\r\n\r\n" path) ;
  sock

(* [response sock] reads [sock] to its end, and gives the summary of the
   response, as [Server_support.summary] does. *)
let response sock =
  let out = Buffer.create 256 and buf = Bytes.create 4096 in
  let rec read () =
    match Net.read sock buf 0 4096 with
    | 0 ->
        ()
    | n ->
        Buffer.add_subbytes out buf 0 n ;
        read ()
  in
  read () ;
  let s = Buffer.contents out in
  match String.split_on_char '\n' s with
  | status :: _ when String.length s > 12 -> (
      let code = int_of_string (String.sub status 9 3) in
      let lower = String.lowercase_ascii s in
      let close = Option.is_some (find_sub "\r\nconnection: close\r\n" lower) in
      match find_sub "\r\n\r\n" s with
      | Some i ->
          Server_support.summary code
            (String.sub s (i + 4) (String.length s - i - 4))
            close
      | None ->
          "no end of the head: " ^ s )
  | _ ->
      Printf.sprintf "no response (%S)" s

let sigterm_ends_request ~domains () =
  in_world ~domains
  @@ fun () ->
  with_program
  @@ fun p line wait ->
  let port = int_of_string (line ()) in
  let sock = send_get port "/slow" in
  Fun.protect ~finally:(fun () -> Net.close sock)
  @@ fun () ->
  equal string "started" (line ()) ;
  Process.kill p SIGTERM ;
  (* /slow gives "timeout" after 2 s with no stop. *)
  equal string ~msg:"the response" {|200 "done" close|} (response sock) ;
  (* The linger of the server waits for the end of our stream. *)
  Net.close sock ;
  equal status ~msg:"the exit" (Exited 0) (wait ())

let third_sigint ~domains () =
  in_world ~domains
  @@ fun () ->
  with_program
  @@ fun p line wait ->
  let port = int_of_string (line ()) in
  let sock = send_get port "/deaf" in
  Fun.protect ~finally:(fun () -> Net.close sock)
  @@ fun () ->
  (* The OS merges two signals of one type that wait. Thus the test waits for
     the line that shows the effect of a signal before it sends the next. *)
  equal string "started" (line ()) ;
  Process.kill p SIGINT ;
  equal string "stopping" (line ()) ;
  Process.kill p SIGINT ;
  equal string "cancelled" (line ()) ;
  (* The handler waits 10 s, deaf to the cancel. Thus a child that does not end
     at the third signal passes the failure limit. *)
  Process.kill p SIGINT ;
  equal status ~msg:"the exit" (Signaled SIGINT) (wait ())

(* 7. Linger, then close *)

(* [with_tcp_pair f] calls [f ~client ~server] with the two sockets of a TCP
   connection on the loopback interface. *)
let with_tcp_pair f =
  let l = Tcp.Listener.listen loopback in
  Fun.protect ~finally:(fun () -> Tcp.Listener.close l)
  @@ fun () ->
  let client = Net.connect (Tcp.Listener.addr l) in
  Fun.protect ~finally:(fun () -> Net.close client)
  @@ fun () ->
  let server, _ = Tcp.Listener.accept l in
  Fun.protect ~finally:(fun () -> Net.close server)
  @@ fun () -> f ~client ~server

let linger_then_close ~domains () =
  in_world ~domains
  @@ fun () ->
  with_tcp_pair
  @@ fun ~client ~server ->
  let client = Tcp.flow client in
  let server = Server_support.counted (Tcp.flow server) in
  Flow.write_string client "req" ;
  Flow.shutdown_send client ;
  let deadline = Private.Deadline.from_now (Span.s 5) in
  Flow.set_read_deadline server.flow deadline ;
  Flow.linger server.flow ~max:100 ;
  equal int ~msg:"the bytes that the linger read" 3
    (Atomic.get server.read_bytes) ;
  is_false ~msg:"the linger ends before its deadline"
    (Private.Deadline.has_passed deadline) ;
  Flow.close server.flow ;
  (* A close with unread input sends a reset. After the linger, no input
     stays. *)
  equal int ~msg:"the end of the stream, not a reset" 0 (read_one client)

(* 8. A shutdown of the send side after a reset *)

let shutdown_after_reset ~domains () =
  in_world ~domains
  @@ fun () ->
  with_tcp_pair
  @@ fun ~client ~server ->
  let server = Tcp.flow server in
  Net.reset client ;
  (* The read waits for the reset itself. *)
  raises Flow.Closed (fun () -> read_one server) ;
  (* Linux gives ENOTCONN. *)
  Flow.shutdown_send server ;
  Flow.close server

(* 9. The names of three accept errors. weft gives the name of each errno, and
   [classify] knows these names. *)

let errnames ~domains () =
  in_world ~domains
  @@ fun () ->
  List.iter
    (fun (errno, name) ->
      equal string
        ~msg:(Printf.sprintf "errname %d" errno)
        name (Weft_posix.errname errno) ;
      is_true
        ~msg:(Printf.sprintf "classify (Other %S) is `Connection" name)
        (Private.Accept_error.classify (Other name) = `Connection) )
    [(71, "EPROTO"); (64, "ENONET"); (112, "EHOSTDOWN")]

(* 10. Date on several domains *)

(* 8 fibers make 20 000 dates each. The seconds come from a set of 4, so that
   the domains change the cache at the same time and also read the second of the
   other domains. *)
let dates ~domains () =
  in_world ~domains
  @@ fun () ->
  let expected p =
    Format.asprintf "%a" Http_date.pp (Private.Date.of_utc (Time.to_utc p))
  in
  let seconds =
    Array.init 4 (fun i ->
        Time.posix_of_unix_s (784_111_777. +. Float.of_int i) )
  in
  let dates = Array.map expected seconds in
  let wrong = Atomic.make None in
  Fiber.scope (fun s ->
      for k = 0 to 7 do
        Fiber.fork s (fun () ->
            for i = 1 to 20_000 do
              let n = (i + k) land 3 in
              let d = Private.Date.of_posix seconds.(n) in
              if d <> dates.(n) then
                ignore
                  (Atomic.compare_and_set wrong None
                     (Some (Printf.sprintf "%S for %S" d dates.(n))) ) ;
              if i mod 1000 = 0 then Fiber.yield ()
            done )
      done ) ;
  is_none ~msg:"a wrong date" ~pp:Format.pp_print_string (Atomic.get wrong)

(* 15. 8 fibers enqueue jobs on the domains while the queue stops. Each job that
   the queue accepted ran or was reported. *)
let jobs_on_domains ~domains () =
  in_world ~domains
  @@ fun () ->
  let ran = Atomic.make 0
  and lost = Atomic.make 0
  and accepted = Atomic.make 0 in
  let q =
    Caravan_jobs.create ~workers:4 ~on_error:(fun _ -> Atomic.incr lost) ()
  in
  let j =
    Caravan_jobs.job ~name:"n" ~retry:Caravan_jobs.no_retry (fun () ->
        Fiber.yield () ; Atomic.incr ran )
  in
  Fiber.scope (fun sw ->
      Caravan_jobs.start q sw ;
      for k = 0 to 7 do
        Fiber.fork sw (fun () ->
            for i = 1 to 2_000 do
              let delay =
                if (i + k) mod 5 = 0 then Some (Span.us i) else None
              in
              ( match Caravan_jobs.enqueue q ?delay j () with
              | Ok () ->
                  Atomic.incr accepted
              | Error _ ->
                  () ) ;
              if i mod 100 = 0 then Fiber.yield ()
            done )
      done ;
      Time.sleep Mono (Span.ms 1) ;
      Caravan_jobs.stop ~grace:(Span.ms 1) q ) ;
  equal int ~msg:"each accepted job ran or was reported" (Atomic.get accepted)
    (Atomic.get ran + Atomic.get lost)

(* 11. The HTTP/1.1 smoke test *)

let mib = 1024 * 1024

(* Not one byte value, so that a write from a wrong offset changes the bytes. *)
let payload = String.init mib (fun i -> Char.chr (i mod 251))

let keep_alive server =
  let flow, r = Server_support.connect server in
  Fun.protect ~finally:(fun () -> Flow.close flow)
  @@ fun () ->
  for _ = 1 to 2 do
    Flow.write_string flow (get "/hello") ;
    equal string ~msg:"a request on the kept-alive connection" {|200 "hello"|}
      (read_response r)
  done

(* The echo of 1 MiB: a real socket gives partial writes and EAGAIN. With the
   default buffers, loopback takes 1 MiB in one write. Thus the client socket
   has a small send buffer, so that its writes are partial and wait. A receive
   buffer below 64 KiB makes the echo take many seconds. The body goes in a
   different fiber, because the server sends the echo while it reads. *)
let echo server =
  let sock = Net.connect (Server.local_addr server) in
  Net.set_option sock Send_buffer 4096 ;
  Net.set_option sock Recv_buffer 65536 ;
  let flow = Tcp.flow sock in
  let r = Buf_reader.of_flow flow in
  Fun.protect ~finally:(fun () -> Flow.close flow)
  @@ fun () ->
  Fiber.scope (fun s ->
      Fiber.fork s (fun () ->
          Flow.write_string flow
            (Printf.sprintf
               "POST /echo HTTP/1.1\r\nHost: x\r\nContent-Length: %d\r\n\r\n%s"
               mib payload ) ) ;
      let status, fields = read_head r in
      equal string ~msg:"the status of the echo" "HTTP/1.1 200 OK" status ;
      equal (option string) ~msg:"the length of the echo"
        (Some (string_of_int mib))
        (List.assoc_opt "content-length" fields) ;
      let body = Server_support.read_buffered_exactly r mib in
      is_true ~msg:"the echo is the payload" (String.equal body payload) )

let tunnel server =
  let up = Tcp.Listener.listen loopback in
  Fun.protect ~finally:(fun () -> Tcp.Listener.close up)
  @@ fun () ->
  let authority =
    match Tcp.Listener.addr up with
    | Net.Tcp (host, port) ->
        Printf.sprintf "%s:%d" host port
    | Net.Unix _ ->
        fail "the upstream has no port"
  in
  let flow, r = Server_support.connect server in
  Fun.protect ~finally:(fun () -> Flow.close flow)
  @@ fun () ->
  Flow.write_string flow
    (Printf.sprintf "CONNECT %s HTTP/1.1\r\nHost: %s\r\n\r\n" authority
       authority ) ;
  let sock, _ = Tcp.Listener.accept up in
  let upstream = Tcp.flow sock in
  Fun.protect ~finally:(fun () -> Flow.close upstream)
  @@ fun () ->
  let status, _ = read_head r in
  equal string ~msg:"the status of CONNECT" "HTTP/1.1 200 OK" status ;
  Flow.write_string flow "ping" ;
  equal string ~msg:"to the upstream" "ping"
    (Server_support.read_exactly upstream 4) ;
  Flow.write_string upstream "pong" ;
  equal string ~msg:"from the upstream" "pong"
    (Server_support.read_buffered_exactly r 4) ;
  Flow.shutdown_send flow ;
  equal int ~msg:"the end of the stream at the upstream" 0 (read_one upstream) ;
  Flow.shutdown_send upstream ;
  is_true ~msg:"the end of the stream at the client" (Server_support.is_eof r)

let fifty_clients server =
  let one () =
    let flow, r = Server_support.connect server in
    Fun.protect ~finally:(fun () -> Flow.close flow)
    @@ fun () ->
    Flow.write_string flow (get ~conn:"close" "/hello") ;
    read_response r
  in
  let responses =
    Fiber.scope (fun s ->
        List.map Promise.await (List.init 50 (fun _ -> Fiber.fork_promise s one)) )
  in
  equal (list string) ~msg:"the responses of 50 clients"
    (List.init 50 (fun _ -> {|200 "hello" close|}))
    responses

let http_smoke ~domains () =
  in_world ~domains
  @@ fun () ->
  Server_support.with_server (fun server ->
      keep_alive server ; echo server ; tunnel server ; fifty_clients server )

(* 12. The WebSocket and client smoke test *)

let ws_handler req =
  match Request.path_only req with
  | "/ws" ->
      Caravan_websocket.accept req (fun c ->
          let rec loop () =
            Caravan_websocket.(send c (recv c)) ;
            loop ()
          in
          loop () )
  | _ ->
      Server_support.handler req

(* The key and the answer of RFC 6455 section 1.3. *)
let ws_key = "dGhlIHNhbXBsZSBub25jZQ=="

let ws_accept = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="

let websocket server =
  let flow, r = Server_support.connect server in
  Fun.protect ~finally:(fun () -> Flow.close flow)
  @@ fun () ->
  Flow.write_string flow
    (Printf.sprintf
       "GET /ws HTTP/1.1\r\n\
        Host: x\r\n\
        Upgrade: websocket\r\n\
        Connection: Upgrade\r\n\
        Sec-WebSocket-Key: %s\r\n\
        Sec-WebSocket-Version: 13\r\n\
        \r\n"
       ws_key ) ;
  let status, fields = read_head r in
  equal string ~msg:"the status of the upgrade"
    "HTTP/1.1 101 Switching Protocols" status ;
  equal (option string) ~msg:"Sec-WebSocket-Accept" (Some ws_accept)
    (List.assoc_opt "sec-websocket-accept" fields) ;
  let w = Buf_writer.make (Flow.write flow) in
  let send (frame : Frame.t) =
    Frame.write ~mask:"\x12\x34\x56\x78" w frame ;
    Buf_writer.flush w
  in
  let recv () = Frame.read r ~masked:false ~max:1000 in
  send {fin= true; opcode= Text; payload= "hello"} ;
  let echo = recv () in
  is_true ~msg:"the echo is the Text frame" (echo.opcode = Text) ;
  equal string ~msg:"the echo" "hello" echo.payload ;
  send {fin= true; opcode= Close; payload= "\x03\xe8"} ;
  let close = recv () in
  is_true ~msg:"the reply is a Close frame" (close.opcode = Close) ;
  equal string ~msg:"the code of the reply" "\x03\xe8" close.payload ;
  is_true ~msg:"the end of the stream, not a reset" (Server_support.is_eof r)

(* "localhost" can give ::1 first, and the server listens only on 127.0.0.1: the
   client tries the next address. *)
let client server =
  let port = port_of server in
  Caravan_client.run (fun c ->
      let resp = Caravan_client.get c (url ~host:"localhost" port "/hello") in
      equal string ~msg:"the GET of localhost" "hello"
        (Body.to_string (Caravan_client.body resp)) ;
      let req =
        Request.make
          ~authority:(Printf.sprintf "127.0.0.1:%d" port)
          ~body:(Body.of_string "data") Method.POST "/echo"
      in
      Caravan_client.with_response c req (fun resp ->
          equal int ~msg:"the status of the POST" 200
            (Status.code (Response.status resp)) ;
          equal string ~msg:"the echo of the POST" "data"
            (Body.to_string (Caravan_client.body resp)) ) )

let websocket_and_client_smoke ~domains () =
  in_world ~domains
  @@ fun () ->
  Server_support.with_server ~handler:ws_handler (fun server ->
      websocket server ; client server )

(* The suite *)

(* 13. Static files on the thread pool *)

(* [static_files] writes a directory with Unix before the world, then serves it
   with caravan.static: the reads go through the pool of weft. A file with mode
   000 gives 404, and its body never leaves, unless the process is root (root
   reads each file). *)
let static_files ~domains () =
  let dir = Filename.temp_dir "caravan_static" "" in
  let data = String.init (100 * 1024) (fun i -> Char.chr (i mod 251)) in
  let write name text =
    Out_channel.with_open_bin (Filename.concat dir name) (fun oc ->
        Out_channel.output_string oc text )
  in
  write "a.bin" data ;
  write "secret" "secret" ;
  Unix.chmod (Filename.concat dir "secret") 0 ;
  Fun.protect ~finally:(fun () ->
      Sys.remove (Filename.concat dir "a.bin") ;
      Sys.remove (Filename.concat dir "secret") ;
      Sys.rmdir dir )
  @@ fun () ->
  in_world ~domains
  @@ fun () ->
  let get segments =
    let r = Caravan_static.dir ~root:dir segments (Request.make GET "/") in
    match Response.kind r with
    | Response.Body b ->
        (Status.code (Response.status r), Body.to_string b)
    | Response.Upgrade _ ->
        fail "an upgrade"
  in
  let code, body = get ["a.bin"] in
  equal int 200 code ;
  is_true ~msg:"the bytes of a.bin" (String.equal data body) ;
  equal int 404 (fst (get ["a.bin"; ""])) ;
  equal int 404 (fst (get ["none"])) ;
  if Unix.geteuid () <> 0 then equal int 404 (fst (get ["secret"])) ;
  (* A special file is not a regular file. *)
  let r = Caravan_static.dir ~root:"/dev" ["null"] (Request.make GET "/") in
  equal int 404 (Status.code (Response.status r))

(* 14. TLS on loopback *)

(* [tls_get] serves HTTPS with caravan.tls and the generator of the OS, and
   sends one GET with the TLS client of caravan.tls. The certificates come from
   Tls_support. *)
let tls_get ~domains () =
  Mirage_crypto_rng_unix.use_default () ;
  let scfg, ccfg =
    Tls_support.configs
      ~time:(fun () -> Ptime.of_float_s (Unix.gettimeofday ()))
      ()
  in
  in_world ~domains
  @@ fun () ->
  Server_support.with_server
    ~engine:(Caravan_tls.engine scfg Server.http1)
    ~handler:(fun req -> Response.string (Request.scheme req))
    (fun server ->
      let flow, _ =
        Caravan_tls.client ccfg
          (Tcp.connect (Net.Tcp ("127.0.0.1", port_of server)))
      in
      Fun.protect ~finally:(fun () -> Flow.close flow)
      @@ fun () ->
      Flow.write_string flow
        "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" ;
      let b = Buffer.create 256 and buf = Bytes.create 4096 in
      let rec go () =
        match Flow.read flow buf 0 4096 with
        | 0 ->
            ()
        | n ->
            Buffer.add_subbytes b buf 0 n ;
            go ()
      in
      go () ;
      let r = Buffer.contents b in
      is_true ~msg:r (String.starts_with ~prefix:"HTTP/1.1 200" r) ;
      is_true ~msg:r (String.ends_with ~suffix:"https" r) )

let tests =
  [ ( "1. EADDRINUSE: a start on the port of a running server gives In_use"
    , in_use )
  ; ( "2. ECONNREFUSED: a client GET to a closed port gives Error (Connect \
       \"Connection refused\")"
    , refused )
  ; ("3. clients during a shutdown leave no socket fd", clients_during_shutdown)
  ; ( "4. a client GET, a connect failure and the end of the client leave no \
       socket fd"
    , client_fds )
  ; ( "5. Server.run: SIGTERM ends the request in progress, then exit 0"
    , sigterm_ends_request )
  ; ( "6. Server.run: a third SIGINT ends a server with a deaf handler"
    , third_sigint )
  ; ( "7. linger, then close, gives the peer the end of the stream"
    , linger_then_close )
  ; ( "8. shutdown_send after a peer reset does nothing (ENOTCONN)"
    , shutdown_after_reset )
  ; ( "9. weft gives the names of EPROTO, ENONET and EHOSTDOWN, and classify \
       knows them"
    , errnames )
  ; ("10. Date on several domains", dates)
  ; ( "11. smoke: keep-alive, a 1 MiB echo, a CONNECT tunnel and 50 clients"
    , http_smoke )
  ; ( "12. smoke: a WebSocket echo and close, a client GET and POST"
    , websocket_and_client_smoke )
  ; ("13. static files on the thread pool, and EACCES", static_files)
  ; ("14. TLS on loopback, with the generator of the OS", tls_get)
  ; ("15. jobs on the domains during the stop of the queue", jobs_on_domains) ]

let on ~domains name =
  group name (List.map (fun (n, t) -> test n (t ~domains)) tests)

let () =
  exit
    (Windtrap.run "linux" [on ~domains:1 "1 domain"; on ~domains:4 "4 domains"])
