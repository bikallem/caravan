(* Tests over real loopback sockets, inside affect's scheduler. *)

open Windtrap
open Affect
open Caravan
module Mtime = Affect_unix.Mtime

let domain_count = Server_support.domain_count

let in_main f = Affect_unix.Unix.main ~domain_count f

let loopback = Server_support.loopback

(* A connected (client, server) pair of flows. *)
let with_pair f =
  in_main
  @@ fun () ->
  let l = Tcp.Listener.listen loopback in
  Fun.protect ~finally:(fun () -> Tcp.Listener.close l)
  @@ fun () ->
  let client = Fun.Async.call (fun () -> Tcp.connect (Tcp.Listener.addr l)) in
  let fd, _ = Tcp.Listener.accept l in
  let server = Tcp.flow fd and client = Fun.Async.get client in
  Fun.protect ~finally:(fun () -> Flow.close client ; Flow.close server)
  @@ fun () -> f client server

let read_all flow =
  let buf = Bytes.create 65536 and out = Buffer.create 65536 in
  let rec loop () =
    match Flow.read flow buf 0 (Bytes.length buf) with
    | 0 ->
        Buffer.contents out
    | n ->
        Buffer.add_subbytes out buf 0 n ;
        loop ()
  in
  loop ()

let read_exactly flow n =
  let buf = Bytes.create n in
  let rec loop got =
    if got < n then loop (got + Flow.read flow buf got (n - got))
  in
  loop 0 ; Bytes.to_string buf

let deadline_in n =
  Mtime.add_span (Mtime.now ()) Mtime.Span.(n * ms) |> Option.get

let elapsed_ms c = Mtime.Span.to_float_ns (Mtime.count c) /. 1e6

(* [call] must end with Timeout in less than 2 s. If it does not, the test
   cancels it, so that a failure does not stop the suite. *)
let times_out call =
  let ended = Action.map (fun () -> `Ended) (Fun.Async.get' call) in
  match
    Action.invoke
      (Action.choose [ended; Mtime.wait_for' Mtime.Span.(2 * s) `Late])
  with
  | exception Flow.Timeout ->
      ()
  | `Ended ->
      fail "ended without Timeout"
  | `Late ->
      Fun.Async.cancel call ;
      (try Fun.Async.get call with _ -> ()) ;
      fail "no Timeout after 2 s"

let tcp_tests =
  group "Tcp"
    [ test "listening on port 0 gets a port" (fun () ->
          in_main
          @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          ( match Tcp.Listener.addr l with
          | Unix.ADDR_INET (_, port) ->
              is_true (port > 0)
          | Unix.ADDR_UNIX _ ->
              fail "not an inet address" ) ;
          Tcp.Listener.close l )
    ; test "a request and its echo" (fun () ->
          with_pair
          @@ fun client server ->
          Flow.write_string client "ping" ;
          equal string "ping" (read_exactly server 4) ;
          Flow.write_string server "pong" ;
          equal string "pong" (read_exactly client 4) )
    ; test "read_now does not wait for input" (fun () ->
          with_pair
          @@ fun client server ->
          let buf = Bytes.create 8 in
          let now () = Flow.read_now server buf 0 8 in
          (* A wakeup may come without input. *)
          let rec next () =
            Action.invoke (Flow.readable' server ()) ;
            match now () with None -> next () | r -> r
          in
          let before = now () in
          Flow.write_string client "hi" ;
          let data = next () in
          Flow.shutdown_send client ;
          equal
            (list (option int))
            [None; Some 2; Some 0]
            [before; data; next ()] )
    ; test "half-close: the peer reads the end and can still answer" (fun () ->
          with_pair
          @@ fun client server ->
          Flow.write_string client "request" ;
          Flow.shutdown_send client ;
          equal string "request" (read_all server) ;
          Flow.write_string server "response" ;
          Flow.shutdown_send server ;
          equal string "response" (read_all client) )
    ; test "4 MiB each way at once" (fun () ->
          with_pair
          @@ fun client server ->
          let data =
            String.init (4 * 1024 * 1024) (fun i -> Char.chr (i land 0xff))
          in
          let send flow =
            Fun.Async.call (fun () ->
                Flow.write_string flow data ;
                Flow.shutdown_send flow )
          in
          let s1 = send client and s2 = send server in
          let r1 = Fun.Async.call (fun () -> read_all server) in
          let r2 = Fun.Async.call (fun () -> read_all client) in
          Fun.Async.get s1 ;
          Fun.Async.get s2 ;
          is_true ~msg:"server received it"
            (String.equal data (Fun.Async.get r1)) ;
          is_true ~msg:"client received it"
            (String.equal data (Fun.Async.get r2)) )
    ; test "a read deadline ends a wait with Timeout" (fun () ->
          with_pair
          @@ fun _client server ->
          let c = Mtime.counter () in
          Flow.set_read_deadline server (Some (deadline_in 50)) ;
          raises Flow.Timeout (fun () -> Flow.read server (Bytes.create 1) 0 1) ;
          let ms = elapsed_ms c in
          is_true
            ~msg:(Printf.sprintf "took %.0fms" ms)
            (ms >= 40. && ms < 1000.) ;
          Flow.set_read_deadline server (Some (Mtime.now ())) ;
          raises Flow.Timeout (fun () -> Flow.read server (Bytes.create 1) 0 1) )
    ; test "without a deadline a read waits for data" (fun () ->
          with_pair
          @@ fun client server ->
          Flow.set_read_deadline server (Some (deadline_in 10)) ;
          Flow.set_read_deadline server None ;
          let _writer =
            Fun.Async.call (fun () ->
                Mtime.wait_for Mtime.Span.(100 * ms) ;
                Flow.write_string client "late" )
          in
          equal string "late" (read_exactly server 4) )
    ; test "a write deadline ends a blocked write with Timeout" (fun () ->
          with_pair
          @@ fun client _server ->
          let chunk = Bytes.make (1024 * 1024) 'x' in
          Flow.set_write_deadline client (Some (deadline_in 200)) ;
          raises Flow.Timeout (fun () ->
              for _ = 1 to 256 do
                Flow.write client chunk 0 (Bytes.length chunk)
              done ) )
    ; (* A deadline set from a different fiber reaches a read or a write that
         waits already. *)
      test "a new deadline ends a read that waits" (fun () ->
          with_pair
          @@ fun _client server ->
          let reader =
            Fun.Async.call (fun () ->
                ignore (Flow.read server (Bytes.create 1) 0 1) )
          in
          Mtime.wait_for Mtime.Span.(20 * ms) ;
          Flow.set_read_deadline server (Some (deadline_in 20)) ;
          times_out reader )
    ; test "a new deadline ends a write that waits" (fun () ->
          with_pair
          @@ fun client _server ->
          let chunk = Bytes.make (1024 * 1024) 'x' in
          let writer =
            Fun.Async.call (fun () ->
                for _ = 1 to 256 do
                  Flow.write client chunk 0 (Bytes.length chunk)
                done )
          in
          Mtime.wait_for Mtime.Span.(50 * ms) ;
          Flow.set_write_deadline client (Some (deadline_in 20)) ;
          times_out writer )
    ; test "a deadline removed from a read that waits stops it no more"
        (fun () ->
          with_pair
          @@ fun client server ->
          Flow.set_read_deadline server (Some (deadline_in 100)) ;
          let reader = Fun.Async.call (fun () -> read_exactly server 4) in
          Mtime.wait_for Mtime.Span.(20 * ms) ;
          Flow.set_read_deadline server None ;
          Mtime.wait_for Mtime.Span.(150 * ms) ;
          Flow.write_string client "late" ;
          equal string "late" (Fun.Async.get reader) )
    ; test "writing to a closed peer raises Closed" (fun () ->
          with_pair
          @@ fun client server ->
          Flow.close server ;
          Mtime.wait_for Mtime.Span.(20 * ms) ;
          raises Flow.Closed (fun () ->
              for _ = 1 to 100 do
                Flow.write_string client "x" ;
                Mtime.wait_for Mtime.Span.(5 * ms)
              done ) )
    ; test "a reset connection raises Closed; shutdown_send then does nothing"
        (fun () ->
          in_main
          @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          (* A raw peer that closes with SO_LINGER 0, which sends a RST. *)
          let peer = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
          Unix.connect peer (Tcp.Listener.addr l) ;
          let fd, _ = Tcp.Listener.accept l in
          let server = Tcp.flow fd in
          Unix.setsockopt_optint peer Unix.SO_LINGER (Some 0) ;
          Unix.close peer ;
          Mtime.wait_for Mtime.Span.(20 * ms) ;
          raises Flow.Closed (fun () -> Flow.read server (Bytes.create 1) 0 1) ;
          Flow.shutdown_send server ;
          Flow.close server ;
          Tcp.Listener.close l )
    ; test "reset makes the peer see a reset, not the end of the stream"
        (fun () ->
          with_pair
          @@ fun client server ->
          Flow.write_string server "cut" ;
          Flow.reset server ;
          equal string "cut" (read_exactly client 3) ;
          raises Flow.Closed (fun () -> Flow.read client (Bytes.create 1) 0 1) ;
          Flow.close client )
    ; test "cancelling a blocked read" (fun () ->
          with_pair
          @@ fun _client server ->
          let reader =
            Fun.Async.call (fun () -> Flow.read server (Bytes.create 1) 0 1)
          in
          Mtime.wait_for Mtime.Span.(50 * ms) ;
          Fun.Async.cancel reader ;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get reader) ;
          Flow.close server ;
          raises Flow.Closed (fun () -> Flow.read server (Bytes.create 1) 0 1) )
    ; (* A different fiber closes the flow. The waits end with Closed, and the
         fd keeps its number until they end. *)
      test "a close ends a read that waits with Closed" (fun () ->
          with_pair
          @@ fun _client server ->
          let reader =
            Fun.Async.call (fun () -> Flow.read server (Bytes.create 1) 0 1)
          in
          Mtime.wait_for Mtime.Span.(20 * ms) ;
          Flow.close server ;
          raises Flow.Closed (fun () -> Fun.Async.get reader) )
    ; test "a close ends a write that waits with Closed" (fun () ->
          with_pair
          @@ fun client _server ->
          let chunk = Bytes.make (1024 * 1024) 'x' in
          let writer =
            Fun.Async.call (fun () ->
                for _ = 1 to 256 do
                  Flow.write client chunk 0 (Bytes.length chunk)
                done )
          in
          Mtime.wait_for Mtime.Span.(50 * ms) ;
          Flow.close client ;
          raises Flow.Closed (fun () -> Fun.Async.get writer) )
    ; test "the fd closes when the last read that waits ends" (fun () ->
          with_pair
          @@ fun client server ->
          let reader =
            Fun.Async.call (fun () -> Flow.read server (Bytes.create 1) 0 1)
          in
          Mtime.wait_for Mtime.Span.(20 * ms) ;
          Flow.close server ;
          raises Flow.Closed (fun () -> Fun.Async.get reader) ;
          Flow.set_read_deadline client (Some (deadline_in 1000)) ;
          equal int ~msg:"the end of the stream" 0
            (Flow.read client (Bytes.create 1) 0 1) )
    ; test
        "a read that waits on a closed flow takes no bytes of a new connection"
        (fun () ->
          in_main
          @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          Fun.protect ~finally:(fun () -> Tcp.Listener.close l)
          @@ fun () ->
          let connect () =
            Fun.Async.call (fun () -> Tcp.connect (Tcp.Listener.addr l))
          in
          let accept () = Tcp.flow (fst (Tcp.Listener.accept l)) in
          let first = connect () in
          let old = accept () in
          let buf = Bytes.create 64 in
          let reader = Fun.Async.call (fun () -> Flow.read old buf 0 64) in
          Mtime.wait_for Mtime.Span.(20 * ms) ;
          Flow.close old ;
          let second = connect () in
          let fresh = accept () in
          let client = Fun.Async.get second in
          Flow.write_string client "0123456789" ;
          raises Flow.Closed (fun () -> Fun.Async.get reader) ;
          equal string "0123456789" (read_exactly fresh 10) ;
          List.iter Flow.close [fresh; client; Fun.Async.get first] )
    ; test "connecting to a closed port fails" (fun () ->
          in_main
          @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          let addr = Tcp.Listener.addr l in
          Tcp.Listener.close l ;
          raises_match
            (function
              | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> true | _ -> false )
            (fun () -> Tcp.connect addr) )
    ; test "wait_acceptable is enabled by a pending connection" (fun () ->
          in_main
          @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          let ready () =
            Action.invoke
              (Action.choose
                 [ Tcp.Listener.wait_acceptable l true
                 ; Mtime.wait_for' Mtime.Span.(30 * ms) false ] )
          in
          is_true ~msg:"nothing pending" (not (ready ())) ;
          let client = Tcp.connect (Tcp.Listener.addr l) in
          is_true ~msg:"a connection is pending" (ready ()) ;
          let fd, _ = Tcp.Listener.accept l in
          Unix.close fd ; Flow.close client ; Tcp.Listener.close l )
    ; test "try_accept does not wait" (fun () ->
          in_main
          @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          is_true ~msg:"nothing pending" (Tcp.Listener.try_accept l = None) ;
          let client = Tcp.connect (Tcp.Listener.addr l) in
          let rec accepted tries =
            match Tcp.Listener.try_accept l with
            | Some (fd, _) ->
                Unix.close fd ; true
            | None ->
                tries > 0
                &&
                ( Mtime.wait_for Mtime.Span.(5 * ms) ;
                  accepted (tries - 1) )
          in
          is_true ~msg:"a pending connection" (accepted 20) ;
          is_true ~msg:"nothing pending again" (Tcp.Listener.try_accept l = None) ;
          Flow.close client ;
          Tcp.Listener.close l ) ]

(* Server *)

let quiet = Server_support.config

(* Set when /sleep or /pause starts, so that a test can stop the server while it
   is in progress. *)
let started = ref (Cell.Once.make ())

let handler req =
  match Request.path_only req with
  | "/sleep" ->
      Cell.Once.try_set !started () ;
      Mtime.wait_for Mtime.Span.(10 * s) ;
      Response.string "late"
  | "/pause" ->
      Cell.Once.try_set !started () ;
      Mtime.wait_for Mtime.Span.(300 * ms) ;
      Response.string "done"
  | "/id" ->
      Response.string
        ( match Request.conn req with
        | Some c ->
            string_of_int (Conn_info.id c)
        | None ->
            "-" )
  | _ ->
      Response.string "hello"

let get ?(close = false) path =
  Printf.sprintf "GET %s HTTP/1.1\r\nHost: x\r\n%s\r\n" path
    (if close then "Connection: close\r\n" else "")

(* Reads one response with a Content-Length body from [r]. *)
let read_response_from r =
  let status = Private.Buf_reader.read_line r ~max:1000 in
  let rec headers len =
    match Private.Buf_reader.read_line r ~max:1000 with
    | "" ->
        len
    | line ->
        let lower = String.lowercase_ascii line in
        if String.starts_with ~prefix:"content-length: " lower then
          headers (int_of_string (String.sub line 16 (String.length line - 16)))
        else headers len
  in
  let len = headers 0 in
  let buf = Bytes.create len in
  let rec body got =
    if got < len then
      match Private.Buf_reader.read_into r buf got (len - got) with
      | 0 ->
          failwith "the response ended early"
      | n ->
          body (got + n)
  in
  body 0 ;
  (status, Bytes.to_string buf)

let read_response flow = read_response_from (Private.Buf_reader.of_flow flow)

let with_server ?(config = quiet) ?engine f =
  Server_support.with_server ~config ?engine ~handler f

let server_tests =
  group "Server"
    [ test "serves a GET on a free port" (fun () ->
          with_server
          @@ fun server ->
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get ~close:true "/") ;
          equal string
            "HTTP/1.1 200 OK\r\n\
             Content-Type: text/plain; charset=utf-8\r\n\
             Content-Length: 5\r\n\
             Connection: close\r\n\
             \r\n\
             hello"
            (read_all c) ;
          Flow.close c )
    ; test "keeps a connection alive" (fun () ->
          with_server
          @@ fun server ->
          let c = Tcp.connect (Server.local_addr server) in
          let r = Private.Buf_reader.of_flow c in
          Flow.write_string c (get "/id") ;
          let status, id1 = read_response_from r in
          Flow.write_string c (get "/id") ;
          let _, id2 = read_response_from r in
          equal string "HTTP/1.1 200 OK" status ;
          equal string id1 id2 ;
          Flow.close c )
    ; test "connections are numbered in order from 0" (fun () ->
          with_server
          @@ fun server ->
          let id () =
            let c = Tcp.connect (Server.local_addr server) in
            Flow.write_string c (get ~close:true "/id") ;
            let _, id = read_response c in
            Flow.close c ; id
          in
          let first = id () in
          let second = id () in
          equal (list string) ["0"; "1"] [first; second] )
    ; test "serves 20 clients at once" (fun () ->
          with_server
          @@ fun server ->
          let clients =
            List.init 20 (fun _ ->
                Fun.Async.call (fun () ->
                    let c = Tcp.connect (Server.local_addr server) in
                    Flow.write_string c (get ~close:true "/") ;
                    let _, body = read_response c in
                    Flow.close c ; body ) )
          in
          equal (list string)
            (List.init 20 (fun _ -> "hello"))
            (Fun.Async.get_all clients) )
    ; test "max_connections holds back extra connections" (fun () ->
          with_server ~config:{quiet with max_connections= 1}
          @@ fun server ->
          let first = Tcp.connect (Server.local_addr server) in
          Flow.write_string first (get "/") ;
          ignore (read_response first) ;
          let second = Tcp.connect (Server.local_addr server) in
          Flow.write_string second (get ~close:true "/") ;
          Flow.set_read_deadline second (Some (deadline_in 200)) ;
          raises Flow.Timeout (fun () -> Flow.read second (Bytes.create 1) 0 1) ;
          Flow.set_read_deadline second None ;
          Flow.close first ;
          let _, body = read_response second in
          equal string "hello" body ; Flow.close second )
    ; test "requests that outlast shutdown_grace are cancelled" (fun () ->
          in_main
          @@ fun () ->
          started := Cell.Once.make () ;
          let config = {quiet with shutdown_grace= Mtime.Span.(100 * ms)} in
          let server = Server.start ~config handler in
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get "/sleep") ;
          Cell.Once.get !started ;
          let clock = Mtime.counter () in
          Server.shutdown server ;
          Server.wait server ;
          let waited = elapsed_ms clock in
          is_true
            ~msg:(Printf.sprintf "waited %.0fms" waited)
            (waited > 90. && waited < 1000.) ;
          equal string "" (read_all c) ;
          Flow.close c )
    ; test "abort cancels requests in progress without a grace" (fun () ->
          in_main
          @@ fun () ->
          started := Cell.Once.make () ;
          let server = Server.start ~config:quiet handler in
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get "/sleep") ;
          Cell.Once.get !started ;
          let clock = Mtime.counter () in
          Server.abort server ;
          Server.wait server ;
          is_true ~msg:"well before the 10s grace" (elapsed_ms clock < 1000.) ;
          equal string "" (read_all c) ;
          Flow.close c )
    ; test "a request in progress at shutdown finishes, closing its connection"
        (fun () ->
          in_main
          @@ fun () ->
          started := Cell.Once.make () ;
          let server = Server.start ~config:quiet handler in
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get "/pause") ;
          Cell.Once.get !started ;
          let clock = Mtime.counter () in
          Server.shutdown server ;
          equal string
            "HTTP/1.1 200 OK\r\n\
             Content-Type: text/plain; charset=utf-8\r\n\
             Content-Length: 4\r\n\
             Connection: close\r\n\
             \r\n\
             done"
            (read_all c) ;
          Flow.close c ;
          Server.wait server ;
          let waited = elapsed_ms clock in
          is_true
            ~msg:(Printf.sprintf "waited %.0fms, not the 10s grace" waited)
            (waited < 2000.) )
    ; test "idle connections close at once on shutdown" (fun () ->
          in_main
          @@ fun () ->
          let server = Server.start ~config:quiet handler in
          let c = Tcp.connect (Server.local_addr server) in
          let r = Private.Buf_reader.of_flow c in
          Flow.write_string c (get "/") ;
          ignore (read_response_from r) ;
          let clock = Mtime.counter () in
          Server.shutdown server ;
          equal int ~msg:"end of stream" 0
            (Private.Buf_reader.read_into r (Bytes.create 1) 0 1) ;
          Server.wait server ;
          is_true ~msg:"well before the 10s grace" (elapsed_ms clock < 2000.) ;
          Flow.close c )
    ; test "an engine sees the server stopping" (fun () ->
          let engine (ctx : Engine.ctx) flow _handler =
            let before = ctx.is_stopping () in
            Flow.write_string flow "ready " ;
            Action.invoke ctx.stopping ;
            Flow.write_string flow
              (Printf.sprintf "%b %b" before (ctx.is_stopping ()))
          in
          in_main
          @@ fun () ->
          let server = Server.start ~config:quiet ~engine handler in
          let c = Tcp.connect (Server.local_addr server) in
          equal string "ready " (read_exactly c 6) ;
          Server.shutdown server ;
          Server.wait server ;
          equal string "false true" (read_all c) ;
          Flow.close c )
    ; test "with every slot busy, shutdown still refuses new clients at once"
        (fun () ->
          in_main
          @@ fun () ->
          started := Cell.Once.make () ;
          let server =
            Server.start ~config:{quiet with max_connections= 1} handler
          in
          let addr = Server.local_addr server in
          let c = Tcp.connect addr in
          Flow.write_string c (get "/pause") ;
          Cell.Once.get !started ;
          Server.shutdown server ;
          (* The listener closes while the only slot is still taken. *)
          let rec refused tries =
            let retry () =
              tries > 0
              &&
              ( Mtime.wait_for Mtime.Span.(5 * ms) ;
                refused (tries - 1) )
            in
            match Tcp.connect addr with
            | exception Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
                true
            (* Queued just as the listener closed. *)
            | exception Unix.Unix_error (Unix.ECONNRESET, _, _) ->
                retry ()
            | c ->
                Flow.close c ; retry ()
          in
          is_true ~msg:"refused" (refused 20) ;
          let _, body = read_response c in
          equal string "done" body ; Flow.close c ; Server.wait server )
    ; test "200 starts and shutdowns in a row all finish" (fun () ->
          (* Readiness of the listener can be spurious: the acceptor must not
             then block in accept, deaf to the stop. *)
          let engine _ctx flow _handler = Flow.write_string flow "x" in
          for _ = 1 to 200 do
            in_main
            @@ fun () ->
            let server = Server.start ~config:quiet ~engine handler in
            let c = Tcp.connect (Server.local_addr server) in
            Flow.set_read_deadline c (Some (deadline_in 2000)) ;
            equal string "x" (read_all c) ;
            Flow.close c ;
            Server.shutdown server ;
            Server.wait server
          done )
    ; test "after shutdown, new clients are refused" (fun () ->
          in_main
          @@ fun () ->
          let server = Server.start ~config:quiet handler in
          let addr = Server.local_addr server in
          Server.shutdown server ;
          Server.wait server ;
          raises_match
            (function
              | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> true | _ -> false )
            (fun () -> Tcp.connect addr) )
    ; test "clients arriving during shutdown leave no descriptor behind"
        (fun () ->
          in_main
          @@ fun () ->
          let fds () = Array.length (Sys.readdir "/proc/self/fd") in
          let before = fds () in
          let server =
            Server.start ~config:{quiet with max_connections= 8} handler
          in
          let addr = Server.local_addr server in
          (* Over 10ms, clients connect, send a request and read until the
             server closes; some arrive after it stopped. *)
          let client i () =
            let delay = i * 200 in
            Mtime.wait_for Mtime.Span.(delay * us) ;
            match Tcp.connect addr with
            | exception Unix.Unix_error _ ->
                ()
            | c -> (
                Fun.protect ~finally:(fun () -> Flow.close c)
                @@ fun () ->
                Flow.set_read_deadline c (Some (deadline_in 2000)) ;
                try
                  Flow.write_string c (get "/") ;
                  ignore (read_all c)
                with Flow.Closed -> () )
          in
          let clients = List.init 50 (fun i -> Fun.Async.call (client i)) in
          Mtime.wait_for Mtime.Span.(5 * ms) ;
          Server.shutdown server ;
          Server.wait server ;
          ignore (Fun.Async.get_all clients) ;
          equal int before (fds ()) )
    ; test "an address in use is an error" (fun () ->
          with_server
          @@ fun server ->
          raises_match
            (function
              | Unix.Unix_error (Unix.EADDRINUSE, _, _) -> true | _ -> false )
            (fun () ->
              Server.start
                ~config:{quiet with addr= Server.local_addr server}
                handler ) )
    ; test "the engine can be replaced" (fun () ->
          let engine _ctx flow _handler =
            Flow.write_string flow "custom engine"
          in
          with_server ~engine
          @@ fun server ->
          let c = Tcp.connect (Server.local_addr server) in
          equal string "custom engine" (read_all c) ;
          Flow.close c )
    ; test "an engine that raises is reported and the server goes on" (fun () ->
          let errors = Atomic.make [] and calls = Atomic.make 0 in
          let on_error e _ =
            Atomic.set errors (Printexc.to_string e :: Atomic.get errors)
          in
          let engine _ctx flow _handler =
            if Atomic.fetch_and_add calls 1 = 0 then failwith "engine"
            else Flow.write_string flow "ok"
          in
          with_server ~config:{quiet with on_error} ~engine
          @@ fun server ->
          let a = Tcp.connect (Server.local_addr server) in
          equal string "" (read_all a) ;
          Flow.close a ;
          let b = Tcp.connect (Server.local_addr server) in
          equal string "ok" (read_all b) ;
          Flow.close b ;
          equal (list string) [{|Failure("engine")|}] (Atomic.get errors) ) ]

(* Races between stopping and the supervisor, which show with more than two
   domains; mutation testing runs with one. *)

let race_tests =
  let domains = if domain_count = 1 then 1 else 4 in
  let cycles stop config =
    for _ = 1 to 200 do
      Affect_unix.Unix.main ~domain_count:domains (fun () ->
          let server = Server.start ~config handler in
          Fun.Async.yield () ; stop server ; Server.wait server )
    done
  in
  group "Stopping races"
    [ test "abort right after start never makes wait raise" (fun () ->
          cycles Server.abort quiet )
    ; test "a shutdown with no grace never makes wait raise" (fun () ->
          cycles Server.shutdown {quiet with shutdown_grace= Mtime.Span.(0 * ms)} )
    ]

(* accept(2) errors *)

let accept_error_tests =
  let module A = Private.Accept_error in
  let show e =
    match A.classify e with
    | `Connection ->
        "retry"
    | `Resources ->
        "wait"
    | `Listener ->
        "stop"
  in
  group "Accept_error"
    [ test "what each accept error is about" (fun () ->
          expect
            (String.concat "\n"
               (List.map
                  (fun (name, e) -> Printf.sprintf "%-13s %s" name (show e))
                  Unix.
                    [ ("ECONNABORTED", ECONNABORTED)
                    ; ("EINTR", EINTR)
                    ; ("ENETDOWN", ENETDOWN)
                    ; ("ENOPROTOOPT", ENOPROTOOPT)
                    ; ("EHOSTDOWN", EHOSTDOWN)
                    ; ("EHOSTUNREACH", EHOSTUNREACH)
                    ; ("EOPNOTSUPP", EOPNOTSUPP)
                    ; ("ENETUNREACH", ENETUNREACH)
                    ; ("EPERM", EPERM)
                    ; ("EPROTO", EUNKNOWNERR 71)
                    ; ("ENONET", EUNKNOWNERR 64)
                    ; ("EMFILE", EMFILE)
                    ; ("ENFILE", ENFILE)
                    ; ("ENOBUFS", ENOBUFS)
                    ; ("ENOMEM", ENOMEM)
                    ; ("EBADF", EBADF)
                    ; ("EINVAL", EINVAL)
                    ; ("ENOTSOCK", ENOTSOCK)
                    ; ("other", EUNKNOWNERR 999) ] ) )
          @@ __POS_OF__
               {|
            ECONNABORTED  retry
            EINTR         retry
            ENETDOWN      retry
            ENOPROTOOPT   retry
            EHOSTDOWN     retry
            EHOSTUNREACH  retry
            EOPNOTSUPP    retry
            ENETUNREACH   retry
            EPERM         retry
            EPROTO        retry
            ENONET        retry
            EMFILE        wait
            ENFILE        wait
            ENOBUFS       wait
            ENOMEM        wait
            EBADF         stop
            EINVAL        stop
            ENOTSOCK      stop
            other         stop
            |} )
    ]

(* The accept loop, with a fake listener. A real socket cannot make accept(2)
   fail on demand: EMFILE needs a process without free descriptors, and Linux
   does not give ECONNABORTED for a reset queued connection. *)

(* A listener without a socket. [try_accept] gives its steps in order. After the
   last step, it waits for clients that never come. But if the server does not
   stop in 2 s, it raises ETIMEDOUT, so that a test that does not stop the
   server ends: the acceptor stops on this error. *)
module Fake_listener = struct
  (* A step: a connection, an accept(2) error, or a gate that holds the next
     steps until its action is enabled. *)
  type step =
    | Conn of Unix.file_descr
    | Error of Unix.error
    | Gate of unit Action.t

  type t =
    { mutable steps: step list (* Only the acceptor changes it. *)
    ; calls: Mtime.t list Atomic.t (* When try_accept ran, the last first. *)
    ; closed: bool Atomic.t
    ; deadline: Mtime.t }

  let make steps =
    { steps
    ; calls= Atomic.make []
    ; closed= Atomic.make false
    ; deadline= deadline_in 2000 }

  let is_late l = not (Mtime.is_earlier (Mtime.now ()) ~than:l.deadline)

  let addr _ = Unix.ADDR_UNIX "fake"

  let wait_acceptable l tag =
    match l.steps with
    | [] ->
        if is_late l then Action.always tag
        else Mtime.wait_for' (Mtime.span (Mtime.now ()) l.deadline) tag
    | Gate gate :: _ ->
        Action.map (fun () -> tag) gate
    | (Conn _ | Error _) :: _ ->
        Action.always tag

  let try_accept l =
    Atomic.set l.calls (Mtime.now () :: Atomic.get l.calls) ;
    match l.steps with
    | [] ->
        if is_late l then
          raise (Unix.Unix_error (ETIMEDOUT, "Fake_listener", "no stop in 2 s"))
        else None
    | Gate _ :: rest ->
        (* A spurious readiness. *)
        l.steps <- rest ;
        None
    | Conn fd :: rest ->
        l.steps <- rest ;
        Some (fd, addr l)
    | Error e :: rest ->
        l.steps <- rest ;
        raise (Unix.Unix_error (e, "accept", ""))

  let close l = Atomic.set l.closed true
end

(* The server end, as the listener gives it, and the client flow of a
   connection. *)
let socket_pair () =
  let server, client =
    Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  Unix.set_nonblock server ;
  Unix.set_nonblock client ;
  (server, Tcp.flow client)

(* Serves [handler] with [config] and [engine] on a [Fake_listener] with
   [steps]. [f] gets the listener, the server and the errors that [on_error]
   got: the helper sets [on_error] of [config]. Then the server stops, if it did
   not before, and the [clients] flows and the connections that the server did
   not take close. A server that does not stop makes the test fail after 2 s
   with ETIMEDOUT, unless the last step is a gate that is never enabled. *)
let with_fake_listener ?(clients = []) ?(config = quiet) ?(handler = handler)
    ?engine steps f =
  let errors = Atomic.make [] in
  let config =
    { config with
      on_error= (fun e _ -> Atomic.set errors (e :: Atomic.get errors)) }
  in
  in_main
  @@ fun () ->
  let l = Fake_listener.make steps in
  let server =
    Server.start_with ~config ?engine (module Fake_listener) l handler
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter Flow.close clients ;
      Server.abort server ;
      (* The acceptor stopped: the steps do not change. *)
      Fun.protect
        ~finally:(fun () ->
          List.iter
            (function
              | Fake_listener.Conn fd -> Unix.close fd | Error _ | Gate _ -> () )
            l.steps )
        (fun () ->
          (* The error of a test that stops the server with EBADF. *)
          try Server.wait server with Unix.Unix_error (EBADF, _, _) -> () ) )
    (fun () -> f l server errors)

(* All that [c] gets until it ends, in 2 s at most. Then [c] closes, so that the
   server does not linger on the connection. *)
let response c =
  Flow.set_read_deadline c (Some (deadline_in 2000)) ;
  Fun.protect ~finally:(fun () -> Flow.close c) (fun () -> read_all c)

let ends_with suffix s = is_true ~msg:s (String.ends_with ~suffix s)

let contains s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

let accept_loop_tests =
  group "Accept loop"
    [ test "EMFILE is reported, then the server waits 100 ms and accepts again"
        (fun () ->
          let fd, c = socket_pair () in
          with_fake_listener ~clients:[c] [Error EMFILE; Conn fd]
          @@ fun l server errors ->
          Flow.write_string c (get ~close:true "/") ;
          ends_with "hello" (response c) ;
          is_true ~msg:"one EMFILE reported"
            ( match Atomic.get errors with
            | [Unix.Unix_error (EMFILE, "accept", _)] ->
                true
            | _ ->
                false ) ;
          ( match Atomic.get l.calls with
          | [conn; emfile] ->
              let pause = Mtime.span emfile conn in
              is_false
                ~msg:(Format.asprintf "a pause of %a" Mtime.Span.pp pause)
                (Mtime.Span.is_shorter pause ~than:Mtime.Span.(100 * ms))
          | calls ->
              fail (Printf.sprintf "%d calls of try_accept" (List.length calls))
          ) ;
          equal bool true (Server.local_addr server = Unix.ADDR_UNIX "fake") )
    ; (* A test does not prove that the retry has no pause: a limit on the time
         makes a test fail on a slow machine. *)
      test "ECONNABORTED is not reported, and the server accepts again"
        (fun () ->
          let fd, c = socket_pair () in
          with_fake_listener ~clients:[c] [Error ECONNABORTED; Conn fd]
          @@ fun _ _ errors ->
          Flow.write_string c (get ~close:true "/") ;
          ends_with "hello" (response c) ;
          equal int 0 (List.length (Atomic.get errors)) )
    ; test
        "EBADF stops the server: the listener closes, a request in progress \
         finishes, and wait raises EBADF" (fun () ->
          started := Cell.Once.make () ;
          let fd, c = socket_pair () in
          with_fake_listener ~clients:[c]
            [Conn fd; Gate (Cell.Once.get' !started); Error EBADF]
          @@ fun l server errors ->
          Flow.write_string c (get "/pause") ;
          let r = response c in
          ends_with "done" r ;
          is_true ~msg:"the response ends the connection"
            (contains r "\r\nConnection: close\r\n") ;
          raises_match
            (function
              | Unix.Unix_error (EBADF, "accept", _) -> true | _ -> false )
            (fun () -> Server.wait server) ;
          is_true ~msg:"listener closed" (Atomic.get l.closed) ;
          equal int ~msg:"calls of try_accept: no call after EBADF" 3
            (List.length (Atomic.get l.calls)) ;
          equal int 0 (List.length (Atomic.get errors)) )
    ; test "a server that cannot start closes the listener" (fun () ->
          in_main
          @@ fun () ->
          let l = Fake_listener.make [] in
          raises_match
            (function Invalid_argument _ -> true | _ -> false)
            (fun () ->
              Server.start_with
                ~config:{quiet with max_connections= -1}
                (module Fake_listener)
                l handler ) ;
          is_true ~msg:"listener closed" (Atomic.get l.closed) )
    ; test "a shutdown closes the listener" (fun () ->
          with_fake_listener []
          @@ fun l server _ ->
          Server.shutdown server ;
          Server.wait server ;
          is_true ~msg:"listener closed" (Atomic.get l.closed) ) ]

(* Stop_signal. Only installing: delivering a signal to the test process could
   kill it, see test_server.ml for that. SIGUSR2 is never sent. *)

let stop_signal_tests =
  group "Stop_signal"
    [ test "installing again gives the same value, with the same signals only"
        (fun () ->
          let t = Stop_signal.install ~signals:[Sys.sigusr2] () in
          is_true ~msg:"same value"
            (t == Stop_signal.install ~signals:[Sys.sigusr2] ()) ;
          raises_match
            (function Invalid_argument _ -> true | _ -> false)
            (fun () -> Stop_signal.install ()) )
    ; test "release unblocks the signals, and installing again blocks them"
        (fun () ->
          let blocked () =
            List.mem Sys.sigusr2 (Thread.sigmask Unix.SIG_BLOCK [])
          in
          let t = Stop_signal.install ~signals:[Sys.sigusr2] () in
          is_true ~msg:"installed" (blocked ()) ;
          Stop_signal.release t ;
          is_true ~msg:"released" (not (blocked ())) ;
          is_true ~msg:"same value"
            (t == Stop_signal.install ~signals:[Sys.sigusr2] ()) ;
          is_true ~msg:"installed again" (blocked ()) ) ]

(* Date across domains *)

let date_tests =
  group "Date"
    [ test "dates are computed safely on several domains at once" (fun () ->
          in_main
          @@ fun () ->
          let expected t =
            Format.asprintf "%a" Http_date.pp (Private.Date.of_unix_time t)
          in
          let worker k =
            Fun.Async.call (fun () ->
                for i = 0 to 20_000 do
                  (* A new second each time, so that every call formats. *)
                  let t = Float.of_int ((i * 8) + k) in
                  if Private.Date.now_at t <> expected t then
                    failwith (Printf.sprintf "wrong date for %.0f" t) ;
                  if i mod 1000 = 0 then Fun.Async.yield ()
                done )
          in
          ignore (Fun.Async.get_all (List.init 8 worker)) ) ]

let () =
  exit
    (run "net"
       [ tcp_tests
       ; server_tests
       ; race_tests
       ; accept_error_tests
       ; accept_loop_tests
       ; stop_signal_tests
       ; date_tests ] )
