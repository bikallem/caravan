(* Tests over real loopback sockets, inside affect's scheduler. *)

open Windtrap
open Affect
open Caravan
module Mtime = Affect_unix.Mtime

(* Two domains by default, to exercise parallelism. Mutation testing forks a
   process per mutant, which OCaml forbids once a domain was spawned: run it
   with TEST_DOMAINS=1. *)
let domain_count =
  Option.value ~default:2 (Option.bind (Sys.getenv_opt "TEST_DOMAINS") int_of_string_opt)

let in_main f = Affect_unix.Unix.main ~domain_count f
let loopback = Unix.ADDR_INET (Unix.inet_addr_loopback, 0)

(* A connected (client, server) pair of flows. *)
let with_pair f =
  in_main @@ fun () ->
  let l = Tcp.Listener.listen loopback in
  Fun.protect ~finally:(fun () -> Tcp.Listener.close l) @@ fun () ->
  let client = Fun.Async.call (fun () -> Tcp.connect (Tcp.Listener.addr l)) in
  let fd, _ = Tcp.Listener.accept l in
  let server = Tcp.flow fd and client = Fun.Async.get client in
  Fun.protect ~finally:(fun () -> Flow.close client; Flow.close server) @@ fun () ->
  f client server

let read_all flow =
  let buf = Bytes.create 65536 and out = Buffer.create 65536 in
  let rec loop () =
    match Flow.read flow buf 0 (Bytes.length buf) with
    | 0 -> Buffer.contents out
    | n -> Buffer.add_subbytes out buf 0 n; loop ()
  in
  loop ()

let read_exactly flow n =
  let buf = Bytes.create n in
  let rec loop got = if got < n then loop (got + Flow.read flow buf got (n - got)) in
  loop 0;
  Bytes.to_string buf

let deadline_in n = Mtime.add_span (Mtime.now ()) Mtime.Span.(n * ms) |> Option.get
let elapsed_ms c = Mtime.Span.to_float_ns (Mtime.count c) /. 1e6

let tcp_tests =
  group "Tcp"
    [ test "listening on port 0 gets a port" (fun () ->
          in_main @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          (match Tcp.Listener.addr l with
          | Unix.ADDR_INET (_, port) -> is_true (port > 0)
          | Unix.ADDR_UNIX _ -> fail "not an inet address");
          Tcp.Listener.close l);
      test "a request and its echo" (fun () ->
          with_pair @@ fun client server ->
          Flow.write_string client "ping";
          equal string "ping" (read_exactly server 4);
          Flow.write_string server "pong";
          equal string "pong" (read_exactly client 4));
      test "half-close: the peer reads the end and can still answer" (fun () ->
          with_pair @@ fun client server ->
          Flow.write_string client "request";
          Flow.shutdown_send client;
          equal string "request" (read_all server);
          Flow.write_string server "response";
          Flow.shutdown_send server;
          equal string "response" (read_all client));
      test "4 MiB each way at once" (fun () ->
          with_pair @@ fun client server ->
          let data = String.init (4 * 1024 * 1024) (fun i -> Char.chr (i land 0xff)) in
          let send flow = Fun.Async.call (fun () -> Flow.write_string flow data; Flow.shutdown_send flow) in
          let s1 = send client and s2 = send server in
          let r1 = Fun.Async.call (fun () -> read_all server) in
          let r2 = Fun.Async.call (fun () -> read_all client) in
          Fun.Async.get s1;
          Fun.Async.get s2;
          is_true ~msg:"server received it" (String.equal data (Fun.Async.get r1));
          is_true ~msg:"client received it" (String.equal data (Fun.Async.get r2)));
      test "a read deadline ends a wait with Timeout" (fun () ->
          with_pair @@ fun _client server ->
          let c = Mtime.counter () in
          Flow.set_read_deadline server (Some (deadline_in 50));
          raises Flow.Timeout (fun () -> Flow.read server (Bytes.create 1) 0 1);
          let ms = elapsed_ms c in
          is_true ~msg:(Printf.sprintf "took %.0fms" ms) (ms >= 40. && ms < 1000.);
          Flow.set_read_deadline server (Some (Mtime.now ()));
          raises Flow.Timeout (fun () -> Flow.read server (Bytes.create 1) 0 1));
      test "without a deadline a read waits for data" (fun () ->
          with_pair @@ fun client server ->
          Flow.set_read_deadline server (Some (deadline_in 10));
          Flow.set_read_deadline server None;
          let _writer =
            Fun.Async.call (fun () ->
                Mtime.wait_for Mtime.Span.(100 * ms);
                Flow.write_string client "late")
          in
          equal string "late" (read_exactly server 4));
      test "a write deadline ends a blocked write with Timeout" (fun () ->
          with_pair @@ fun client _server ->
          let chunk = Bytes.make (1024 * 1024) 'x' in
          Flow.set_write_deadline client (Some (deadline_in 200));
          raises Flow.Timeout (fun () ->
              for _ = 1 to 256 do Flow.write client chunk 0 (Bytes.length chunk) done));
      test "writing to a closed peer raises Closed" (fun () ->
          with_pair @@ fun client server ->
          Flow.close server;
          Mtime.wait_for Mtime.Span.(20 * ms);
          raises Flow.Closed (fun () ->
              for _ = 1 to 100 do
                Flow.write_string client "x";
                Mtime.wait_for Mtime.Span.(5 * ms)
              done));
      test "a reset connection raises Closed; shutdown_send then does nothing" (fun () ->
          in_main @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          (* A raw peer that closes with SO_LINGER 0, which sends a RST. *)
          let peer = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
          Unix.connect peer (Tcp.Listener.addr l);
          let fd, _ = Tcp.Listener.accept l in
          let server = Tcp.flow fd in
          Unix.setsockopt_optint peer Unix.SO_LINGER (Some 0);
          Unix.close peer;
          Mtime.wait_for Mtime.Span.(20 * ms);
          raises Flow.Closed (fun () -> Flow.read server (Bytes.create 1) 0 1);
          Flow.shutdown_send server;
          Flow.close server;
          Tcp.Listener.close l);
      test "cancelling a blocked read" (fun () ->
          with_pair @@ fun _client server ->
          let reader = Fun.Async.call (fun () -> Flow.read server (Bytes.create 1) 0 1) in
          Mtime.wait_for Mtime.Span.(50 * ms);
          Fun.Async.cancel reader;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get reader);
          Flow.close server;
          raises Flow.Closed (fun () -> Flow.read server (Bytes.create 1) 0 1));
      test "connecting to a closed port fails" (fun () ->
          in_main @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          let addr = Tcp.Listener.addr l in
          Tcp.Listener.close l;
          raises_match
            (function Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> true | _ -> false)
            (fun () -> Tcp.connect addr));
      test "wait_acceptable is enabled by a pending connection" (fun () ->
          in_main @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          let ready () =
            Action.invoke
              (Action.choose
                 [ Tcp.Listener.wait_acceptable l true; Mtime.wait_for' Mtime.Span.(30 * ms) false ])
          in
          is_true ~msg:"nothing pending" (not (ready ()));
          let client = Tcp.connect (Tcp.Listener.addr l) in
          is_true ~msg:"a connection is pending" (ready ());
          let fd, _ = Tcp.Listener.accept l in
          Unix.close fd;
          Flow.close client;
          Tcp.Listener.close l);
      test "try_accept does not wait" (fun () ->
          in_main @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          is_true ~msg:"nothing pending" (Tcp.Listener.try_accept l = None);
          let client = Tcp.connect (Tcp.Listener.addr l) in
          let rec accepted tries =
            match Tcp.Listener.try_accept l with
            | Some (fd, _) -> Unix.close fd; true
            | None -> tries > 0 && (Mtime.wait_for Mtime.Span.(5 * ms); accepted (tries - 1))
          in
          is_true ~msg:"a pending connection" (accepted 20);
          is_true ~msg:"nothing pending again" (Tcp.Listener.try_accept l = None);
          Flow.close client;
          Tcp.Listener.close l) ]

(* Server *)

let quiet = { Config.default with addr = loopback; date_header = false }

(* Set when /sleep or /pause starts, so that a test can stop the server
   while it is in progress. *)
let started = ref (Cell.Once.make ())

let handler req =
  match Request.path_only req with
  | "/sleep" ->
      Cell.Once.try_set !started ();
      Mtime.wait_for Mtime.Span.(10 * s);
      Response.string "late"
  | "/pause" ->
      Cell.Once.try_set !started ();
      Mtime.wait_for Mtime.Span.(300 * ms);
      Response.string "done"
  | "/id" ->
      Response.string
        (match Request.conn req with Some c -> string_of_int (Conn_info.id c) | None -> "-")
  | _ -> Response.string "hello"

let get ?(close = false) path =
  Printf.sprintf "GET %s HTTP/1.1\r\nHost: x\r\n%s\r\n" path
    (if close then "Connection: close\r\n" else "")

(* Reads one response with a Content-Length body from [r]. *)
let read_response_from r =
  let status = Private.Buf_reader.read_line r ~max:1000 in
  let rec headers len =
    match Private.Buf_reader.read_line r ~max:1000 with
    | "" -> len
    | line ->
        let lower = String.lowercase_ascii line in
        if String.starts_with ~prefix:"content-length: " lower
        then headers (int_of_string (String.sub line 16 (String.length line - 16)))
        else headers len
  in
  let len = headers 0 in
  let buf = Bytes.create len in
  let rec body got = if got < len then body (got + Private.Buf_reader.read_into r buf got (len - got)) in
  body 0;
  (status, Bytes.to_string buf)

let read_response flow = read_response_from (Private.Buf_reader.of_flow flow)

let with_server ?(config = quiet) ?engine f =
  in_main @@ fun () ->
  let server = Server.start ~config ?engine handler in
  Fun.protect ~finally:(fun () -> Server.shutdown server; Server.wait server) @@ fun () ->
  f server

let server_tests =
  group "Server"
    [ test "serves a GET on a free port" (fun () ->
          with_server @@ fun server ->
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get ~close:true "/");
          equal string "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello"
            (read_all c);
          Flow.close c);
      test "keeps a connection alive" (fun () ->
          with_server @@ fun server ->
          let c = Tcp.connect (Server.local_addr server) in
          let r = Private.Buf_reader.of_flow c in
          Flow.write_string c (get "/id");
          let status, id1 = read_response_from r in
          Flow.write_string c (get "/id");
          let _, id2 = read_response_from r in
          equal string "HTTP/1.1 200 OK" status;
          equal string id1 id2;
          Flow.close c);
      test "connections are numbered in order from 0" (fun () ->
          with_server @@ fun server ->
          let id () =
            let c = Tcp.connect (Server.local_addr server) in
            Flow.write_string c (get ~close:true "/id");
            let _, id = read_response c in
            Flow.close c;
            id
          in
          let first = id () in
          let second = id () in
          equal (list string) [ "0"; "1" ] [ first; second ]);
      test "serves 20 clients at once" (fun () ->
          with_server @@ fun server ->
          let clients =
            List.init 20 (fun _ ->
                Fun.Async.call (fun () ->
                    let c = Tcp.connect (Server.local_addr server) in
                    Flow.write_string c (get ~close:true "/");
                    let _, body = read_response c in
                    Flow.close c;
                    body))
          in
          equal (list string) (List.init 20 (fun _ -> "hello")) (Fun.Async.get_all clients));
      test "max_connections holds back extra connections" (fun () ->
          with_server ~config:{ quiet with max_connections = 1 } @@ fun server ->
          let first = Tcp.connect (Server.local_addr server) in
          Flow.write_string first (get "/");
          ignore (read_response first);
          let second = Tcp.connect (Server.local_addr server) in
          Flow.write_string second (get ~close:true "/");
          Flow.set_read_deadline second (Some (deadline_in 200));
          raises Flow.Timeout (fun () -> Flow.read second (Bytes.create 1) 0 1);
          Flow.set_read_deadline second None;
          Flow.close first;
          let _, body = read_response second in
          equal string "hello" body;
          Flow.close second);
      test "requests that outlast shutdown_grace are cancelled" (fun () ->
          in_main @@ fun () ->
          started := Cell.Once.make ();
          let config = { quiet with shutdown_grace = Mtime.Span.(100 * ms) } in
          let server = Server.start ~config handler in
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get "/sleep");
          Cell.Once.get !started;
          let clock = Mtime.counter () in
          Server.shutdown server;
          Server.wait server;
          let waited = elapsed_ms clock in
          is_true ~msg:(Printf.sprintf "waited %.0fms" waited) (waited > 90. && waited < 1000.);
          equal string "" (read_all c);
          Flow.close c);
      test "abort cancels requests in progress without a grace" (fun () ->
          in_main @@ fun () ->
          started := Cell.Once.make ();
          let server = Server.start ~config:quiet handler in
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get "/sleep");
          Cell.Once.get !started;
          let clock = Mtime.counter () in
          Server.abort server;
          Server.wait server;
          is_true ~msg:"well before the 10s grace" (elapsed_ms clock < 1000.);
          equal string "" (read_all c);
          Flow.close c);
      test "a request in progress at shutdown finishes, closing its connection" (fun () ->
          in_main @@ fun () ->
          started := Cell.Once.make ();
          let server = Server.start ~config:quiet handler in
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get "/pause");
          Cell.Once.get !started;
          let clock = Mtime.counter () in
          Server.shutdown server;
          equal string
            "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 4\r\nConnection: close\r\n\r\ndone"
            (read_all c);
          Flow.close c;
          Server.wait server;
          let waited = elapsed_ms clock in
          is_true ~msg:(Printf.sprintf "waited %.0fms, not the 10s grace" waited) (waited < 2000.));
      test "idle connections close at once on shutdown" (fun () ->
          in_main @@ fun () ->
          let server = Server.start ~config:quiet handler in
          let c = Tcp.connect (Server.local_addr server) in
          let r = Private.Buf_reader.of_flow c in
          Flow.write_string c (get "/");
          ignore (read_response_from r);
          let clock = Mtime.counter () in
          Server.shutdown server;
          equal int ~msg:"end of stream" 0 (Private.Buf_reader.read_into r (Bytes.create 1) 0 1);
          Server.wait server;
          is_true ~msg:"well before the 10s grace" (elapsed_ms clock < 2000.);
          Flow.close c);
      test "an engine sees the server stopping" (fun () ->
          let engine (ctx : Engine.ctx) flow _handler =
            let before = ctx.is_stopping () in
            Flow.write_string flow "ready ";
            Action.invoke ctx.stopping;
            Flow.write_string flow (Printf.sprintf "%b %b" before (ctx.is_stopping ()))
          in
          in_main @@ fun () ->
          let server = Server.start ~config:quiet ~engine handler in
          let c = Tcp.connect (Server.local_addr server) in
          equal string "ready " (read_exactly c 6);
          Server.shutdown server;
          Server.wait server;
          equal string "false true" (read_all c);
          Flow.close c);
      test "with every slot busy, shutdown still refuses new clients at once" (fun () ->
          in_main @@ fun () ->
          started := Cell.Once.make ();
          let server = Server.start ~config:{ quiet with max_connections = 1 } handler in
          let addr = Server.local_addr server in
          let c = Tcp.connect addr in
          Flow.write_string c (get "/pause");
          Cell.Once.get !started;
          Server.shutdown server;
          (* The listener closes while the only slot is still taken. *)
          let rec refused tries =
            let retry () = tries > 0 && (Mtime.wait_for Mtime.Span.(5 * ms); refused (tries - 1)) in
            match Tcp.connect addr with
            | exception Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> true
            (* Queued just as the listener closed. *)
            | exception Unix.Unix_error (Unix.ECONNRESET, _, _) -> retry ()
            | c -> Flow.close c; retry ()
          in
          is_true ~msg:"refused" (refused 20);
          let _, body = read_response c in
          equal string "done" body;
          Flow.close c;
          Server.wait server);
      test "200 starts and shutdowns in a row all finish" (fun () ->
          (* Readiness of the listener can be spurious: the acceptor must not
             then block in accept, deaf to the stop. *)
          let engine _ctx flow _handler = Flow.write_string flow "x" in
          for _ = 1 to 200 do
            in_main @@ fun () ->
            let server = Server.start ~config:quiet ~engine handler in
            let c = Tcp.connect (Server.local_addr server) in
            Flow.set_read_deadline c (Some (deadline_in 2000));
            equal string "x" (read_all c);
            Flow.close c;
            Server.shutdown server;
            Server.wait server
          done);
      test "after shutdown, new clients are refused" (fun () ->
          in_main @@ fun () ->
          let server = Server.start ~config:quiet handler in
          let addr = Server.local_addr server in
          Server.shutdown server;
          Server.wait server;
          raises_match
            (function Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> true | _ -> false)
            (fun () -> Tcp.connect addr));
      test "clients arriving during shutdown leave no descriptor behind" (fun () ->
          in_main @@ fun () ->
          let fds () = Array.length (Sys.readdir "/proc/self/fd") in
          let before = fds () in
          let server = Server.start ~config:{ quiet with max_connections = 8 } handler in
          let addr = Server.local_addr server in
          (* Over 10ms, clients connect, send a request and read until the
             server closes; some arrive after it stopped. *)
          let client i () =
            let delay = i * 200 in
            Mtime.wait_for Mtime.Span.(delay * us);
            match Tcp.connect addr with
            | exception Unix.Unix_error _ -> ()
            | c ->
                Fun.protect ~finally:(fun () -> Flow.close c) @@ fun () ->
                Flow.set_read_deadline c (Some (deadline_in 2000));
                try Flow.write_string c (get "/"); ignore (read_all c) with Flow.Closed -> ()
          in
          let clients = List.init 50 (fun i -> Fun.Async.call (client i)) in
          Mtime.wait_for Mtime.Span.(5 * ms);
          Server.shutdown server;
          Server.wait server;
          ignore (Fun.Async.get_all clients);
          equal int before (fds ()));
      test "an address in use is an error" (fun () ->
          with_server @@ fun server ->
          raises_match
            (function Unix.Unix_error (Unix.EADDRINUSE, _, _) -> true | _ -> false)
            (fun () -> Server.start ~config:{ quiet with addr = Server.local_addr server } handler));
      test "the engine can be replaced" (fun () ->
          let engine _ctx flow _handler = Flow.write_string flow "custom engine" in
          with_server ~engine @@ fun server ->
          let c = Tcp.connect (Server.local_addr server) in
          equal string "custom engine" (read_all c);
          Flow.close c);
      test "an engine that raises is reported and the server goes on" (fun () ->
          let errors = Atomic.make [] and calls = Atomic.make 0 in
          let on_error e _ = Atomic.set errors (Printexc.to_string e :: Atomic.get errors) in
          let engine _ctx flow _handler =
            if Atomic.fetch_and_add calls 1 = 0 then failwith "engine" else Flow.write_string flow "ok"
          in
          with_server ~config:{ quiet with on_error } ~engine @@ fun server ->
          let a = Tcp.connect (Server.local_addr server) in
          equal string "" (read_all a);
          Flow.close a;
          let b = Tcp.connect (Server.local_addr server) in
          equal string "ok" (read_all b);
          Flow.close b;
          equal (list string) [ {|Failure("engine")|} ] (Atomic.get errors)) ]

(* Stop_signal. Only installing: delivering a signal to the test process
   could kill it, see test_server.ml for that. SIGUSR2 is never sent. *)

let stop_signal_tests =
  group "Stop_signal"
    [ test "installing again gives the same value, with the same signals only" (fun () ->
          let t = Stop_signal.install ~signals:[ Sys.sigusr2 ] () in
          is_true ~msg:"same value" (t == Stop_signal.install ~signals:[ Sys.sigusr2 ] ());
          raises_match
            (function Invalid_argument _ -> true | _ -> false)
            (fun () -> Stop_signal.install ())) ]

(* Date across domains *)

let date_tests =
  group "Date"
    [ test "dates are computed safely on several domains at once" (fun () ->
          in_main @@ fun () ->
          let expected t = Format.asprintf "%a" Http_date.pp (Private.Date.of_unix_time t) in
          let worker k =
            Fun.Async.call (fun () ->
                for i = 0 to 20_000 do
                  (* A new second each time, so that every call formats. *)
                  let t = Float.of_int ((i * 8) + k) in
                  if Private.Date.now_at t <> expected t
                  then failwith (Printf.sprintf "wrong date for %.0f" t);
                  if i mod 1000 = 0 then Fun.Async.yield ()
                done)
          in
          ignore (Fun.Async.get_all (List.init 8 worker))) ]

let () = exit (run "net" [ tcp_tests; server_tests; stop_signal_tests; date_tests ])
