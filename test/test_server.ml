(* Integration tests: the server over real sockets, used as clients do. *)

open Windtrap
open Affect
open Caravan
open Server_support

let connect server =
  let flow = Tcp.connect (Server.local_addr server) in
  (flow, Buf_reader.of_flow flow)

let get ?(conn = None) path = serialize { meth = "GET"; path; v10 = false; conn; body = No_body }

let integration_tests =
  group "Server over sockets"
    [ test "a burst of pipelined requests in one write" (fun () ->
          with_server @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/hello" ^ get "/empty" ^ get ~conn:(Some "close") "/stream");
          let responses = List.init 3 (fun _ -> read_response ~head:false r) in
          equal (list string) [ {|200 "hello"|}; {|204 ""|}; {|200 "abcd" close|} ] responses;
          Flow.close flow);
      test "a client that waits for 100 Continue gets it from a streaming echo" (fun () ->
          with_server @@ fun server ->
          let flow, r = connect server in
          Flow.set_read_deadline flow (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s));
          Flow.write_string flow
            "POST /echo HTTP/1.1\r\nHost: x\r\nContent-Length: 5\r\nExpect: 100-continue\r\n\r\n";
          (* Like curl: the body only goes once the server asked for it. *)
          equal string "HTTP/1.1 100 Continue" (Buf_reader.read_line r ~max:100);
          equal string "" (Buf_reader.read_line r ~max:100);
          Flow.write_string flow "hello";
          equal string {|200 "hello"|} (read_response ~head:false r);
          Flow.close flow);
      test "an HTTP/1.0 client sees a body cut short as a reset" (fun () ->
          with_server ~config:{ config with on_error = (fun _ _ -> ()) } @@ fun server ->
          let flow, r = connect server in
          Flow.set_read_deadline flow (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s));
          Flow.write_string flow "GET /partial HTTP/1.0\r\n\r\n";
          let rec head () = if Buf_reader.read_line r ~max:1000 <> "" then head () in
          head ();
          let buf = Bytes.create 7 in
          let rec fill got = if got < 7 then fill (got + Buf_reader.read_into r buf got (7 - got)) in
          fill 0;
          equal string "partial" (Bytes.to_string buf);
          raises Flow.Closed (fun () -> Buf_reader.read_into r (Bytes.create 1) 0 1);
          Flow.close flow);
      test ~timeout:30. "concurrent clients on four domains are all answered" (fun () ->
          (* The disconnect watcher once waited on the handler's call and the
             connection in one choice, and under several domains affect
             could miss the call's return: about one round in 30 hung. *)
          let domains = if domain_count = 1 then 1 else 4 in
          for _ = 1 to 100 do
            Affect_unix.Unix.main ~domain_count:domains @@ fun () ->
            let server = Server.start ~config handler in
            let client () =
              let flow, r = connect server in
              Flow.set_read_deadline flow (Mtime.add_span (Mtime.now ()) Mtime.Span.(2 * s));
              Flow.write_string flow (get ~conn:(Some "close") "/hello");
              let response = read_response ~head:false r in
              Flow.close flow;
              response
            in
            let responses = Fun.Async.get_all (List.init 20 (fun _ -> Fun.Async.call client)) in
            Server.shutdown server;
            Server.wait server;
            equal (list string) (List.init 20 (fun _ -> {|200 "hello" close|})) responses
          done);
      test "a chunked upload in small delayed pieces is echoed" (fun () ->
          with_server @@ fun server ->
          let flow, r = connect server in
          let data = String.init 3000 (fun i -> Char.chr (65 + (i mod 26))) in
          Flow.write_string flow "POST /echo HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n";
          let rec send i =
            if i < String.length data
            then begin
              let n = Int.min 97 (String.length data - i) in
              let chunk = Printf.sprintf "%x\r\n%s\r\n" n (String.sub data i n) in
              (* Split each chunk across writes too. *)
              let half = String.length chunk / 2 in
              Flow.write_string flow (String.sub chunk 0 half);
              Mtime.wait_for Mtime.Span.(1 * ms);
              Flow.write_string flow (String.sub chunk half (String.length chunk - half));
              send (i + n)
            end
          in
          send 0;
          Flow.write_string flow "0\r\n\r\n";
          equal string (summary 200 data false) (read_response ~head:false r);
          Flow.close flow);
      test "8 MiB echoed while it uploads" (fun () ->
          with_server @@ fun server ->
          let flow, r = connect server in
          let data = String.init (8 * 1024 * 1024) (fun i -> Char.chr (i land 0xff)) in
          let upload =
            Fun.Async.call (fun () ->
                Flow.write_string flow
                  (Printf.sprintf "POST /echo HTTP/1.1\r\nHost: x\r\nContent-Length: %d\r\n\r\n"
                     (String.length data));
                Flow.write_string flow data)
          in
          let response = read_response ~head:false r in
          Fun.Async.get upload;
          is_true ~msg:"echoed exactly" (String.equal response (summary 200 data false));
          Flow.close flow);
      test "a streamed response arrives as it is produced" (fun () ->
          with_server @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/drip");
          let clock = Mtime.counter () in
          let rec header () = if Buf_reader.read_line r ~max:1000 <> "" then header () in
          header ();
          (* Chunks: size line, data line. Note when each data byte arrives. *)
          let arrivals =
            List.init 3 (fun _ ->
                ignore (Buf_reader.read_line r ~max:100);
                let data = Buf_reader.read_line r ~max:100 in
                (data, Mtime.Span.to_float_ns (Mtime.count clock) /. 1e6))
          in
          equal (list string) [ "1"; "2"; "3" ] (List.map fst arrivals);
          let times = List.map snd arrivals in
          is_true ~msg:(String.concat ", " (List.map (Printf.sprintf "%.0fms") times))
            (List.nth times 1 -. List.nth times 0 > 60. && List.nth times 2 -. List.nth times 1 > 60.);
          Flow.close flow);
      test "clients that leave mid-request do not disturb others" (fun () ->
          with_server @@ fun server ->
          let a, _ = connect server in
          Flow.write_string a "GET /hello HTTP/1.1\r\nHo";
          Flow.close a;
          let b, _ = connect server in
          Flow.write_string b "POST /echo HTTP/1.1\r\nHost: x\r\nContent-Length: 10\r\n\r\nabc";
          Flow.close b;
          let c, r = connect server in
          Flow.write_string c (get "/hello");
          equal string {|200 "hello"|} (read_response ~head:false r);
          Flow.close c);
      test "a closing response over an unread body is not reset" (fun () ->
          with_server @@ fun server ->
          let flow, r = connect server in
          let body = Length (String.make 60_000 'x') in
          Flow.write_string flow
            (serialize { meth = "POST"; path = "/ignore"; v10 = false; conn = Some "close"; body });
          equal string {|200 "ignored" close|} (read_response ~head:false r);
          equal int ~msg:"end of stream" 0 (Buf_reader.read_into r (Bytes.create 1) 0 1);
          Flow.close flow);
      test "a client that never closes is let go after linger_timeout" (fun () ->
          let config =
            { config with max_connections = 1; linger_timeout = Mtime.Span.(200 * ms) }
          in
          with_server ~config @@ fun server ->
          let a, ra = connect server in
          Flow.write_string a (get ~conn:(Some "close") "/hello");
          equal string {|200 "hello" close|} (read_response ~head:false ra);
          (* [a] stays open and holds the only connection slot while the
             server lingers on it. *)
          let clock = Mtime.counter () in
          let b, rb = connect server in
          Flow.set_read_deadline b (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s));
          Flow.write_string b (get "/hello");
          equal string {|200 "hello"|} (read_response ~head:false rb);
          let waited = Mtime.Span.to_float_ns (Mtime.count clock) /. 1e6 in
          is_true ~msg:(Printf.sprintf "waited %.0fms" waited) (waited > 150.);
          Flow.close a;
          Flow.close b) ]

(* Timeouts of 100ms. The clients wait at most 5s for anything. *)
let timeout_tests =
  let config =
    { config with header_timeout = Mtime.Span.(100 * ms); idle_timeout = Mtime.Span.(100 * ms);
                  read_timeout = Mtime.Span.(100 * ms); write_timeout = Mtime.Span.(100 * ms) }
  in
  let connect server =
    let flow, r = connect server in
    Flow.set_read_deadline flow (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s));
    (flow, r)
  in
  let eof r = equal int ~msg:"end of stream" 0 (Buf_reader.read_into r (Bytes.create 1) 0 1) in
  group "Server timeouts"
    [ test "a request head sent too slowly gets 408" (fun () ->
          with_server ~config @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow "GET /hello HTTP/1.1\r\nHo";
          equal string {|408 "Request Timeout\n" close|} (read_response ~head:false r);
          eof r;
          Flow.close flow);
      test "an idle connection is closed" (fun () ->
          with_server ~config @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/hello");
          equal string {|200 "hello"|} (read_response ~head:false r);
          eof r;
          Flow.close flow);
      test "a body trickled after the response is not drained forever" (fun () ->
          with_server ~config @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow
            ("POST /ignore HTTP/1.1\r\nHost: x\r\nContent-Length: 1000\r\n\r\n" ^ String.make 10 'x');
          equal string {|200 "ignored"|} (read_response ~head:false r);
          (* A byte every 60ms, within the 100ms read timeout, for 2s. *)
          let trickle =
            Fun.Async.call (fun () ->
                try
                  for _ = 1 to 33 do
                    Mtime.wait_for Mtime.Span.(60 * ms);
                    Flow.write_string flow "x"
                  done
                with Flow.Closed -> ())
          in
          let clock = Mtime.counter () in
          (match Buf_reader.read_into r (Bytes.create 1) 0 1 with
          | 0 | (exception Flow.Closed) -> ()
          | _ -> fail "unexpected bytes");
          let waited = Mtime.Span.to_float_ns (Mtime.count clock) /. 1e6 in
          is_true ~msg:(Printf.sprintf "ended after %.0fms" waited) (waited < 1000.);
          Fun.Async.cancel trickle;
          Flow.close flow);
      test "a stalled upload gets 408" (fun () ->
          with_server ~config @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow "POST /hello HTTP/1.1\r\nHost: x\r\nContent-Length: 10\r\n\r\nabc";
          equal string {|408 "Request Timeout\n" close|} (read_response ~head:false r);
          eof r;
          Flow.close flow);
      test "a slow upload that keeps sending is not cut off" (fun () ->
          with_server ~config @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow "POST /hello HTTP/1.1\r\nHost: x\r\nContent-Length: 6\r\n\r\n";
          (* 300ms in all, but never 100ms without a byte. *)
          for _ = 1 to 6 do
            Mtime.wait_for Mtime.Span.(50 * ms);
            Flow.write_string flow "x"
          done;
          equal string {|200 "hello"|} (read_response ~head:false r);
          Flow.close flow);
      test "a client that does not read its response is let go" (fun () ->
          with_server ~config:{ config with max_connections = 1 } @@ fun server ->
          let a, _ = connect server in
          Flow.write_string a (get "/big");
          (* [a] never reads: once the socket buffers are full, the server's
             writes block, and it lets [a] go after write_timeout. *)
          let b, rb = connect server in
          Flow.write_string b (get "/hello");
          equal string {|200 "hello"|} (read_response ~head:false rb);
          Flow.close a;
          Flow.close b) ]

(* Upgrades over sockets *)

let upgrade_tests =
  let connect server =
    let flow, r = connect server in
    Flow.set_read_deadline flow (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s));
    (flow, r)
  in
  let read_head r =
    let status = Buf_reader.read_line r ~max:1000 in
    let rec fields () = if Buf_reader.read_line r ~max:1000 <> "" then fields () in
    fields ();
    status
  in
  let read_n r n =
    let buf = Bytes.create n in
    let rec fill got =
      if got < n
      then match Buf_reader.read_into r buf got (n - got) with
      | 0 -> failwith "end of stream"
      | k -> fill (got + k)
    in
    fill 0;
    Bytes.to_string buf
  in
  let eof r = equal int ~msg:"end of stream" 0 (Buf_reader.read_into r (Bytes.create 1) 0 1) in
  group "Upgrades over sockets"
    [ test "after a 101, bytes are echoed as they arrive, without HTTP's timeouts" (fun () ->
          let short = Mtime.Span.(100 * ms) in
          let config =
            { config with header_timeout = short; read_timeout = short; write_timeout = short }
          in
          with_server ~config @@ fun server ->
          let flow, r = connect server in
          (* In two parts, so that the server reads the second under its head
             deadline. *)
          Flow.write_string flow "GET /ws-echo HTTP/1.1\r\nHost: x\r\n";
          Mtime.wait_for Mtime.Span.(20 * ms);
          Flow.write_string flow "Upgrade: echo\r\nConnection: Upgrade\r\n\r\n";
          equal string "HTTP/1.1 101 Switching Protocols" (read_head r);
          List.iter
            (fun msg -> Flow.write_string flow msg; equal string msg (read_n r (String.length msg)))
            [ "ping"; "pong"; String.make 10_000 'x' ];
          (* Quiet for longer than the server's HTTP timeouts. *)
          Mtime.wait_for Mtime.Span.(250 * ms);
          Flow.write_string flow "still there";
          equal string "still there" (read_n r 11);
          Flow.shutdown_send flow;
          eof r;
          Flow.close flow);
      test "a CONNECT tunnel reaches another server" (fun () ->
          with_server @@ fun server ->
          let upstream = Tcp.Listener.listen loopback in
          let echo =
            Fun.Async.call (fun () ->
                let fd, _ = Tcp.Listener.accept upstream in
                let flow = Tcp.flow fd in
                Fun.protect ~finally:(fun () -> Flow.close flow) (fun () -> copy flow flow))
          in
          let target = Format.asprintf "%a" Conn_info.pp_addr (Tcp.Listener.addr upstream) in
          let flow, r = connect server in
          Flow.write_string flow (Printf.sprintf "CONNECT %s HTTP/1.1\r\nHost: %s\r\n\r\n" target target);
          equal string "HTTP/1.1 200 OK" (read_head r);
          let msg = "through the tunnel and back" in
          Flow.write_string flow msg;
          equal string msg (read_n r (String.length msg));
          Flow.shutdown_send flow;
          eof r;
          Fun.Async.get echo;
          Tcp.Listener.close upstream;
          Flow.close flow) ]

(* Clients that leave while their handler runs *)

let disconnect_tests =
  let within limit cell =
    Action.invoke
      (Action.choose
         [ Action.map (fun () -> true) (Cell.Once.get' cell);
           Mtime.wait_for' Mtime.Span.(limit * ms) false ])
  in
  let reset () =
    waiting := Cell.Once.make ();
    cancelled := Cell.Once.make ()
  in
  group "Client disconnects"
    [ test "a client that closes cancels its handler" (fun () ->
          reset ();
          with_server @@ fun server ->
          let flow, _ = connect server in
          Flow.write_string flow (get "/wait");
          Cell.Once.get !waiting;
          let clock = Mtime.counter () in
          Flow.close flow;
          is_true ~msg:"cancelled" (within 1000 !cancelled);
          let ms = Mtime.Span.to_float_ns (Mtime.count clock) /. 1e6 in
          is_true ~msg:(Printf.sprintf "after %.0fms" ms) (ms < 500.));
      test "the cancellation reaches the handler's upstream connection" (fun () ->
          reset ();
          with_server @@ fun server ->
          let l = Tcp.Listener.listen loopback in
          upstream := Tcp.Listener.addr l;
          let flow, _ = connect server in
          Flow.write_string flow (get "/fetch");
          let fd, _ = Tcp.Listener.accept l in
          let up = Tcp.flow fd in
          Flow.set_read_deadline up (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s));
          let buf = Bytes.create 8 in
          let rec read_question got =
            if got < 8 then read_question (got + Flow.read up buf got (8 - got))
          in
          read_question 0;
          Flow.close flow;
          (* The upstream never answers: the proxy side closes it. *)
          equal int ~msg:"upstream closed by the server" 0 (Flow.read up buf 0 1);
          is_true ~msg:"cancelled" (Cell.Once.is_set !cancelled);
          Flow.close up;
          Tcp.Listener.close l) ]

(* Server.run in a child process, stopped with signals: sending signals to
   the test process itself could kill it. *)
let signal_tests =
  let in_main f = Affect_unix.Unix.main ~domain_count f in
  let program = Filename.concat (Filename.dirname Sys.executable_name) "serve_until_signal.exe" in
  (* Runs [f pid out] with the program started: its pid, and a reader on its
     standard output. Kills the program if [f] leaves it running. *)
  let with_program ?(args = []) f =
    let out_r, out_w = Unix.pipe ~cloexec:true () in
    let pid =
      Unix.create_process program (Array.of_list (program :: args)) Unix.stdin out_w Unix.stderr
    in
    Unix.close out_w;
    Unix.set_nonblock out_r;
    let out = Tcp.flow out_r in
    let finally () =
      Flow.close out;
      match Unix.waitpid [ Unix.WNOHANG ] pid with
      | 0, _ -> Unix.kill pid Sys.sigkill; ignore (Unix.waitpid [] pid)
      | _ -> ()
      | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
    in
    Fun.protect ~finally (fun () -> f pid (Buf_reader.of_flow out))
  in
  let connect out =
    let port = int_of_string (Buf_reader.read_line out ~max:100) in
    let flow = Tcp.connect (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) in
    Flow.set_read_deadline flow (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s));
    (flow, Buf_reader.of_flow flow)
  in
  (* The exit status of [pid] and the milliseconds it took, killing it after
     5s. *)
  let exited pid =
    let clock = Mtime.counter () in
    let rec poll () =
      match Unix.waitpid [ Unix.WNOHANG ] pid with
      | 0, _ when Mtime.Span.to_float_ns (Mtime.count clock) > 5e9 ->
          failwith "the server did not exit"
      | 0, _ -> Mtime.wait_for Mtime.Span.(10 * ms); poll ()
      | _, status -> (status, Mtime.Span.to_float_ns (Mtime.count clock) /. 1e6)
    in
    poll ()
  in
  let exited_ok pid ~within =
    let status, ms = exited pid in
    equal bool ~msg:"exit 0" true (status = Unix.WEXITED 0);
    is_true ~msg:(Printf.sprintf "exited after %.0fms" ms) (ms < within)
  in
  group "Server.run and signals"
    [ test "SIGTERM: the request in progress finishes, then the process exits" (fun () ->
          in_main @@ fun () ->
          with_program @@ fun pid out ->
          let flow, r = connect out in
          Flow.write_string flow (get "/slow");
          equal string "started" (Buf_reader.read_line out ~max:100);
          Unix.kill pid Sys.sigterm;
          equal string {|200 "done" close|} (read_response ~head:false r);
          Flow.close flow;
          exited_ok pid ~within:3000.);
      test "a second SIGINT aborts the request in progress" (fun () ->
          in_main @@ fun () ->
          with_program @@ fun pid out ->
          let flow, r = connect out in
          Flow.write_string flow (get "/stuck");
          equal string "started" (Buf_reader.read_line out ~max:100);
          Unix.kill pid Sys.sigint;
          Mtime.wait_for Mtime.Span.(50 * ms);
          Unix.kill pid Sys.sigint;
          exited_ok pid ~within:1000.;
          is_true ~msg:"no response" (Buf_reader.peek_eof r);
          Flow.close flow);
      test "SIGTERM with nothing in progress exits at once" (fun () ->
          in_main @@ fun () ->
          with_program @@ fun pid out ->
          let flow, r = connect out in
          Flow.write_string flow (get "/hello");
          equal string {|200 "hello"|} (read_response ~head:false r);
          Unix.kill pid Sys.sigterm;
          is_true ~msg:"the idle connection is closed" (Buf_reader.peek_eof r);
          Flow.close flow;
          exited_ok pid ~within:1000.);
      test "a third signal ends a server stuck in a handler deaf to cancellation" (fun () ->
          in_main @@ fun () ->
          with_program @@ fun pid out ->
          let flow, _ = connect out in
          Flow.write_string flow (get "/deaf");
          equal string "started" (Buf_reader.read_line out ~max:100);
          for _ = 1 to 3 do
            Unix.kill pid Sys.sigint;
            Mtime.wait_for Mtime.Span.(50 * ms)
          done;
          let status, ms = exited pid in
          equal bool ~msg:"killed by SIGINT" true (status = Unix.WSIGNALED Sys.sigint);
          is_true ~msg:(Printf.sprintf "after %.0fms" ms) (ms < 2000.);
          Flow.close flow);
      test "once run returned, the signals act as before" (fun () ->
          in_main @@ fun () ->
          with_program ~args:[ "after-run" ] @@ fun pid out ->
          ignore (Buf_reader.read_line out ~max:100);
          Unix.kill pid Sys.sigterm;
          equal string "stopped" (Buf_reader.read_line out ~max:100);
          Unix.kill pid Sys.sigterm;
          let status, ms = exited pid in
          equal bool ~msg:"killed by SIGTERM" true (status = Unix.WSIGNALED Sys.sigterm);
          is_true ~msg:(Printf.sprintf "after %.0fms" ms) (ms < 2000.)) ]

let () = exit (run "server" [ integration_tests; timeout_tests; upgrade_tests; disconnect_tests; signal_tests ])
