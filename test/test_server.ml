(* Integration tests: the server over real sockets, used as clients do. *)

open Windtrap
open Affect
open Caravan
open Server_support

let connect server =
  let flow = Tcp.connect (Server.local_addr server) in
  (flow, Buf_reader.of_flow flow)

let get ?(conn = None) path =
  serialize {meth= "GET"; path; v10= false; conn; body= No_body}

let integration_tests =
  group "Server over sockets"
    [ test "a burst of pipelined requests in one write" (fun () ->
          with_server
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow
            (get "/hello" ^ get "/empty" ^ get ~conn:(Some "close") "/stream") ;
          let responses = List.init 3 (fun _ -> read_response ~head:false r) in
          equal (list string)
            [{|200 "hello"|}; {|204 ""|}; {|200 "abcd" close|}]
            responses ;
          Flow.close flow )
    ; test "a client that waits for 100 Continue gets it from a streaming echo"
        (fun () ->
          with_server
          @@ fun server ->
          let flow, r = connect server in
          Flow.set_read_deadline flow
            (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s)) ;
          Flow.write_string flow
            "POST /echo HTTP/1.1\r\n\
             Host: x\r\n\
             Content-Length: 5\r\n\
             Expect: 100-continue\r\n\
             \r\n" ;
          (* Like curl: the body only goes once the server asked for it. *)
          equal string "HTTP/1.1 100 Continue" (Buf_reader.read_line r ~max:100) ;
          equal string "" (Buf_reader.read_line r ~max:100) ;
          Flow.write_string flow "hello" ;
          equal string {|200 "hello"|} (read_response ~head:false r) ;
          Flow.close flow )
    ; test "an HTTP/1.0 client sees a body cut short as a reset" (fun () ->
          with_server ~config:{config with on_error= (fun _ _ -> ())}
          @@ fun server ->
          let flow, r = connect server in
          Flow.set_read_deadline flow
            (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s)) ;
          Flow.write_string flow "GET /partial HTTP/1.0\r\n\r\n" ;
          let rec head () =
            if Buf_reader.read_line r ~max:1000 <> "" then head ()
          in
          head () ;
          let buf = Bytes.create 7 in
          let rec fill got =
            if got < 7 then fill (got + Buf_reader.read_into r buf got (7 - got))
          in
          fill 0 ;
          equal string "partial" (Bytes.to_string buf) ;
          raises Flow.Closed (fun () ->
              Buf_reader.read_into r (Bytes.create 1) 0 1 ) ;
          Flow.close flow )
    ; test ~timeout:30. "concurrent clients on four domains are all answered"
        (fun () ->
          (* The disconnect watcher once waited on the handler's call and the
             connection in one choice, and under several domains affect could
             miss the call's return: about one round in 30 hung. *)
          let domains = if domain_count = 1 then 1 else 4 in
          (* The race needs several domains. On one domain, as in the mutation
             check, 10 rounds are sufficient. *)
          let rounds = if domains = 1 then 10 else 100 in
          for _ = 1 to rounds do
            Affect_unix.Unix.main ~domain_count:domains
            @@ fun () ->
            let server = Server.start ~config handler in
            let client () =
              let flow, r = connect server in
              Flow.set_read_deadline flow
                (Mtime.add_span (Mtime.now ()) Mtime.Span.(2 * s)) ;
              Flow.write_string flow (get ~conn:(Some "close") "/hello") ;
              let response = read_response ~head:false r in
              Flow.close flow ; response
            in
            let responses =
              Fun.Async.get_all (List.init 20 (fun _ -> Fun.Async.call client))
            in
            Server.shutdown server ;
            Server.wait server ;
            equal (list string)
              (List.init 20 (fun _ -> {|200 "hello" close|}))
              responses
          done )
    ; test "a chunked upload in many small writes is echoed" (fun () ->
          (* The writes can arrive together. The h1_server tests split the input
             at known points (test_h1_server.ml, "the bytes after the head are
             the same however the input is split"). *)
          with_server
          @@ fun server ->
          let flow, r = connect server in
          let data = String.init 3000 (fun i -> Char.chr (65 + (i mod 26))) in
          Flow.write_string flow
            "POST /echo HTTP/1.1\r\n\
             Host: x\r\n\
             Transfer-Encoding: chunked\r\n\
             \r\n" ;
          let rec send i =
            if i < String.length data then begin
              let n = Int.min 97 (String.length data - i) in
              let chunk =
                Printf.sprintf "%x\r\n%s\r\n" n (String.sub data i n)
              in
              (* Split each chunk across writes too. *)
              let half = String.length chunk / 2 in
              Flow.write_string flow (String.sub chunk 0 half) ;
              Flow.write_string flow
                (String.sub chunk half (String.length chunk - half)) ;
              send (i + n)
            end
          in
          send 0 ;
          Flow.write_string flow "0\r\n\r\n" ;
          equal string (summary 200 data false) (read_response ~head:false r) ;
          Flow.close flow )
    ; test "8 MiB echoed while it uploads" (fun () ->
          with_server
          @@ fun server ->
          let flow, r = connect server in
          let data =
            String.init (8 * 1024 * 1024) (fun i -> Char.chr (i land 0xff))
          in
          let upload =
            Fun.Async.call (fun () ->
                Flow.write_string flow
                  (Printf.sprintf
                     "POST /echo HTTP/1.1\r\n\
                      Host: x\r\n\
                      Content-Length: %d\r\n\
                      \r\n"
                     (String.length data) ) ;
                Flow.write_string flow data )
          in
          let response = read_response ~head:false r in
          Fun.Async.get upload ;
          is_true ~msg:"echoed exactly"
            (String.equal response (summary 200 data false)) ;
          Flow.close flow )
    ; test "a streamed response arrives as it is produced" (fun () ->
          (* The handler writes chunk i + 1 only after the client got chunk i
             and set [got.(i - 1)]. *)
          let got = Array.init 2 (fun _ -> Cell.Once.make ()) in
          let handler _ =
            Response.stream (fun w ->
                for i = 1 to 3 do
                  Body.Writer.write_string w (string_of_int i) ;
                  Body.Writer.flush w ;
                  if i < 3 then Cell.Once.get got.(i - 1)
                done )
          in
          with_server ~handler
          @@ fun server ->
          let flow, r = connect server in
          Fun.protect ~finally:(fun () ->
              Array.iter (fun c -> Cell.Once.try_set c ()) got ;
              Flow.close flow )
          @@ fun () ->
          Flow.set_read_deadline flow
            (Mtime.add_span (Mtime.now ()) Mtime.Span.(2 * s)) ;
          Flow.write_string flow (get "/") ;
          let rec header () =
            if Buf_reader.read_line r ~max:1000 <> "" then header ()
          in
          header () ;
          (* Chunks: size line, data line. *)
          let chunk i =
            ignore (Buf_reader.read_line r ~max:100) ;
            let data = Buf_reader.read_line r ~max:100 in
            if i < 2 then Cell.Once.try_set got.(i) () ;
            data
          in
          equal (list string) ["1"; "2"; "3"] (List.init 3 chunk) )
    ; test "clients that leave mid-request do not disturb others" (fun () ->
          with_server
          @@ fun server ->
          let a, _ = connect server in
          Flow.write_string a "GET /hello HTTP/1.1\r\nHo" ;
          Flow.close a ;
          let b, _ = connect server in
          Flow.write_string b
            "POST /echo HTTP/1.1\r\nHost: x\r\nContent-Length: 10\r\n\r\nabc" ;
          Flow.close b ;
          let c, r = connect server in
          Flow.write_string c (get "/hello") ;
          equal string {|200 "hello"|} (read_response ~head:false r) ;
          Flow.close c )
    ; test "a closing response over an unread body is not reset" (fun () ->
          with_server
          @@ fun server ->
          let flow, r = connect server in
          let body = Length (String.make 60_000 'x') in
          Flow.write_string flow
            (serialize
               { meth= "POST"
               ; path= "/ignore"
               ; v10= false
               ; conn= Some "close"
               ; body } ) ;
          equal string {|200 "ignored" close|} (read_response ~head:false r) ;
          equal int ~msg:"end of stream" 0
            (Buf_reader.read_into r (Bytes.create 1) 0 1) ;
          Flow.close flow ) ]

(* Timeouts on a fake time line. The test_net and test_h1_server suites have the
   other timeouts. *)
let timeout_tests =
  group "Server timeouts"
    [ test "a body trickled after the response is not drained forever" (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f in
          with_fake_time_server f
          @@ fun server ->
          let flow, r = connect server in
          Flow.set_read_deadline flow
            (Mtime.add_span (Mtime.now ()) Mtime.Span.(2 * s)) ;
          let m = Fake.mark f in
          Flow.write_string flow
            ( "POST /ignore HTTP/1.1\r\nHost: x\r\nContent-Length: 1000\r\n\r\n"
            ^ String.make 10 'x' ) ;
          equal string {|200 "ignored"|} (read_response ~head:false r) ;
          (* The drain waits for the rest of the body until t0 + R. *)
          Fake.wait_mono_timer f ~since:m (t0 +: read_r) ;
          (* A byte each second. After each byte, the next read waits for the
             same deadline. This proves that the drain took the byte, and that
             the byte did not renew the deadline. *)
          for _ = 1 to 12 do
            Fake.advance f Mtime.Span.s ;
            let m = Fake.mark f in
            Flow.write_string flow "x" ;
            Fake.wait_mono_timer f ~since:m (t0 +: read_r)
          done ;
          Fake.advance f Mtime.Span.(read_r - (12 * s) - ns) ;
          let m = Fake.mark f in
          Flow.write_string flow "x" ;
          Fake.wait_mono_timer f ~since:m (t0 +: read_r) ;
          (* At t0 + R, the server ends the connection. *)
          Fake.advance f Mtime.Span.ns ;
          ( match Buf_reader.read_into r (Bytes.create 1) 0 1 with
          | 0 | (exception Flow.Closed) ->
              ()
          | _ ->
              fail "unexpected bytes" ) ;
          Flow.close flow ) ]

(* Upgrades over sockets *)

let upgrade_tests =
  let connect server =
    let flow, r = connect server in
    Flow.set_read_deadline flow
      (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s)) ;
    (flow, r)
  in
  let read_head r =
    let status = Buf_reader.read_line r ~max:1000 in
    let rec fields () =
      if Buf_reader.read_line r ~max:1000 <> "" then fields ()
    in
    fields () ; status
  in
  let read_n r n =
    let buf = Bytes.create n in
    let rec fill got =
      if got < n then
        match Buf_reader.read_into r buf got (n - got) with
        | 0 ->
            failwith "end of stream"
        | k ->
            fill (got + k)
    in
    fill 0 ; Bytes.to_string buf
  in
  let eof r =
    equal int ~msg:"end of stream" 0
      (Buf_reader.read_into r (Bytes.create 1) 0 1)
  in
  group "Upgrades over sockets"
    [ test
        "after a 101, bytes are echoed as they arrive, without HTTP's timeouts"
        (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f in
          let m = Fake.mark f in
          (* [half_head] asks for "/". *)
          let handler _ =
            Response.upgrade
              ~headers:(Headers.of_list [("Upgrade", "echo")])
              (fun flow -> copy flow flow)
          in
          with_fake_time_server f ~handler
          @@ fun server ->
          let flow, r = connect server in
          (* In two parts, so that the server reads the second under its head
             deadline. *)
          half_head f flow ~t0 ~since:m ;
          Flow.write_string flow
            "Host: x\r\nUpgrade: echo\r\nConnection: Upgrade\r\n\r\n" ;
          equal string "HTTP/1.1 101 Switching Protocols" (read_head r) ;
          List.iter
            (fun msg ->
              Flow.write_string flow msg ;
              equal string msg (read_n r (String.length msg)) )
            ["ping"; "pong"; String.make 10_000 'x'] ;
          (* Past all the HTTP deadlines of the connection. *)
          Fake.advance f Mtime.Span.(60 * s) ;
          Flow.write_string flow "still there" ;
          equal string "still there" (read_n r 11) ;
          Flow.shutdown_send flow ;
          eof r ;
          Flow.close flow )
    ; test "a CONNECT tunnel reaches another server" (fun () ->
          with_server
          @@ fun server ->
          let upstream = Tcp.Listener.listen loopback in
          let echo =
            Fun.Async.call (fun () ->
                let fd, _ = Tcp.Listener.accept upstream in
                let flow = Tcp.flow fd in
                Fun.protect
                  ~finally:(fun () -> Flow.close flow)
                  (fun () -> copy flow flow) )
          in
          let target =
            Format.asprintf "%a" Conn_info.pp_addr (Tcp.Listener.addr upstream)
          in
          let flow, r = connect server in
          Flow.write_string flow
            (Printf.sprintf "CONNECT %s HTTP/1.1\r\nHost: %s\r\n\r\n" target
               target ) ;
          equal string "HTTP/1.1 200 OK" (read_head r) ;
          let msg = "through the tunnel and back" in
          Flow.write_string flow msg ;
          equal string msg (read_n r (String.length msg)) ;
          Flow.shutdown_send flow ;
          eof r ;
          Fun.Async.get echo ;
          Tcp.Listener.close upstream ;
          Flow.close flow ) ]

(* Clients that leave while their handler runs *)

let disconnect_tests =
  let reset () =
    waiting := Cell.Once.make () ;
    cancelled := Cell.Once.make ()
  in
  group "Client disconnects"
    [ test "a client that closes cancels its handler" (fun () ->
          reset () ;
          with_server
          @@ fun server ->
          (* The handler waits 10 s if no cancel comes. On a failure, the abort
             cancels it: the test does not wait for the 10 s. *)
          Fun.protect ~finally:(fun () -> Server.abort server)
          @@ fun () ->
          let flow, _ = connect server in
          Flow.write_string flow (get "/wait") ;
          within_2s (fun () -> Cell.Once.get !waiting) ;
          Flow.close flow ;
          within_2s (fun () -> Cell.Once.get !cancelled) )
    ; test "the cancellation reaches the handler's upstream connection"
        (fun () ->
          reset () ;
          with_server
          @@ fun server ->
          let l = Tcp.Listener.listen loopback in
          upstream := Tcp.Listener.addr l ;
          let flow, _ = connect server in
          Flow.write_string flow (get "/fetch") ;
          let fd, _ = Tcp.Listener.accept l in
          let up = Tcp.flow fd in
          Flow.set_read_deadline up
            (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s)) ;
          let buf = Bytes.create 8 in
          let rec read_question got =
            if got < 8 then read_question (got + Flow.read up buf got (8 - got))
          in
          read_question 0 ;
          Flow.close flow ;
          (* The upstream never answers: the proxy side closes it. *)
          equal int ~msg:"upstream closed by the server" 0
            (Flow.read up buf 0 1) ;
          is_true ~msg:"cancelled" (Cell.Once.is_set !cancelled) ;
          Flow.close up ;
          Tcp.Listener.close l ) ]

(* Server.run in a child process, stopped with signals: sending signals to the
   test process itself could kill it. *)
let signal_tests =
  let in_main f = Affect_unix.Unix.main ~domain_count f in
  let program =
    Filename.concat
      (Filename.dirname Sys.executable_name)
      "serve_until_signal.exe"
  in
  (* Runs [f pid line] with the program started: its pid, and [line ()], the
     next line of its standard output. [line] waits at most 5 s. Kills the
     program if [f] leaves it running. *)
  let with_program ?(args = []) f =
    let out_r, out_w = Unix.pipe ~cloexec:true () in
    let pid =
      Unix.create_process program
        (Array.of_list (program :: args))
        Unix.stdin out_w Unix.stderr
    in
    Unix.close out_w ;
    Unix.set_nonblock out_r ;
    let out = Tcp.flow out_r in
    let finally () =
      Flow.close out ;
      match Unix.waitpid [Unix.WNOHANG] pid with
      | 0, _ ->
          Unix.kill pid Sys.sigkill ;
          ignore (Unix.waitpid [] pid)
      | _ ->
          ()
      | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
          ()
    in
    let r = Buf_reader.of_flow out in
    let line () =
      Flow.set_read_deadline out
        (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s)) ;
      Buf_reader.read_line r ~max:100
    in
    Fun.protect ~finally (fun () -> f pid line)
  in
  let connect port =
    let flow = Tcp.connect (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) in
    Flow.set_read_deadline flow
      (Mtime.add_span (Mtime.now ()) Mtime.Span.(5 * s)) ;
    (flow, Buf_reader.of_flow flow)
  in
  (* The first line of the program is its port. *)
  let port line = int_of_string (line ()) in
  (* The exit status of [pid] and the milliseconds it took, killing it after 5s.
     The grace of the server is 10 s: thus a server that waits for it fails. *)
  let exited pid =
    let clock = Mtime.counter () in
    let rec poll () =
      match Unix.waitpid [Unix.WNOHANG] pid with
      | 0, _ when Mtime.Span.to_float_ns (Mtime.count clock) > 5e9 ->
          failwith "the server did not exit"
      | 0, _ ->
          Mtime.wait_for Mtime.Span.(10 * ms) ;
          poll ()
      | _, status ->
          (status, Mtime.Span.to_float_ns (Mtime.count clock) /. 1e6)
    in
    poll ()
  in
  let exited_ok pid =
    let status, _ = exited pid in
    equal bool ~msg:"exit 0" true (status = Unix.WEXITED 0)
  in
  (* Signals of a type merge while they are pending. Thus each test waits for
     the line that shows the effect of a signal before it sends the next. *)
  group "Server.run and signals"
    [ test "SIGTERM: the request in progress finishes, then the process exits"
        (fun () ->
          in_main
          @@ fun () ->
          with_program
          @@ fun pid line ->
          let flow, r = connect (port line) in
          Flow.write_string flow (get "/slow") ;
          equal string "started" (line ()) ;
          Unix.kill pid Sys.sigterm ;
          (* /slow gives "timeout" after 2 s without the stop. *)
          equal string {|200 "done" close|} (read_response ~head:false r) ;
          Flow.close flow ;
          exited_ok pid )
    ; test "a second SIGINT aborts the request in progress" (fun () ->
          in_main
          @@ fun () ->
          with_program
          @@ fun pid line ->
          let flow, r = connect (port line) in
          Flow.write_string flow (get "/stuck") ;
          equal string "started" (line ()) ;
          Unix.kill pid Sys.sigint ;
          equal string "stopping" (line ()) ;
          Unix.kill pid Sys.sigint ;
          exited_ok pid ;
          is_true ~msg:"no response" (Buf_reader.peek_eof r) ;
          Flow.close flow )
    ; test "SIGTERM with nothing in progress exits at once" (fun () ->
          in_main
          @@ fun () ->
          with_program
          @@ fun pid line ->
          let flow, r = connect (port line) in
          Flow.write_string flow (get "/hello") ;
          equal string {|200 "hello"|} (read_response ~head:false r) ;
          Unix.kill pid Sys.sigterm ;
          is_true ~msg:"the idle connection is closed" (Buf_reader.peek_eof r) ;
          Flow.close flow ;
          exited_ok pid )
    ; test
        "a third signal ends a server stuck in a handler deaf to cancellation"
        (fun () ->
          in_main
          @@ fun () ->
          with_program
          @@ fun pid line ->
          let flow, _ = connect (port line) in
          Flow.write_string flow (get "/deaf") ;
          equal string "started" (line ()) ;
          Unix.kill pid Sys.sigint ;
          equal string "stopping" (line ()) ;
          Unix.kill pid Sys.sigint ;
          equal string "cancelled" (line ()) ;
          Unix.kill pid Sys.sigint ;
          let status, ms = exited pid in
          equal bool ~msg:"killed by SIGINT" true
            (status = Unix.WSIGNALED Sys.sigint) ;
          is_true ~msg:(Printf.sprintf "after %.0fms" ms) (ms < 2000.) ;
          Flow.close flow )
    ; test "once run returned, the signals act as before" (fun () ->
          in_main
          @@ fun () ->
          with_program ~args:["after-run"]
          @@ fun pid line ->
          ignore (line ()) ;
          Unix.kill pid Sys.sigterm ;
          equal string "stopped" (line ()) ;
          Unix.kill pid Sys.sigterm ;
          let status, ms = exited pid in
          equal bool ~msg:"killed by SIGTERM" true
            (status = Unix.WSIGNALED Sys.sigterm) ;
          is_true ~msg:(Printf.sprintf "after %.0fms" ms) (ms < 2000.) ) ]

(* The stop of the server, as a handler sees it *)

let stop_tests =
  let config = {config with shutdown_grace= Mtime.Span.(30 * s)} in
  group "Server stop"
    [ test "before the stop, the stopping action is not enabled" (fun () ->
          with_server ~config
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/is-stopping") ;
          equal string {|200 "no"|} (read_response ~head:false r) ;
          Flow.close flow )
    ; test "a handler sees that the server stops" (fun () ->
          waiting := Cell.Once.make () ;
          with_server ~config
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/stopping") ;
          Cell.Once.get !waiting ;
          Server.shutdown server ;
          (* The handler gives "timeout" after 2 s without the stop. *)
          equal string {|200 "stopping" close|} (read_response ~head:false r) ;
          Flow.close flow )
    ; test "a grace of 2^63 ns or more does not cancel the handlers" (fun () ->
          (* 2^63 ns + 1 s: affect ends a wait of 2^63 ns or more at once. *)
          let grace = Mtime.Span.(add (of_uint64_ns Int64.min_int) s) in
          let started = Cell.Once.make ()
          and release = Cell.Once.make ()
          and cancelled = Cell.Once.make () in
          let handler _ =
            Cell.Once.try_set started () ;
            match Cell.Once.get release with
            | () ->
                Response.string "done"
            | exception Fun.Async.Cancelled ->
                Cell.Once.try_set cancelled () ;
                raise Fun.Async.Cancelled
          in
          with_server ~config:{config with shutdown_grace= grace} ~handler
          @@ fun server ->
          let flow, r = connect server in
          Flow.write_string flow (get "/hold") ;
          within_2s (fun () -> Cell.Once.get started) ;
          Server.shutdown server ;
          is_false ~msg:"no cancel within 50 ms"
            (Fake_time.within_50ms (Cell.Once.get' cancelled)) ;
          Cell.Once.try_set release () ;
          equal string {|200 "done" close|} (read_response ~head:false r) ;
          Flow.close flow ) ]

let () =
  exit
    (run "server"
       [ integration_tests
       ; timeout_tests
       ; upgrade_tests
       ; disconnect_tests
       ; signal_tests
       ; stop_tests ] )
