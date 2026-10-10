(* Tests over real loopback sockets, inside affect's scheduler. *)

open Windtrap
open Affect
open Caravan
module Mtime = Affect_unix.Mtime

let domain_count = Server_support.domain_count

let in_main f = Affect_unix.Unix.main ~domain_count f

let loopback = Server_support.loopback

module Fake = Fake_time

let ( +: ) = Fake.( +: )

let within_2s = Server_support.within_2s

(* [f server client] with a connected pair of flows. The server end is on the
   time line [time]. The client end stays on the real time line: a real deadline
   on a fake time line is about one day in the past, and gives Timeout at
   once. *)
let with_flows = Server_support.with_flows

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

(* More than the socket buffers hold: the write blocks when the peer does not
   read. *)
let write_much flow =
  let chunk = Bytes.make (1024 * 1024) 'x' in
  for _ = 1 to 256 do
    Flow.write flow chunk 0 (Bytes.length chunk)
  done

let get_2s = Server_support.get_2s

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
          with_flows
          @@ fun server client ->
          Flow.write_string client "ping" ;
          equal string "ping" (read_exactly server 4) ;
          Flow.write_string server "pong" ;
          equal string "pong" (read_exactly client 4) )
    ; test "read_now does not wait for input" (fun () ->
          with_flows
          @@ fun server client ->
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
          with_flows
          @@ fun server client ->
          Flow.write_string client "request" ;
          Flow.shutdown_send client ;
          equal string "request" (read_all server) ;
          Flow.write_string server "response" ;
          Flow.shutdown_send server ;
          equal string "response" (read_all client) )
    ; test "4 MiB each way at once" (fun () ->
          with_flows
          @@ fun server client ->
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
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server _client ->
          let span = Mtime.Span.(5 * s) in
          let d = Fake.mono_now f +: span in
          Flow.set_read_deadline server (Some d) ;
          Fake.with_call_at_timer f d (fun () ->
              ignore (Flow.read server (Bytes.create 1) 0 1) )
          @@ fun reader ->
          (* No data comes: only an advance can end the read. *)
          Fake.advance f Mtime.Span.(span - ns) ;
          is_false ~msg:"no end 1 ns before the deadline"
            (Fake.ends_within_50ms reader) ;
          Fake.advance f Mtime.Span.ns ;
          times_out reader ;
          (* A deadline at the time of the time line has passed. *)
          Flow.set_read_deadline server (Some (Fake.mono_now f)) ;
          raises Flow.Timeout (fun () -> Flow.read server (Bytes.create 1) 0 1) )
    ; test "without a deadline a read waits for data" (fun () ->
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server client ->
          let span = Mtime.Span.(5 * s) in
          Flow.set_read_deadline server (Some (Fake.mono_now f +: span)) ;
          Flow.set_read_deadline server None ;
          let reader = Fun.Async.call (fun () -> read_exactly server 4) in
          Fun.protect ~finally:(fun () -> Fun.Async.cancel reader)
          @@ fun () ->
          (* Past the deadline that the read does not have. *)
          Fake.advance f Mtime.Span.(2 * span) ;
          is_false ~msg:"no end" (Fake.ends_within_50ms reader) ;
          Flow.write_string client "late" ;
          equal string "late" (get_2s reader) )
    ; test "a write deadline ends a blocked write with Timeout" (fun () ->
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server _client ->
          let span = Mtime.Span.(5 * s) in
          let d = Fake.mono_now f +: span in
          Flow.set_write_deadline server (Some d) ;
          (* The client does not read: the write blocks with the deadline. *)
          Fake.with_call_at_timer f d (fun () -> write_much server)
          @@ fun writer -> Fake.advance f span ; times_out writer )
    ; (* A deadline set from a different fiber reaches a read or a write that
         waits already. The wait starts with a deadline D1, whose timer proves
         the wait. Then the next two tests show a change from D1 to an earlier
         D2, and a change from no deadline to a deadline D3. *)
      test "a new deadline ends a read that waits" (fun () ->
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server _client ->
          let t0 = Fake.mono_now f in
          let d1 = t0 +: Mtime.Span.(10 * s)
          and d2 = t0 +: Mtime.Span.(5 * s)
          and d3 = t0 +: Mtime.Span.(7 * s) in
          Flow.set_read_deadline server (Some d1) ;
          Fake.with_call_at_timer f d1 (fun () ->
              ignore (Flow.read server (Bytes.create 1) 0 1) )
          @@ fun reader ->
          let m = Fake.mark f in
          Flow.set_read_deadline server (Some d2) ;
          Fake.wait_mono_timer f ~since:m d2 ;
          (* The read waits again, with no deadline. No timer can prove this
             wait: the check of 50 ms gives it the time, and it cannot fail on
             correct code. *)
          Flow.set_read_deadline server None ;
          is_false ~msg:"no end without a deadline"
            (Fake.ends_within_50ms reader) ;
          let m = Fake.mark f in
          Flow.set_read_deadline server (Some d3) ;
          Fake.wait_mono_timer f ~since:m d3 ;
          Fake.advance f Mtime.Span.(7 * s) ;
          times_out reader )
    ; test "a new deadline ends a write that waits" (fun () ->
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server _client ->
          let t0 = Fake.mono_now f in
          let d1 = t0 +: Mtime.Span.(10 * s)
          and d2 = t0 +: Mtime.Span.(5 * s)
          and d3 = t0 +: Mtime.Span.(7 * s) in
          Flow.set_write_deadline server (Some d1) ;
          Fake.with_call_at_timer f d1 (fun () -> write_much server)
          @@ fun writer ->
          let m = Fake.mark f in
          Flow.set_write_deadline server (Some d2) ;
          Fake.wait_mono_timer f ~since:m d2 ;
          (* The write waits again, with no deadline. No timer can prove this
             wait: the check of 50 ms gives it the time, and it cannot fail on
             correct code. *)
          Flow.set_write_deadline server None ;
          is_false ~msg:"no end without a deadline"
            (Fake.ends_within_50ms writer) ;
          let m = Fake.mark f in
          Flow.set_write_deadline server (Some d3) ;
          Fake.wait_mono_timer f ~since:m d3 ;
          Fake.advance f Mtime.Span.(7 * s) ;
          times_out writer )
    ; test "a deadline removed from a read that waits stops it no more"
        (fun () ->
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server client ->
          let d = Fake.mono_now f +: Mtime.Span.(5 * s) in
          Flow.set_read_deadline server (Some d) ;
          Fake.with_call_at_timer f d (fun () -> read_exactly server 4)
          @@ fun reader ->
          Flow.set_read_deadline server None ;
          Fake.advance f Mtime.Span.(10 * s) ;
          is_false ~msg:"no end" (Fake.ends_within_50ms reader) ;
          Flow.write_string client "late" ;
          equal string "late" (get_2s reader) )
    ; test "writing to a closed peer raises Closed" (fun () ->
          with_flows
          @@ fun server client ->
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
          (* The RST makes the socket readable. *)
          within_2s (fun () -> Action.invoke (Flow.readable' server ())) ;
          raises Flow.Closed (fun () -> Flow.read server (Bytes.create 1) 0 1) ;
          Flow.shutdown_send server ;
          Flow.close server ;
          Tcp.Listener.close l )
    ; test "reset makes the peer see a reset, not the end of the stream"
        (fun () ->
          with_flows
          @@ fun server client ->
          Flow.write_string server "cut" ;
          Flow.reset server ;
          equal string "cut" (read_exactly client 3) ;
          raises Flow.Closed (fun () -> Flow.read client (Bytes.create 1) 0 1) ;
          Flow.close client )
    ; (* In the next tests, the timer of a read or a write deadline proves that
         the call waits. No advance comes: the deadline does not end the
         call. *)
      test "cancelling a blocked read" (fun () ->
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server _client ->
          let d = Fake.mono_now f +: Mtime.Span.(100 * s) in
          Flow.set_read_deadline server (Some d) ;
          Fake.with_call_at_timer f d (fun () ->
              Flow.read server (Bytes.create 1) 0 1 )
          @@ fun reader ->
          Fun.Async.cancel reader ;
          raises Fun.Async.Cancelled (fun () -> get_2s reader) ;
          Flow.close server ;
          raises Flow.Closed (fun () -> Flow.read server (Bytes.create 1) 0 1) )
    ; (* A different fiber closes the flow. The waits end with Closed, and the
         fd keeps its number until they end. *)
      test "a close ends a read that waits with Closed" (fun () ->
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server _client ->
          let d = Fake.mono_now f +: Mtime.Span.(100 * s) in
          Flow.set_read_deadline server (Some d) ;
          Fake.with_call_at_timer f d (fun () ->
              Flow.read server (Bytes.create 1) 0 1 )
          @@ fun reader ->
          Flow.close server ;
          raises Flow.Closed (fun () -> get_2s reader) )
    ; test "a close ends a write that waits with Closed" (fun () ->
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server _client ->
          let d = Fake.mono_now f +: Mtime.Span.(100 * s) in
          Flow.set_write_deadline server (Some d) ;
          Fake.with_call_at_timer f d (fun () -> write_much server)
          @@ fun writer ->
          Flow.close server ;
          raises Flow.Closed (fun () -> get_2s writer) )
    ; test "the fd closes when the last read that waits ends" (fun () ->
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server client ->
          let d = Fake.mono_now f +: Mtime.Span.(100 * s) in
          Flow.set_read_deadline server (Some d) ;
          Fake.with_call_at_timer f d (fun () ->
              Flow.read server (Bytes.create 1) 0 1 )
          @@ fun reader ->
          Flow.close server ;
          raises Flow.Closed (fun () -> get_2s reader) ;
          Flow.set_read_deadline client (Some (deadline_in 2000)) ;
          equal int ~msg:"the end of the stream" 0
            (Flow.read client (Bytes.create 1) 0 1) )
    ; test
        "a read that waits on a closed flow takes no bytes of a new connection"
        (fun () ->
          in_main
          @@ fun () ->
          let f = Fake.make () in
          let l = Tcp.Listener.listen loopback in
          Fun.protect ~finally:(fun () -> Tcp.Listener.close l)
          @@ fun () ->
          let connect () =
            Fun.Async.call (fun () -> Tcp.connect (Tcp.Listener.addr l))
          in
          let accept ?time () = Tcp.flow ?time (fst (Tcp.Listener.accept l)) in
          let first = connect () in
          let old = accept ~time:(Fake.mono f) () in
          let d = Fake.mono_now f +: Mtime.Span.(100 * s) in
          Flow.set_read_deadline old (Some d) ;
          let buf = Bytes.create 64 in
          Fake.with_call_at_timer f d (fun () -> Flow.read old buf 0 64)
          @@ fun reader ->
          Flow.close old ;
          let second = connect () in
          let fresh = accept () in
          let client = Fun.Async.get second in
          Flow.write_string client "0123456789" ;
          raises Flow.Closed (fun () -> get_2s reader) ;
          Flow.set_read_deadline fresh (Some (deadline_in 2000)) ;
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
          let ready within =
            Action.invoke
              (Action.choose
                 [ Tcp.Listener.wait_acceptable l true
                 ; Mtime.wait_for' within false ] )
          in
          is_true ~msg:"nothing pending" (not (ready Mtime.Span.(30 * ms))) ;
          let client = Tcp.connect (Tcp.Listener.addr l) in
          (* A slow machine can take more than 30 ms. *)
          is_true ~msg:"a connection is pending" (ready Mtime.Span.(2 * s)) ;
          let fd, _ = Tcp.Listener.accept l in
          Unix.close fd ; Flow.close client ; Tcp.Listener.close l )
    ; test "try_accept does not wait" (fun () ->
          in_main
          @@ fun () ->
          let l = Tcp.Listener.listen loopback in
          Fun.protect ~finally:(fun () -> Tcp.Listener.close l)
          @@ fun () ->
          let none () =
            within_2s (fun () -> Tcp.Listener.try_accept l) = None
          in
          is_true ~msg:"nothing pending" (none ()) ;
          let client = Tcp.connect (Tcp.Listener.addr l) in
          (* The readiness can be spurious: after a None, it waits again. *)
          let rec accept () =
            Action.invoke (Tcp.Listener.wait_acceptable l ()) ;
            match Tcp.Listener.try_accept l with
            | Some (fd, _) ->
                Unix.close fd
            | None ->
                accept ()
          in
          within_2s accept ;
          is_true ~msg:"nothing pending again" (none ()) ;
          Flow.close client ) ]

(* Server *)

let quiet = Server_support.config

(* Set when /pause starts, so that a test can stop the server while it is in
   progress. *)
let started = ref (Cell.Once.make ())

let handler req =
  match Request.path_only req with
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

(* Reads one response with a Content-Length body from [r]. [field] gets each
   field line. *)
let read_response_from ?(field = ignore) r =
  let status = Private.Buf_reader.read_line r ~max:1000 in
  let rec headers len =
    match Private.Buf_reader.read_line r ~max:1000 with
    | "" ->
        len
    | line ->
        field line ;
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
          let started = Cell.Once.make () and release = Cell.Once.make () in
          let handler _ =
            Cell.Once.try_set started () ;
            Cell.Once.get release ;
            Response.string "done"
          in
          in_main
          @@ fun () ->
          let server =
            Server.start ~config:{quiet with max_connections= 1} handler
          in
          (* A failure must not hang: the request ends with the test. *)
          Fun.protect ~finally:(fun () -> Cell.Once.try_set release ())
          @@ fun () ->
          let addr = Server.local_addr server in
          let c = Tcp.connect addr in
          Flow.write_string c (get "/") ;
          within_2s (fun () -> Cell.Once.get started) ;
          Server.shutdown server ;
          (* The listener closes while the only slot is still taken. The test
             tries again for 2 s at most. *)
          let deadline = deadline_in 2000 in
          let rec refused () =
            let retry () =
              Mtime.is_earlier (Mtime.now ()) ~than:deadline
              &&
              ( Mtime.wait_for Mtime.Span.(5 * ms) ;
                refused () )
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
          is_true ~msg:"refused" (refused ()) ;
          Cell.Once.try_set release () ;
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          let _, body = read_response c in
          equal string "done" body ;
          Flow.close c ;
          within_2s (fun () -> Server.wait server) )
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
    [ (* A test does not prove that the retry has no pause: a limit on the time
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

(* A stop before the supervisor runs. These tests use one domain, whatever
   TEST_DOMAINS is: there, the calls that Server.start makes do not run before
   the caller blocks, so an abort right after start cancels the supervisor
   before it sees the stop. The tests depend on this order of affect: with more
   domains, the supervisor can end normally, and a cancelled caller of
   Fun.Async.get then gets its result. *)

let early_stop_tests =
  let start () =
    Server.start_with ~config:quiet
      (module Fake_listener)
      (Fake_listener.make [Gate Action.never])
      handler
  in
  group "A stop before the supervisor runs"
    [ (* The supervisor ends with Cancelled: the grace call gets it from the
         supervisor, and wait gets it too. The two take it as the end. *)
      test "an abort right after start: wait returns" (fun () ->
          Affect_unix.Unix.main ~domain_count:1
          @@ fun () ->
          let server = start () in
          Server.abort server ;
          within_2s (fun () -> Server.wait server) )
    ; (* wait takes Cancelled from the supervisor as the end, but not the cancel
         of its own caller. *)
      test "after that abort, a cancelled caller of wait gets Cancelled"
        (fun () ->
          Affect_unix.Unix.main ~domain_count:1
          @@ fun () ->
          let server = start () in
          Server.abort server ;
          within_2s (fun () -> Server.wait server) ;
          let w =
            Fun.Async.call (fun () ->
                Fun.Async.cancel_current () ;
                Server.wait server )
          in
          (* If this fails on correct code, examine affect first: the test needs
             a call that does not run before its caller blocks, on one
             domain. *)
          raises
            ~msg:
              "Cancelled (needs: on one domain, a call of start runs only \
               after the caller blocks)" Fun.Async.Cancelled (fun () ->
              get_2s w ) ) ]

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
                  let p = Option.get (Affect_unix.Ptime.of_float_s t) in
                  if Private.Date.of_posix p <> expected t then
                    failwith (Printf.sprintf "wrong date for %.0f" t) ;
                  if i mod 1000 = 0 then Fun.Async.yield ()
                done )
          in
          ignore (Fun.Async.get_all (List.init 8 worker)) ) ]

(* The server on a fake time line *)

(* The spans and the config of the server on a fake time line. *)
let header_h = Server_support.header_h

let idle_i = Server_support.idle_i

let grace_g = Server_support.grace_g

let linger_l = Server_support.linger_l

let read_r = Server_support.read_r

let write_w = Server_support.write_w

let fake_config = Server_support.fake_config

(* [with_fake_listener] with the fake time [f] and the config [config] (default
   [fake_config f]). When [test] ends, also by a failure, the fake time advances
   one day: then each wait on it ends, for example an accept pause, and the
   server can stop. *)
let with_fake_server f ?(config = fake_config f) ?handler ?engine ~clients steps
    test =
  with_fake_listener ~config ?handler ?engine ~clients steps
  @@ fun l server errors ->
  Fun.protect
    ~finally:(fun () -> Fake.advance f Mtime.Span.day)
    (fun () -> test l server errors)

let half_head = Server_support.half_head

let status_is code (status, _) =
  is_true ~msg:status (String.starts_with ~prefix:("HTTP/1.1 " ^ code) status)

let fake_time_tests =
  group "Server on a fake time line"
    [ test "with_fake_time_server ends when a request is in progress" (fun () ->
          (* Only a cancel ends the handler. A shutdown in place of the abort
             can compute the grace deadline after the advance, and then wait for
             ever. *)
          let f = Fake.make () and started = Cell.Once.make () in
          let handler _ =
            Cell.Once.try_set started () ;
            Cell.Once.get (Cell.Once.make ())
          in
          Server_support.with_fake_time_server f ~handler
          @@ fun server ->
          (* The client stays open: the server must end it. *)
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get "/") ;
          within_2s (fun () -> Cell.Once.get started) )
    ; test "EMFILE pauses the accepts for 100 ms" (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let fd, c = socket_pair () in
          with_fake_server f ~clients:[c]
            [Error EMFILE; Conn fd; Gate Action.never]
          @@ fun l server errors ->
          Fake.wait_mono_timer f ~since:m (t0 +: Mtime.Span.(100 * ms)) ;
          equal int ~msg:"calls of try_accept in the pause" 1
            (List.length (Atomic.get l.calls)) ;
          is_true ~msg:"one EMFILE reported"
            ( match Atomic.get errors with
            | [Unix.Unix_error (EMFILE, "accept", _)] ->
                true
            | _ ->
                false ) ;
          Fake.advance f Mtime.Span.(100 * ms) ;
          Flow.write_string c (get ~close:true "/") ;
          ends_with "hello" (response c) ;
          (* The gate is never enabled: no more calls come. *)
          equal int ~msg:"calls of try_accept: the EMFILE and the connection" 2
            (List.length (Atomic.get l.calls)) ;
          equal bool true (Server.local_addr server = Unix.ADDR_UNIX "fake") )
    ; test "a head that completes 1 ns before the header timeout gets 200"
        (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let fd, c = socket_pair () in
          with_fake_server f ~clients:[c] [Conn fd; Gate Action.never]
          @@ fun _ _ _ ->
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          half_head f c ~t0 ~since:m ;
          Fake.advance f Mtime.Span.(header_h - ns) ;
          Flow.write_string c "Host: x\r\n\r\n" ;
          status_is "200" (read_response c) )
    ; test "a head that is not complete at the header timeout gets 408"
        (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let fd, c = socket_pair () in
          with_fake_server f ~clients:[c] [Conn fd; Gate Action.never]
          @@ fun _ _ _ ->
          half_head f c ~t0 ~since:m ;
          Fake.advance f header_h ;
          let r = response c in
          is_true ~msg:r (String.starts_with ~prefix:"HTTP/1.1 408" r) )
    ; test "a kept-alive connection closes at the idle timeout" (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let fd, c = socket_pair () in
          with_fake_server f ~clients:[c] [Conn fd; Gate Action.never]
          @@ fun _ _ _ ->
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          Fake.wait_mono_timer f ~since:m (t0 +: header_h) ;
          let r = Private.Buf_reader.of_flow c in
          let m = Fake.mark f in
          Flow.write_string c (get "/") ;
          status_is "200" (read_response_from r) ;
          Fake.wait_mono_timer f ~since:m (t0 +: idle_i) ;
          Fake.advance f Mtime.Span.(idle_i - ns) ;
          let m = Fake.mark f in
          Flow.write_string c (get "/") ;
          status_is "200" (read_response_from r) ;
          let t2 = t0 +: Mtime.Span.(idle_i - ns) in
          Fake.wait_mono_timer f ~since:m (t2 +: idle_i) ;
          Fake.advance f idle_i ;
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          equal int ~msg:"the end of the stream" 0
            (Private.Buf_reader.read_into r (Bytes.create 1) 0 1) )
    ; test "the Date field is on the POSIX time line of the config" (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let fd, c = socket_pair () in
          let config = {(fake_config f) with date_header= true} in
          with_fake_server f ~config ~clients:[c] [Conn fd; Gate Action.never]
          @@ fun _ _ _ ->
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          let r = Private.Buf_reader.of_flow c in
          let date () =
            let date = ref "none" in
            let field line =
              if String.starts_with ~prefix:"Date: " line then date := line
            in
            Flow.write_string c (get "/") ;
            status_is "200" (read_response_from ~field r) ;
            !date
          in
          equal string "Date: Sun, 06 Nov 1994 08:49:37 GMT" (date ()) ;
          (* The connection waits for the next request until t0 + I. An advance
             of 1 s is before each deadline of the test. *)
          Fake.wait_mono_timer f ~since:m (t0 +: idle_i) ;
          Fake.advance f Mtime.Span.s ;
          equal string "Date: Sun, 06 Nov 1994 08:49:38 GMT" (date ()) )
    ; test "a body read that waits until the read timeout gets 408" (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let handler req =
            Response.string (Body.to_string (Request.body req))
          in
          let fd, c = socket_pair () in
          with_fake_server f ~handler ~clients:[c] [Conn fd; Gate Action.never]
          @@ fun _ _ _ ->
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          Flow.write_string c
            "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 2\r\n\r\n" ;
          (* The handler reads the body: each read waits [read_timeout]. *)
          Fake.wait_mono_timer f ~since:m (t0 +: read_r) ;
          Fake.advance f Mtime.Span.(read_r - ns) ;
          let m = Fake.mark f in
          Flow.write_string c "a" ;
          let t1 = t0 +: Mtime.Span.(read_r - ns) in
          Fake.wait_mono_timer f ~since:m (t1 +: read_r) ;
          Fake.advance f read_r ;
          let r = response c in
          is_true ~msg:r (String.starts_with ~prefix:"HTTP/1.1 408" r) )
    ; test
        "a response write that waits until the write timeout ends the \
         connection" (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          (* More than the buffers of the socket pair hold. *)
          let size = 8 * 1024 * 1024 in
          let handler req =
            match Request.path_only req with
            | "/big" ->
                Response.string (String.make size 'x')
            | _ ->
                Response.string "hello"
          in
          let fd, c = socket_pair () and fd2, c2 = socket_pair () in
          let config = {(fake_config f) with max_connections= 1} in
          with_fake_server f ~config ~handler ~clients:[c; c2]
            [Conn fd; Conn fd2; Gate Action.never]
          @@ fun _ _ _ ->
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          Flow.write_string c (get "/big") ;
          (* The client does not read until the timeout: only an advance can
             wake the write that waits. A read before the timeout wakes the
             write more than once, and then the test cannot prove which wait is
             the last. *)
          Fake.wait_mono_timer f ~since:m (t0 +: write_w) ;
          (* [c2] waits for the only slot, which [c] holds until the write
             timeout. *)
          Flow.write_string c2 (get ~close:true "/") ;
          Fake.advance f Mtime.Span.(write_w - ns) ;
          let r2 = Fun.Async.call (fun () -> response c2) in
          is_false ~msg:"no slot 1 ns before the write timeout"
            (Fake.ends_within_50ms r2) ;
          Fake.advance f Mtime.Span.ns ;
          let rest = read_all c in
          is_true ~msg:"the connection ends before the whole body"
            (String.length rest < size) ;
          (* The end of the connection frees its slot. *)
          ends_with "hello" (get_2s r2) )
    ; test "a stop gives the requests in progress the shutdown grace" (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f in
          let started_a = Cell.Once.make ()
          and started_b = Cell.Once.make ()
          and release_a = Cell.Once.make ()
          and release_b = Cell.Once.make ()
          and cancelled_a = Cell.Once.make () in
          let handler req =
            match Request.path_only req with
            | "/a" -> (
                Cell.Once.try_set started_a () ;
                match Cell.Once.get release_a with
                | () ->
                    Response.string "a"
                | exception Fun.Async.Cancelled ->
                    Cell.Once.try_set cancelled_a () ;
                    raise Fun.Async.Cancelled )
            | _ ->
                Cell.Once.try_set started_b () ;
                Cell.Once.get release_b ;
                Response.string "b"
          in
          let fd1, c1 = socket_pair () and fd2, c2 = socket_pair () in
          with_fake_server f ~handler ~clients:[c1; c2]
            [Conn fd1; Conn fd2; Gate Action.never]
          @@ fun _ server _ ->
          Flow.write_string c1 (get "/a") ;
          Flow.write_string c2 (get "/b") ;
          within_2s (fun () ->
              Cell.Once.get started_a ; Cell.Once.get started_b ) ;
          (* The grace starts at the stop, not at the start of the server. *)
          Fake.advance f Mtime.Span.s ;
          let m = Fake.mark f in
          Server.shutdown server ;
          Fake.wait_mono_timer f ~since:m (t0 +: Mtime.Span.s +: grace_g) ;
          Fake.advance f Mtime.Span.(grace_g - ns) ;
          Cell.Once.try_set release_b () ;
          let r = response c2 in
          ends_with "b" r ;
          is_true ~msg:"the response ends the connection"
            (contains r "\r\nConnection: close\r\n") ;
          Fake.advance f Mtime.Span.ns ;
          within_2s (fun () -> Server.wait server) ;
          is_true ~msg:"the request in progress is cancelled"
            (Cell.Once.is_set cancelled_a) )
    ; test "abort cancels requests in progress without a grace" (fun () ->
          (* No advance: the fake grace never ends. Thus only the abort can end
             the request. *)
          let f = Fake.make () and started = Cell.Once.make () in
          let handler _ =
            Cell.Once.try_set started () ;
            Cell.Once.get (Cell.Once.make () : unit Cell.Once.t) ;
            Response.string "late"
          in
          let fd, c = socket_pair () in
          with_fake_server f ~handler ~clients:[c] [Conn fd; Gate Action.never]
          @@ fun _ server _ ->
          Flow.write_string c (get "/") ;
          within_2s (fun () -> Cell.Once.get started) ;
          Server.abort server ;
          within_2s (fun () -> Server.wait server) ;
          (* The cancelled request ends its connection with no bytes. *)
          equal string "" (response c) )
    ; test "idle connections close at once on shutdown" (fun () ->
          (* No advance: the fake idle timeout and the fake grace never end.
             Thus this test shows that [Server.wait] ends when the last
             connection ends. *)
          let f = Fake.make () in
          let fd, c = socket_pair () in
          with_fake_server f ~clients:[c] [Conn fd; Gate Action.never]
          @@ fun _ server _ ->
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          let r = Private.Buf_reader.of_flow c in
          Flow.write_string c (get "/") ;
          ignore (read_response_from r) ;
          Server.shutdown server ;
          equal int ~msg:"the end of the stream" 0
            (Private.Buf_reader.read_into r (Bytes.create 1) 0 1) ;
          within_2s (fun () -> Server.wait server) )
    ; test "each of 20 idle connections closes on shutdown" (fun () ->
          (* Each connection waits on its own stop cell (#11): the stop must
             reach each one. No advance, as above. *)
          let f = Fake.make () in
          let pairs = List.init 20 (fun _ -> socket_pair ()) in
          let clients = List.map snd pairs in
          with_fake_server f ~clients
            ( List.map (fun (fd, _) -> Fake_listener.Conn fd) pairs
            @ [Gate Action.never] )
          @@ fun _ server _ ->
          let readers =
            List.map
              (fun c ->
                Flow.set_read_deadline c (Some (deadline_in 2000)) ;
                let r = Private.Buf_reader.of_flow c in
                Flow.write_string c (get "/") ;
                ignore (read_response_from r) ;
                r )
              clients
          in
          Server.shutdown server ;
          List.iteri
            (fun i r ->
              equal int
                ~msg:(Printf.sprintf "the end of stream %d" i)
                0
                (Private.Buf_reader.read_into r (Bytes.create 1) 0 1) )
            readers ;
          within_2s (fun () -> Server.wait server) )
    ; test "a handler that masks the cancel sees the stop after abort"
        (fun () ->
          let f = Fake.make () in
          let started = Cell.Once.make () and saw = Cell.Once.make () in
          let handler req =
            Fun.Async.mask_cancellation (fun () ->
                Cell.Once.try_set started () ;
                (* The real limit ends the handler if the stop does not come:
                   then the test fails, and does not hang. *)
                match
                  Action.invoke
                    (Action.choose
                       [ Action.map
                           (fun () -> `Stop)
                           (Conn_info.stopping (Option.get (Request.conn req)))
                       ; Affect_unix.Mtime.wait_for' Mtime.Span.(3 * s) `Limit
                       ] )
                with
                | `Stop ->
                    Cell.Once.try_set saw ()
                | `Limit ->
                    () ) ;
            Response.string "late"
          in
          let fd, c = socket_pair () in
          with_fake_server f ~handler ~clients:[c] [Conn fd; Gate Action.never]
          @@ fun _ server _ ->
          Flow.write_string c (get "/") ;
          within_2s (fun () -> Cell.Once.get started) ;
          Server.abort server ;
          within_2s (fun () -> Cell.Once.get saw) ;
          within_2s (fun () -> Server.wait server) )
    ; test
        "a handler that masks the cancel sees the stop when the caller of the \
         server is cancelled" (fun () ->
          (* No shutdown: the cancel of the caller stops the server. *)
          let started = Cell.Once.make () and saw = Atomic.make false in
          let handler req =
            Fun.Async.mask_cancellation (fun () ->
                Cell.Once.try_set started () ;
                match
                  Action.invoke
                    (Action.choose
                       [ Action.map
                           (fun () -> `Stop)
                           (Conn_info.stopping (Option.get (Request.conn req)))
                       ; Affect_unix.Mtime.wait_for' Mtime.Span.(3 * s) `Limit
                       ] )
                with
                | `Stop ->
                    Atomic.set saw true
                | `Limit ->
                    () ) ;
            Response.string "late"
          in
          let fd, c = socket_pair () in
          in_main
          @@ fun () ->
          let caller =
            Fun.Async.call (fun () ->
                let l = Fake_listener.make [Conn fd; Gate Action.never] in
                let server =
                  Server.start_with ~config:quiet
                    (module Fake_listener)
                    l handler
                in
                Server.wait server )
          in
          Stdlib.Fun.protect ~finally:(fun () -> Flow.close c)
          @@ fun () ->
          Flow.write_string c (get "/") ;
          within_2s (fun () -> Cell.Once.get started) ;
          Fun.Async.cancel caller ;
          (* The masked handler ends at the stop, or after its 3 s limit. With 4
             domains this test can miss a missing stop: the finally of the
             connection and that of the supervisor race. With 1 or 2 domains it
             is deterministic. *)
          ( match
              Server_support.within
                Mtime.Span.(5 * s)
                (fun () -> Fun.Async.get caller)
            with
          | () ->
              ()
          | exception Fun.Async.Cancelled ->
              () ) ;
          is_true ~msg:"the handler saw the stop" (Atomic.get saw) )
    ; test
        "a connection that ends while the server runs does not enable stopping"
        (fun () ->
          let f = Fake.make () in
          let kept = ref None in
          let handler req =
            kept := Request.conn req ;
            Response.string "one"
          in
          let fd, c = socket_pair () in
          with_fake_server f ~handler ~clients:[c] [Conn fd; Gate Action.never]
          @@ fun _ server _ ->
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          Flow.write_string c (get ~close:true "/") ;
          ignore (read_response c) ;
          Flow.close c ;
          Server_support.eventually ~msg:"the connection ended" (fun () ->
              Server.open_connections server = 0 ) ;
          is_true ~msg:"the action is not enabled"
            (not (Fake.within_50ms (Conn_info.stopping (Option.get !kept)))) )
    ; test "a response after a shutdown in its handler closes the connection"
        (fun () ->
          (* The engine polls the cell of the server when it decides to keep the
             connection. *)
          let f = Fake.make () in
          let server_ref = ref None in
          let handler _ =
            Server.shutdown (Option.get !server_ref) ;
            Response.string "bye"
          in
          let fd, c = socket_pair () in
          with_fake_server f ~handler ~clients:[c] [Conn fd; Gate Action.never]
          @@ fun _ server _ ->
          server_ref := Some server ;
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          Flow.write_string c (get "/") ;
          let fields = ref [] in
          ignore
            (read_response_from
               ~field:(fun l -> fields := String.lowercase_ascii l :: !fields)
               (Private.Buf_reader.of_flow c) ) ;
          is_true
            ~msg:(String.concat "; " !fields)
            (List.mem "connection: close" !fields) )
    ; test "an idle connection closes when the listener fails" (fun () ->
          (* Only the supervisor sets the cells here: no shutdown. No advance:
             the idle timeout never ends. *)
          let f = Fake.make () in
          let fd, c = socket_pair () in
          let fail = Cell.Once.make () in
          with_fake_server f ~clients:[c]
            [Conn fd; Gate (Cell.Once.get' fail); Error EBADF]
          @@ fun _ _ _ ->
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          let r = Private.Buf_reader.of_flow c in
          Flow.write_string c (get "/") ;
          ignore (read_response_from r) ;
          Cell.Once.try_set fail () ;
          equal int ~msg:"the end of the stream" 0
            (Private.Buf_reader.read_into r (Bytes.create 1) 0 1) )
    ; test "the server forgets each connection that ends" (fun () ->
          let f = Fake.make () in
          let pairs = List.init 50 (fun _ -> socket_pair ()) in
          let clients = List.map snd pairs in
          with_fake_server f ~clients
            ( List.map (fun (fd, _) -> Fake_listener.Conn fd) pairs
            @ [Gate Action.never] )
          @@ fun _ server _ ->
          List.iter
            (fun c ->
              Flow.set_read_deadline c (Some (deadline_in 2000)) ;
              Flow.write_string c (get ~close:true "/") ;
              ignore (read_response c) ;
              (* The server lingers until the client closes: the fake linger
                 timeout never ends. *)
              Flow.close c )
            clients ;
          Server_support.eventually ~msg:"no open connection" (fun () ->
              Server.open_connections server = 0 ) )
    ; test "a connection that the server ends lingers for the linger timeout"
        (fun () ->
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let served = Cell.Once.make () in
          let engine ctx flow handler =
            Fun.protect
              ~finally:(fun () -> Cell.Once.try_set served ())
              (fun () -> Private.H1_server.serve ctx flow handler)
          in
          let fd, c = socket_pair () and fd2, c2 = socket_pair () in
          let config = {(fake_config f) with max_connections= 1} in
          with_fake_server f ~config ~engine ~clients:[c; c2]
            [Conn fd; Conn fd2; Gate Action.never]
          @@ fun _ _ _ ->
          Flow.write_string c (get ~close:true "/") ;
          (* The server half-closes at once: the client gets the response and
             the end of the stream, and keeps its socket open. *)
          Flow.set_read_deadline c (Some (deadline_in 2000)) ;
          ends_with "hello" (read_all c) ;
          (* [c2] waits for the only slot, which [c] holds while the server
             lingers on it. *)
          Flow.write_string c2 (get ~close:true "/") ;
          Fake.wait_mono_timer f ~since:m (t0 +: linger_l) ;
          Fake.advance f Mtime.Span.(linger_l - ns) ;
          let m = Fake.mark f in
          Flow.write_string c "more" ;
          (* The linger read the bytes and waits again, to the same deadline. *)
          Fake.wait_mono_timer f ~since:m (t0 +: linger_l) ;
          is_false ~msg:"the engine ended" (Cell.Once.is_set served) ;
          Fake.advance f Mtime.Span.ns ;
          within_2s (fun () -> Cell.Once.get served) ;
          (* The end of the linger frees the slot. *)
          ends_with "hello" (response c2) )
    ; test "the engine uses the time line of its flow" (fun () ->
          in_main
          @@ fun () ->
          let f = Fake.make () in
          let fd, c = socket_pair () in
          let flow = Tcp.flow ~time:(Fake.mono f) fd in
          let ctx =
            { Engine.config= {(fake_config f) with mono_time= Time.mono}
            ; conn=
                Conn_info.make ~id:1 ~peer:(Unix.ADDR_UNIX "peer")
                  ~local:(Unix.ADDR_UNIX "local") ()
            ; stopping= Action.never
            ; is_stopping= (fun () -> false) }
          in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let serving =
            Fun.Async.call (fun () -> Private.H1_server.serve ctx flow handler)
          in
          Fun.protect ~finally:(fun () ->
              Fun.Async.cancel serving ; Flow.close c ; Flow.close flow )
          @@ fun () ->
          half_head f c ~t0 ~since:m ;
          Fake.advance f header_h ;
          let r = response c in
          is_true ~msg:r (String.starts_with ~prefix:"HTTP/1.1 408" r) ) ]

let () =
  exit
    (run "net"
       [ tcp_tests
       ; server_tests
       ; race_tests
       ; accept_error_tests
       ; accept_loop_tests
       ; early_stop_tests
       ; fake_time_tests
       ; stop_signal_tests
       ; date_tests ] )
