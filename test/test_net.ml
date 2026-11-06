(* The tests of Server on the simulator of weft. Each test runs in Sim_support:
   1 seed and 1 domain, except a test whose name starts with "race:" (100 seeds
   on 4 domains). A test that checks a time runs in manual mode.

   Most tests use Fake_engine, so that they examine only the server. The tests
   of the HTTP/1.1 behavior over the server use the H1 engine. *)

open Windtrap
open Weft
open Caravan
open Server_support

let fake = Fake_engine.serve

let h1 = Private.H1_server.serve

let ( +: ) = Sim_support.( +: )

let just_before = Sim_support.just_before

(* Filled when /pause starts. A test makes a new Ivar before it starts. *)
let started : unit Ivar.t ref = ref (Ivar.create ())

let handler req =
  match Request.path_only req with
  | "/pause" ->
      ignore (Ivar.try_fill !started ()) ;
      Time.sleep Mono (Span.ms 300) ;
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

(* [with_server] of Server_support with the handler of this suite. The engine is
   Fake_engine by default. *)
let with_server ?(config = config) ?(engine = fake) ?(handler = handler) f =
  Server_support.with_server ~config ~engine ~handler f

(* [collect config] is [config] with an [on_error] that keeps the errors, and
   the list of the errors, the last first. *)
let collect (config : Config.t) =
  let errors = Atomic.make [] in
  let rec push e =
    let l = Atomic.get errors in
    if not (Atomic.compare_and_set errors l (e :: l)) then push e
  in
  ({config with on_error= (fun e _ -> push e)}, errors)

let is_bad_fd = function Net.Error (Bad_fd, _) -> true | _ -> false

let is_refused = function Net.Error (Refused, _) -> true | _ -> false

let is_emfile = function
  | Net.Error (No_resources "EMFILE", _) ->
      true
  | _ ->
      false

let ends_with suffix s = is_true ~msg:s (String.ends_with ~suffix s)

(* [read_all] of [c], and an end of the stream by a reset. *)
let read_to_end c = try read_all c with Flow.Closed -> ""

(* [on_error_failures n] runs [n] handlers that fail at the same time, with an
   [on_error] that raises [Sys_error "Broken pipe"] (the error of a broken
   stderr). It gives the name of the exception of [Server.wait], then the names
   of the exceptions in it if it is [Fiber.Multiple]. *)
let on_error_failures n =
  let config =
    {config with on_error= (fun _ _ -> raise (Sys_error "Broken pipe"))}
  in
  let go = Ivar.create () and all_running = Ivar.create () in
  let running = Atomic.make 0 in
  let handler req =
    if Atomic.fetch_and_add running 1 = n - 1 then Ivar.fill all_running () ;
    sync (Ivar.read go) ;
    failwith ("handler " ^ Request.path_only req)
  in
  Sim_support.run
  @@ fun _ ->
  Fiber.scope
  @@ fun sw ->
  let server = Server.start ~config ~engine:h1 sw handler in
  Fiber.scope (fun s ->
      for i = 1 to n do
        ignore
          (Sim_support.call ~name:"client" s (fun () ->
               let c, r = connect server in
               Fun.protect ~finally:(fun () -> Flow.close c)
               @@ fun () ->
               Flow.write_string c (get (Printf.sprintf "/%d" i)) ;
               (* The cancel of the connections closes them. *)
               try ignore (read_response r)
               with Flow.Closed | Buf_reader.Eof | Failure _ -> () ) )
      done ;
      sync (Ivar.read all_running) ;
      Ivar.fill go () ) ;
  match Server.wait server with
  | () ->
      ["no exception"]
  | exception Fiber.Multiple l ->
      "Multiple" :: List.map (fun (e, _) -> Printexc.to_string e) l
  | exception e ->
      [Printexc.to_string e]

(* [with_broken_stderr f] runs [f ()] with the standard error on a pipe with no
   reader and with SIGPIPE ignored, as in the world of weft. Then it puts back
   the standard error and the SIGPIPE behavior. It runs outside the fibers. *)
let with_broken_stderr f =
  flush stderr ;
  let saved = Unix.dup ~cloexec:true Unix.stderr in
  let sigpipe = ref None in
  let restore () =
    (* A failed write keeps its text in the buffer of [stderr]. /dev/null takes
       this text, so that it does not come out later. If /dev/null does not
       open, the restore continues. *)
    ( try
        let null = Unix.openfile "/dev/null" [O_WRONLY; O_CLOEXEC] 0 in
        Unix.dup2 null Unix.stderr ; Unix.close null
      with Unix.Unix_error _ -> () ) ;
    (try flush stderr with Sys_error _ -> ()) ;
    Unix.dup2 saved Unix.stderr ;
    Unix.close saved ;
    Option.iter (Sys.set_signal Sys.sigpipe) !sigpipe
  in
  Fun.protect ~finally:restore (fun () ->
      sigpipe := Some (Sys.signal Sys.sigpipe Sys.Signal_ignore) ;
      let r, w = Unix.pipe ~cloexec:true () in
      Unix.close r ;
      Fun.protect
        ~finally:(fun () -> Unix.close w)
        (fun () -> Unix.dup2 w Unix.stderr) ;
      f () )

let server_tests =
  group "Server"
    [ test "serves a GET on a free port" (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:h1
          @@ fun server ->
          ( match Server.local_addr server with
          | Net.Tcp ("127.0.0.1", port) ->
              is_true ~msg:"a port that the world selected" (port > 0)
          | _ ->
              fail "not a TCP address on 127.0.0.1" ) ;
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get ~conn:"close" "/") ;
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
          Sim_support.run
          @@ fun _ ->
          with_server ~engine:h1
          @@ fun server ->
          let c, r = connect server in
          Flow.write_string c (get "/id") ;
          let first = read_response r in
          Flow.write_string c (get "/id") ;
          let second = read_response r in
          equal string {|200 "0"|} first ;
          equal string first second ;
          Flow.close c )
    ; test "connections are numbered in order from 0" (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_server
          @@ fun server ->
          let id () =
            let c, r = connect server in
            Flow.write_string c (get ~conn:"close" "/id") ;
            let response = read_response r in
            Flow.close c ; response
          in
          let first = id () in
          let second = id () in
          equal (list string)
            [{|200 "0" close|}; {|200 "1" close|}]
            [first; second] )
    ; test "max_connections holds back extra connections" (fun () ->
          Sim_support.run
          @@ fun sim ->
          with_server ~config:{config with max_connections= 1}
          @@ fun server ->
          let first, r1 = connect server in
          Flow.write_string first (get "/") ;
          equal string {|200 "hello"|} (read_response r1) ;
          let second, r2 = connect server in
          Flow.write_string second (get ~conn:"close" "/") ;
          Weft_sim.settle sim ;
          is_true ~msg:"no byte for the second client"
            (Flow.read_now second (Bytes.create 1) 0 1 = None) ;
          Flow.close first ;
          equal string {|200 "hello" close|} (read_response r2) ;
          Flow.close second )
    ; test "an engine sees the server stopping" (fun () ->
          let engine (ctx : Engine.ctx) flow _handler =
            let before = ctx.is_stopping () in
            Flow.write_string flow "ready " ;
            sync (Conn_info.stopping ctx.conn) ;
            Flow.write_string flow
              (Printf.sprintf "%b %b" before (ctx.is_stopping ()))
          in
          Sim_support.run
          @@ fun _ ->
          Fiber.scope
          @@ fun sw ->
          let server = Server.start ~config ~engine sw handler in
          let c = Tcp.connect (Server.local_addr server) in
          equal string "ready " (read_exactly c 6) ;
          Server.shutdown server ;
          Server.wait server ;
          equal string "false true" (read_all c) ;
          Flow.close c )
    ; test
        "on 1 domain, with every slot busy, shutdown refuses new clients at \
         once" (fun () ->
          (* On 1 domain, the close(2) of the listener occurs in [shutdown]. On
             more than one domain, it can occur later, and a client can connect
             and then get a reset. *)
          Sim_support.run
          @@ fun _ ->
          let started = Ivar.create () and release = Ivar.create () in
          let handler _ =
            Ivar.fill started () ;
            sync (Ivar.read release) ;
            Response.string "done"
          in
          with_server ~config:{config with max_connections= 1} ~handler
          @@ fun server ->
          let addr = Server.local_addr server in
          let c, r = connect server in
          Flow.write_string c (get "/") ;
          sync (Ivar.read started) ;
          Server.shutdown server ;
          (* The listener closes while the only slot is taken. *)
          raises_match ~msg:"the first connect after the shutdown" is_refused
            (fun () -> Tcp.connect addr ) ;
          Ivar.fill release () ;
          equal string {|200 "done" close|} (read_response r) ;
          Flow.close c )
    ; test "race: clients during a shutdown leave no fd" (fun () ->
          Sim_support.race
          @@ fun sim ->
          Sim_support.no_fd_leak sim
          @@ fun () ->
          Fiber.scope
          @@ fun sw ->
          let server =
            Server.start
              ~config:{config with max_connections= 8}
              ~engine:fake sw handler
          in
          let addr = Server.local_addr server in
          (* Over 10 ms, clients connect, send a request and read until the
             server closes. Some come after the stop. *)
          let client i () =
            Time.sleep Mono (Span.us (i * 200)) ;
            match Tcp.connect addr with
            | exception Net.Error _ ->
                ()
            | c -> (
                Fun.protect ~finally:(fun () -> Flow.close c)
                @@ fun () ->
                try
                  Flow.write_string c (get "/") ;
                  ignore (read_all c)
                with Flow.Closed -> () )
          in
          Fiber.scope
          @@ fun s ->
          let clients =
            List.init 50 (fun i -> Sim_support.call ~name:"client" s (client i))
          in
          Time.sleep Mono (Span.ms 5) ;
          Server.shutdown server ;
          Server.wait server ;
          List.iter Sim_support.await clients )
    ; test "the engine can be replaced" (fun () ->
          let engine _ctx flow _handler =
            Flow.write_string flow "custom engine"
          in
          Sim_support.run
          @@ fun _ ->
          with_server ~engine
          @@ fun server ->
          let c = Tcp.connect (Server.local_addr server) in
          equal string "custom engine" (read_all c) ;
          Flow.close c )
    ; test "an engine that raises is reported and the server goes on" (fun () ->
          let calls = Atomic.make 0 in
          let engine _ctx flow _handler =
            if Atomic.fetch_and_add calls 1 = 0 then failwith "engine"
            else Flow.write_string flow "ok"
          in
          let config, errors = collect config in
          Sim_support.run
          @@ fun _ ->
          with_server ~config ~engine
          @@ fun server ->
          let a = Tcp.connect (Server.local_addr server) in
          equal string "" (read_all a) ;
          Flow.close a ;
          let b = Tcp.connect (Server.local_addr server) in
          equal string "ok" (read_all b) ;
          Flow.close b ;
          equal (list string) [{|Failure("engine")|}]
            (List.map Printexc.to_string (Atomic.get errors)) )
    ; test "an on_error that raises at a handler error makes wait raise it"
        (fun () ->
          let names = on_error_failures 1 in
          equal (list string) [{|Sys_error("Broken pipe")|}] names )
    ; test
        "two exceptions of on_error at the same time make wait raise Multiple"
        (fun () ->
          let names = on_error_failures 2 in
          equal (list string)
            [ {|Multiple|}
            ; {|Sys_error("Broken pipe")|}
            ; {|Sys_error("Broken pipe")|} ]
            names )
    ; test "the default on_error ignores a broken stderr" (fun () ->
          (* Windows has no SIGPIPE, and a write to a pipe with no reader gives
             EPIPE there with no signal. *)
          if Sys.win32 then skip ~reason:"no SIGPIPE on Windows" () ;
          let handler _ = failwith "handler" in
          let served =
            with_broken_stderr
            @@ fun () ->
            Sim_support.run
            @@ fun _ ->
            (* [config] keeps the default on_error. *)
            with_server ~engine:h1 ~handler
            @@ fun server ->
            let request () =
              let c, r = connect server in
              Flow.write_string c (get ~conn:"close" "/") ;
              let status = read_response r in
              Flow.close c ; status
            in
            let first = request () in
            let second = request () in
            [first; second]
          in
          equal int ~msg:"responses" 2 (List.length served) ;
          List.iter
            (fun s ->
              is_true ~msg:"a 500 response"
                (String.length s >= 3 && String.sub s 0 3 = "500") )
            served ) ]

(* A stop at once after the start, on 4 domains: the server fiber can run before
   or after the stop. *)
let race_tests =
  let cycle stop config =
    Sim_support.race
    @@ fun _ ->
    Fiber.scope
    @@ fun sw ->
    let server = Server.start ~config ~engine:fake sw handler in
    Fiber.yield () ; stop server ; Server.wait server
  in
  group "Stopping races"
    [ test "race: abort right after start never makes wait raise" (fun () ->
          cycle Server.abort config )
    ; test "race: a shutdown with no grace never makes wait raise" (fun () ->
          cycle Server.shutdown {config with shutdown_grace= Span.zero} ) ]

(* The accept loop. The simulator cannot make some errors of accept(2), for
   example EPROTO. The fake listener raises the errors of a list, one for each
   call of [accept]. Then it forwards each call to a real listener. *)
module Fake_listener = struct
  type t = {real: Tcp.Listener.t; errors: Net.error list Atomic.t}

  let make errors real = {real; errors= Atomic.make errors}

  let addr l = Tcp.Listener.addr l.real

  let accept l =
    match Atomic.get l.errors with
    | e :: rest ->
        Atomic.set l.errors rest ;
        raise (Net.Error (e, "Net.accept"))
    | [] ->
        Tcp.Listener.accept l.real

  let close l = Tcp.Listener.close l.real
end

(* A listener with connections that wait in its queue: [accept] gives them with
   no wait, as accept(2) when connections wait. When the queue is empty, the
   first [accept] calls [empty]. Then [accept] waits until [close], and raises
   EBADF. Only the acceptor calls [accept]. *)
module Fake_queued_listener = struct
  type t =
    { queue: Net.t list Atomic.t
    ; empty: (unit -> unit) Atomic.t
    ; closed: unit Ivar.t }

  let peer = Net.Tcp ("10.0.0.2", 40000)

  let make ?(empty = ignore) sockets =
    { queue= Atomic.make sockets
    ; empty= Atomic.make empty
    ; closed= Ivar.create () }

  let addr _ = Net.Tcp ("10.0.0.1", 80)

  let accept l =
    match Atomic.get l.queue with
    | c :: rest ->
        Atomic.set l.queue rest ; (c, peer)
    | [] ->
        (Atomic.exchange l.empty ignore) () ;
        sync (Ivar.read l.closed) ;
        raise (Net.Error (Bad_fd, "Net.accept"))

  let close l = ignore (Ivar.try_fill l.closed ())
end

(* An engine that keeps its connection until the stop. *)
let until_stop (ctx : Engine.ctx) _flow _handler =
  sync (Conn_info.stopping ctx.conn)

let accept_loop_tests =
  group "Accept loop"
    [ test
        "a connection error of accept (EPROTO) is not reported, and the server \
         accepts again" (fun () ->
          Sim_support.run
          @@ fun _ ->
          let config, errors = collect config in
          let l =
            Fake_listener.make [Other "EPROTO"] (Tcp.Listener.listen loopback)
          in
          Fiber.scope
          @@ fun sw ->
          let server =
            Server.start_with ~config ~engine:fake sw
              (module Fake_listener)
              l handler
          in
          let c, r = connect server in
          Flow.write_string c (get ~conn:"close" "/") ;
          equal string {|200 "hello" close|} (read_response r) ;
          Flow.close c ;
          equal int ~msg:"errors reported" 0 (List.length (Atomic.get errors)) ;
          Server.shutdown server ;
          Server.wait server )
    ; test
        "EBADF stops the server: a request in progress finishes, and wait \
         raises EBADF" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let config, errors = collect config in
          started := Ivar.create () ;
          let t0 = Time.now Mono in
          let l = Tcp.Listener.listen loopback in
          Fiber.scope
          @@ fun sw ->
          let server =
            Server.start_with ~config ~engine:fake sw
              (module Tcp.Listener)
              l handler
          in
          let c, r = connect server in
          Fiber.scope
          @@ fun s ->
          let response = Sim_support.call s (fun () -> read_response r) in
          Flow.write_string c (get "/pause") ;
          sync (Ivar.read !started) ;
          (* The stop is empty: the acceptor gets EBADF as a failure. *)
          Tcp.Listener.close l ;
          equal string {|200 "done" close|}
            (Sim_support.ends_at sim ~t0 (Span.ms 300) response) ;
          raises_match is_bad_fd (fun () -> Server.wait server) ;
          equal int ~msg:"errors reported" 0 (List.length (Atomic.get errors)) ;
          Flow.close c )
    ; test "the acceptor lets the other fibers run after 16 accepts" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let pairs = List.init 17 (fun _ -> Weft_sim.socket_pair sim) in
          let l = Fake_queued_listener.make (List.map snd pairs) in
          Fiber.scope
          @@ fun sw ->
          let server =
            Server.start_with ~config ~engine:until_stop sw
              (module Fake_queued_listener)
              l handler
          in
          (* On 1 domain, the server fiber runs before this fiber. *)
          let seen =
            Fiber.scope (fun s ->
                Sim_support.await
                  (Sim_support.call s (fun () -> Server.open_connections server)) )
          in
          (* 16, not 17: the acceptor yields after the 16th accept. Not 1: it
             does not yield after each accept, because on a busy domain each
             yield slows the accepts. *)
          equal int ~msg:"connections at the first switch" 16 seen ;
          Weft_sim.settle sim ;
          equal int ~msg:"open connections" 17 (Server.open_connections server) ;
          Server.shutdown server ;
          Server.wait server ;
          List.iter (fun (c, _) -> Net.close c) pairs )
    ; test
        "a Cancelled of a different scope from accept is a failure of the \
         listener: a request in progress gets the grace" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let config, errors = collect config in
          started := Ivar.create () ;
          let go = Ivar.create () in
          let client, socket = Weft_sim.socket_pair sim in
          (* The second accept raises a cancel of no scope of the server. *)
          let l =
            Fake_queued_listener.make [socket] ~empty:(fun () ->
                sync (Ivar.read go) ;
                raise (Fiber.Cancelled Exit) )
          in
          let t0 = Time.now Mono in
          Fiber.scope
          @@ fun sw ->
          let server =
            Server.start_with ~config ~engine:fake sw
              (module Fake_queued_listener)
              l handler
          in
          let c = Tcp.flow client in
          let r = Buf_reader.of_flow c in
          Fiber.scope
          @@ fun s ->
          let response = Sim_support.call s (fun () -> read_response r) in
          Flow.write_string c (get "/pause") ;
          sync (Ivar.read !started) ;
          Ivar.fill go () ;
          equal string {|200 "done" close|}
            (Sim_support.ends_at sim ~t0 (Span.ms 300) response) ;
          raises (Fiber.Cancelled Exit) (fun () -> Server.wait server) ;
          equal int ~msg:"errors reported" 0 (List.length (Atomic.get errors)) ;
          Flow.close c )
    ; test "a server that cannot start closes the listener" (fun () ->
          Sim_support.run
          @@ fun _ ->
          List.iter
            (fun n ->
              let l = Tcp.Listener.listen loopback in
              Fiber.scope (fun sw ->
                  raises_match
                    (function Invalid_argument _ -> true | _ -> false)
                    (fun () ->
                      Server.start_with
                        ~config:{config with max_connections= n}
                        sw
                        (module Tcp.Listener)
                        l handler ) ) ;
              raises_match
                ~msg:(Printf.sprintf "max_connections %d: listener closed" n)
                is_refused (fun () -> Tcp.connect (Tcp.Listener.addr l) ) )
            [-1; 0] )
    ; test "a cancel of the scope of the server closes the listener" (fun () ->
          Sim_support.run
          @@ fun _ ->
          let l = Tcp.Listener.listen loopback in
          let started = Ivar.create () in
          Fiber.scope
          @@ fun s ->
          (* No shutdown: only the cancel stops the server. *)
          let caller =
            Sim_support.call ~name:"caller" s (fun () ->
                Fiber.scope (fun sw ->
                    let server =
                      Server.start_with ~config ~engine:fake sw
                        (module Tcp.Listener)
                        l handler
                    in
                    Ivar.fill started () ; Server.wait server ) )
          in
          sync (Ivar.read started) ;
          Sim_support.cancel caller ;
          raises Promise.Cancelled (fun () -> Sim_support.await caller) ;
          raises_match ~msg:"listener closed" is_bad_fd (fun () ->
              Tcp.Listener.accept l ) ) ]

(* A stop before the server fiber runs. In manual mode on 1 domain, the forked
   server fiber runs only when the test fiber waits. The "Stopping races" run an
   abort at once after the start on 4 domains. *)
let early_stop_tests =
  group "A stop before the server fiber runs"
    [ test "after an abort, a cancelled caller of wait gets Cancelled" (fun () ->
          Sim_support.manual
          @@ fun _ ->
          Fiber.scope
          @@ fun sw ->
          let server = Server.start ~config ~engine:fake sw handler in
          Server.abort server ;
          Server.wait server ;
          Fiber.scope (fun s ->
              Fiber.cancel s Exit ;
              raises (Fiber.Cancelled Exit) (fun () -> Server.wait server) ) )
    ]

(* A listener for one connection that comes at the time of a stop. The first
   [accept] waits until the test calls [go]. Then it runs [act], for example a
   shutdown, and gives [socket]: as accept(2) that returns a connection at the
   time of the stop. A later [accept] waits until [close], then raises EBADF. *)
module Fake_late_listener = struct
  type t =
    { socket: Net.t
    ; act: unit -> unit
    ; first: bool Atomic.t
    ; go: unit Ivar.t
    ; closed: unit Ivar.t }

  let peer = Net.Tcp ("10.0.0.2", 40000)

  let make ~act socket =
    { socket
    ; act
    ; first= Atomic.make true
    ; go= Ivar.create ()
    ; closed= Ivar.create () }

  let go l = Ivar.fill l.go ()

  let addr _ = Net.Tcp ("10.0.0.1", 80)

  let accept l =
    let closed () = raise (Net.Error (Bad_fd, "Net.accept")) in
    if Atomic.exchange l.first false then
      sync
        (choose
           [ wrap (Ivar.read l.go) (fun () -> l.act () ; (l.socket, peer))
           ; wrap (Ivar.read l.closed) closed ] )
    else sync (wrap (Ivar.read l.closed) closed)

  let close l = ignore (Ivar.try_fill l.closed ())
end

(* [counted_engine ()] is Fake_engine with a count of its calls. *)
let counted_engine () =
  let calls = Atomic.make 0 in
  ((fun ctx flow h -> Atomic.incr calls ; fake ctx flow h), calls)

(* [is_closed_by_server client] tells if the server closed its side of the
   connection of [client]: [client] reads the end of the stream with no wait. *)
let is_closed_by_server client =
  Net.read_now client (Bytes.create 1) 0 1 = Some 0

(* The connection that accept gives at the time of a stop or of a cancel (also
   at the fork of the grace), and the error of Net.local_addr on an accepted
   socket. *)
let late_accept_tests =
  group "A connection that comes at the stop"
    [ test "a connection that the acceptor gets after a shutdown is closed"
        (fun () ->
          Sim_support.run
          @@ fun sim ->
          Sim_support.with_pair sim
          @@ fun client socket ->
          let config, errors = collect config in
          let engine, calls = counted_engine () in
          let server = ref None in
          let l =
            Fake_late_listener.make socket ~act:(fun () ->
                Server.shutdown (Option.get !server) )
          in
          Fiber.scope
          @@ fun sw ->
          let s =
            Server.start_with ~config ~engine sw
              (module Fake_late_listener)
              l handler
          in
          server := Some s ;
          Fake_late_listener.go l ;
          Server.wait s ;
          equal int ~msg:"engine calls" 0 (Atomic.get calls) ;
          equal int ~msg:"open connections" 0 (Server.open_connections s) ;
          equal int ~msg:"errors reported" 0 (List.length (Atomic.get errors)) ;
          is_true ~msg:"the server closed the connection"
            (is_closed_by_server client) )
    ; test
        "a connection that the acceptor gets after a cancel of the scope of \
         the server is closed, and wait returns" (fun () ->
          Sim_support.run
          @@ fun sim ->
          Sim_support.with_pair sim
          @@ fun client socket ->
          let config, errors = collect config in
          let engine, calls = counted_engine () in
          let started = Ivar.create () in
          Fiber.scope
          @@ fun s ->
          let caller =
            Sim_support.call ~name:"caller" s (fun () ->
                Fiber.scope (fun sw ->
                    let l =
                      Fake_late_listener.make socket ~act:(fun () ->
                          Fiber.cancel sw Exit )
                    in
                    Ivar.fill started
                      ( Server.start_with ~config ~engine sw
                          (module Fake_late_listener)
                          l handler
                      , l ) ) )
          in
          let server, l = sync (Ivar.read started) in
          Fake_late_listener.go l ;
          Server.wait server ;
          Sim_support.await caller ;
          equal int ~msg:"engine calls" 0 (Atomic.get calls) ;
          equal int ~msg:"open connections" 0 (Server.open_connections server) ;
          equal int ~msg:"errors reported" 0 (List.length (Atomic.get errors)) ;
          is_true ~msg:"the server closed the connection"
            (is_closed_by_server client) )
    ; test
        "a cancel of the scope of the server at the end of the acceptor: wait \
         returns" (fun () ->
          Sim_support.run
          @@ fun sim ->
          Sim_support.with_pair sim
          @@ fun client socket ->
          let config, errors = collect config in
          let engine, calls = counted_engine () in
          let started = Ivar.create () in
          let server = ref None in
          Fiber.scope
          @@ fun s ->
          let caller =
            Sim_support.call ~name:"caller" s (fun () ->
                Fiber.scope (fun sw ->
                    let l =
                      Fake_late_listener.make socket ~act:(fun () ->
                          Fiber.cancel sw Exit ;
                          Server.shutdown (Option.get !server) )
                    in
                    let t =
                      Server.start_with ~config ~engine sw
                        (module Fake_late_listener)
                        l handler
                    in
                    server := Some t ;
                    Ivar.fill started (t, l) ) )
          in
          let server, l = sync (Ivar.read started) in
          Fake_late_listener.go l ;
          Server.wait server ;
          Sim_support.await caller ;
          equal int ~msg:"engine calls" 0 (Atomic.get calls) ;
          equal int ~msg:"errors reported" 0 (List.length (Atomic.get errors)) ;
          is_true ~msg:"the server closed the connection"
            (is_closed_by_server client) )
    ; test "an error of local_addr gives the peer as the local address"
        (fun () ->
          Sim_support.run
          @@ fun sim ->
          Sim_support.with_pair sim
          @@ fun _ socket ->
          let config, errors = collect config in
          let local = Ivar.create () in
          let engine (ctx : Engine.ctx) _ _ =
            Ivar.fill local (Conn_info.local ctx.conn)
          in
          (* The close makes Net.local_addr raise EBADF. *)
          let l =
            Fake_late_listener.make socket ~act:(fun () -> Net.close socket)
          in
          Fiber.scope
          @@ fun sw ->
          let server =
            Server.start_with ~config ~engine sw
              (module Fake_late_listener)
              l handler
          in
          Fake_late_listener.go l ;
          Weft_sim.settle sim ;
          is_true ~msg:"the engine got the peer as the local address"
            (Ivar.peek local = Some Fake_late_listener.peer) ;
          Server.shutdown server ;
          Server.wait server ;
          equal int ~msg:"errors reported" 0 (List.length (Atomic.get errors)) )
    ]

(* The accepts after EMFILE. The limit of fds makes [accept] fail. *)
let emfile_tests =
  group "Accept errors for resources"
    [ test "EMFILE pauses the accepts for 100 ms" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let config, errors = collect config in
          with_server ~config
          @@ fun server ->
          Weft_sim.settle sim ;
          (* One fd for the client socket. Then the accept of its connection
             fails. *)
          Weft_sim.set_fd_limit sim (Some (Weft_sim.open_fds sim + 1)) ;
          let c, r = connect server in
          Weft_sim.settle sim ;
          let t0 = Time.now Mono in
          is_true ~msg:"one EMFILE reported"
            (match Atomic.get errors with [e] -> is_emfile e | _ -> false) ;
          Sim_support.advance sim (Span.ms 50) ;
          Weft_sim.set_fd_limit sim None ;
          Fiber.scope
          @@ fun s ->
          let response = Sim_support.call s (fun () -> read_response r) in
          Flow.write_string c (get ~conn:"close" "/") ;
          equal string {|200 "hello" close|}
            (Sim_support.ends_at sim ~t0 (Span.ms 100) response) ;
          equal int ~msg:"errors reported" 1 (List.length (Atomic.get errors)) ;
          Flow.close c )
    ; test "a shutdown ends the pause after EMFILE at once" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let config, errors = collect config in
          Fiber.scope
          @@ fun sw ->
          let server = Server.start ~config ~engine:fake sw handler in
          Weft_sim.settle sim ;
          Weft_sim.set_fd_limit sim (Some (Weft_sim.open_fds sim + 1)) ;
          let c = Tcp.connect (Server.local_addr server) in
          Weft_sim.settle sim ;
          is_true ~msg:"the EMFILE reported before the pause"
            (match Atomic.get errors with [e] -> is_emfile e | _ -> false) ;
          Fiber.scope (fun s ->
              let waiting = Sim_support.call s (fun () -> Server.wait server) in
              Server.shutdown server ;
              (* Manual mode: the time does not move. *)
              is_true ~msg:"wait ended at the shutdown"
                (Sim_support.ended sim waiting) ;
              Sim_support.await waiting ) ;
          Weft_sim.set_fd_limit sim None ;
          Flow.close c )
    ; test "at the fd limit with no client, the server reports EMFILE one time"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let config, errors = collect config in
          let count () = List.length (Atomic.get errors) in
          with_server ~config
          @@ fun server ->
          (* The server fiber did not run yet: its first accept fails. *)
          Weft_sim.set_fd_limit sim (Some (Weft_sim.open_fds sim)) ;
          Sim_support.advance sim (Span.s 1) ;
          equal int ~msg:"errors after the first second" 1 (count ()) ;
          Weft_sim.set_fd_limit sim None ;
          let c, r = connect server in
          Flow.write_string c (get ~conn:"close" "/") ;
          Fiber.scope (fun s ->
              let response = Sim_support.call s (fun () -> read_response r) in
              (* The next accept after the pause gets the connection. *)
              Sim_support.advance sim (Span.ms 100) ;
              equal string {|200 "hello" close|} (Sim_support.await response) ) ;
          Flow.close c ;
          (* A new series: one fd for the client socket. *)
          Weft_sim.set_fd_limit sim (Some (Weft_sim.open_fds sim + 1)) ;
          let c2 = Tcp.connect (Server.local_addr server) in
          Sim_support.advance sim (Span.s 1) ;
          equal int ~msg:"errors after the second series" 2 (count ()) ;
          Weft_sim.set_fd_limit sim None ;
          Flow.close c2 ) ]

(* The HTTP/1.1 deadlines over the server, in manual mode, with the spans of
   [timed_config]. These tests also examine the server: the slot of a
   connection, the stop and the Date field. test_h1_server has the head and read
   deadlines. *)
let h1_deadline_tests =
  let with_server ?(config = timed_config) ?handler f =
    with_server ~config ~engine:h1 ?handler f
  in
  group "H1 deadlines over the server"
    [ test "a kept-alive connection closes at the idle timeout" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_server
          @@ fun server ->
          let t0 = Time.now Mono in
          let c, r = connect server in
          Flow.write_string c (get "/") ;
          equal string {|200 "hello"|} (read_response r) ;
          (* The connection waits for the next request until t0 + I. *)
          let t2 = just_before (t0 +: idle_i) in
          Sim_support.advance_to sim t2 ;
          Flow.write_string c (get "/") ;
          equal string {|200 "hello"|} (read_response r) ;
          Fiber.scope
          @@ fun s ->
          let eof = Sim_support.call s (fun () -> is_eof r) in
          is_true ~msg:"the end of the stream"
            (Sim_support.ends_at sim ~t0:t2 idle_i eof) ;
          Flow.close c )
    ; test "the Date field is the world's POSIX time" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          Weft_sim.set_posix sim (Time.posix_of_unix_s 784111777.) ;
          with_server ~config:{timed_config with date_header= true}
          @@ fun server ->
          let c, r = connect server in
          let date () =
            let date = ref "none" in
            let field line =
              if String.starts_with ~prefix:"Date: " line then date := line
            in
            Flow.write_string c (get "/") ;
            equal string {|200 "hello"|} (read_response ~field r) ;
            !date
          in
          equal string "Date: Sun, 06 Nov 1994 08:49:37 GMT" (date ()) ;
          (* 1 s is before each deadline of the connection. *)
          Sim_support.advance sim (Span.s 1) ;
          equal string "Date: Sun, 06 Nov 1994 08:49:38 GMT" (date ()) ;
          Flow.close c )
    ; test
        "a response write that waits until the write timeout ends the \
         connection" (fun () ->
          let handler req =
            match Request.path_only req with
            | "/big" ->
                Response.string (String.make big 'x')
            | _ ->
                Response.string "hello"
          in
          Sim_support.manual
          @@ fun sim ->
          with_server ~config:{timed_config with max_connections= 1} ~handler
          @@ fun server ->
          let t0 = Time.now Mono in
          let c = Tcp.connect (Server.local_addr server) in
          (* The client does not read: the write of the response waits. *)
          Flow.write_string c (get "/big") ;
          (* [c2] waits for the only slot, which [c] holds until the write
             timeout. *)
          let c2, r2 = connect server in
          Flow.write_string c2 (get ~conn:"close" "/") ;
          Fiber.scope
          @@ fun s ->
          let second = Sim_support.call s (fun () -> read_response r2) in
          equal string {|200 "hello" close|}
            (Sim_support.ends_at sim ~t0 write_w second) ;
          is_true ~msg:"the connection ends before the whole body"
            (String.length (read_to_end c) < big) ;
          Flow.close c ;
          Flow.close c2 )
    ; test "a response after a shutdown in its handler closes the connection"
        (fun () ->
          let server_ref = ref None in
          let handler _ =
            Server.shutdown (Option.get !server_ref) ;
            Response.string "bye"
          in
          Sim_support.run
          @@ fun _ ->
          with_server ~handler
          @@ fun server ->
          server_ref := Some server ;
          let c, r = connect server in
          Flow.write_string c (get "/") ;
          equal string {|200 "bye" close|} (read_response r) ;
          Flow.close c )
    ; test "a connection that the server ends lingers for the linger timeout"
        (fun () ->
          let served = Ivar.create () in
          let engine ctx flow handler =
            Fun.protect
              ~finally:(fun () -> ignore (Ivar.try_fill served ()))
              (fun () -> h1 ctx flow handler)
          in
          Sim_support.manual
          @@ fun sim ->
          Server_support.with_server ~handler ~engine
            ~config:{timed_config with max_connections= 1}
          @@ fun server ->
          let t0 = Time.now Mono in
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get ~conn:"close" "/") ;
          (* The server half-closes at once: the client gets the response and
             the end of the stream, and keeps its socket open. *)
          ends_with "hello" (read_all c) ;
          (* [c2] waits for the only slot, which [c] holds while the server
             lingers on it. *)
          let c2, r2 = connect server in
          Flow.write_string c2 (get ~conn:"close" "/") ;
          Sim_support.advance_to sim (just_before (t0 +: linger_l)) ;
          (* The linger reads the bytes and waits again, to the same
             deadline. *)
          Flow.write_string c "more" ;
          Weft_sim.settle sim ;
          is_true ~msg:"the engine lingers" (Ivar.peek served = None) ;
          Sim_support.advance_to sim (t0 +: linger_l) ;
          is_true ~msg:"the engine ended" (Ivar.peek served <> None) ;
          (* The end of the linger frees the slot. *)
          equal string {|200 "hello" close|} (read_response r2) ;
          Flow.close c ;
          Flow.close c2 ) ]

(* A handler that masks the cancel. In a shield, it fills [started], then it
   fills [saw] with "stop" at the stop of the server, or with "limit" after 3 s.
   The limit ends the handler if the stop does not come. *)
let shielded_handler ~started ~saw req =
  let stopping = Conn_info.stopping (Option.get (Request.conn req)) in
  Fiber.shield (fun () ->
      Ivar.fill started () ;
      Ivar.fill saw
        (sync
           (choose
              [ wrap stopping (fun () -> "stop")
              ; wrap (Time.after Mono (Span.s 3)) (fun () -> "limit") ] ) ) ) ;
  Response.string "late"

(* The stop of the server and the connections, with Fake_engine. *)
let stop_tests =
  group "Server stop and connections"
    [ test "a stop gives the requests in progress the shutdown grace" (fun () ->
          let started_a = Ivar.create ()
          and started_b = Ivar.create ()
          and release_a = Ivar.create ()
          and release_b = Ivar.create ()
          and cancelled_a = Ivar.create () in
          let handler req =
            match Request.path_only req with
            | "/a" -> (
                Ivar.fill started_a () ;
                match sync (Ivar.read release_a) with
                | () ->
                    Response.string "a"
                | exception (Fiber.Cancelled _ as e) ->
                    Ivar.fill cancelled_a () ; raise e )
            | _ ->
                Ivar.fill started_b () ;
                sync (Ivar.read release_b) ;
                Response.string "b"
          in
          Sim_support.manual
          @@ fun sim ->
          with_server ~config:timed_config ~handler
          @@ fun server ->
          let c1, _ = connect server and c2, r2 = connect server in
          Flow.write_string c1 (get "/a") ;
          Flow.write_string c2 (get "/b") ;
          sync (Ivar.read started_a) ;
          sync (Ivar.read started_b) ;
          (* The grace starts at the stop, not at the start of the server. *)
          Sim_support.advance sim (Span.s 1) ;
          let t1 = Time.now Mono in
          Server.shutdown server ;
          Sim_support.advance_to sim (just_before (t1 +: grace_g)) ;
          Ivar.fill release_b () ;
          equal string {|200 "b" close|} (read_response r2) ;
          is_true ~msg:"a is not cancelled before the end of the grace"
            (Ivar.peek cancelled_a = None) ;
          Sim_support.advance_to sim (t1 +: grace_g) ;
          is_true ~msg:"a is cancelled at the end of the grace"
            (Ivar.peek cancelled_a <> None) ;
          Server.wait server ;
          Flow.close c1 ;
          Flow.close c2 )
    ; test "abort cancels requests in progress without a grace" (fun () ->
          let started = Ivar.create () in
          let handler _ =
            Ivar.fill started () ;
            sync (Ivar.read (Ivar.create () : unit Ivar.t)) ;
            Response.string "late"
          in
          Sim_support.manual
          @@ fun _ ->
          with_server ~config:timed_config ~handler
          @@ fun server ->
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get "/") ;
          sync (Ivar.read started) ;
          Server.abort server ;
          (* Manual mode: the time does not move. *)
          Server.wait server ;
          (* The cancelled request ends its connection with no bytes. *)
          equal string "" (read_to_end c) ;
          Flow.close c )
    ; test "each of 20 idle connections closes on shutdown" (fun () ->
          (* All connections wait on the one stop Ivar of the server (#11): the
             stop must reach each one. *)
          Sim_support.manual
          @@ fun _ ->
          with_server ~config:timed_config
          @@ fun server ->
          let clients =
            List.init 20 (fun _ ->
                let c, r = connect server in
                Flow.write_string c (get "/") ;
                equal string {|200 "hello"|} (read_response r) ;
                (c, r) )
          in
          Server.shutdown server ;
          List.iteri
            (fun i (_, r) ->
              is_true ~msg:(Printf.sprintf "the end of stream %d" i) (is_eof r) )
            clients ;
          Server.wait server ;
          List.iter (fun (c, _) -> Flow.close c) clients )
    ; test "a handler that masks the cancel sees the stop after abort"
        (fun () ->
          let started = Ivar.create () and saw = Ivar.create () in
          let handler = shielded_handler ~started ~saw in
          Sim_support.manual
          @@ fun sim ->
          with_server ~config:timed_config ~handler
          @@ fun server ->
          let c = Tcp.connect (Server.local_addr server) in
          Flow.write_string c (get "/") ;
          sync (Ivar.read started) ;
          Server.abort server ;
          Weft_sim.settle sim ;
          equal (option string) ~msg:"at the abort" (Some "stop")
            (Ivar.peek saw) ;
          Server.wait server ;
          Flow.close c )
    ; test
        "a handler that masks the cancel sees the stop when the caller of the \
         server is cancelled" (fun () ->
          let started = Ivar.create () and saw = Ivar.create () in
          let handler = shielded_handler ~started ~saw in
          Sim_support.manual
          @@ fun sim ->
          let addr = Ivar.create () in
          Fiber.scope
          @@ fun s ->
          (* No shutdown: the cancel of the caller stops the server. *)
          let caller =
            Sim_support.call ~name:"caller" s (fun () ->
                Fiber.scope (fun sw ->
                    let server =
                      Server.start ~config:timed_config ~engine:fake sw handler
                    in
                    Ivar.fill addr (Server.local_addr server) ;
                    Server.wait server ) )
          in
          let c = Tcp.connect (sync (Ivar.read addr)) in
          Flow.write_string c (get "/") ;
          sync (Ivar.read started) ;
          Sim_support.cancel caller ;
          Weft_sim.settle sim ;
          equal (option string) ~msg:"at the cancel" (Some "stop")
            (Ivar.peek saw) ;
          raises Promise.Cancelled (fun () -> Sim_support.await caller) ;
          Flow.close c )
    ; test
        "a connection that ends while the server runs does not enable stopping"
        (fun () ->
          let kept = ref None in
          let handler req =
            kept := Request.conn req ;
            Response.string "one"
          in
          Sim_support.run
          @@ fun sim ->
          with_server ~handler
          @@ fun server ->
          let c, r = connect server in
          Flow.write_string c (get ~conn:"close" "/") ;
          equal string {|200 "one" close|} (read_response r) ;
          Flow.close c ;
          Weft_sim.settle sim ;
          equal int ~msg:"open connections" 0 (Server.open_connections server) ;
          let stopping = Conn_info.stopping (Option.get !kept) in
          Fiber.scope (fun s ->
              let probe = Sim_support.call s (fun () -> sync stopping) in
              is_false ~msg:"stopping is ready" (Sim_support.ended sim probe) ;
              Sim_support.cancel probe ;
              raises Promise.Cancelled (fun () -> Sim_support.await probe) ) )
    ; test "an idle connection closes when the listener fails" (fun () ->
          Sim_support.manual
          @@ fun _ ->
          let l = Tcp.Listener.listen loopback in
          Fiber.scope
          @@ fun sw ->
          let server =
            Server.start_with ~config:timed_config ~engine:fake sw
              (module Tcp.Listener)
              l handler
          in
          let c, r = connect server in
          Flow.write_string c (get "/") ;
          equal string {|200 "hello"|} (read_response r) ;
          (* No shutdown: the stop is empty when the acceptor gets EBADF. *)
          Tcp.Listener.close l ;
          is_true ~msg:"the end of the stream" (is_eof r) ;
          raises_match is_bad_fd (fun () -> Server.wait server) ;
          Flow.close c )
    ; test "the server forgets each connection that ends" (fun () ->
          Sim_support.run
          @@ fun sim ->
          with_server
          @@ fun server ->
          for _ = 1 to 50 do
            let c, r = connect server in
            Flow.write_string c (get ~conn:"close" "/") ;
            equal string {|200 "hello" close|} (read_response r) ;
            Flow.close c
          done ;
          Weft_sim.settle sim ;
          equal int ~msg:"open connections" 0 (Server.open_connections server) )
    ]

let () =
  exit
    (Windtrap.run "net"
       [ server_tests
       ; race_tests
       ; accept_loop_tests
       ; early_stop_tests
       ; late_accept_tests
       ; emfile_tests
       ; h1_deadline_tests
       ; stop_tests ] )
