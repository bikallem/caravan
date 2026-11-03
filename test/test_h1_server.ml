(* The HTTP/1.1 engine over in-memory flows in the simulator of weft: whole
   connections, golden bytes. Each test runs 1 seed on 1 domain, except a test
   whose name starts with "race:". *)

open Windtrap
open Weft
open Caravan

let show_crlf s =
  let b = Buffer.create (String.length s) and n = String.length s in
  let rec loop i =
    if i < n then
      if s.[i] = '\r' && i + 1 < n && s.[i + 1] = '\n' then (
        Buffer.add_string b "\\r\\n\n" ;
        loop (i + 2) )
      else (
        Buffer.add_char b s.[i] ;
        loop (i + 1) )
  in
  loop 0 ; Buffer.contents b

(* [conn ~scheme ~stopping ()] is the connection of each test. [scheme] is
   [`Http] and [stopping] is [Weft.never] by default. *)
let conn ?scheme ?stopping () =
  Conn_info.make ~id:7
    ~peer:(Net.Tcp ("127.0.0.1", 50000))
    ~local:(Net.Tcp ("127.0.0.1", 8080))
    ?scheme ?stopping ()

(* [ctx ~scheme ~stopping ~is_stopping config] is the context of an engine. By
   default the server does not stop. *)
let ctx ?scheme ?stopping ?(is_stopping = fun () -> false) config =
  {Engine.config; conn= conn ?scheme ?stopping (); is_stopping}

(* After an upgrade: what the client sends, until it ends. *)
let echo_upgraded flow =
  let buf = Bytes.create 64 and got = Buffer.create 64 in
  let rec loop () =
    match Flow.read flow buf 0 64 with
    | 0 ->
        ()
    | n ->
        Buffer.add_subbytes got buf 0 n ;
        loop ()
  in
  loop () ;
  Flow.write_string flow ("got " ^ Buffer.contents got)

let upgrade_echo = Headers.of_list [("Upgrade", "echo")]

(* How the last slow handler ended: finished or cancelled. *)
let slow_outcome = ref ""

(* Filled when a slow handler starts to wait. *)
let holding = ref (Ivar.create ())

(* Answers "late" after [span] of virtual time. Before the wait, it reads the
   request body if [read] is [true]. *)
let slowly ~read ~span req =
  match
    if read then ignore (Body.to_string (Request.body req)) ;
    ignore (Ivar.try_fill !holding ()) ;
    Time.sleep Mono span
  with
  | () ->
      slow_outcome := "[handler finished]" ;
      Response.string "late"
  | exception (Fiber.Cancelled _ as e) ->
      slow_outcome := "[handler cancelled]" ;
      raise e

(* /slow waits 50 ms: a test that needs the handler to finish uses it. *)
let slow = Span.ms 50

(* /hold waits 1 s: a test that needs a cancel uses it. The cancel comes at
   once. A cancel that does not come gives "[handler finished]" after 1 s,
   before the guard of 2 s. *)
let hold = Span.s 1

(* Filled by /endless after it flushed its first bytes. *)
let flushed = ref (Ivar.create ())

(* The writer that /keep-writer keeps after its producer returns. *)
let kept_writer = ref None

(* Set by /stop (a server that stops while its response is written), and by
   /ws-stopping, /ws-bare-stopping and the CONNECT to stopping.example (a server
   that stops while the handler runs). *)
let stopping = ref false

let handler req =
  if Method.equal (Request.meth req) Method.CONNECT then
    match Request.authority req with
    | Some "plain.example:443" ->
        Response.string "not a tunnel"
    | Some "denied.example:443" ->
        Response.string ~status:Status.forbidden "denied"
    | Some "stopping.example:443" ->
        stopping := true ;
        Response.upgrade ~status:Status.ok echo_upgraded
    | _ ->
        Response.upgrade ~status:Status.ok echo_upgraded
  else
    match Request.path_only req with
    | "/swallow" -> (
      (* A handler that answers a body it cannot read itself. *)
      match Body.to_string (Request.body req) with
      | s ->
          Response.string s
      | exception _ ->
          Response.string ~status:Status.bad_request "unreadable body\n" )
    | "/early-hints" ->
        Response.make ~status:(Status.v 103) Body.empty
    | "/stream-fail" ->
        Response.stream (fun _ -> failwith "producer")
    | "/keep-writer" ->
        Response.stream (fun w ->
            kept_writer := Some w ;
            Body.Writer.write_string w "kept" )
    | "/late-write" -> (
      match
        Option.iter (fun w -> Body.Writer.write_string w "LATE") !kept_writer
      with
      | () ->
          Response.string "late write went through"
      | exception Invalid_argument _ ->
          Response.string "late write refused" )
    | "/reset" ->
        Response.stream ~status:Status.reset_content (fun _ ->
            failwith "the producer of a 205 ran" )
    (* Failures of the handler's own upstream, not of the request. *)
    | "/upstream-truncated" ->
        raise Body.Truncated
    | "/upstream-malformed" ->
        raise (Private.H1_error.E (Bad_request "upstream"))
    | "/upstream-body-truncated" ->
        let sent = ref false in
        Response.make
          (Body.of_source ~length:10 (fun buf off _ ->
               if !sent then raise Body.Truncated ;
               sent := true ;
               Bytes.blit_string "abc" 0 buf off 3 ;
               3 ) )
    | "/endless" ->
        Response.stream (fun w ->
            Body.Writer.write_string w "first" ;
            Body.Writer.flush w ;
            ignore (Ivar.try_fill !flushed ()) ;
            Time.sleep Mono (Span.s 10) )
    | "/partial" ->
        Response.stream (fun w ->
            Body.Writer.write_string w "partial" ;
            Body.Writer.flush w ;
            failwith "cut short" )
    | "/slow" ->
        slowly ~read:false ~span:slow req
    | "/hold" ->
        slowly ~read:false ~span:hold req
    | "/read-hold" ->
        slowly ~read:true ~span:hold req
    | "/hold-cleanup-fails" -> (
      try slowly ~read:false ~span:hold req
      with Fiber.Cancelled _ -> failwith "cleanup failed" )
    | "/ws" ->
        Response.upgrade ~headers:upgrade_echo echo_upgraded
    | "/ws-bare" ->
        Response.upgrade echo_upgraded
    | "/ws-as" ->
        (* A 101 to the protocols of the query. *)
        let protocols = Option.value (Request.query req) ~default:"" in
        Response.upgrade
          ~headers:(Headers.of_list [("Upgrade", protocols)])
          echo_upgraded
    | "/ws-stopping" ->
        (* The server starts to stop while the handler runs. *)
        stopping := true ;
        Response.upgrade ~headers:upgrade_echo echo_upgraded
    | "/ws-bare-stopping" ->
        stopping := true ;
        Response.upgrade echo_upgraded
    | "/ws-read" ->
        ignore (Body.to_string (Request.body req)) ;
        Response.upgrade ~headers:upgrade_echo echo_upgraded
    | "/ws-fail" ->
        Response.upgrade ~headers:upgrade_echo (fun _ -> failwith "ws")
    | "/tunnel" ->
        Response.upgrade ~status:Status.ok echo_upgraded
    | "/stop" ->
        Response.stream (fun w ->
            Body.Writer.write_string w "bye" ;
            (* After the head went out as keep-alive. *)
            Body.Writer.flush w ;
            stopping := true )
    | "/echo" ->
        Response.string (Body.to_string (Request.body req))
    | "/stream-echo" ->
        Response.stream (Body.write_to (Request.body req))
    | "/ignore" ->
        Response.string "ignored"
    | "/stream" ->
        Response.stream (fun w ->
            Body.Writer.write_string w "a" ;
            Body.Writer.flush w ;
            Body.Writer.write_string w "b" )
    | "/fail" ->
        failwith "boom"
    | "/cancelled-child" ->
        (* The handler cancels a child and awaits it. *)
        Fiber.scope (fun s ->
            let child =
              Fiber.fork_promise s (fun () ->
                  Time.sleep Mono (Span.s 10) ;
                  Response.string "child" )
            in
            Promise.cancel child ; Promise.await child )
    | "/cancelled-scope" ->
        (* The handler cancels a scope of its own. Its Cancelled is not a cancel
           of the connection. *)
        Fiber.scope (fun s ->
            Fiber.cancel s (Failure "inner") ;
            Fiber.yield () ) ;
        Response.string "after the scope"
    | "/stream-cancelled-scope" ->
        (* The producer cancels a scope of its own before its first output. *)
        Response.stream (fun _ ->
            Fiber.scope (fun s ->
                Fiber.cancel s (Failure "inner") ;
                Fiber.yield () ) )
    | "/ws-cancelled-scope" ->
        Response.upgrade ~headers:upgrade_echo (fun _ ->
            Fiber.scope (fun s ->
                Fiber.cancel s (Failure "inner") ;
                Fiber.yield () ) )
    | "/ended-scope" ->
        (* The handler forks into a scope that ended. *)
        let ended = Fiber.scope Fun.id in
        Fiber.fork ended ignore ; Response.string "forked"
    | "/short" ->
        Response.stream ~length:5 (fun w -> Body.Writer.write_string w "abc")
    | "/close" ->
        Response.string
          ~headers:(Headers.of_list [("Connection", "close")])
          "bye"
    | "/empty" ->
        Response.empty ()
    | "/info" ->
        Response.string
          (Format.asprintf "%a %s %s %s %a %s" Method.pp (Request.meth req)
             (Request.scheme req)
             (Option.value ~default:"-" (Request.authority req))
             (Request.path req) Version.pp (Request.version req)
             ( match Request.conn req with
             | Some c ->
                 Format.asprintf "%a" Conn_info.pp c
             | None ->
                 "-" ) )
    | _ ->
        Response.string "hello"

(* [engine ~handler s ctx flow] runs the engine with [handler] on [flow] in a
   call of the scope [s]. *)
let engine ?(handler = handler) s ctx flow =
  Sim_support.call ~name:"engine" s (fun () ->
      Private.H1_server.serve ctx flow handler )

(* [span_sum a b] is the span [a] and then [b]. *)
let span_sum a b = Span.of_ns (Int64.add (Span.to_ns a) (Span.to_ns b))

(* An in-memory connection: the flow of the engine, and [output ()], the bytes
   that the engine wrote. The bytes have [[FIN]] where the engine half-closed,
   [[RST]] where it reset, and [[unread n]] if it left input unread. *)
type memory = {flow: Flow.t; output: unit -> string}

(* [memory input] is a connection whose client sends [input]. At the end of
   [input] the client closes. With [~stall:true] it stops: the flow is not
   readable any more, and reads raise [Flow.Timeout]. With [~reset:true] it
   resets the connection. With [~unwritable:true] each write raises as on a
   reset connection. With [~false_wakes:n] the flow is first readable [n] times
   with nothing to read, and a read that waits then fails. With [~ready:iv] the
   flow is readable and has input only after a fill of [iv]. *)
let memory ?chunk ?(stall = false) ?(reset = false) ?(unwritable = false)
    ?(false_wakes = 0) ?ready input =
  let out = Buffer.create 256 and consumed = ref 0 in
  let inner = Flow.of_string ?chunk input out in
  let stalled () = stall && !consumed = String.length input in
  let before_input () =
    match ready with Some iv -> Option.is_none (Ivar.peek iv) | None -> false
  in
  let false_wakes = ref false_wakes in
  let read buf off len =
    if !false_wakes > 0 then failwith "blocking read after a false wakeup" ;
    if before_input () then failwith "blocking read before the input" ;
    if stalled () then raise Flow.Timeout ;
    if reset && !consumed = String.length input then raise Flow.Closed ;
    let n = Flow.read inner buf off len in
    consumed := !consumed + n ;
    n
  in
  let read_now buf off len =
    if before_input () then None
    else if !false_wakes > 0 then (decr false_wakes ; None)
    else if stalled () then None
    else Some (read buf off len)
  in
  let readable =
    match ready with
    | Some iv ->
        Ivar.read iv
    | None ->
        guard (fun () ->
            if !false_wakes = 0 && stalled () then never else always () )
  in
  let flow =
    let write buf off len =
      if unwritable then raise Flow.Closed else Flow.write inner buf off len
    in
    Flow.make ~read ~read_now ~write ~readable
      ~shutdown_send:(fun () -> Buffer.add_string out "[FIN]")
      ~close:ignore
      ~reset:(fun () -> Buffer.add_string out "[RST]")
      ~set_read_deadline:ignore ~set_write_deadline:ignore ()
  in
  let output () =
    let unread = String.length input - !consumed in
    Buffer.contents out
    ^ if unread > 0 then Printf.sprintf "[unread %d]" unread else ""
  in
  {flow; output}

(* A fake client socket. As a weft socket, it stays readable after its input
   until a read with no wait finds nothing. The client sends [parts]: the first
   part at once, and the next part at each write of the engine. The empty part
   is the end of the stream. With [~error:e], the first read with no wait that
   finds no input raises [e], and then the stream ends. A read that waits fails.
   [empty_reads ()] is the number of reads with no wait that found nothing.
   [end_stream ()] ends the stream at once. *)
type fake_socket =
  { socket: Flow.t
  ; written: unit -> string
  ; empty_reads: unit -> int
  ; end_stream: unit -> unit }

let fake_socket ?error parts =
  let input = Buffer.create 64 and pos = ref 0 in
  let eof = ref false and stale = ref false and empty = ref 0 in
  let error = ref error and wake = ref (Ivar.create ()) in
  let send part =
    if part = "" then eof := true else Buffer.add_string input part
  in
  let later =
    ref (match parts with [] -> [] | first :: rest -> send first ; rest)
  in
  let wake_reader () =
    Ivar.fill !wake () ;
    wake := Ivar.create ()
  in
  let send_next () =
    match !later with
    | [] ->
        ()
    | part :: rest ->
        later := rest ;
        send part ;
        wake_reader ()
  in
  let available () = Buffer.length input - !pos in
  let take buf off len =
    let n = Int.min len (available ()) in
    Buffer.blit input !pos buf off n ;
    pos := !pos + n ;
    stale := true ;
    n
  in
  let read_now buf off len =
    if available () > 0 then Some (take buf off len)
    else
      match !error with
      | Some e ->
          error := None ;
          eof := true ;
          raise e
      | None ->
          if !eof then Some 0
          else (
            incr empty ;
            stale := false ;
            None )
  in
  let read buf off len =
    if available () > 0 then take buf off len
    else if !eof then 0
    else failwith "a read that waits"
  in
  let readable =
    guard (fun () ->
        if available () > 0 || !eof || !stale || Option.is_some !error then
          always ()
        else Ivar.read !wake )
  in
  let out = Buffer.create 256 in
  let write buf off len =
    Buffer.add_subbytes out buf off len ;
    send_next ()
  in
  let socket =
    Flow.make ~read ~read_now ~write ~readable
      ~shutdown_send:(fun () -> Buffer.add_string out "[FIN]")
      ~close:ignore ~set_read_deadline:ignore ~set_write_deadline:ignore ()
  in
  { socket
  ; written= (fun () -> Buffer.contents out)
  ; empty_reads= (fun () -> !empty)
  ; end_stream= (fun () -> send "" ; wake_reader ()) }

(* [config_for config errors ~watch] is [config] with an [on_error] that adds
   each error to [errors]. *)
let config_for config errors ~watch =
  { config with
    Config.on_error= (fun e _ -> errors := Printexc.to_string e :: !errors)
  ; cancel_on_disconnect= watch }

(* Serves [input] on one connection of {!memory}. The result is the bytes that
   the engine wrote, and the errors that it reported.

   The engine runs in [Sim_support.run], thus in auto mode. With
   [~ends_after:d], it runs in manual mode, and it must end at [d] after the
   start ([Sim_support.ends_at]). With [~cancel_on:ready], the test cancels the
   engine when the event [ready ()] is ready, as a stop of the server does.
   [handler] is {!handler} by default. *)
let serve ?(config = {Config.default with date_header= false})
    ?(handler = handler) ?chunk ?ends_after ?cancel_on ?stall ?reset ?unwritable
    ?(watch = false) ?false_wakes input =
  let errors = ref [] in
  (* The end of the input looks like a client that leaves: the engine watches
     for it only if the test asks. *)
  let config = config_for config errors ~watch in
  slow_outcome := "" ;
  let m = memory ?chunk ?stall ?reset ?unwritable ?false_wakes input in
  stopping := false ;
  let ctx = ctx ~is_stopping:(fun () -> !stopping) config in
  ( match (ends_after, cancel_on) with
  | Some d, _ ->
      Sim_support.manual (fun sim ->
          let t0 = Time.now Mono in
          Fiber.scope (fun s ->
              Sim_support.ends_at sim ~t0 d (engine ~handler s ctx m.flow) ) )
  | None, None ->
      Sim_support.run (fun _ -> Private.H1_server.serve ctx m.flow handler)
  | None, Some ready ->
      Sim_support.run (fun _ ->
          Fiber.scope (fun s ->
              let c = engine ~handler s ctx m.flow in
              sync (ready ()) ;
              Sim_support.cancel c ;
              match Sim_support.await c with
              | () ->
                  ()
              | exception Promise.Cancelled ->
                  () ) ) ) ;
  (m.output (), List.rev !errors)

let show ?config ?handler ?chunk ?ends_after ?cancel_on ?stall ?reset
    ?unwritable ?watch ?false_wakes input =
  let out, errors =
    serve ?config ?handler ?chunk ?ends_after ?cancel_on ?stall ?reset
      ?unwritable ?watch ?false_wakes input
  in
  show_crlf out
  ^ String.concat "" (List.map (fun e -> "\n! " ^ e) errors)
  ^ if !slow_outcome = "" then "" else "\n" ^ !slow_outcome

let get ?(v = "HTTP/1.1") ?(headers = []) path =
  Printf.sprintf "GET %s %s\r\nHost: a.example\r\n%s\r\n" path v
    (String.concat "" (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") headers))

let post ?(headers = []) path body =
  Printf.sprintf
    "POST %s HTTP/1.1\r\nHost: a.example\r\nContent-Length: %d\r\n%s\r\n%s" path
    (String.length body)
    (String.concat "" (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") headers))
    body

let connection_tests =
  group "H1_server connections"
    [ test "a GET" (fun () ->
          expect (show (get "/"))
          @@ __POS_OF__
               {|
      HTTP/1.1 200 OK\r\n
      Content-Type: text/plain; charset=utf-8\r\n
      Content-Length: 5\r\n
      \r\n
      hello
      |} )
    ; test "a 205 has Content-Length: 0 and never produces its body" (fun () ->
          (* RFC 9110 15.3.6. The connection stays usable after it. *)
          expect
            (String.concat "\n---\n"
               [ show (get "/reset" ^ get "/")
               ; show
                   ("HEAD /reset HTTP/1.1\r\nHost: a.example\r\n\r\n" ^ get "/")
               ; show (get ~v:"HTTP/1.0" "/reset")
               ; show
                   ( get ~v:"HTTP/1.0"
                       ~headers:[("Connection", "keep-alive")]
                       "/reset"
                   ^ get "/" ) ] )
          @@ __POS_OF__
               {|
            HTTP/1.1 205 Reset Content\r\n
            Content-Length: 0\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            ---
            HTTP/1.1 205 Reset Content\r\n
            Content-Length: 0\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            ---
            HTTP/1.1 205 Reset Content\r\n
            Content-Length: 0\r\n
            Connection: close\r\n
            \r\n
            [FIN]
            ---
            HTTP/1.1 205 Reset Content\r\n
            Content-Length: 0\r\n
            Connection: keep-alive\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |} )
    ; test
        "a producer that keeps its writer cannot write into the next response"
        (fun () ->
          kept_writer := None ;
          expect (show (get "/keep-writer" ^ get "/late-write"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            4\r\n
            kept\r\n
            0\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 18\r\n
            \r\n
            late write refused
            |} )
    ; test "pipelined requests are answered in order" (fun () ->
          expect (show ~chunk:7 (get "/" ^ get "/info" ^ get "/ignore"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            helloHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 70\r\n
            \r\n
            GET http a.example /info HTTP/1.1 #7 127.0.0.1:50000 -> 127.0.0.1:8080HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored
            |} )
    ; test "what the handler sees" (fun () ->
          expect (show (get "/info?q=1"))
          @@ __POS_OF__
               {|
        HTTP/1.1 200 OK\r\n
        Content-Type: text/plain; charset=utf-8\r\n
        Content-Length: 74\r\n
        \r\n
        GET http a.example /info?q=1 HTTP/1.1 #7 127.0.0.1:50000 -> 127.0.0.1:8080
        |} )
    ; test "attrs never go on the wire" (fun () ->
          let k : string Attrs.key = Attrs.key ~name:"k" () in
          let seen = ref None in
          let handler ~attr req =
            seen := Some (Attrs.is_empty (Request.attrs req)) ;
            let r = Response.string "hello" in
            if attr then Response.with_attr k "v" r else r
          in
          let with_attr, _ = serve ~handler:(handler ~attr:true) (get "/") in
          is_true ~msg:"request attrs are empty" (!seen = Some true) ;
          let without, _ = serve ~handler:(handler ~attr:false) (get "/") in
          equal string without with_attr )
    ; test "a connection with TLS gives the https scheme" (fun () ->
          let m = memory (get "/info") in
          let ctx =
            ctx ~scheme:`Https {Config.default with date_header= false}
          in
          Sim_support.run (fun _ -> Private.H1_server.serve ctx m.flow handler) ;
          expect (show_crlf (m.output ()))
          @@ __POS_OF__
               {|
        HTTP/1.1 200 OK\r\n
        Content-Type: text/plain; charset=utf-8\r\n
        Content-Length: 71\r\n
        \r\n
        GET https a.example /info HTTP/1.1 #7 127.0.0.1:50000 -> 127.0.0.1:8080
        |} )
    ; test "HTTP/1.0 closes unless kept alive" (fun () ->
          expect
            ( show (get ~v:"HTTP/1.0" "/" ^ get "/")
            ^ "\n--\n"
            ^ show
                ( get ~v:"HTTP/1.0" ~headers:[("Connection", "keep-alive")] "/"
                ^ get "/ignore" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            Connection: close\r\n
            \r\n
            hello[FIN]
            --
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            Connection: keep-alive\r\n
            \r\n
            helloHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored
            |} )
    ; test
        "Connection: close from the client or the handler ends the connection"
        (fun () ->
          expect
            ( show (get ~headers:[("Connection", "close")] "/" ^ get "/ignore")
            ^ "\n--\n"
            ^ show (get "/close" ^ get "/ignore") )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            Connection: close\r\n
            \r\n
            hello[FIN]
            --
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 3\r\n
            Connection: close\r\n
            \r\n
            bye[FIN]
            |} )
    ; test "request bodies: read, unread and drained, chunked" (fun () ->
          expect
            (show
               ( post "/echo" "hello"
               ^ post "/ignore" "unread body"
               ^ get "/"
               ^ "POST /echo HTTP/1.1\r\n\
                  Host: a\r\n\
                  Transfer-Encoding: chunked\r\n\
                  \r\n\
                  3\r\n\
                  abc\r\n\
                  2\r\n\
                  de\r\n\
                  0\r\n\
                  \r\n" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            helloHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignoredHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            helloHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            abcde
            |} )
    ; test "streamed responses: chunked for 1.1, close-delimited for 1.0"
        (fun () ->
          expect
            (show
               ( get "/stream"
               ^ get ~v:"HTTP/1.0"
                   ~headers:[("Connection", "keep-alive")]
                   "/stream"
               ^ get "/" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            1\r\n
            a\r\n
            1\r\n
            b\r\n
            0\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Connection: close\r\n
            \r\n
            ab[FIN]
            |} )
    ; test "HEAD" (fun () ->
          expect (show ("HEAD / HTTP/1.1\r\nHost: a\r\n\r\n" ^ get "/ignore"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored
            |} )
    ; test "a last response without a body is sent" (fun () ->
          expect
            ( show "HEAD / HTTP/1.1\r\nHost: a\r\nConnection: close\r\n\r\n"
            ^ "--\n"
            ^ show (get ~headers:[("Connection", "close")] "/empty") )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            Connection: close\r\n
            \r\n
            [FIN]--
            HTTP/1.1 204 No Content\r\n
            Connection: close\r\n
            \r\n
            [FIN]
            |} )
    ; test "malformed requests get 400 and close" (fun () ->
          expect (show ("GET / HTTP/1.1\r\n\r\n" ^ get "/"))
          @@ __POS_OF__
               {|
            HTTP/1.1 400 Bad Request\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 12\r\n
            Connection: close\r\n
            \r\n
            Bad Request
            [FIN]
            |} )
    ; test "a body over max_body gets 413" (fun () ->
          let config =
            {Config.default with date_header= false; max_body= Some 3}
          in
          expect (show ~config (post "/echo" "hello"))
          @@ __POS_OF__
               {|
            HTTP/1.1 413 Content Too Large\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 18\r\n
            Connection: close\r\n
            \r\n
            Content Too Large
            [FIN]
            |} )
    ; test "100-continue is sent when the handler reads the body" (fun () ->
          expect
            (show
               ( post ~headers:[("Expect", "100-continue")] "/echo" "hello"
               ^ get "/ignore" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            helloHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored
            |} )
    ; test
        "an expectation other than 100-continue gets 417, without the handler"
        (fun () ->
          expect
            (String.concat "--"
               (List.map
                  (fun e -> show (get ~headers:[("Expect", e)] "/fail"))
                  ["foo"; "100-continue, foo"; "100-continue=1"] ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 417 Expectation Failed\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 19\r\n
            Connection: close\r\n
            \r\n
            Expectation Failed
            [FIN]--HTTP/1.1 417 Expectation Failed\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 19\r\n
            Connection: close\r\n
            \r\n
            Expectation Failed
            [FIN]--HTTP/1.1 417 Expectation Failed\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 19\r\n
            Connection: close\r\n
            \r\n
            Expectation Failed
            [FIN]
            |} )
    ; test "error responses to HEAD have no body" (fun () ->
          expect
            (String.concat "--"
               [ show "HEAD / HTTP/1.1\r\nHost: a\r\nExpect: foo\r\n\r\n"
               ; show
                   "HEAD / HTTP/1.1\r\n\
                    Host: a\r\n\
                    Transfer-Encoding: gzip, chunked\r\n\
                    \r\n" ] )
          @@ __POS_OF__
               {|
            HTTP/1.1 417 Expectation Failed\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 19\r\n
            Connection: close\r\n
            \r\n
            [FIN]--HTTP/1.1 501 Not Implemented\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 16\r\n
            Connection: close\r\n
            \r\n
            [FIN]
            |} )
    ; test "100-continue is case-insensitive" (fun () ->
          expect
            (show (post ~headers:[("Expect", "100-Continue")] "/echo" "hi"))
          @@ __POS_OF__
               {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 2\r\n
            \r\n
            hi
            |} )
    ; test "100-continue from HTTP/1.0 is ignored" (fun () ->
          expect
            (show (get ~v:"HTTP/1.0" ~headers:[("Expect", "100-continue")] "/"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            Connection: close\r\n
            \r\n
            hello[FIN]
            |} )
    ; test "an unread chunked body over max_drain closes after the response"
        (fun () ->
          let config =
            {Config.default with date_header= false; max_drain= 10}
          in
          let body =
            Printf.sprintf "64\r\n%s\r\n0\r\n\r\n" (String.make 100 'x')
          in
          expect
            (show ~config
               ( "POST /ignore HTTP/1.1\r\n\
                  Host: a\r\n\
                  Transfer-Encoding: chunked\r\n\
                  \r\n" ^ body ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored[FIN]
            |} )
    ; test
        "100-continue is sent before the head of a response that streams the \
         body" (fun () ->
          expect
            (show
               ( post
                   ~headers:[("Expect", "100-continue")]
                   "/stream-echo" "hello"
               ^ get "/" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            5\r\n
            hello\r\n
            0\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |} )
    ; test "a close-delimited body cut short resets the connection" (fun () ->
          expect (show (get ~v:"HTTP/1.0" "/partial"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Connection: close\r\n
            \r\n
            partial[RST]
            ! Failure("cut short")
            |} )
    ; test
        "a close-delimited echo of a request body cut short resets the \
         connection" (fun () ->
          expect
            (show "POST /stream-echo HTTP/1.0\r\nContent-Length: 10\r\n\r\nhel")
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Connection: close\r\n
            \r\n
            hel[RST]
            |} )
    ; test
        "a close-delimited body cut short by a failed write resets the \
         connection" (fun () ->
          expect (show ~unwritable:true (get ~v:"HTTP/1.0" "/stream"))
          @@ __POS_OF__ {| [RST] |} )
    ; test
        "a close-delimited body cut short by cancellation resets the connection"
        (fun () ->
          flushed := Ivar.create () ;
          expect
            (show
               ~cancel_on:(fun () -> Ivar.read !flushed)
               (get ~v:"HTTP/1.0" "/endless") )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Connection: close\r\n
            \r\n
            first[RST]
            |} )
    ; test "a chunked body cut short ends without a last chunk" (fun () ->
          expect (show (get "/partial"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            7\r\n
            partial\r\n
            [FIN]
            ! Failure("cut short")
            |} )
    ; test "the handler's own body errors are server errors, not the client's"
        (fun () ->
          expect
            (String.concat "--"
               (List.map
                  (fun p -> show (get p))
                  [ "/upstream-truncated"
                  ; "/upstream-malformed"
                  ; "/upstream-body-truncated" ] ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Caravan__Body.Truncated--HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! H1_error.E (400 Bad Request (upstream))--HTTP/1.1 200 OK\r\n
            Content-Length: 10\r\n
            \r\n
            abc[FIN]
            ! Caravan__Body.Truncated
            |} )
    ; test "a streamed body that fails before any output gets 500" (fun () ->
          expect (show (get "/stream-fail" ^ get "/"))
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("producer")
            |} )
    ; test "100-continue owed and never sent closes the connection" (fun () ->
          expect
            (show
               ( post ~headers:[("Expect", "100-continue")] "/ignore" "hello"
               ^ get "/" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            Connection: close\r\n
            \r\n
            ignored[FIN]
            |} )
    ; test "100-continue with an empty body keeps the connection" (fun () ->
          expect
            (show
               ( post ~headers:[("Expect", "100-continue")] "/ignore" ""
               ^ get "/" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignoredHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |} )
    ; test "a server that starts stopping takes no further request" (fun () ->
          expect (show (get "/stop" ^ get "/"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            3\r\n
            bye\r\n
            0\r\n
            \r\n
            [FIN]
            |} )
    ; test "a stopping server ends an idle connection without reading"
        (fun () ->
          let flow =
            Flow.make
              ~read:(fun _ _ _ -> failwith "read while stopping")
              ~read_now:(fun _ _ _ -> failwith "read while stopping")
              ~write:(fun _ _ _ -> ())
              ~readable:never ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          let ctx =
            ctx ~stopping:(always ())
              ~is_stopping:(fun () -> true)
              Config.default
          in
          Sim_support.run (fun _ -> Private.H1_server.serve ctx flow handler) )
    ; test "an error response to a client that left is dropped" (fun () ->
          let input =
            Flow.of_string "GET / HTTP/1.1\r\n\r\n" (Buffer.create 0)
          in
          let flow =
            Flow.make ~read:(Flow.read input) ~read_now:(Flow.read_now input)
              ~write:(fun _ _ _ -> raise Flow.Closed)
              ~readable:(Flow.readable input) ~shutdown_send:ignore
              ~close:ignore ~set_read_deadline:ignore ~set_write_deadline:ignore
              ()
          in
          Sim_support.run (fun _ ->
              Private.H1_server.serve (ctx Config.default) flow handler ) )
    ; test "a failing handler gets 500 and is reported" (fun () ->
          expect (show (get "/fail" ^ get "/"))
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("boom")
            |} )
    ; test "a body shorter than its length closes the connection" (fun () ->
          expect (show (get "/short" ^ get "/"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Length: 5\r\n
            \r\n
            abc[FIN]
            ! response body length differs from its Content-Length
            |} )
    ; test
        "a closing response is followed by a half-close, and input is read to \
         its end" (fun () ->
          expect
            (show
               ( post
                   ~headers:[("Connection", "close")]
                   "/ignore" (String.make 100 'x')
               ^ get "/" ) )
          @@ __POS_OF__
               {|
             HTTP/1.1 200 OK\r\n
             Content-Type: text/plain; charset=utf-8\r\n
             Content-Length: 7\r\n
             Connection: close\r\n
             \r\n
             ignored[FIN]
             |} )
    ; test "lingering reads at most max_drain bytes" (fun () ->
          let config =
            {Config.default with date_header= false; max_drain= 10}
          in
          expect (show ~config ~chunk:16 (post "/ignore" (String.make 100 'x')))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored[FIN][unread 89]
            |} )
    ; test "a client that leaves is not lingered on" (fun () ->
          expect (show (get "/" ^ "GET / HT"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |} )
    ; test "a streamed request body cut short ends the response unreported"
        (fun () ->
          expect
            (show
               "POST /stream-echo HTTP/1.1\r\n\
                Host: a\r\n\
                Content-Length: 10\r\n\
                \r\n\
                hel" )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            3\r\n
            hel\r\n
            [FIN]
            |} )
    ; test "responses get a Date field" (fun () ->
          let out, _ = serve ~config:Config.default (get "/") in
          let date =
            List.find
              (String.starts_with ~prefix:"Date: ")
              (String.split_on_char '\n' out)
          in
          (* An IMF-fixdate, such as "Sun, 06 Nov 1994 08:49:37 GMT", then
             CR. *)
          equal int (6 + 29 + 1) (String.length date) ;
          is_true ~msg:date (String.ends_with ~suffix:" GMT\r" date) )
    ; test "error responses get a Date field too" (fun () ->
          (* RFC 9110 §6.6.1: a 4xx must have one when the server has a
             clock. *)
          let out, _ = serve ~config:Config.default "GET / HTTP/1.1\r\n\r\n" in
          is_true ~msg:out (String.starts_with ~prefix:"HTTP/1.1 400" out) ;
          let date =
            List.find
              (String.starts_with ~prefix:"Date: ")
              (String.split_on_char '\n' out)
          in
          equal int (6 + 29 + 1) (String.length date) )
    ; test "a connection reset during the request body is not a handler error"
        (fun () ->
          expect
            (show ~reset:true
               "POST /echo HTTP/1.1\r\nHost: a\r\nContent-Length: 10\r\n\r\nhel" )
          @@ __POS_OF__ {||} )
    ; test "a client that cannot take its 100 Continue is not a handler error"
        (fun () ->
          expect
            (show ~unwritable:true
               (post ~headers:[("Expect", "100-continue")] "/echo" "hello") )
          @@ __POS_OF__ {||} )
    ; test "an https target over plain HTTP gets 421" (fun () ->
          expect
            (show
               "GET https://a.example/info HTTP/1.1\r\nHost: a.example\r\n\r\n" )
          @@ __POS_OF__
               {|
            HTTP/1.1 421 Misdirected Request\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 20\r\n
            Connection: close\r\n
            \r\n
            Misdirected Request
            [FIN]
            |} )
    ; test "a request cut short gets no response" (fun () ->
          expect
            ( show "GET / HTTP/1.1\r\nHost: a"
            ^ "|"
            ^ show
                "POST /echo HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\nhel"
            )
          @@ __POS_OF__ {| | |} ) ]

let timeout_tests =
  let body_10 path =
    Printf.sprintf
      "POST %s HTTP/1.1\r\nHost: a\r\nContent-Length: 10\r\n\r\nhel" path
  in
  group "H1_server timeouts"
    [ test "a request head that is late gets 408" (fun () ->
          expect (show ~stall:true "GET / HTTP/1.1\r\nHost: a")
          @@ __POS_OF__
               {|
            HTTP/1.1 408 Request Timeout\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 16\r\n
            Connection: close\r\n
            \r\n
            Request Timeout
            [FIN]
            |} )
    ; test "a request body that is late gets 408" (fun () ->
          expect (show ~stall:true (body_10 "/echo"))
          @@ __POS_OF__
               {|
            HTTP/1.1 408 Request Timeout\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 16\r\n
            Connection: close\r\n
            \r\n
            Request Timeout
            [FIN]
            |} )
    ; test "a body that is late once the response started ends it" (fun () ->
          expect (show ~stall:true (body_10 "/stream-echo"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            3\r\n
            hel\r\n
            [FIN]
            |} )
    ; test "an unread body that is late when drained closes" (fun () ->
          expect (show ~stall:true (body_10 "/ignore"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored[FIN]
            |} )
    ; test "a new connection that sends nothing is closed" (fun () ->
          (* The first wait for a request has the header timeout. *)
          expect
            (show ~config:Server_support.timed_config ~stall:true
               ~ends_after:Server_support.header_h "" )
          @@ __POS_OF__ {||} )
    ; test "an idle connection is closed" (fun () ->
          (* The close is not a linger: no [FIN]. *)
          expect
            (show ~config:Server_support.timed_config ~stall:true
               ~ends_after:Server_support.idle_i (get "/") )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |} )
    ; (* Readiness can be stale: waking without input must neither block in a
         read nor give up on the connection. *)
      test "a wakeup without input keeps waiting for the request" (fun () ->
          expect (show ~false_wakes:2 (get "/"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |} )
    ; test "a wakeup without input leaves the timeout running" (fun () ->
          (* The flow is readable only when the test fills [wake], and then it
             has nothing to read. A wake 1 s after the start must keep the
             deadline of the first wait. *)
          let woken = ref 0 and pending = ref false in
          let wake = ref (Ivar.create ()) in
          let readable =
            guard (fun () -> if !pending then always () else Ivar.read !wake)
          in
          let read_now _ _ _ =
            if !pending then begin
              incr woken ;
              wake := Ivar.create () ;
              pending := false
            end ;
            None
          in
          let flow =
            Flow.make
              ~read:(fun _ _ _ -> failwith "blocking read")
              ~read_now
              ~write:(fun _ _ _ -> ())
              ~readable
              ~shutdown_send:(fun () -> failwith "shutdown_send")
              ~close:ignore ~set_read_deadline:ignore ~set_write_deadline:ignore
              ()
          in
          Sim_support.manual
          @@ fun sim ->
          let t0 = Time.now Mono in
          Fiber.scope
          @@ fun s ->
          let c = engine s (ctx Server_support.timed_config) flow in
          Sim_support.advance sim (Span.s 1) ;
          pending := true ;
          Ivar.fill !wake () ;
          (* The engine woke, found nothing, and waits again: for the old
             deadline, not for a new one 1 s later. *)
          is_false ~msg:"the engine waits" (Sim_support.ended sim c) ;
          equal int ~msg:"wakeups" 1 !woken ;
          Sim_support.ends_at sim ~t0 Server_support.header_h c )
    ; test "a wakeup without input does not hold up a stopping server"
        (fun () ->
          (* The server stops when the flow finds nothing to read. *)
          let woken = ref false and stop = Ivar.create () in
          let flow =
            Flow.make
              ~read:(fun _ _ _ -> failwith "blocking read")
              ~read_now:(fun _ _ _ ->
                woken := true ;
                ignore (Ivar.try_fill stop ()) ;
                None )
              ~write:(fun _ _ _ -> ())
              ~readable:(guard (fun () -> if !woken then never else always ()))
              ~shutdown_send:ignore ~close:ignore ~set_read_deadline:ignore
              ~set_write_deadline:ignore ()
          in
          let ctx =
            ctx ~stopping:(Ivar.read stop)
              {Config.default with header_timeout= Span.s 60}
          in
          Sim_support.manual (fun sim ->
              Fiber.scope (fun s ->
                  let c = engine s ctx flow in
                  is_true ~msg:"the engine ends at once"
                    (Sim_support.ended sim c) ;
                  Sim_support.await c ) ) ;
          is_true ~msg:"woken" !woken ) ]

let upgrade_tests =
  let ws ?(v = "HTTP/1.1")
      ?(headers = [("Upgrade", "echo"); ("Connection", "Upgrade")]) path =
    get ~v ~headers path
  in
  let expect_100 = ("Expect", "100-continue") in
  let small_drain = {Config.default with date_header= false; max_drain= 2} in
  group "H1_server upgrades"
    [ test "Expect 100-continue and no body: 100, then 101 (RFC 9110 7.8)"
        (fun () ->
          expect
            (show
               ( ws
                   ~headers:
                     [("Upgrade", "echo"); ("Connection", "Upgrade"); expect_100]
                   "/ws"
               ^ "after" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n
            got after
            |} )
    ; test "Expect 100-continue and Content-Length: 0: 100, then 101" (fun () ->
          expect
            (show
               (ws
                  ~headers:
                    [ ("Upgrade", "echo")
                    ; ("Connection", "Upgrade")
                    ; expect_100
                    ; ("Content-Length", "0") ]
                  "/ws" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n
            got
            |} )
    ; test "Expect 100-continue and a body not read: 100, drain, 101" (fun () ->
          expect
            (show
               ( ws
                   ~headers:
                     [ ("Upgrade", "echo")
                     ; ("Connection", "Upgrade")
                     ; expect_100
                     ; ("Content-Length", "5") ]
                   "/ws"
               ^ "helloafter" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n
            got after
            |} )
    ; test
        "Expect 100-continue and a Content-Length above max_drain: 500, no 100"
        (fun () ->
          expect
            (show ~config:small_drain
               ( ws
                   ~headers:
                     [ ("Upgrade", "echo")
                     ; ("Connection", "Upgrade")
                     ; expect_100
                     ; ("Content-Length", "5") ]
                   "/ws"
               ^ "hello" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            |} )
    ; test "Expect 100-continue and a chunked body above max_drain: 100, 500"
        (fun () ->
          expect
            (show ~config:small_drain
               ( ws
                   ~headers:
                     [ ("Upgrade", "echo")
                     ; ("Connection", "Upgrade")
                     ; expect_100
                     ; ("Transfer-Encoding", "chunked") ]
                   "/ws"
               ^ "5\r\nhello\r\n0\r\n\r\n" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            |} )
    ; test "Expect 100-continue and a body that the handler read: one 100"
        (fun () ->
          expect
            (show
               ( ws
                   ~headers:
                     [ ("Upgrade", "echo")
                     ; ("Connection", "Upgrade")
                     ; expect_100
                     ; ("Content-Length", "5") ]
                   "/ws-read"
               ^ "helloafter" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n
            got after
            |} )
    ; test "Expect 100-continue, CONNECT with a body: 100, drain, 200"
        (fun () ->
          expect
            (show
               "CONNECT x.example:443 HTTP/1.1\r\n\
                Host: x.example:443\r\n\
                Expect: 100-continue\r\n\
                Content-Length: 5\r\n\
                \r\n\
                helloafter" )
          @@ __POS_OF__
               {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            \r\n
            got after
            |} )
    ; test "Expect 100-continue, CONNECT with no body: 200, no 100" (fun () ->
          expect
            (show
               "CONNECT x.example:443 HTTP/1.1\r\n\
                Host: x.example:443\r\n\
                Expect: 100-continue\r\n\
                \r\n\
                after" )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            \r\n
            got after
            |} )
    ; test "Expect 100-continue, CONNECT with Content-Length: 0: 200, no 100"
        (fun () ->
          (* The body is complete at once, so the drain reads nothing and sends
             no 100. *)
          expect
            (show
               "CONNECT x.example:443 HTTP/1.1\r\n\
                Host: x.example:443\r\n\
                Expect: 100-continue\r\n\
                Content-Length: 0\r\n\
                \r\n\
                after" )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            \r\n
            got after
            |} )
    ; test "Expect 100-continue and an upgrade that is refused: 500, no 100"
        (fun () ->
          expect
            (show
               (ws
                  ~headers:
                    [("Upgrade", "echo"); ("Connection", "Upgrade"); expect_100]
                  "/ws-bare" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("Response.upgrade: 101 without an Upgrade field")
            |} )
    ; test "Expect 100-continue and a client that left: nothing, no error"
        (fun () ->
          expect
            (show ~unwritable:true
               (ws
                  ~headers:
                    [("Upgrade", "echo"); ("Connection", "Upgrade"); expect_100]
                  "/ws" ) )
          @@ __POS_OF__ {||} )
    ; test "Expect 100-continue in HTTP/1.0 CONNECT with a body: no 100"
        (fun () ->
          (* RFC 9110 10.1.1: a server ignores the expectation of HTTP/1.0. The
             head of the HTTP/1.0 tunnel has no Connection field. *)
          expect
            (show
               "CONNECT x.example:443 HTTP/1.0\r\n\
                Host: x.example:443\r\n\
                Expect: 100-continue\r\n\
                Content-Length: 5\r\n\
                \r\n\
                helloafter" )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            \r\n
            got after
            |} )
    ; test "a 101 names only protocols that the request offered (RFC 9110 7.8)"
        (fun () ->
          let row (offer, answer) =
            let headers =
              List.map (fun o -> ("Upgrade", o)) offer
              @ [("Connection", "Upgrade")]
            in
            let out, errors = serve (ws ~headers ("/ws-as?" ^ answer)) in
            let status = List.hd (String.split_on_char '\r' out) in
            Printf.sprintf "%-12s -> %-8s => %s%s"
              (String.concat " + " offer)
              answer status
              (String.concat "" (List.map (fun e -> " ! " ^ e) errors))
          in
          expect
            (String.concat "\n"
               (List.map row
                  [ (["echo"], "other")
                  ; (["ECHO"], "echo")
                  ; (["echo/2"], "echo")
                  ; (["echo/2"], "echo/3")
                  ; (["echo/2"], "echo/2")
                  ; (["echo"], "echo/2")
                  ; (["Echo/V2"], "echo/v2")
                  ; (["A/b/c"], "a/b/c")
                  ; (["A/b/c"], "a/B/c")
                  ; ([""], "echo")
                  ; (["a, b"], "a,b")
                  ; (["a"], "a,b")
                  ; (["a"; "b"], "b")
                  ; (["echo"], "") ] ) )
          @@ __POS_OF__
               {|
            echo         -> other    => HTTP/1.1 500 Internal Server Error ! Failure("Response.upgrade: 101 to a protocol that the request did not offer: other")
            ECHO         -> echo     => HTTP/1.1 101 Switching Protocols
            echo/2       -> echo     => HTTP/1.1 500 Internal Server Error ! Failure("Response.upgrade: 101 to a protocol that the request did not offer: echo")
            echo/2       -> echo/3   => HTTP/1.1 500 Internal Server Error ! Failure("Response.upgrade: 101 to a protocol that the request did not offer: echo/3")
            echo/2       -> echo/2   => HTTP/1.1 101 Switching Protocols
            echo         -> echo/2   => HTTP/1.1 500 Internal Server Error ! Failure("Response.upgrade: 101 to a protocol that the request did not offer: echo/2")
            Echo/V2      -> echo/v2  => HTTP/1.1 500 Internal Server Error ! Failure("Response.upgrade: 101 to a protocol that the request did not offer: echo/v2")
            A/b/c        -> a/b/c    => HTTP/1.1 101 Switching Protocols
            A/b/c        -> a/B/c    => HTTP/1.1 500 Internal Server Error ! Failure("Response.upgrade: 101 to a protocol that the request did not offer: a/B/c")
                         -> echo     => HTTP/1.1 500 Internal Server Error ! Failure("Response.upgrade: 101 to a request that did not ask for an upgrade")
            a, b         -> a,b      => HTTP/1.1 101 Switching Protocols
            a            -> a,b      => HTTP/1.1 500 Internal Server Error ! Failure("Response.upgrade: 101 to a protocol that the request did not offer: b")
            a + b        -> b        => HTTP/1.1 101 Switching Protocols
            echo         ->          => HTTP/1.1 500 Internal Server Error ! Failure("Response.upgrade: 101 without an Upgrade field")
            |} )
    ; test "a 101 to a protocol not offered gets no 100 Continue" (fun () ->
          expect
            (show
               (ws
                  ~headers:
                    [ ("Upgrade", "echo")
                    ; ("Connection", "Upgrade")
                    ; ("Expect", "100-continue") ]
                  "/ws-as?other" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("Response.upgrade: 101 to a protocol that the request did not offer: other")
            |} )
    ; test "a server that stops opens no new upgrade: 503 (#26)" (fun () ->
          let ws_stopping ?(headers = []) () =
            ws
              ~headers:
                ([("Upgrade", "echo"); ("Connection", "Upgrade")] @ headers)
              "/ws-stopping"
          in
          expect
            (String.concat "\n---\n"
               [ show (ws_stopping () ^ "early bytes")
               ; show
                   ( ws_stopping
                       ~headers:[expect_100; ("Content-Length", "5")]
                       ()
                   ^ "hello" )
               ; show ~config:small_drain
                   (ws_stopping ~headers:[("Content-Length", "4")] () ^ "abcd")
               ; show (ws_stopping () ^ get "/")
               ; show
                   "CONNECT stopping.example:443 HTTP/1.1\r\n\
                    Host: stopping.example:443\r\n\
                    \r\n\
                    tunnel bytes"
               ; show
                   (ws
                      ~headers:[("Upgrade", "echo"); ("Connection", "Upgrade")]
                      "/ws-bare-stopping" ) ] )
          @@ __POS_OF__
               {|
            HTTP/1.1 503 Service Unavailable\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 20\r\n
            Connection: close\r\n
            \r\n
            Service Unavailable
            [FIN]
            ---
            HTTP/1.1 503 Service Unavailable\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 20\r\n
            Connection: close\r\n
            \r\n
            Service Unavailable
            [FIN]
            ---
            HTTP/1.1 503 Service Unavailable\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 20\r\n
            Connection: close\r\n
            \r\n
            Service Unavailable
            [FIN]
            ---
            HTTP/1.1 503 Service Unavailable\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 20\r\n
            Connection: close\r\n
            \r\n
            Service Unavailable
            [FIN]
            ---
            HTTP/1.1 503 Service Unavailable\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 20\r\n
            Connection: close\r\n
            \r\n
            Service Unavailable
            [FIN]
            ---
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("Response.upgrade: 101 without an Upgrade field")
            |} )
    ; test "a stop at the 100: 503 after the 100 (#26)" (fun () ->
          (* The stop starts when the engine writes the 100: the second check,
             just before the switch, sees it. *)
          let input =
            ws
              ~headers:
                [ ("Upgrade", "echo")
                ; ("Connection", "Upgrade")
                ; expect_100
                ; ("Content-Length", "5") ]
              "/ws"
            ^ "hello"
          in
          let out = Buffer.create 256 and stop = ref false in
          let inner = Flow.of_string input out in
          let flow =
            Flow.make ~read:(Flow.read inner) ~read_now:(Flow.read_now inner)
              ~write:(fun buf off len ->
                Flow.write inner buf off len ;
                if
                  Bytes.sub_string buf off len = "HTTP/1.1 100 Continue\r\n\r\n"
                then stop := true )
              ~readable:(Flow.readable inner) ~shutdown_send:ignore
              ~close:ignore ~set_read_deadline:ignore ~set_write_deadline:ignore
              ()
          in
          let ctx =
            ctx
              ~is_stopping:(fun () -> !stop)
              { Config.default with
                date_header= false
              ; cancel_on_disconnect= false }
          in
          Sim_support.run (fun _ -> Private.H1_server.serve ctx flow handler) ;
          expect (show_crlf (Buffer.contents out))
          @@ __POS_OF__
               {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 503 Service Unavailable\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 20\r\n
            Connection: close\r\n
            \r\n
            Service Unavailable
            |} )
    ; test "a stop during the drain of the body: 503 (#26)" (fun () ->
          (* The head comes in one read, the body in the next one, and the stop
             starts at that read: only the check after the drain sees it. *)
          let head =
            ws
              ~headers:
                [ ("Upgrade", "echo")
                ; ("Connection", "Upgrade")
                ; ("Content-Length", "5") ]
              "/ws"
          in
          let parts = ref [head; "ZZZZZ"] and stop = ref false in
          let out = Buffer.create 256 in
          let read buf off len =
            match !parts with
            | [] ->
                0
            | p :: rest ->
                if p = "ZZZZZ" then stop := true ;
                let n = Int.min len (String.length p) in
                Bytes.blit_string p 0 buf off n ;
                parts :=
                  if n = String.length p then rest
                  else String.sub p n (String.length p - n) :: rest ;
                n
          in
          let flow =
            Flow.make ~read
              ~read_now:(fun b o l -> Some (read b o l))
              ~write:(fun buf off len -> Buffer.add_subbytes out buf off len)
              ~readable:(always ()) ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          let ctx =
            ctx
              ~is_stopping:(fun () -> !stop)
              { Config.default with
                date_header= false
              ; cancel_on_disconnect= false }
          in
          Sim_support.run (fun _ -> Private.H1_server.serve ctx flow handler) ;
          expect (show_crlf (Buffer.contents out))
          @@ __POS_OF__
               {|
            HTTP/1.1 503 Service Unavailable\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 20\r\n
            Connection: close\r\n
            \r\n
            Service Unavailable
            |} )
    ; test "101 hands over the connection, with the bytes already received"
        (fun () ->
          expect (show (ws "/ws" ^ "early bytes, " ^ "then more"))
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n
            got early bytes, then more
            |} )
    ; test "the bytes after the head are the same however the input is split"
        (fun () ->
          let input =
            ws "/ws" ^ String.init 300 (fun i -> Char.chr (97 + (i mod 26)))
          in
          equal string (show input) (show ~chunk:7 input) )
    ; test "a request body is read before switching" (fun () ->
          expect
            (show
               "GET /ws HTTP/1.1\r\n\
                Host: a\r\n\
                Upgrade: echo\r\n\
                Connection: Upgrade\r\n\
                Content-Length: 4\r\n\
                \r\n\
                bodyafter" )
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n
            got after
            |} )
    ; test "CONNECT 2xx opens a tunnel" (fun () ->
          expect
            (show
               "CONNECT a.example:443 HTTP/1.1\r\n\
                Host: a.example:443\r\n\
                \r\n\
                hello" )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            \r\n
            got hello
            |} )
    ; test "upgrades that break the rules get 500 and are reported" (fun () ->
          expect
            (String.concat "--"
               [ show (get "/ws")
               ; show (ws ~v:"HTTP/1.0" "/ws")
               ; show (ws ~headers:[("Upgrade", "echo")] "/ws")
               ; show (ws "/ws-bare")
               ; show (get "/tunnel") ] )
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("Response.upgrade: 101 to a request that did not ask for an upgrade")--HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("Response.upgrade: 101 to an HTTP/1.0 request")--HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("Response.upgrade: 101 to a request that did not ask for an upgrade")--HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("Response.upgrade: 101 without an Upgrade field")--HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("Response.upgrade: 2xx upgrade to a GET request")
            |} )
    ; test "a 2xx to CONNECT that is not an upgrade gets 500, and no tunnel"
        (fun () ->
          expect
            (show
               "CONNECT plain.example:443 HTTP/1.1\r\n\
                Host: plain.example:443\r\n\
                \r\n\
                GET /inside HTTP/1.1\r\n\
                Host: a\r\n\
                \r\n" )
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("a 2xx response to CONNECT must be a Response.upgrade")
            |} )
    ; test "a CONNECT refused with 403 is an ordinary response" (fun () ->
          expect
            (show
               ( "CONNECT denied.example:443 HTTP/1.1\r\n\
                  Host: denied.example:443\r\n\
                  \r\n" ^ get "/" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 403 Forbidden\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 6\r\n
            \r\n
            deniedHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |} )
    ; test "a handler answering with a 1xx gets 500" (fun () ->
          expect (show (get "/early-hints"))
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Invalid_argument("Response.make: 103 is not a final status")
            |} )
    ; test "an upgrade with a body too large to drain gets 500" (fun () ->
          let config = {Config.default with date_header= false; max_drain= 2} in
          expect
            (show ~config
               "GET /ws HTTP/1.1\r\n\
                Host: a\r\n\
                Upgrade: echo\r\n\
                Connection: Upgrade\r\n\
                Content-Length: 4\r\n\
                \r\n\
                body" )
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            |} )
    ; test "an upgrade callback that raises is reported" (fun () ->
          expect (show (ws "/ws-fail"))
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n

            ! Failure("ws")
            |} )
    ; test "an upgrade gets a flow with no deadline and no timeout" (fun () ->
          (* The flow records if each of its four limits has a value. Each one
             has a value at the start. The flow gives at most 8 bytes for each
             read. Thus the engine reads the head with a deadline. When the
             handler reads the body, the engine reads it with a timeout. *)
          let limits_at_upgrade ~read input =
            let m = memory ~chunk:8 input in
            let limits = Array.make 4 true in
            let set i v = limits.(i) <- Option.is_some v in
            let flow =
              Flow.make ~read:(Flow.read m.flow)
                ~read_now:(Flow.read_now m.flow) ~write:(Flow.write m.flow)
                ~readable:(Flow.readable m.flow)
                ~shutdown_send:(fun () -> Flow.shutdown_send m.flow)
                ~close:ignore ~set_read_deadline:(set 0)
                ~set_write_deadline:(set 1) ~set_read_timeout:(set 2)
                ~set_write_timeout:(set 3) ()
            in
            let seen = ref [] in
            let handler req =
              if read then ignore (Body.to_string (Request.body req)) ;
              Response.upgrade ~headers:upgrade_echo (fun _ ->
                  seen := Array.to_list limits )
            in
            Sim_support.run (fun _ ->
                Private.H1_server.serve (ctx Config.default) flow handler ) ;
            !seen
          in
          let none = [false; false; false; false] in
          equal
            (list (list bool))
            ~msg:
              "a value for the read deadline, the write deadline, the read \
               timeout and the write timeout"
            [none; none]
            [ limits_at_upgrade ~read:false (ws "/ws")
            ; limits_at_upgrade ~read:true
                ( ws
                    ~headers:
                      [ ("Upgrade", "echo")
                      ; ("Connection", "Upgrade")
                      ; ("Content-Length", "20") ]
                    "/ws"
                ^ String.make 20 'x' ) ] ) ]

let disconnect_tests =
  group "H1_server client disconnects"
    [ test "a client that leaves cancels its handler" (fun () ->
          expect (show ~watch:true (get "/hold"))
          @@ __POS_OF__ {| [handler cancelled] |} )
    ; test "without cancel_on_disconnect the handler finishes" (fun () ->
          expect (show (get "/slow"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 4\r\n
            \r\n
            late
            [handler finished]
            |} )
    ; test "a failing handler is still answered while the connection is watched"
        (fun () ->
          let config =
            {Config.default with date_header= false; idle_timeout= Span.ms 10}
          in
          expect (show ~watch:true ~stall:true ~config (get "/fail"))
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("boom")
            |} )
    ; test "a request arriving during the handler is not a disconnect"
        (fun () ->
          (* After the two responses, the connection is idle. *)
          let first = get "/slow" in
          expect
            (show ~watch:true ~stall:true ~config:Server_support.timed_config
               ~chunk:(String.length first)
               ~ends_after:(span_sum slow Server_support.idle_i)
               (first ^ get "/") )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 4\r\n
            \r\n
            lateHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            [handler finished]
            |} )
    ; test "a request with an empty body is watched at once" (fun () ->
          expect (show ~watch:true (post "/hold" ""))
          @@ __POS_OF__ {| [handler cancelled] |} )
    ; test "nothing is watched while the request body is unread" (fun () ->
          expect (show ~watch:true (post "/slow" "hello"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 4\r\n
            \r\n
            late
            [handler finished]
            |} )
    ; test "a handler that fails while cancelled is reported" (fun () ->
          expect (show ~watch:true (get "/hold-cleanup-fails"))
          @@ __POS_OF__
               {|
            ! Failure("cleanup failed")
            [handler cancelled]
            |} )
    ; test "a client that leaves after its body was read cancels its handler"
        (fun () ->
          expect (show ~watch:true (post "/read-hold" "hello"))
          @@ __POS_OF__ {| [handler cancelled] |} )
    ; test "race: on one domain, a fast handler costs no read with no data"
        (fun () ->
          (* Two keep-alive requests, each after the response to the one before.
             After a request the socket stays readable with no data. A handler
             that ends at once, also with a failure, gives its response before
             the engine watches the connection. If the engine watches, weft can
             select the stale readiness. *)
          let expected =
            "HTTP/1.1 200 OK\r\n\
             Content-Type: text/plain; charset=utf-8\r\n\
             Content-Length: 5\r\n\
             \r\n\
             helloHTTP/1.1 500 Internal Server Error\r\n\
             Content-Type: text/plain; charset=utf-8\r\n\
             Content-Length: 22\r\n\
             Connection: close\r\n\
             \r\n\
             Internal Server Error\n\
             [FIN]"
          in
          Sim_support.race ~domains:1 (fun _ ->
              let c = fake_socket [get "/"; get "/fail"; ""]
              and errors = ref [] in
              let config =
                config_for
                  {Config.default with date_header= false}
                  errors ~watch:true
              in
              Private.H1_server.serve (ctx config) c.socket handler ;
              equal (list string) ~msg:"errors" ["Failure(\"boom\")"] !errors ;
              equal int ~msg:"reads with no data" 0 (c.empty_reads ()) ;
              equal string ~msg:"the responses" expected (c.written ()) ) )
    ; test "a wakeup without input while the handler runs is not a disconnect"
        (fun () ->
          (* Once the request is read the client is quiet: the flow is readable
             once more with nothing to read, and a read would block, which
             fails. *)
          let input = get "/slow" and out = Buffer.create 64 in
          let pos = ref 0 and woken = ref false in
          let quiet () = !pos = String.length input in
          let read buf off len =
            if quiet () then failwith "blocking read" ;
            let n = Int.min len (String.length input - !pos) in
            Bytes.blit_string input !pos buf off n ;
            pos := !pos + n ;
            n
          in
          let read_now buf off len =
            if quiet () then (
              woken := true ;
              None )
            else Some (read buf off len)
          in
          let flow =
            Flow.make ~read ~read_now
              ~write:(fun b off len -> Buffer.add_subbytes out b off len)
              ~readable:
                (guard (fun () ->
                     if quiet () && !woken then never else always () ) )
              ~shutdown_send:ignore ~close:ignore ~set_read_deadline:ignore
              ~set_write_deadline:ignore ()
          in
          slow_outcome := "" ;
          let ctx =
            ctx {Server_support.timed_config with cancel_on_disconnect= true}
          in
          Sim_support.manual (fun sim ->
              let t0 = Time.now Mono in
              (* After the response, the connection is idle. *)
              Fiber.scope (fun s ->
                  Sim_support.ends_at sim ~t0
                    (span_sum slow Server_support.idle_i)
                    (engine s ctx flow) ) ) ;
          is_true ~msg:"woken" !woken ;
          expect (show_crlf (Buffer.contents out) ^ "\n" ^ !slow_outcome)
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 4\r\n
            \r\n
            late
            [handler finished]
            |} )
    ]

(* A request smuggled in a malformed chunked body must never be served, even
   when the handler swallows the body error and answers. *)
let smuggling_tests =
  let chunked_post body =
    "POST /swallow HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n"
    ^ body
  in
  let smuggled = "GET /smuggled HTTP/1.1\r\nHost: a\r\n\r\n" in
  let config = {Config.default with date_header= false; max_body= Some 100} in
  let contains s sub =
    let n = String.length s and m = String.length sub in
    let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
    at 0
  in
  (* The ways a chunked body can go wrong in the middle, each followed by what
     an attacker wants read as the next request. *)
  let malformed =
    Gen.of_list
      ~pp:(fun ppf s -> Format.fprintf ppf "%S" s)
      [ "zz\r\n0\r\n\r\n" ^ smuggled
      ; "-1\r\n0\r\n\r\n" ^ smuggled
      ; "3\r\nabcX0\r\n\r\n" ^ smuggled
      ; "3;=\r\nabc\r\n0\r\n\r\n" ^ smuggled
      ; "1000000000000000\r\n0\r\n\r\n" ^ smuggled
      ; (* Over max_body: the data is the smuggled request. *)
        Printf.sprintf "%x\r\n0\r\n\r\n%s%s\r\n0\r\n\r\n" 200 smuggled
          (String.make (200 - 5 - String.length smuggled) 'x') ]
  in
  group "H1_server smuggling"
    [ test "a bad chunk size line ends the connection after the handler's answer"
        (fun () ->
          expect (show (chunked_post ("zz\r\n0\r\n\r\n" ^ smuggled)))
          @@ __POS_OF__
               {|
            HTTP/1.1 400 Bad Request\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 16\r\n
            Connection: close\r\n
            \r\n
            unreadable body
            [FIN]
            |} )
    ; test
        "a chunk over max_body ends the connection after the handler's answer"
        (fun () ->
          let data = "0\r\n\r\n" ^ smuggled in
          let body =
            Printf.sprintf "c8\r\n%s%s\r\n0\r\n\r\n" data
              (String.make (200 - String.length data) 'x')
          in
          expect (show ~config (chunked_post body))
          @@ __POS_OF__
               {|
            HTTP/1.1 400 Bad Request\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 16\r\n
            Connection: close\r\n
            \r\n
            unreadable body
            [FIN]
            |} )
    ; prop "no request smuggled in a malformed chunked body is ever served"
        Gen.(
          triple
            (list ~size:(int_range 0 3) (int_range 1 9))
            malformed (int_range 1 64) )
        (fun (sizes, bad, chunk) ->
          let valid =
            String.concat ""
              (List.map
                 (fun n -> Printf.sprintf "%x\r\n%s\r\n" n (String.make n 'x'))
                 sizes )
          in
          let out, _ = serve ~config ~chunk (chunked_post (valid ^ bad)) in
          (* The default handler would answer /smuggled with "hello". *)
          is_true ~msg:out (not (contains out "hello")) ) ]

(* The response of the default handler. *)
let hello =
  "HTTP/1.1 200 OK\r\n\
   Content-Type: text/plain; charset=utf-8\r\n\
   Content-Length: 5\r\n\
   \r\n\
   hello"

(* The orders that weft can select at random: input and a stop, input and a
   passed deadline, input and a cancel. *)
let race_tests =
  let config errors =
    config_for {Config.default with date_header= false} errors ~watch:false
  in
  group "H1_server races"
    [ test "race: a stop that comes with input starts no new exchange" (fun () ->
          (* The stop and the input are ready at the same wait. The engine reads
             the request and lingers, or it closes with no read. *)
          let input = get "/" in
          let paths = ref [] in
          (* The check of the two paths needs more than one seed. Thus the seed
             count is fixed, also when CARAVAN_RACE_SEEDS has a value. *)
          Sim_support.race ~seeds:100 (fun _ ->
              let m = memory input and errors = ref [] in
              let checks = ref 0 in
              (* [false] at the first check, before the wait. *)
              let is_stopping () = incr checks ; !checks > 1 in
              Private.H1_server.serve
                (ctx ~stopping:(always ()) ~is_stopping (config errors))
                m.flow handler ;
              let out = m.output () in
              let stop_path =
                Printf.sprintf "[unread %d]" (String.length input)
              in
              if not (List.mem out ["[FIN]"; stop_path]) then
                failf "the engine wrote %S" out ;
              equal (list string) ~msg:"errors" [] !errors ;
              if not (List.mem out !paths) then paths := out :: !paths ) ;
          (* A replay of one seed takes one path only. *)
          if Option.is_none (Sys.getenv_opt "CARAVAN_SEED") then
            equal int ~msg:"the two paths occur over the seeds" 2
              (List.length !paths) )
    ; test
        "race: input and a passed idle deadline at the same time: the request \
         is served" (fun () ->
          Sim_support.race (fun _ ->
              let m = memory (get "/") and errors = ref [] in
              let config = {(config errors) with header_timeout= Span.zero} in
              Private.H1_server.serve (ctx config) m.flow handler ;
              equal string ~msg:"the response" hello (m.output ()) ;
              equal (list string) ~msg:"errors" [] !errors ) )
    ; test "a cancel right after the input is ready reports no error" (fun () ->
          (* The fill of [input] commits the wait of the engine. The cancel
             comes before the engine continues. The engine then reads the
             request with no wait, and the fork of the exchange gives
             Scope_closed. *)
          let input = Ivar.create () in
          let m = memory ~ready:input (get "/") and errors = ref [] in
          Sim_support.run (fun sim ->
              Fiber.scope (fun s ->
                  let c = engine s (ctx (config errors)) m.flow in
                  is_false ~msg:"the engine waits for input"
                    (Sim_support.ended sim c) ;
                  Ivar.fill input () ;
                  Sim_support.cancel c ;
                  (* As the server, which forks each connection with
                     fork_isolated. *)
                  match Sim_support.await c with
                  | () | (exception Promise.Cancelled) ->
                      ()
                  | exception e ->
                      errors := Printexc.to_string e :: !errors ) ) ;
          equal (list string) ~msg:"errors" [] !errors ;
          equal string ~msg:"no response" "" (m.output ()) ) ]

(* The exceptions of a handler that the engine must not take for a cancel of the
   connection. *)
let handler_error_tests =
  group "H1_server handler errors"
    [ test "a handler that raises Promise.Cancelled is reported" (fun () ->
          expect (show (get "/cancelled-child"))
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Weft__Promise.Cancelled
            |} )
    ; test "a handler that raises Scope_closed is reported" (fun () ->
          expect (show (get "/ended-scope"))
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Weft__Sched.Scope_closed
            |} )
    ; test "a handler that raises the Cancelled of its own scope is reported"
        (fun () ->
          expect (show (get "/cancelled-scope"))
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Weft__Sched.Cancelled(_)
            |} )
    ; test
        "a response producer that raises the Cancelled of its own scope is \
         reported" (fun () ->
          expect (show (get "/stream-cancelled-scope"))
          @@ __POS_OF__
               {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Weft__Sched.Cancelled(_)
            |} )
    ; test "an upgrade that raises the Cancelled of its own scope is reported"
        (fun () ->
          expect
            (show
               (get
                  ~headers:[("Upgrade", "echo"); ("Connection", "Upgrade")]
                  "/ws-cancelled-scope" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n

            ! Weft__Sched.Cancelled(_)
            |} )
    ; test "a handler that forks after the cancel of its connection: no error"
        (fun () ->
          (* The wait of the handler ends, then the cancel comes before the
             handler continues. Its fork then gives Scope_closed. *)
          let started = Ivar.create () and go = Ivar.create () in
          let handler _ =
            Ivar.fill started () ;
            sync (Ivar.read go) ;
            Fiber.scope (fun s -> Fiber.fork s ignore) ;
            Response.string "forked"
          in
          expect
            (show ~handler
               ~cancel_on:(fun () ->
                 wrap (Ivar.read started) (fun () -> Ivar.fill go ()) )
               (get "/") )
          @@ __POS_OF__ {| |} )
    ; test
        "race: a handler that raises the Cancelled of its own scope at the \
         cancel of its connection is reported" (fun () ->
          (* The handler fails, and the test cancels the connection at the same
             time. The handler records if its context was cancelled when it
             raised: then weft gives Promise.Cancelled, a cancel. If not, its
             Cancelled is a failure, and the engine reports it one time: in the
             shield, or with a 500. *)
          Sim_support.race (fun _ ->
              let started = Ivar.create () and go = Ivar.create () in
              let failed = ref false in
              let handler _ =
                Ivar.fill started () ;
                sync (Ivar.read go) ;
                ( try
                    Fiber.scope (fun s ->
                        Fiber.cancel s (Failure "inner") ;
                        sync (always ()) )
                  with e ->
                    failed := not (Private.Cancel.is_cancelled ()) ;
                    raise e ) ;
                Response.string "after the scope"
              in
              let m = memory (get "/") and errors = ref [] in
              let config =
                config_for
                  {Config.default with date_header= false}
                  errors ~watch:false
              in
              Fiber.scope (fun s ->
                  let c = engine ~handler s (ctx config) m.flow in
                  sync (Ivar.read started) ;
                  Ivar.fill go () ;
                  Fiber.yield () ;
                  Sim_support.cancel c ;
                  match Sim_support.await c with
                  | () | (exception Promise.Cancelled) ->
                      () ) ;
              let reported =
                if !failed then ["Weft__Sched.Cancelled(_)"] else []
              in
              equal (list string) ~msg:"errors" reported !errors ;
              if not !failed then
                equal string ~msg:"no response" "" (m.output ()) ) )
    ; test
        "a handler that raises the Cancelled of its own scope after its client \
         left is reported" (fun () ->
          (* The client leaves while the handler waits. Then the handler fails
             before the engine reads the end of the stream. [left] cancels the
             exchange after the failure: it gets the Cancelled of the handler, a
             failure. *)
          let c = fake_socket [get "/"] and errors = ref [] in
          let config =
            config_for
              {Config.default with date_header= false}
              errors ~watch:true
          in
          let started = Ivar.create () and go = Ivar.create () in
          let failed = ref false in
          let handler _ =
            Ivar.fill started () ;
            sync (Ivar.read go) ;
            ( try
                Fiber.scope (fun s ->
                    Fiber.cancel s (Failure "inner") ;
                    sync (always ()) )
              with e ->
                failed := not (Private.Cancel.is_cancelled ()) ;
                raise e ) ;
            Response.string "after the scope"
          in
          Sim_support.run (fun _ ->
              Fiber.scope (fun s ->
                  let e = engine ~handler s (ctx config) c.socket in
                  sync (Ivar.read started) ;
                  (* The engine waits in its watch after these yields. *)
                  for _ = 1 to 5 do
                    Fiber.yield ()
                  done ;
                  (* The order of the two fills is important. With this order,
                     the handler fails, then the engine reads the end of the
                     stream. With the other order, the cancel comes first. *)
                  c.end_stream () ;
                  Ivar.fill go () ;
                  Sim_support.await e ) ) ;
          is_true ~msg:"the handler failed" !failed ;
          equal (list string) ~msg:"errors" ["Weft__Sched.Cancelled(_)"] !errors ;
          equal string ~msg:"no response" "" (c.written ()) )
    ; test "a read error while the handler runs is reported, and cancels it"
        (fun () ->
          (* After the request, the read with no wait of the watch raises a
             network error that is not a close. The engine cannot use the
             connection: it ends at once. *)
          let error = Net.Error (Net.Other "ETIMEDOUT", "read") in
          let c = fake_socket ~error [get "/hold"] and errors = ref [] in
          let config =
            config_for
              {Config.default with date_header= false}
              errors ~watch:true
          in
          slow_outcome := "" ;
          let waited =
            Sim_support.run (fun _ ->
                let t0 = Time.now Mono in
                Private.H1_server.serve (ctx config) c.socket handler ;
                Span.to_ns (Time.diff Mono (Time.now Mono) t0) )
          in
          equal int64 ~msg:"the wait of the engine" 0L waited ;
          equal string ~msg:"no response" "" (c.written ()) ;
          equal (list string) ~msg:"errors" [Printexc.to_string error] !errors ;
          equal string ~msg:"the handler" "[handler cancelled]" !slow_outcome )
    ; test "a handler that fails after its connection is cancelled is reported"
        (fun () ->
          (* The cancel comes when the handler waits, and the client does not
             leave. *)
          holding := Ivar.create () ;
          expect
            (show
               ~cancel_on:(fun () -> Ivar.read !holding)
               (get "/hold-cleanup-fails") )
          @@ __POS_OF__
               {|
            ! Failure("cleanup failed")
            [handler cancelled]
            |} )
    ; test "a cancel of the connection while the handler runs reports no error"
        (fun () ->
          holding := Ivar.create () ;
          expect (show ~cancel_on:(fun () -> Ivar.read !holding) (get "/hold"))
          @@ __POS_OF__ {| [handler cancelled] |} )
    ; test "a cancel of an upgraded connection reports no error" (fun () ->
          let started = Ivar.create () in
          let handler _ =
            Response.upgrade ~headers:upgrade_echo (fun _ ->
                Ivar.fill started () ;
                Time.sleep Mono (Span.s 10) )
          in
          expect
            (show ~handler
               ~cancel_on:(fun () -> Ivar.read started)
               (get
                  ~headers:[("Upgrade", "echo"); ("Connection", "Upgrade")]
                  "/" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n
            |} )
    ]

(* The engine on a Tcp flow: the deadlines and the timeouts of each phase, and a
   failed body. H is the header timeout, R is the read timeout, I is the idle
   timeout, W is the write timeout and L is the linger timeout. H is
   Server_support.header_h. The test sets R, I, W and L, so that H and R are
   less than I. *)
let tcp_tests =
  let read_r = Span.s 4 and write_w = Span.s 6 and linger_l = Span.s 7 in
  let config =
    { Server_support.timed_config with
      read_timeout= read_r
    ; idle_timeout= Span.s 10
    ; write_timeout= write_w
    ; linger_timeout= linger_l }
  in
  let ignored =
    "HTTP/1.1 200 OK\r\n\
     Content-Type: text/plain; charset=utf-8\r\n\
     Content-Length: 7\r\n\
     \r\n\
     ignored"
  in
  let request_timeout =
    "HTTP/1.1 408 Request Timeout\r\n\
     Content-Type: text/plain; charset=utf-8\r\n\
     Content-Length: 16\r\n\
     Connection: close\r\n\
     \r\n\
     Request Timeout\n"
  in
  (* [on_tcp ~handler sim f] runs the engine with [handler] on the server side
     of a Tcp pair, in a call [c]. It runs [f s client c] on the client side.
     Then the client closes, and the engine must end with no error. *)
  let on_tcp ?(handler = handler) sim f =
    Server_support.with_flows sim
    @@ fun ~server ~client ->
    let errors = ref [] in
    let ctx = ctx (config_for config errors ~watch:true) in
    Fiber.scope (fun s ->
        let c = engine ~handler s ctx server in
        f s client c ; Flow.close client ; Sim_support.await c ) ;
    equal (list string) ~msg:"errors" [] !errors
  in
  let response client expected =
    equal string ~msg:"the response" expected
      (Server_support.read_exactly client (String.length expected))
  in
  group "H1_server on Tcp"
    [ test "a head that comes one byte at a time gets 408 at the header timeout"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          on_tcp sim
          @@ fun s client _ ->
          let t0 = Time.now Mono in
          let head = get "/" in
          let writer =
            Sim_support.call ~name:"client" s (fun () ->
                String.iter
                  (fun c ->
                    Flow.write_string client (String.make 1 c) ;
                    Time.sleep Mono (Span.s 1) )
                  head )
          in
          let reader =
            Sim_support.call ~name:"reader" s (fun () ->
                response client request_timeout )
          in
          Sim_support.ends_at sim ~t0 Server_support.header_h reader ;
          Sim_support.cancel writer )
    ; test "the second head of a connection gets its own header timeout"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          on_tcp sim
          @@ fun _ client _ ->
          (* Each head comes in two parts, 1 s apart. *)
          let request () =
            Flow.write_string client "GET / HTTP/1.1\r\n" ;
            Sim_support.advance sim (Span.s 1) ;
            Flow.write_string client "Host: a.example\r\n\r\n" ;
            response client hello
          in
          request () ;
          (* More than H, less than I. *)
          Sim_support.advance sim (Span.s 5) ;
          request () )
    ; test "the second drain of a connection gets its own drain deadline"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          on_tcp sim
          @@ fun _ client _ ->
          (* The handler does not read the body. The second half of the body
             comes 1 s after the response, in the drain. *)
          let request () =
            Flow.write_string client
              "POST /ignore HTTP/1.1\r\n\
               Host: a.example\r\n\
               Content-Length: 10\r\n\
               \r\n\
               01234" ;
            response client ignored ;
            Sim_support.advance sim (Span.s 1) ;
            Flow.write_string client "56789"
          in
          request () ;
          (* More than R, less than I. *)
          Sim_support.advance sim (Span.s 5) ;
          request () ;
          Flow.write_string client (get "/") ;
          response client hello )
    ; test "a response that the client does not read ends at the write timeout"
        (fun () ->
          (* The write of the response waits from its start: the client does not
             read. *)
          let handler _ =
            Response.string (String.make Server_support.big 'x')
          in
          Sim_support.manual
          @@ fun sim ->
          on_tcp ~handler sim
          @@ fun _ client c ->
          let t0 = Time.now Mono in
          Flow.write_string client (get "/") ;
          Sim_support.ends_at sim ~t0 write_w c )
    ; test "a request body that stops gets 408 at the read timeout" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          on_tcp sim
          @@ fun s client _ ->
          let t0 = Time.now Mono in
          Flow.write_string client
            "POST /echo HTTP/1.1\r\n\
             Host: a.example\r\n\
             Content-Length: 10\r\n\
             \r\n\
             01234" ;
          let reader =
            Sim_support.call ~name:"reader" s (fun () ->
                response client request_timeout )
          in
          Sim_support.ends_at sim ~t0 read_r reader )
    ; test "each read of a request body gets the read timeout" (fun () ->
          (* The body has no deadline. Its bytes come 3 s apart, less than R,
             and all of them take more than R. *)
          Sim_support.manual
          @@ fun sim ->
          on_tcp sim
          @@ fun _ client _ ->
          Flow.write_string client
            "POST /echo HTTP/1.1\r\n\
             Host: a.example\r\n\
             Content-Length: 4\r\n\
             \r\n" ;
          String.iter
            (fun c ->
              Sim_support.advance sim (Span.s 3) ;
              Flow.write_string client (String.make 1 c) )
            "abcd" ;
          response client
            "HTTP/1.1 200 OK\r\n\
             Content-Type: text/plain; charset=utf-8\r\n\
             Content-Length: 4\r\n\
             \r\n\
             abcd" )
    ; test "a drain that comes one byte at a time ends at the drain deadline"
        (fun () ->
          (* The handler does not read the body. The engine drains it, and the
             client sends one byte each second. At the drain deadline the engine
             lingers: the client reads the end of the stream. *)
          Sim_support.manual
          @@ fun sim ->
          on_tcp sim
          @@ fun s client _ ->
          let t0 = Time.now Mono in
          Flow.write_string client
            "POST /ignore HTTP/1.1\r\n\
             Host: a.example\r\n\
             Content-Length: 10\r\n\
             \r\n" ;
          response client ignored ;
          let writer =
            Sim_support.call ~name:"client" s (fun () ->
                for _ = 1 to 10 do
                  Time.sleep Mono (Span.s 1) ;
                  Flow.write_string client "x"
                done )
          in
          let reader =
            Sim_support.call ~name:"reader" s (fun () ->
                Flow.read client (Bytes.create 1) 0 1 )
          in
          equal int ~msg:"the end of the stream" 0
            (Sim_support.ends_at sim ~t0 read_r reader) ;
          Sim_support.cancel writer )
    ; test "a linger ends at the linger timeout" (fun () ->
          (* A request with no Host gets 400, then the engine lingers. The
             client sends nothing more and does not close. *)
          Sim_support.manual
          @@ fun sim ->
          on_tcp sim
          @@ fun _ client c ->
          let t0 = Time.now Mono in
          Flow.write_string client "GET / HTTP/1.1\r\n\r\n" ;
          Sim_support.ends_at sim ~t0 linger_l c )
    ; test "an upgrade after a cancelled read of the body gets 500" (fun () ->
          (* A child of the handler reads the body and waits for the rest. The
             handler cancels the child, so the body failed with the cancel of a
             different fiber. The engine must not read the body again for the
             upgrade. *)
          let handler req =
            Fiber.scope (fun s ->
                let child =
                  Fiber.fork_promise s (fun () ->
                      Body.to_string (Request.body req) )
                in
                Fiber.yield () ;
                Promise.cancel child ;
                match Promise.await child with
                | _ ->
                    ()
                | exception Promise.Cancelled ->
                    () ) ;
            Response.upgrade ~headers:upgrade_echo echo_upgraded
          in
          Sim_support.manual
          @@ fun sim ->
          on_tcp ~handler sim
          @@ fun _ client _ ->
          Flow.write_string client
            "GET /ws HTTP/1.1\r\n\
             Host: a.example\r\n\
             Upgrade: echo\r\n\
             Connection: Upgrade\r\n\
             Content-Length: 5\r\n\
             \r\n\
             ab" ;
          response client
            "HTTP/1.1 500 Internal Server Error\r\n\
             Content-Type: text/plain; charset=utf-8\r\n\
             Content-Length: 22\r\n\
             Connection: close\r\n\
             \r\n\
             Internal Server Error\n" ) ]

let () =
  exit
    (Windtrap.run "h1_server"
       [ connection_tests
       ; timeout_tests
       ; upgrade_tests
       ; disconnect_tests
       ; smuggling_tests
       ; race_tests
       ; handler_error_tests
       ; tcp_tests ] )
