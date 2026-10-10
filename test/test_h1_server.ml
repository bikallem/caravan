(* The HTTP/1.1 engine over in-memory flows: whole connections, golden bytes. *)

open Windtrap
open Affect
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

let loopback port = Unix.ADDR_INET (Unix.inet_addr_loopback, port)

let conn = Conn_info.make ~id:7 ~peer:(loopback 50000) ~local:(loopback 8080) ()

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

(* Answers "late" after [span] of real time, having read the request body or
   not. *)
let slowly ~read ~span req =
  match
    if read then ignore (Body.to_string (Request.body req)) ;
    Affect_unix.Mtime.wait_for span
  with
  | () ->
      slow_outcome := "[handler finished]" ;
      Response.string "late"
  | exception Fun.Async.Cancelled ->
      slow_outcome := "[handler cancelled]" ;
      raise Fun.Async.Cancelled

(* /slow waits 50 ms: a test that needs the handler to finish uses it. *)
let slow = Affect_unix.Mtime.Span.(50 * ms)

(* /hold waits 2 s: a test that needs a cancel uses it. The cancel comes at
   once, and a missing cancel gives "[handler finished]" after 2 s, not a
   hang. *)
let hold = Affect_unix.Mtime.Span.(2 * s)

(* Set by /endless after it flushed its first bytes. *)
let flushed = ref (Cell.Once.make ())

(* Set by /stop, as a server stopping while its response is written. *)
let stopping = ref false

let handler req =
  if Method.equal (Request.meth req) Method.CONNECT then
    match Request.authority req with
    | Some "plain.example:443" ->
        Response.string "not a tunnel"
    | Some "denied.example:443" ->
        Response.string ~status:Status.forbidden "denied"
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
            ignore (Cell.Once.try_set !flushed ()) ;
            Affect_unix.Mtime.wait_for Affect_unix.Mtime.Span.(10 * s) )
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
      with Fun.Async.Cancelled -> failwith "cleanup failed" )
    | "/ws" ->
        Response.upgrade ~headers:upgrade_echo echo_upgraded
    | "/ws-bare" ->
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

(* Serves [input] on one connection. The bytes written, with [[FIN]] where the
   server half-closed and [[unread n]] if it left input unread, and the errors
   reported. At the end of [input] the client closes; with [~stall:true] it goes
   quiet instead (the flow is no longer readable and reads time out), and with
   [~reset:true] it resets the connection. With [~unwritable:true] every write
   fails as on a reset connection. With [~false_wakes:n] the flow is first
   readable [n] times with nothing to read yet, and a read that would block then
   fails. The flow is on the time line [time] (default: the real one).

   With [~drive] or [~cancel_on], the engine runs in a call. [drive] gets the
   call, for example to advance a fake time. Then, when [cancel_on] is enabled,
   the call is cancelled, as by a server shutdown. At the end, also on a
   failure, [serve] cancels the call, so that a failure does not hang. *)
let serve ?(config = {Config.default with date_header= false}) ?chunk ?time
    ?drive ?cancel_on ?(stall = false) ?(reset = false) ?(unwritable = false)
    ?(watch = false) ?(false_wakes = 0) input =
  let errors = ref [] in
  (* The input's end looks like a client leaving: only watched for if asked. *)
  let config =
    { config with
      on_error= (fun e _ -> errors := Printexc.to_string e :: !errors)
    ; cancel_on_disconnect= watch }
  in
  slow_outcome := "" ;
  let out = Buffer.create 256 and consumed = ref 0 in
  let inner = Flow.of_string ?chunk input out in
  let stalled () = stall && !consumed = String.length input in
  let false_wakes = ref false_wakes in
  let read buf off len =
    if !false_wakes > 0 then failwith "blocking read after a false wakeup" ;
    if stalled () then raise Flow.Timeout ;
    if reset && !consumed = String.length input then raise Flow.Closed ;
    let n = Flow.read inner buf off len in
    consumed := !consumed + n ;
    n
  in
  let read_now buf off len =
    if !false_wakes > 0 then (decr false_wakes ; None)
    else if stalled () then None
    else Some (read buf off len)
  in
  let readable =
    Action.guard (fun () ->
        if !false_wakes = 0 && stalled () then Action.never
        else Action.always () )
  in
  let flow =
    let write buf off len =
      if unwritable then raise Flow.Closed else Flow.write inner buf off len
    in
    Flow.make
      ~time:(Option.value time ~default:(Flow.time inner))
      ~read ~read_now ~write ~readable
      ~shutdown_send:(fun () -> Buffer.add_string out "[FIN]")
      ~close:ignore
      ~reset:(fun () -> Buffer.add_string out "[RST]")
      ~set_read_deadline:ignore ~set_write_deadline:ignore ()
  in
  stopping := false ;
  Affect_unix.Unix.main ~domain_count:1 (fun () ->
      let ctx =
        { Engine.config
        ; conn
        ; stopping= Action.never
        ; is_stopping= (fun () -> !stopping) }
      in
      let serve () = Private.H1_server.serve ctx flow handler in
      if drive = None && cancel_on = None then serve ()
      else
        let c = Fun.Async.call serve in
        Stdlib.Fun.protect
          ~finally:(fun () -> Fun.Async.cancel c)
          (fun () ->
            Option.iter (fun drive -> drive c) drive ;
            Option.iter Action.invoke cancel_on ) ;
        try Fun.Async.get c with Fun.Async.Cancelled -> () ) ;
  let unread = String.length input - !consumed in
  if unread > 0 then Printf.bprintf out "[unread %d]" unread ;
  (Buffer.contents out, List.rev !errors)

let show ?config ?chunk ?time ?drive ?cancel_on ?stall ?reset ?unwritable ?watch
    ?false_wakes input =
  let out, errors =
    serve ?config ?chunk ?time ?drive ?cancel_on ?stall ?reset ?unwritable
      ?watch ?false_wakes input
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

(* The engine on a fake time line *)

module Fake = Fake_time

let ( +: ) = Fake.( +: )

let header_h = Server_support.header_h

let idle_i = Server_support.idle_i

(* The spans of [Server_support], on the fake time line [f]. *)
let fake_config = Server_support.fake_config

let get_2s = Server_support.get_2s

let ends_at = Server_support.ends_at

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
          flushed := Cell.Once.make () ;
          expect
            (show ~cancel_on:(Cell.Once.get' !flushed)
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
            Flow.make ~time:Time.mono
              ~read:(fun _ _ _ -> failwith "read while stopping")
              ~read_now:(fun _ _ _ -> failwith "read while stopping")
              ~write:(fun _ _ _ -> ())
              ~readable:Action.never ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              let ctx =
                { Engine.config= Config.default
                ; conn
                ; stopping= Action.always ()
                ; is_stopping= (fun () -> true) }
              in
              Private.H1_server.serve ctx flow handler ) )
    ; test "an error response to a client that left is dropped" (fun () ->
          let input =
            Flow.of_string "GET / HTTP/1.1\r\n\r\n" (Buffer.create 0)
          in
          let flow =
            Flow.make ~time:(Flow.time input) ~read:(Flow.read input)
              ~read_now:(Flow.read_now input)
              ~write:(fun _ _ _ -> raise Flow.Closed)
              ~readable:(Flow.readable' input ()) ~shutdown_send:ignore
              ~close:ignore ~set_read_deadline:ignore ~set_write_deadline:ignore
              ()
          in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              let ctx =
                { Engine.config= Config.default
                ; conn
                ; stopping= Action.never
                ; is_stopping= (fun () -> false) }
              in
              Private.H1_server.serve ctx flow handler ) )
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
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          expect
            (show ~config:(fake_config f) ~time:(Fake.mono f) ~stall:true
               ~drive:(ends_at f ~since:m ~t0 header_h)
               "" )
          @@ __POS_OF__ {||} )
    ; test "an idle connection is closed" (fun () ->
          (* The close is not a linger: no [FIN]. *)
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          expect
            (show ~config:(fake_config f) ~time:(Fake.mono f) ~stall:true
               ~drive:(ends_at f ~since:m ~t0 idle_i)
               (get "/") )
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
          (* The flow is readable only when the test sets [wake], and then it
             has nothing to read. A wakeup 1 s after the start must keep the
             deadline of the first wait. *)
          let f = Fake.make () in
          let woken = Atomic.make 0 and pending = Atomic.make false in
          let wake = ref (Cell.Once.make ()) in
          let readable =
            Action.guard (fun () ->
                if Atomic.get pending then Action.always ()
                else Cell.Once.get' !wake )
          in
          let read_now _ _ _ =
            if Atomic.get pending then begin
              Atomic.incr woken ;
              wake := Cell.Once.make () ;
              Atomic.set pending false
            end ;
            None
          in
          let flow =
            Flow.make ~time:(Fake.mono f)
              ~read:(fun _ _ _ -> failwith "blocking read")
              ~read_now
              ~write:(fun _ _ _ -> ())
              ~readable
              ~shutdown_send:(fun () -> failwith "shutdown_send")
              ~close:ignore ~set_read_deadline:ignore ~set_write_deadline:ignore
              ()
          in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              let ctx =
                { Engine.config= fake_config f
                ; conn
                ; stopping= Action.never
                ; is_stopping= (fun () -> false) }
              in
              let c =
                Fun.Async.call (fun () ->
                    Private.H1_server.serve ctx flow handler )
              in
              Stdlib.Fun.protect ~finally:(fun () -> Fun.Async.cancel c)
              @@ fun () ->
              Fake.wait_mono_timer f ~since:m (t0 +: header_h) ;
              Fake.advance f Affect_unix.Mtime.Span.s ;
              let m2 = Fake.mark f in
              Atomic.set pending true ;
              ignore (Cell.Once.try_set !wake ()) ;
              (* The engine woke, found nothing, and waits again: for the old
                 deadline, not for a new one 1 s later. *)
              Fake.wait_mono_timer f ~since:m2 (t0 +: header_h) ;
              equal int ~msg:"wakeups" 1 (Atomic.get woken) ;
              Fake.advance f Affect_unix.Mtime.Span.(header_h - s - ns) ;
              is_false ~msg:"ended 1 ns before" (Fake.ends_within_50ms c) ;
              Fake.advance f Affect_unix.Mtime.Span.ns ;
              get_2s c ) )
    ; test "a wakeup without input does not hold up a stopping server"
        (fun () ->
          (* The server stops when the flow finds nothing to read. *)
          let woken = ref false and stop = Cell.Once.make () in
          let flow =
            Flow.make ~time:Time.mono
              ~read:(fun _ _ _ -> failwith "blocking read")
              ~read_now:(fun _ _ _ ->
                woken := true ;
                ignore (Cell.Once.try_set stop ()) ;
                None )
              ~write:(fun _ _ _ -> ())
              ~readable:
                (Action.guard (fun () ->
                     if !woken then Action.never else Action.always () ) )
              ~shutdown_send:ignore ~close:ignore ~set_read_deadline:ignore
              ~set_write_deadline:ignore ()
          in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              let ctx =
                { Engine.config=
                    { Config.default with
                      header_timeout= Affect_unix.Mtime.Span.(60 * s) }
                ; conn
                ; stopping= Cell.Once.get' stop
                ; is_stopping= (fun () -> false) }
              in
              Server_support.within_2s (fun () ->
                  Private.H1_server.serve ctx flow handler ) ) ;
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
    ]

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
            { Config.default with
              date_header= false
            ; idle_timeout= Affect_unix.Mtime.Span.(10 * ms) }
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
          let f = Fake.make () in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let first = get "/slow" in
          expect
            (show ~watch:true ~stall:true ~config:(fake_config f)
               ~time:(Fake.mono f) ~chunk:(String.length first)
               ~drive:(ends_at f ~since:m ~t0 idle_i)
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
    ; test "a wakeup without input while the handler runs is not a disconnect"
        (fun () ->
          (* Once the request is read the client is quiet: the flow is readable
             once more with nothing to read, and a read would block, which
             fails. *)
          let f = Fake.make () in
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
            Flow.make ~time:(Fake.mono f) ~read ~read_now
              ~write:(fun b off len -> Buffer.add_subbytes out b off len)
              ~readable:
                (Action.guard (fun () ->
                     if quiet () && !woken then Action.never
                     else Action.always () ) )
              ~shutdown_send:ignore ~close:ignore ~set_read_deadline:ignore
              ~set_write_deadline:ignore ()
          in
          slow_outcome := "" ;
          let t0 = Fake.mono_now f and m = Fake.mark f in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              let ctx =
                { Engine.config=
                    {(fake_config f) with cancel_on_disconnect= true}
                ; conn
                ; stopping= Action.never
                ; is_stopping= (fun () -> false) }
              in
              let c =
                Fun.Async.call (fun () ->
                    Private.H1_server.serve ctx flow handler )
              in
              (* After the response, the connection is idle. *)
              Stdlib.Fun.protect
                ~finally:(fun () -> Fun.Async.cancel c)
                (fun () -> ends_at f ~since:m ~t0 idle_i c) ) ;
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

let () =
  exit
    (run "h1_server"
       [ connection_tests
       ; timeout_tests
       ; upgrade_tests
       ; disconnect_tests
       ; smuggling_tests ] )
