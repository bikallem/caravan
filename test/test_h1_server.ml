(* The HTTP/1.1 engine over in-memory flows: whole connections, golden bytes. *)

open Windtrap
open Affect
open Caravan

let show_crlf s =
  let b = Buffer.create (String.length s) and n = String.length s in
  let rec loop i =
    if i < n
    then
      if s.[i] = '\r' && i + 1 < n && s.[i + 1] = '\n'
      then (Buffer.add_string b "\\r\\n\n"; loop (i + 2))
      else (Buffer.add_char b s.[i]; loop (i + 1))
  in
  loop 0;
  Buffer.contents b

let loopback port = Unix.ADDR_INET (Unix.inet_addr_loopback, port)
let conn = Conn_info.make ~id:7 ~peer:(loopback 50000) ~local:(loopback 8080) ()

(* After an upgrade: what the client sends, until it ends. *)
let echo_upgraded flow =
  let buf = Bytes.create 64 and got = Buffer.create 64 in
  let rec loop () =
    match Flow.read flow buf 0 64 with
    | 0 -> ()
    | n -> Buffer.add_subbytes got buf 0 n; loop ()
  in
  loop ();
  Flow.write_string flow ("got " ^ Buffer.contents got)

let upgrade_echo = Headers.of_list [ ("Upgrade", "echo") ]

(* How the last slow handler ended: finished or cancelled. *)
let slow_outcome = ref ""

(* Answers "late" after 50ms, having read the request body or not. *)
let slowly ~read req =
  match
    if read then ignore (Body.to_string (Request.body req));
    Affect_unix.Mtime.wait_for Affect_unix.Mtime.Span.(50 * ms)
  with
  | () -> slow_outcome := "[handler finished]"; Response.string "late"
  | exception Fun.Async.Cancelled ->
      slow_outcome := "[handler cancelled]";
      raise Fun.Async.Cancelled

(* Set by /stop, as a server stopping while its response is written. *)
let stopping = ref false

let handler req =
  if Method.equal (Request.meth req) Method.CONNECT
  then
    match Request.authority req with
    | Some "plain.example:443" -> Response.string "not a tunnel"
    | Some "denied.example:443" -> Response.string ~status:Status.forbidden "denied"
    | _ -> Response.upgrade ~status:Status.ok echo_upgraded
  else
  match Request.path_only req with
  | "/swallow" -> (
      (* A handler that answers a body it cannot read itself. *)
      match Body.to_string (Request.body req) with
      | s -> Response.string s
      | exception _ -> Response.string ~status:Status.bad_request "unreadable body\n")
  | "/early-hints" -> Response.make ~status:(Status.v 103) Body.empty
  | "/stream-fail" -> Response.stream (fun _ -> failwith "producer")
  (* Failures of the handler's own upstream, not of the request. *)
  | "/upstream-truncated" -> raise Body.Truncated
  | "/upstream-malformed" -> raise (Private.H1_error.E (Bad_request "upstream"))
  | "/upstream-body-truncated" ->
      let sent = ref false in
      Response.make
        (Body.of_source ~length:10 (fun buf off _ ->
             if !sent then raise Body.Truncated;
             sent := true;
             Bytes.blit_string "abc" 0 buf off 3;
             3))
  | "/endless" ->
      Response.stream (fun w ->
          Body.Writer.write_string w "first";
          Body.Writer.flush w;
          Affect_unix.Mtime.wait_for Affect_unix.Mtime.Span.(10 * s))
  | "/partial" ->
      Response.stream (fun w ->
          Body.Writer.write_string w "partial";
          Body.Writer.flush w;
          failwith "cut short")
  | "/slow" -> slowly ~read:false req
  | "/read-slow" -> slowly ~read:true req
  | "/slow-cleanup-fails" -> (
      try slowly ~read:false req with Fun.Async.Cancelled -> failwith "cleanup failed")
  | "/ws" -> Response.upgrade ~headers:upgrade_echo echo_upgraded
  | "/ws-bare" -> Response.upgrade echo_upgraded
  | "/ws-fail" -> Response.upgrade ~headers:upgrade_echo (fun _ -> failwith "ws")
  | "/tunnel" -> Response.upgrade ~status:Status.ok echo_upgraded
  | "/stop" ->
      Response.stream (fun w ->
          Body.Writer.write_string w "bye";
          (* After the head went out as keep-alive. *)
          Body.Writer.flush w;
          stopping := true)
  | "/echo" -> Response.string (Body.to_string (Request.body req))
  | "/stream-echo" -> Response.stream (Body.write_to (Request.body req))
  | "/ignore" -> Response.string "ignored"
  | "/stream" ->
      Response.stream (fun w ->
          Body.Writer.write_string w "a";
          Body.Writer.flush w;
          Body.Writer.write_string w "b")
  | "/fail" -> failwith "boom"
  | "/short" -> Response.stream ~length:5 (fun w -> Body.Writer.write_string w "abc")
  | "/close" -> Response.string ~headers:(Headers.of_list [ ("Connection", "close") ]) "bye"
  | "/empty" -> Response.empty ()
  | "/info" ->
      Response.string
        (Format.asprintf "%a %s %s %s %a %s" Method.pp (Request.meth req) (Request.scheme req)
           (Option.value ~default:"-" (Request.authority req))
           (Request.path req) Version.pp (Request.version req)
           (match Request.conn req with Some c -> Format.asprintf "%a" Conn_info.pp c | None -> "-"))
  | _ -> Response.string "hello"

(* Serves [input] on one connection. The bytes written, with [[FIN]] where
   the server half-closed and [[unread n]] if it left input unread, and the
   errors reported. At the end of [input] the client closes; with
   [~stall:true] it goes quiet instead (the flow is no longer readable and
   reads time out), and with [~reset:true] it resets the connection. With
   [~unwritable:true] every write fails as on a reset connection. With
   [~cancel_after] the connection is cancelled after that time, as by a
   server shutdown. With [~false_wakes:n] the flow is first readable [n]
   times with nothing to read yet, and a read that would block then fails. *)
let serve ?(config = { Config.default with date_header = false }) ?chunk ?(stall = false)
    ?(reset = false) ?(unwritable = false) ?(watch = false) ?cancel_after ?(false_wakes = 0)
    input =
  let errors = ref [] in
  (* The input's end looks like a client leaving: only watched for if asked. *)
  let config =
    { config with on_error = (fun e _ -> errors := Printexc.to_string e :: !errors);
                  cancel_on_disconnect = watch }
  in
  slow_outcome := "";
  let out = Buffer.create 256 and consumed = ref 0 in
  let inner = Flow.of_string ?chunk input out in
  let stalled () = stall && !consumed = String.length input in
  let false_wakes = ref false_wakes in
  let read buf off len =
    if !false_wakes > 0 then failwith "blocking read after a false wakeup";
    if stalled () then raise Flow.Timeout;
    if reset && !consumed = String.length input then raise Flow.Closed;
    let n = Flow.read inner buf off len in
    consumed := !consumed + n;
    n
  in
  let read_now buf off len =
    if !false_wakes > 0 then (decr false_wakes; None)
    else if stalled () then None
    else Some (read buf off len)
  in
  let readable =
    Action.guard (fun () ->
        if !false_wakes = 0 && stalled () then Action.never else Action.always ())
  in
  let flow =
    let write buf off len = if unwritable then raise Flow.Closed else Flow.write inner buf off len in
    Flow.make ~read ~read_now ~write ~readable
      ~shutdown_send:(fun () -> Buffer.add_string out "[FIN]")
      ~close:ignore
      ~reset:(fun () -> Buffer.add_string out "[RST]")
      ~set_read_deadline:ignore ~set_write_deadline:ignore ()
  in
  stopping := false;
  Affect_unix.Unix.main ~domain_count:1 (fun () ->
      let ctx =
        { Engine.config; conn; stopping = Action.never; is_stopping = (fun () -> !stopping) }
      in
      match cancel_after with
      | None -> Private.H1_server.serve ctx flow handler
      | Some span ->
          let c = Fun.Async.call (fun () -> Private.H1_server.serve ctx flow handler) in
          Affect_unix.Mtime.wait_for span;
          Fun.Async.cancel c;
          (try Fun.Async.get c with Fun.Async.Cancelled -> ()));
  let unread = String.length input - !consumed in
  if unread > 0 then Printf.bprintf out "[unread %d]" unread;
  (Buffer.contents out, List.rev !errors)

let show ?config ?chunk ?stall ?reset ?unwritable ?watch ?cancel_after ?false_wakes input =
  let out, errors =
    serve ?config ?chunk ?stall ?reset ?unwritable ?watch ?cancel_after ?false_wakes input
  in
  show_crlf out
  ^ String.concat "" (List.map (fun e -> "\n! " ^ e) errors)
  ^ if !slow_outcome = "" then "" else "\n" ^ !slow_outcome

let get ?(v = "HTTP/1.1") ?(headers = []) path =
  Printf.sprintf "GET %s %s\r\nHost: a.example\r\n%s\r\n" path v
    (String.concat "" (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") headers))

let post ?(headers = []) path body =
  Printf.sprintf "POST %s HTTP/1.1\r\nHost: a.example\r\nContent-Length: %d\r\n%s\r\n%s" path
    (String.length body)
    (String.concat "" (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") headers))
    body

let connection_tests =
  group "H1_server connections"
    [ test "a GET" (fun () -> expect (show (get "/")) @@ __POS_OF__ {|
      HTTP/1.1 200 OK\r\n
      Content-Type: text/plain; charset=utf-8\r\n
      Content-Length: 5\r\n
      \r\n
      hello
      |});
      test "pipelined requests are answered in order" (fun () ->
          expect (show ~chunk:7 (get "/" ^ get "/info" ^ get "/ignore")) @@ __POS_OF__ {|
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
            |});
      test "what the handler sees" (fun () -> expect (show (get "/info?q=1")) @@ __POS_OF__ {|
        HTTP/1.1 200 OK\r\n
        Content-Type: text/plain; charset=utf-8\r\n
        Content-Length: 74\r\n
        \r\n
        GET http a.example /info?q=1 HTTP/1.1 #7 127.0.0.1:50000 -> 127.0.0.1:8080
        |});
      test "HTTP/1.0 closes unless kept alive" (fun () ->
          expect
            (show (get ~v:"HTTP/1.0" "/" ^ get "/")
             ^ "\n--\n"
             ^ show (get ~v:"HTTP/1.0" ~headers:[ ("Connection", "keep-alive") ] "/" ^ get "/ignore"))
          @@ __POS_OF__ {|
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
            |});
      test "Connection: close from the client or the handler ends the connection" (fun () ->
          expect
            (show (get ~headers:[ ("Connection", "close") ] "/" ^ get "/ignore")
             ^ "\n--\n" ^ show (get "/close" ^ get "/ignore"))
          @@ __POS_OF__ {|
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
            |});
      test "request bodies: read, unread and drained, chunked" (fun () ->
          expect
            (show
               (post "/echo" "hello" ^ post "/ignore" "unread body" ^ get "/"
                ^ "POST /echo HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nabc\r\n2\r\nde\r\n0\r\n\r\n"))
          @@ __POS_OF__ {|
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
            |});
      test "streamed responses: chunked for 1.1, close-delimited for 1.0" (fun () ->
          expect (show (get "/stream" ^ get ~v:"HTTP/1.0" ~headers:[ ("Connection", "keep-alive") ] "/stream" ^ get "/"))
          @@ __POS_OF__ {|
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
            |});
      test "HEAD" (fun () ->
          expect (show ("HEAD / HTTP/1.1\r\nHost: a\r\n\r\n" ^ get "/ignore")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored
            |});
      test "a last response without a body is sent" (fun () ->
          expect
            (show ("HEAD / HTTP/1.1\r\nHost: a\r\nConnection: close\r\n\r\n")
             ^ "--\n"
             ^ show (get ~headers:[ ("Connection", "close") ] "/empty"))
          @@ __POS_OF__ {|
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
            |});
      test "malformed requests get 400 and close" (fun () ->
          expect (show ("GET / HTTP/1.1\r\n\r\n" ^ get "/")) @@ __POS_OF__ {|
            HTTP/1.1 400 Bad Request\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 12\r\n
            Connection: close\r\n
            \r\n
            Bad Request
            [FIN]
            |});
      test "a body over max_body gets 413" (fun () ->
          let config = { Config.default with date_header = false; max_body = Some 3 } in
          expect (show ~config (post "/echo" "hello")) @@ __POS_OF__ {|
            HTTP/1.1 413 Content Too Large\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 18\r\n
            Connection: close\r\n
            \r\n
            Content Too Large
            [FIN]
            |});
      test "100-continue is sent when the handler reads the body" (fun () ->
          expect
            (show (post ~headers:[ ("Expect", "100-continue") ] "/echo" "hello" ^ get "/ignore"))
          @@ __POS_OF__ {|
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
            |});
      test "an expectation other than 100-continue gets 417, without the handler" (fun () ->
          expect
            (String.concat "--"
               (List.map
                  (fun e -> show (get ~headers:[ ("Expect", e) ] "/fail"))
                  [ "foo"; "100-continue, foo"; "100-continue=1" ]))
          @@ __POS_OF__ {|
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
            |});
      test "error responses to HEAD have no body" (fun () ->
          expect
            (String.concat "--"
               [ show "HEAD / HTTP/1.1\r\nHost: a\r\nExpect: foo\r\n\r\n";
                 show "HEAD / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: gzip, chunked\r\n\r\n" ])
          @@ __POS_OF__ {|
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
            |});
      test "100-continue is case-insensitive" (fun () ->
          expect (show (post ~headers:[ ("Expect", "100-Continue") ] "/echo" "hi"))
          @@ __POS_OF__ {|
            HTTP/1.1 100 Continue\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 2\r\n
            \r\n
            hi
            |});
      test "100-continue from HTTP/1.0 is ignored" (fun () ->
          expect (show (get ~v:"HTTP/1.0" ~headers:[ ("Expect", "100-continue") ] "/"))
          @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            Connection: close\r\n
            \r\n
            hello[FIN]
            |});
      test "an unread chunked body over max_drain closes after the response" (fun () ->
          let config = { Config.default with date_header = false; max_drain = 10 } in
          let body = Printf.sprintf "64\r\n%s\r\n0\r\n\r\n" (String.make 100 'x') in
          expect
            (show ~config
               ("POST /ignore HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n" ^ body))
          @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored[FIN]
            |});
      test "100-continue is sent before the head of a response that streams the body" (fun () ->
          expect
            (show (post ~headers:[ ("Expect", "100-continue") ] "/stream-echo" "hello" ^ get "/"))
          @@ __POS_OF__ {|
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
            |});
      test "a close-delimited body cut short resets the connection" (fun () ->
          expect (show (get ~v:"HTTP/1.0" "/partial")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Connection: close\r\n
            \r\n
            partial[RST]
            ! Failure("cut short")
            |});
      test "a close-delimited echo of a request body cut short resets the connection" (fun () ->
          expect (show "POST /stream-echo HTTP/1.0\r\nContent-Length: 10\r\n\r\nhel")
          @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Connection: close\r\n
            \r\n
            hel[RST]
            |});
      test "a close-delimited body cut short by a failed write resets the connection" (fun () ->
          expect (show ~unwritable:true (get ~v:"HTTP/1.0" "/stream")) @@ __POS_OF__ {| [RST] |});
      test "a close-delimited body cut short by cancellation resets the connection" (fun () ->
          expect
            (show ~cancel_after:Affect_unix.Mtime.Span.(20 * ms) (get ~v:"HTTP/1.0" "/endless"))
          @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Connection: close\r\n
            \r\n
            first[RST]
            |});
      test "a chunked body cut short ends without a last chunk" (fun () ->
          expect (show (get "/partial")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            7\r\n
            partial\r\n
            [FIN]
            ! Failure("cut short")
            |});
      test "the handler's own body errors are server errors, not the client's" (fun () ->
          expect
            (String.concat "--"
               (List.map
                  (fun p -> show (get p))
                  [ "/upstream-truncated"; "/upstream-malformed"; "/upstream-body-truncated" ]))
          @@ __POS_OF__ {|
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
            |});
      test "a streamed body that fails before any output gets 500" (fun () ->
          expect (show (get "/stream-fail" ^ get "/")) @@ __POS_OF__ {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("producer")
            |});
      test "100-continue owed and never sent closes the connection" (fun () ->
          expect
            (show (post ~headers:[ ("Expect", "100-continue") ] "/ignore" "hello" ^ get "/"))
          @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            Connection: close\r\n
            \r\n
            ignored[FIN]
            |});
      test "100-continue with an empty body keeps the connection" (fun () ->
          expect
            (show (post ~headers:[ ("Expect", "100-continue") ] "/ignore" "" ^ get "/"))
          @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignoredHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |});
      test "a server that starts stopping takes no further request" (fun () ->
          expect (show (get "/stop" ^ get "/")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            3\r\n
            bye\r\n
            0\r\n
            \r\n
            [FIN]
            |});
      test "a stopping server ends an idle connection without reading" (fun () ->
          let flow =
            Flow.make
              ~read:(fun _ _ _ -> failwith "read while stopping")
              ~read_now:(fun _ _ _ -> failwith "read while stopping")
              ~write:(fun _ _ _ -> ()) ~readable:Action.never ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              let ctx =
                { Engine.config = Config.default; conn; stopping = Action.always ();
                  is_stopping = (fun () -> true) }
              in
              Private.H1_server.serve ctx flow handler));
      test "an error response to a client that left is dropped" (fun () ->
          let input = Flow.of_string "GET / HTTP/1.1\r\n\r\n" (Buffer.create 0) in
          let flow =
            Flow.make ~read:(Flow.read input) ~read_now:(Flow.read_now input)
              ~write:(fun _ _ _ -> raise Flow.Closed)
              ~readable:(Flow.readable' input ()) ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              let ctx =
                { Engine.config = Config.default; conn; stopping = Action.never;
                  is_stopping = (fun () -> false) }
              in
              Private.H1_server.serve ctx flow handler));
      test "a failing handler gets 500 and is reported" (fun () ->
          expect (show (get "/fail" ^ get "/")) @@ __POS_OF__ {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("boom")
            |});
      test "a body shorter than its length closes the connection" (fun () ->
          expect (show (get "/short" ^ get "/")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Length: 5\r\n
            \r\n
            abc[FIN]
            ! response body length differs from its Content-Length
            |});
      test "a closing response is followed by a half-close, and input is read to its end"
        (fun () ->
           expect
             (show
                (post ~headers:[ ("Connection", "close") ] "/ignore" (String.make 100 'x')
                 ^ get "/"))
           @@ __POS_OF__ {|
             HTTP/1.1 200 OK\r\n
             Content-Type: text/plain; charset=utf-8\r\n
             Content-Length: 7\r\n
             Connection: close\r\n
             \r\n
             ignored[FIN]
             |});
      test "lingering reads at most max_drain bytes" (fun () ->
          let config = { Config.default with date_header = false; max_drain = 10 } in
          expect (show ~config ~chunk:16 (post "/ignore" (String.make 100 'x'))) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored[FIN][unread 89]
            |});
      test "a client that leaves is not lingered on" (fun () ->
          expect (show (get "/" ^ "GET / HT")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |});
      test "a streamed request body cut short ends the response unreported" (fun () ->
          expect
            (show "POST /stream-echo HTTP/1.1\r\nHost: a\r\nContent-Length: 10\r\n\r\nhel")
          @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            3\r\n
            hel\r\n
            [FIN]
            |});
      test "responses get a Date field" (fun () ->
          let out, _ = serve ~config:Config.default (get "/") in
          let date =
            List.find (String.starts_with ~prefix:"Date: ") (String.split_on_char '\n' out)
          in
          (* An IMF-fixdate, such as "Sun, 06 Nov 1994 08:49:37 GMT", then CR. *)
          equal int (6 + 29 + 1) (String.length date);
          is_true ~msg:date (String.ends_with ~suffix:" GMT\r" date));
      test "error responses get a Date field too" (fun () ->
          (* RFC 9110 §6.6.1: a 4xx must have one when the server has a clock. *)
          let out, _ = serve ~config:Config.default "GET / HTTP/1.1\r\n\r\n" in
          is_true ~msg:out (String.starts_with ~prefix:"HTTP/1.1 400" out);
          let date =
            List.find (String.starts_with ~prefix:"Date: ") (String.split_on_char '\n' out)
          in
          equal int (6 + 29 + 1) (String.length date));
      test "a connection reset during the request body is not a handler error" (fun () ->
          expect
            (show ~reset:true "POST /echo HTTP/1.1\r\nHost: a\r\nContent-Length: 10\r\n\r\nhel")
          @@ __POS_OF__ {||});
      test "a client that cannot take its 100 Continue is not a handler error" (fun () ->
          expect (show ~unwritable:true (post ~headers:[ ("Expect", "100-continue") ] "/echo" "hello"))
          @@ __POS_OF__ {||});
      test "an https target over plain HTTP gets 421" (fun () ->
          expect (show "GET https://a.example/info HTTP/1.1\r\nHost: a.example\r\n\r\n")
          @@ __POS_OF__ {|
            HTTP/1.1 421 Misdirected Request\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 20\r\n
            Connection: close\r\n
            \r\n
            Misdirected Request
            [FIN]
            |});
      test "a request cut short gets no response" (fun () ->
          expect
            (show "GET / HTTP/1.1\r\nHost: a"
             ^ "|"
             ^ show "POST /echo HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\nhel")
          @@ __POS_OF__ {| | |}) ]

let timeout_tests =
  let quick =
    { Config.default with date_header = false;
                          header_timeout = Affect_unix.Mtime.Span.(10 * ms);
                          idle_timeout = Affect_unix.Mtime.Span.(10 * ms) }
  in
  let body_10 path = Printf.sprintf "POST %s HTTP/1.1\r\nHost: a\r\nContent-Length: 10\r\n\r\nhel" path in
  group "H1_server timeouts"
    [ test "a request head that is late gets 408" (fun () ->
          expect (show ~stall:true "GET / HTTP/1.1\r\nHost: a") @@ __POS_OF__ {|
            HTTP/1.1 408 Request Timeout\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 16\r\n
            Connection: close\r\n
            \r\n
            Request Timeout
            [FIN]
            |});
      test "a request body that is late gets 408" (fun () ->
          expect (show ~stall:true (body_10 "/echo")) @@ __POS_OF__ {|
            HTTP/1.1 408 Request Timeout\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 16\r\n
            Connection: close\r\n
            \r\n
            Request Timeout
            [FIN]
            |});
      test "a body that is late once the response started ends it" (fun () ->
          expect (show ~stall:true (body_10 "/stream-echo")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            3\r\n
            hel\r\n
            [FIN]
            |});
      test "an unread body that is late when drained closes" (fun () ->
          expect (show ~stall:true (body_10 "/ignore")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 7\r\n
            \r\n
            ignored[FIN]
            |});
      test "a new connection that sends nothing is closed" (fun () ->
          expect (show ~config:quick ~stall:true "") @@ __POS_OF__ {||});
      test "an idle connection is closed" (fun () ->
          expect (show ~config:quick ~stall:true (get "/")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |});
      (* Readiness can be stale: waking without input must neither block in a
         read nor give up on the connection. *)
      test "a wakeup without input keeps waiting for the request" (fun () ->
          expect (show ~false_wakes:2 (get "/")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |});
      test "a wakeup without input leaves the timeout running" (fun () ->
          expect (show ~config:quick ~stall:true ~false_wakes:2 "") @@ __POS_OF__ {||});
      test ~timeout:5. "a wakeup without input does not hold up a stopping server" (fun () ->
          let woken = ref false in
          let flow =
            Flow.make
              ~read:(fun _ _ _ -> failwith "blocking read")
              ~read_now:(fun _ _ _ -> woken := true; None)
              ~write:(fun _ _ _ -> ())
              ~readable:(Action.guard (fun () -> if !woken then Action.never else Action.always ()))
              ~shutdown_send:ignore ~close:ignore ~set_read_deadline:ignore
              ~set_write_deadline:ignore ()
          in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              let ctx =
                { Engine.config = { Config.default with header_timeout = Affect_unix.Mtime.Span.(60 * s) };
                  conn;
                  stopping = Affect_unix.Mtime.wait_for' Affect_unix.Mtime.Span.(20 * ms) ();
                  is_stopping = (fun () -> false) }
              in
              Private.H1_server.serve ctx flow handler);
          is_true ~msg:"woken" !woken) ]

let upgrade_tests =
  let ws ?(v = "HTTP/1.1") ?(headers = [ ("Upgrade", "echo"); ("Connection", "Upgrade") ]) path =
    get ~v ~headers path
  in
  group "H1_server upgrades"
    [ test "101 hands over the connection, with the bytes already received" (fun () ->
          expect (show (ws "/ws" ^ "early bytes, " ^ "then more")) @@ __POS_OF__ {|
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n
            got early bytes, then more
            |});
      test "the bytes after the head are the same however the input is split" (fun () ->
          let input = ws "/ws" ^ String.init 300 (fun i -> Char.chr (97 + (i mod 26))) in
          equal string (show input) (show ~chunk:7 input));
      test "a request body is read before switching" (fun () ->
          expect
            (show
               ("GET /ws HTTP/1.1\r\nHost: a\r\nUpgrade: echo\r\nConnection: Upgrade\r\n\
                 Content-Length: 4\r\n\r\nbodyafter"))
          @@ __POS_OF__ {|
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n
            got after
            |});
      test "CONNECT 2xx opens a tunnel" (fun () ->
          expect (show "CONNECT a.example:443 HTTP/1.1\r\nHost: a.example:443\r\n\r\nhello")
          @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            \r\n
            got hello
            |});
      test "upgrades that break the rules get 500 and are reported" (fun () ->
          expect
            (String.concat "--"
               [ show (get "/ws");
                 show (ws ~v:"HTTP/1.0" "/ws");
                 show (ws ~headers:[ ("Upgrade", "echo") ] "/ws");
                 show (ws "/ws-bare");
                 show (get "/tunnel") ])
          @@ __POS_OF__ {|
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
            |});
      test "a 2xx to CONNECT that is not an upgrade gets 500, and no tunnel" (fun () ->
          expect
            (show
               "CONNECT plain.example:443 HTTP/1.1\r\nHost: plain.example:443\r\n\r\n\
                GET /inside HTTP/1.1\r\nHost: a\r\n\r\n")
          @@ __POS_OF__ {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("a 2xx response to CONNECT must be a Response.upgrade")
            |});
      test "a CONNECT refused with 403 is an ordinary response" (fun () ->
          expect
            (show
               ("CONNECT denied.example:443 HTTP/1.1\r\nHost: denied.example:443\r\n\r\n"
                ^ get "/"))
          @@ __POS_OF__ {|
            HTTP/1.1 403 Forbidden\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 6\r\n
            \r\n
            deniedHTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            hello
            |});
      test "a handler answering with a 1xx gets 500" (fun () ->
          expect (show (get "/early-hints")) @@ __POS_OF__ {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Invalid_argument("Response.make: 103 is not a final status")
            |});
      test "an upgrade with a body too large to drain gets 500" (fun () ->
          let config = { Config.default with date_header = false; max_drain = 2 } in
          expect
            (show ~config
               ("GET /ws HTTP/1.1\r\nHost: a\r\nUpgrade: echo\r\nConnection: Upgrade\r\n\
                 Content-Length: 4\r\n\r\nbody"))
          @@ __POS_OF__ {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            |});
      test "an upgrade callback that raises is reported" (fun () ->
          expect (show (ws "/ws-fail")) @@ __POS_OF__ {|
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: echo\r\n
            Connection: Upgrade\r\n
            \r\n

            ! Failure("ws")
            |}) ]

let disconnect_tests =
  let quick =
    { Config.default with date_header = false; idle_timeout = Affect_unix.Mtime.Span.(10 * ms) }
  in
  group "H1_server client disconnects"
    [ test "a client that leaves cancels its handler" (fun () ->
          expect (show ~watch:true (get "/slow")) @@ __POS_OF__ {| [handler cancelled] |});
      test "without cancel_on_disconnect the handler finishes" (fun () ->
          expect (show (get "/slow")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 4\r\n
            \r\n
            late
            [handler finished]
            |});
      test "a failing handler is still answered while the connection is watched" (fun () ->
          let config = { Config.default with date_header = false; idle_timeout = Affect_unix.Mtime.Span.(10 * ms) } in
          expect (show ~watch:true ~stall:true ~config (get "/fail")) @@ __POS_OF__ {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("boom")
            |});
      test "a request arriving during the handler is not a disconnect" (fun () ->
          let first = get "/slow" in
          expect
            (show ~watch:true ~stall:true ~config:quick ~chunk:(String.length first)
               (first ^ get "/"))
          @@ __POS_OF__ {|
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
            |});
      test "a request with an empty body is watched at once" (fun () ->
          expect (show ~watch:true (post "/slow" "")) @@ __POS_OF__ {| [handler cancelled] |});
      test "nothing is watched while the request body is unread" (fun () ->
          expect (show ~watch:true (post "/slow" "hello")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 4\r\n
            \r\n
            late
            [handler finished]
            |});
      test "a handler that fails while cancelled is reported" (fun () ->
          expect (show ~watch:true (get "/slow-cleanup-fails")) @@ __POS_OF__ {|
            ! Failure("cleanup failed")
            [handler cancelled]
            |});
      test "a client that leaves after its body was read cancels its handler" (fun () ->
          expect (show ~watch:true (post "/read-slow" "hello")) @@ __POS_OF__ {| [handler cancelled] |});
      test "a wakeup without input while the handler runs is not a disconnect" (fun () ->
          (* Once the request is read the client is quiet: the flow is
             readable once more with nothing to read, and a read would
             block, which fails. *)
          let input = get "/slow" and out = Buffer.create 64 in
          let pos = ref 0 and woken = ref false in
          let quiet () = !pos = String.length input in
          let read buf off len =
            if quiet () then failwith "blocking read";
            let n = Int.min len (String.length input - !pos) in
            Bytes.blit_string input !pos buf off n;
            pos := !pos + n;
            n
          in
          let read_now buf off len =
            if quiet () then (woken := true; None) else Some (read buf off len)
          in
          let flow =
            Flow.make ~read ~read_now
              ~write:(fun b off len -> Buffer.add_subbytes out b off len)
              ~readable:
                (Action.guard (fun () ->
                     if quiet () && !woken then Action.never else Action.always ()))
              ~shutdown_send:ignore ~close:ignore ~set_read_deadline:ignore
              ~set_write_deadline:ignore ()
          in
          slow_outcome := "";
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              let ctx =
                { Engine.config = { quick with cancel_on_disconnect = true }; conn;
                  stopping = Action.never; is_stopping = (fun () -> false) }
              in
              Private.H1_server.serve ctx flow handler);
          is_true ~msg:"woken" !woken;
          expect (show_crlf (Buffer.contents out) ^ "\n" ^ !slow_outcome) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 4\r\n
            \r\n
            late
            [handler finished]
            |}) ]

(* A request smuggled in a malformed chunked body must never be served, even
   when the handler swallows the body error and answers. *)
let smuggling_tests =
  let chunked_post body =
    "POST /swallow HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n" ^ body
  in
  let smuggled = "GET /smuggled HTTP/1.1\r\nHost: a\r\n\r\n" in
  let config = { Config.default with date_header = false; max_body = Some 100 } in
  let contains s sub =
    let n = String.length s and m = String.length sub in
    let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
    at 0
  in
  (* The ways a chunked body can go wrong in the middle, each followed by
     what an attacker wants read as the next request. *)
  let malformed =
    Gen.of_list
      ~pp:(fun ppf s -> Format.fprintf ppf "%S" s)
      [ "zz\r\n0\r\n\r\n" ^ smuggled;
        "-1\r\n0\r\n\r\n" ^ smuggled;
        "3\r\nabcX0\r\n\r\n" ^ smuggled;
        "3;=\r\nabc\r\n0\r\n\r\n" ^ smuggled;
        "1000000000000000\r\n0\r\n\r\n" ^ smuggled;
        (* Over max_body: the data is the smuggled request. *)
        Printf.sprintf "%x\r\n0\r\n\r\n%s%s\r\n0\r\n\r\n" 200 smuggled
          (String.make (200 - 5 - String.length smuggled) 'x') ]
  in
  group "H1_server smuggling"
    [ test "a bad chunk size line ends the connection after the handler's answer" (fun () ->
          expect (show (chunked_post ("zz\r\n0\r\n\r\n" ^ smuggled))) @@ __POS_OF__ {|
            HTTP/1.1 400 Bad Request\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 16\r\n
            Connection: close\r\n
            \r\n
            unreadable body
            [FIN]
            |});
      test "a chunk over max_body ends the connection after the handler's answer" (fun () ->
          let data = "0\r\n\r\n" ^ smuggled in
          let body =
            Printf.sprintf "c8\r\n%s%s\r\n0\r\n\r\n" data (String.make (200 - String.length data) 'x')
          in
          expect (show ~config (chunked_post body)) @@ __POS_OF__ {|
            HTTP/1.1 400 Bad Request\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 16\r\n
            Connection: close\r\n
            \r\n
            unreadable body
            [FIN]
            |});
      prop "no request smuggled in a malformed chunked body is ever served"
        Gen.(triple (list ~size:(int_range 0 3) (int_range 1 9)) malformed (int_range 1 64))
        (fun (sizes, bad, chunk) ->
           let valid =
             String.concat "" (List.map (fun n -> Printf.sprintf "%x\r\n%s\r\n" n (String.make n 'x')) sizes)
           in
           let out, _ = serve ~config ~chunk (chunked_post (valid ^ bad)) in
           (* The default handler would answer /smuggled with "hello". *)
           is_true ~msg:out (not (contains out "hello"))) ]

let () =
  exit
    (run "h1_server"
       [ connection_tests; timeout_tests; upgrade_tests; disconnect_tests; smuggling_tests ])
