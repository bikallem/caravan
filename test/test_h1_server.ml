(* The HTTP/1.1 engine over in-memory flows: whole connections, golden bytes. *)

open Windtrap
open Affect
open Affect_http

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
  then Response.upgrade ~status:Status.ok echo_upgraded
  else
  match Request.path_only req with
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
          stopping := true;
          Body.Writer.write_string w "bye")
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
   errors reported. With [~stall:true] the client goes quiet at the end of
   [input] instead of closing: the flow is no longer readable and reads time
   out. *)
let serve ?(config = { Config.default with date_header = false }) ?chunk ?(stall = false)
    ?(watch = false) input =
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
  let read buf off len =
    if stalled () then raise Flow.Timeout;
    let n = Flow.read inner buf off len in
    consumed := !consumed + n;
    n
  in
  let readable = Action.guard (fun () -> if stalled () then Action.never else Action.always ()) in
  let flow =
    Flow.make ~read ~write:(Flow.write inner) ~readable
      ~shutdown_send:(fun () -> Buffer.add_string out "[FIN]")
      ~close:ignore ~set_read_deadline:ignore ~set_write_deadline:ignore
  in
  stopping := false;
  Affect_unix.Unix.main ~domain_count:1 (fun () ->
      let ctx =
        { Engine.config; conn; stopping = Action.never; is_stopping = (fun () -> !stopping) }
      in
      Private.H1_server.serve ctx flow handler);
  let unread = String.length input - !consumed in
  if unread > 0 then Printf.bprintf out "[unread %d]" unread;
  (Buffer.contents out, List.rev !errors)

let show ?config ?chunk ?stall ?watch input =
  let out, errors = serve ?config ?chunk ?stall ?watch input in
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
              ~write:(fun _ _ _ -> ()) ~readable:Action.never ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore
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
            Flow.make ~read:(Flow.read input)
              ~write:(fun _ _ _ -> raise Flow.Closed)
              ~readable:(Flow.readable' input ()) ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore
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
            ignored
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
            |}) ]

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
          expect (show ~watch:true (post "/read-slow" "hello")) @@ __POS_OF__ {| [handler cancelled] |}) ]

let () =
  exit (run "h1_server" [ connection_tests; timeout_tests; upgrade_tests; disconnect_tests ])
