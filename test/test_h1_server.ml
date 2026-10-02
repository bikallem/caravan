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

let handler req =
  match Request.path_only req with
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
  | "/upgrade" -> Response.upgrade ignore
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
let serve ?(config = { Config.default with date_header = false }) ?chunk ?(stall = false) input
  =
  let errors = ref [] in
  let config = { config with on_error = (fun e _ -> errors := Printexc.to_string e :: !errors) } in
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
  Affect_unix.Unix.main ~domain_count:1 (fun () ->
      let ctx =
        { Engine.config; conn; stopping = Action.never; is_stopping = (fun () -> false) }
      in
      Private.H1_server.serve ctx flow handler);
  let unread = String.length input - !consumed in
  if unread > 0 then Printf.bprintf out "[unread %d]" unread;
  (Buffer.contents out, List.rev !errors)

let show ?config ?chunk ?stall input =
  let out, errors = serve ?config ?chunk ?stall input in
  show_crlf out ^ String.concat "" (List.map (fun e -> "\n! " ^ e) errors)

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
      test "upgrades are not supported yet" (fun () ->
          expect (show (get "/upgrade")) @@ __POS_OF__ {|
            HTTP/1.1 500 Internal Server Error\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 22\r\n
            Connection: close\r\n
            \r\n
            Internal Server Error
            [FIN]
            ! Failure("Response.upgrade is not supported yet")
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
            ignored[FIN][unread 79]
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

let () = exit (run "h1_server" [ connection_tests; timeout_tests ])
