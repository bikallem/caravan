open Windtrap
open Affect
open Caravan
open Caravan.Private
open Caravan_client.Private
module Mtime = Affect_unix.Mtime
module Client_config = Caravan_client.Config

let ms n = Mtime.Span.(n * ms)
let elapsed_ms c = Mtime.Span.to_float_ns (Mtime.count c) /. 1e6
let within_2s = Server_support.within_2s
let server_limits = H1_server.limits Config.default

let req ?(meth = Method.GET) ?(headers = []) ?body path =
  Request.make ~authority:"a.example" ~headers:(Headers.of_list headers) ?body meth path

let body r = match Response.kind r with Response.Body b -> b | Upgrade _ -> fail "an upgrade"
let status r = Status.code (Response.status r)

let error_text f =
  match f () with
  | _ -> "no error"
  | exception Caravan_client.Error e -> Printexc.to_string (Caravan_client.Error e)
  | exception e -> Printexc.to_string e

(* One connection: [server] is the flow of a scripted server, [run] sends
   a request on the other end. The release records [reusable], and closes
   a connection that is not reusable, as the pool does. *)
type t = {
  server : Flow.t;
  run : ?config:Client_config.t -> Request.t -> Response.t;
  releases : unit -> bool list;
  writers : Writers.t;
}

let with_conn f =
  Server_support.with_flows @@ fun server client ->
  let writers = Writers.make () in
  Stdlib.Fun.protect ~finally:(fun () -> Writers.stop writers; Writers.wait writers) @@ fun () ->
  let conn = Conn.make ~host:"a.example" ~port:80 client in
  let releases = ref [] in
  let on_release (c : Conn.t) ~reusable =
    releases := reusable :: !releases;
    if not reusable then Flow.close c.flow
  in
  let run ?(config = Client_config.default) r = Exchange.run config writers conn ~on_release r in
  f { server; run; releases = (fun () -> List.rev !releases); writers }

(* The scripted server, in a child call: [script r] gets a reader on the
   server flow. *)
let serve t script = Fun.Async.call (fun () -> script (Buf_reader.of_flow t.server))

(* The end of the scripted server, with a guard. *)
let await s = within_2s (fun () -> Fun.Async.get s)

let read_head r = H1_parse.request_head server_limits r

(* Reads a request and its body. *)
let read_request r =
  let h = read_head r in
  let b = H1_body.body (H1_body.make ~limits:server_limits r (H1_parse.request_framing h)) in
  (h, Body.to_string b)

let respond t s = Flow.write_string t.server s

(* A body that writes chunks of 64 KiB until a write raises. Then it
   sets [raised] and raises again. *)
let endless raised =
  let chunk = String.make 65536 'x' in
  Body.of_writer (fun w ->
      try
        while true do
          Body.Writer.write_string w chunk;
          Body.Writer.flush w
        done
      with e -> Cell.Once.try_set raised (); raise e)

let exchange_tests =
  group "Exchange"
    [ test "a GET gets the response and its body" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello")
          in
          let resp = t.run (req "/") in
          equal int 200 (status resp);
          equal string "hello" (Body.to_string (body resp));
          await s;
          equal (list bool) [ true ] (t.releases ()));
      test "when a connection is reusable" (fun () ->
          let case ?(meth = Method.GET) ?(headers = []) ?(read = true) ?(close = false) response =
            with_conn @@ fun t ->
            let s =
              serve t (fun r ->
                  ignore (read_request r);
                  respond t response;
                  if close then Flow.shutdown_send t.server)
            in
            let resp = t.run (req ~meth ~headers "/") in
            (if read then ignore (error_text (fun () -> Body.to_string (body resp)))
             else if meth <> Method.HEAD then Body.close (body resp));
            await s;
            match t.releases () with [ b ] -> string_of_bool b | l -> Printf.sprintf "%d releases" (List.length l)
          in
          let cl = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" in
          expect
            (String.concat "\n"
               [ "CL body read => " ^ case cl; "CL body closed => " ^ case ~read:false cl;
                 "chunked => " ^ case "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n2\r\nok\r\n0\r\n\r\n";
                 "until close => " ^ case ~close:true "HTTP/1.1 200 OK\r\n\r\nok";
                 "response Connection: close => "
                 ^ case "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: 2\r\n\r\nok";
                 "request Connection: close => " ^ case ~headers:[ ("Connection", "close") ] cl;
                 "HTTP/1.0 with keep-alive => "
                 ^ case "HTTP/1.0 200 OK\r\nConnection: keep-alive\r\nContent-Length: 2\r\n\r\nok";
                 "a malformed chunk => " ^ case "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\n";
                 "one byte after the body => " ^ case (cl ^ "X");
                 "HEAD, no read => "
                 ^ case ~meth:Method.HEAD ~read:false "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n" ])
          @@ __POS_OF__ {|
            CL body read => true
            CL body closed => false
            chunked => true
            until close => false
            response Connection: close => false
            request Connection: close => false
            HTTP/1.0 with keep-alive => false
            a malformed chunk => false
            one byte after the body => false
            HEAD, no read => true
            |});
      test "a 100 and a 103 are skipped" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                respond t
                  "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 103 Early Hints\r\nLink: </a>\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
          in
          equal int 200 (status (t.run (req "/")));
          await s);
      test "eleven 1xx responses give Bad_response" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                respond t (String.concat "" (List.init 11 (fun _ -> "HTTP/1.1 100 Continue\r\n\r\n"))))
          in
          equal string "Caravan_client.Error: bad response: 400 Bad Request (more than 10 interim responses)"
            (within_2s (fun () -> error_text (fun () -> t.run (req "/"))));
          await s);
      test "a 413 with Connection: close stops the upload, and its body arrives" (fun () ->
          with_conn @@ fun t ->
          let raised = Cell.Once.make () in
          let s =
            serve t (fun r ->
                ignore (read_head r);
                ignore (Buf_reader.read_into r (Bytes.create 1024) 0 1024);
                respond t "HTTP/1.1 413 Content Too Large\r\nConnection: close\r\nContent-Length: 9\r\n\r\n";
                within_2s (fun () -> Cell.Once.get raised);
                respond t "too large")
          in
          let resp = t.run (req ~meth:Method.POST ~body:(endless raised) "/") in
          equal int 413 (status resp);
          within_2s (fun () -> Cell.Once.get raised);
          equal string "too large" (Body.to_string (body resp));
          await s;
          equal (list bool) [ false ] (t.releases ()));
      test "a streamed 2xx before the end of the upload does not stop it" (fun () ->
          with_conn @@ fun t ->
          let data = String.init (8 * 1024 * 1024) (fun i -> Char.chr (i land 0xff)) in
          let s =
            serve t (fun r ->
                let h = read_head r in
                respond t "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n";
                let b = H1_body.body (H1_body.make ~limits:server_limits r (H1_parse.request_framing h)) in
                let buf = Bytes.create 65536 in
                let rec echo () =
                  match Body.read b buf 0 65536 with
                  | 0 -> respond t "0\r\n\r\n"
                  | n ->
                      respond t (Printf.sprintf "%x\r\n%s\r\n" n (Bytes.sub_string buf 0 n));
                      echo ()
                in
                echo ())
          in
          let upload =
            Body.of_writer (fun w ->
                let rec go i =
                  if i < String.length data
                  then begin
                    let n = Int.min 65536 (String.length data - i) in
                    Body.Writer.write_string w (String.sub data i n);
                    Body.Writer.flush w;
                    go (i + n)
                  end
                in
                go 0)
          in
          let got =
            Server_support.within_2s (fun () ->
                let resp = t.run (req ~meth:Method.POST ~body:upload "/") in
                Body.to_string (body resp))
          in
          is_true ~msg:"the echo is equal" (got = data);
          await s);
      test "a response head that does not come gives Timeout `Response" (fun () ->
          with_conn @@ fun t ->
          let s = serve t (fun r -> ignore (read_request r)) in
          let config = { Client_config.default with response_timeout = ms 100 } in
          let c = Mtime.counter () in
          equal string "Caravan_client.Error: response timeout"
            (within_2s (fun () -> error_text (fun () -> t.run ~config (req "/"))));
          let took = elapsed_ms c in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took >= 90.);
          await s);
      test "the end of the input before the head gives Closed" (fun () ->
          with_conn @@ fun t ->
          let s = serve t (fun r -> ignore (read_request r); Flow.close t.server) in
          equal string "Caravan_client.Error: the connection closed before the response"
            (within_2s (fun () -> error_text (fun () -> t.run (req "/"))));
          await s);
      test "the end of the input after a part of a head gives Bad_response" (fun () ->
          let ended_after part =
            with_conn @@ fun t ->
            let s = serve t (fun r -> ignore (read_request r); respond t part; Flow.close t.server) in
            let e = within_2s (fun () -> error_text (fun () -> t.run (req "/"))) in
            await s;
            e
          in
          equal string "Caravan_client.Error: bad response: the connection ended in a response head"
            (ended_after "HTTP/1.1 20");
          equal string "Caravan_client.Error: bad response: the connection ended in a response head"
            (ended_after "HTTP/1.1 100 Continue\r\n\r\n"));
      test "a reset before the head gives Closed" (fun () ->
          with_conn @@ fun t ->
          let s = serve t (fun r -> ignore (read_request r); Flow.reset t.server) in
          equal string "Caravan_client.Error: the connection closed before the response"
            (within_2s (fun () -> error_text (fun () -> t.run (req "/"))));
          await s);
      test "a slow body read gives Timeout `Read, and releases the connection" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab")
          in
          let config = { Client_config.default with read_timeout = ms 100 } in
          let resp = t.run ~config (req "/") in
          let b = body resp in
          equal int 2 (Body.read b (Bytes.create 8) 0 8);
          equal string "Caravan_client.Error: read timeout"
            (within_2s (fun () -> error_text (fun () -> Body.read b (Bytes.create 8) 0 8)));
          equal (list bool) [ false ] (t.releases ());
          await s);
      test "a cancel of the caller closes the connection" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab";
                (* The end of the stream comes when the client closes. *)
                within_2s (fun () -> ignore (Buf_reader.peek_eof r)))
          in
          let reading = Cell.Once.make () in
          let caller =
            Fun.Async.call (fun () ->
                let b = body (t.run (req "/")) in
                ignore (Body.read b (Bytes.create 8) 0 8);
                Cell.Once.try_set reading ();
                Body.to_string b)
          in
          within_2s (fun () -> Cell.Once.get reading);
          Fun.Async.cancel caller;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get caller);
          equal (list bool) [ false ] (t.releases ());
          await s);
      test "a 101 gives Unsupported" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                respond t "HTTP/1.1 101 Switching Protocols\r\nUpgrade: x\r\nConnection: upgrade\r\n\r\n";
                within_2s (fun () -> ignore (Buf_reader.peek_eof r)))
          in
          equal string "Caravan_client.Error: unsupported: protocol switch"
            (error_text (fun () -> t.run (req "/")));
          await s;
          equal (list bool) [ false ] (t.releases ()));
      test "the response deadline starts at the end of the upload" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n")
          in
          let slow =
            Body.of_writer (fun w ->
                for _ = 1 to 3 do
                  Body.Writer.write_string w "abc";
                  Body.Writer.flush w;
                  Mtime.wait_for (ms 100)
                done)
          in
          let config = { Client_config.default with response_timeout = ms 150 } in
          equal int 200 (status (t.run ~config (req ~meth:Method.POST ~body:slow "/")));
          await s);
      test "a failure of the producer goes to the caller at once" (fun () ->
          with_conn @@ fun t ->
          let s = serve t (fun r -> ignore (error_text (fun () -> read_request r))) in
          let failing =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc";
                Body.Writer.flush w;
                failwith "x")
          in
          let config = { Client_config.default with response_timeout = ms 10_000 } in
          equal string "Failure(\"x\")"
            (within_2s (fun () -> error_text (fun () -> t.run ~config (req ~meth:Method.POST ~body:failing "/"))));
          equal (list bool) [ false ] (t.releases ());
          await s);
      test "a response head that comes slowly gives Timeout `Response" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                try
                  String.iter
                    (fun c ->
                       respond t (String.make 1 c);
                       Mtime.wait_for (ms 50))
                    "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"
                with Flow.Closed -> ())
          in
          let config = { Client_config.default with response_timeout = ms 200 } in
          equal string "Caravan_client.Error: response timeout"
            (within_2s (fun () -> error_text (fun () -> t.run ~config (req "/"))));
          await s);
      test "a 431 during a large request head, and its body arrives" (fun () ->
          with_conn @@ fun t ->
          let returned = Cell.Once.make () in
          let s =
            serve t (fun _ ->
                ignore (Flow.read t.server (Bytes.create 1024) 0 1024);
                respond t "HTTP/1.1 431 Too Large\r\nConnection: close\r\nContent-Length: 7\r\n\r\n";
                within_2s (fun () -> Cell.Once.get returned);
                respond t "too big")
          in
          let resp = t.run (req ~headers:[ ("X-Big", String.make (8 * 1024 * 1024) 'b') ] "/") in
          Cell.Once.try_set returned ();
          equal int 431 (status resp);
          equal string "too big" (Body.to_string (body resp));
          await s;
          equal (list bool) [ false ] (t.releases ()));
      test "a truncated body gives Truncated" (fun () ->
          let truncated ~reset response =
            with_conn @@ fun t ->
            let s =
              serve t (fun r ->
                  ignore (read_request r);
                  respond t response;
                  if reset then Flow.reset t.server else Flow.close t.server)
            in
            let resp = t.run (req "/") in
            let e = error_text (fun () -> Body.to_string (body resp)) in
            await s;
            e
          in
          equal string "Caravan_client.Error: the response body is truncated"
            (truncated ~reset:false "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab");
          equal string "Caravan_client.Error: the response body is truncated"
            (truncated ~reset:true "HTTP/1.1 200 OK\r\n\r\nab"));
      test "trailers come after a chunked body" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                respond t "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n2\r\nok\r\n0\r\nX-T: 1\r\n\r\n")
          in
          let b = body (t.run (req "/")) in
          equal string "ok" (Body.to_string b);
          equal (list (pair string string)) [ ("X-T", "1") ] (Headers.to_list (Body.trailers b));
          await s);
      test "an empty body releases the connection at once" (fun () ->
          let released ?(meth = Method.GET) response =
            with_conn @@ fun t ->
            let s = serve t (fun r -> ignore (read_request r); respond t response) in
            ignore (t.run (req ~meth "/"));
            await s;
            t.releases ()
          in
          List.iter
            (fun (meth, r) -> equal (list bool) [ true ] (released ~meth r))
            Method.
              [ (HEAD, "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n"); (GET, "HTTP/1.1 204 No Content\r\n\r\n");
                (GET, "HTTP/1.1 304 Not Modified\r\nContent-Length: 5\r\n\r\n");
                (GET, "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n") ]);
      test "a response that ends before the upload waits for the upload" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                let h = read_head r in
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok";
                (* The server keeps the connection, thus it reads the rest. *)
                ignore
                  (Body.to_string (H1_body.body (H1_body.make ~limits:server_limits r (H1_parse.request_framing h)))))
          in
          let slow =
            Body.of_writer (fun w ->
                for _ = 1 to 3 do
                  Body.Writer.write_string w "abc";
                  Body.Writer.flush w;
                  Mtime.wait_for (ms 50)
                done)
          in
          let resp = t.run (req ~meth:Method.POST ~body:slow "/") in
          equal string "ok" (within_2s (fun () -> Body.to_string (body resp)));
          equal (list bool) [ true ] (t.releases ());
          await s);
      test "Body.close before the end stops the upload" (fun () ->
          with_conn @@ fun t ->
          let raised = Cell.Once.make () in
          let s =
            serve t (fun r ->
                ignore (read_head r);
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n")
          in
          let resp = t.run (req ~meth:Method.POST ~body:(endless raised) "/") in
          within_2s (fun () -> Body.close (body resp));
          within_2s (fun () -> Cell.Once.get raised);
          equal (list bool) [ false ] (t.releases ());
          await s);
      test "a Body.close after the end does nothing" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok")
          in
          let b = body (t.run (req "/")) in
          equal string "ok" (Body.to_string b);
          equal int 0 (Body.read b (Bytes.create 4) 0 4);
          Body.close b;
          equal (list bool) [ true ] (t.releases ());
          await s);
      test "a bad head from a peer that does not read" (fun () ->
          with_conn @@ fun t ->
          let raised = Cell.Once.make () in
          let s = serve t (fun r -> ignore (read_head r); respond t "HTTP/1.1 2x0 X\r\n\r\n") in
          equal string "Caravan_client.Error: bad response: 400 Bad Request (invalid status code \"2x0\")"
            (within_2s (fun () ->
                 error_text (fun () -> t.run (req ~meth:Method.POST ~body:(endless raised) "/"))));
          within_2s (fun () -> Cell.Once.get raised);
          equal (list bool) [ false ] (t.releases ());
          await s);
      test "a peer that never reads gives Timeout `Write" (fun () ->
          with_conn @@ fun t ->
          let config = { Client_config.default with write_timeout = ms 100 } in
          equal string "Caravan_client.Error: write timeout"
            (within_2s (fun () ->
                 error_text (fun () ->
                     t.run ~config (req ~meth:Method.POST ~body:(endless (Cell.Once.make ())) "/")))));
      test "a wrong length gives Invalid_argument" (fun () ->
          with_conn @@ fun t ->
          let s = serve t (fun r -> ignore (error_text (fun () -> read_request r))) in
          let pos = ref 0 in
          let short =
            Body.of_source ~length:5 (fun buf off _ ->
                if !pos = 3 then 0 else (Bytes.set buf off 'a'; incr pos; 1))
          in
          equal string
            "Invalid_argument(\"Caravan_client: the request body length differs from its Content-Length\")"
            (within_2s (fun () -> error_text (fun () -> t.run (req ~meth:Method.POST ~body:short "/"))));
          await s);
      test "a transfer coding other than chunked gives Unsupported" (fun () ->
          with_conn @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r);
                respond t "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip\r\n\r\n")
          in
          equal string "Caravan_client.Error: unsupported: transfer codings other than chunked"
            (error_text (fun () -> t.run (req "/")));
          await s);
      test "Body.close raises the exception of the producer" (fun () ->
          with_conn @@ fun t ->
          let go = Cell.Once.make () and raised = Cell.Once.make () in
          let s =
            serve t (fun r ->
                ignore (read_head r);
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 1000000\r\n\r\n")
          in
          let failing =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc";
                Body.Writer.flush w;
                Cell.Once.get go;
                Cell.Once.try_set raised ();
                failwith "x")
          in
          let resp = t.run (req ~meth:Method.POST ~body:failing "/") in
          Cell.Once.try_set go ();
          within_2s (fun () -> Cell.Once.get raised);
          equal string "Failure(\"x\")" (error_text (fun () -> Body.close (body resp)));
          equal (list bool) [ false ] (t.releases ());
          await s);
      test "the writer is not a child of the caller" (fun () ->
          with_conn @@ fun t ->
          let gate = Cell.Once.make () in
          let s =
            serve t (fun r ->
                let h = read_head r in
                respond t "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n";
                let b = H1_body.body (H1_body.make ~limits:server_limits r (H1_parse.request_framing h)) in
                let buf = Bytes.create 64 in
                let rec echo () =
                  match Body.read b buf 0 64 with
                  | 0 -> respond t "0\r\n\r\n"
                  | n -> respond t (Printf.sprintf "%x\r\n%s\r\n" n (Bytes.sub_string buf 0 n)); echo ()
                in
                echo ())
          in
          let four =
            Body.of_writer (fun w ->
                for i = 1 to 4 do
                  Body.Writer.write_string w (Printf.sprintf "part %d;" i);
                  Body.Writer.flush w;
                  Cell.Once.get gate
                done)
          in
          let f = Fun.Async.call (fun () -> t.run (req ~meth:Method.POST ~body:four "/")) in
          (* F ends before the upload: the writer belongs to the client. *)
          let resp =
            match within_2s (fun () -> Fun.Async.get f) with
            | r -> Cell.Once.try_set gate (); r
            | exception e -> Cell.Once.try_set gate (); raise e
          in
          equal string "part 1;part 2;part 3;part 4;" (Body.to_string (body resp));
          await s);
      test "a stopped writer supervisor refuses a job" (fun () ->
          with_conn @@ fun t ->
          Writers.stop t.writers;
          equal string "Invalid_argument(\"Caravan_client: the client is closed\")"
            (within_2s (fun () -> error_text (fun () -> t.run (req "/"))));
          equal (list bool) [ false ] (t.releases ()));
      test "a cancel during an offer that does not synchronize" (fun () ->
          Server_support.with_flows @@ fun _ client ->
          (* A supervisor that never takes a job. *)
          let writers =
            { Writers.jobs = Port.make (); stopping = Cell.Once.make (); supervisor = Fun.Async.call ignore }
          in
          let conn = Conn.make ~host:"a.example" ~port:80 client in
          let releases = ref [] in
          let on_release _ ~reusable = releases := reusable :: !releases in
          let f =
            Fun.Async.call (fun () -> Exchange.run Client_config.default writers conn ~on_release (req "/"))
          in
          Mtime.wait_for (ms 20);
          Fun.Async.cancel f;
          within_2s (fun () -> raises Fun.Async.Cancelled (fun () -> Fun.Async.get f));
          equal (list bool) [ false ] !releases) ]

(* Resolver and connect *)

let inet a port = Unix.ADDR_INET (Unix.inet_addr_of_string a, port)

let show_addr = function
| Unix.ADDR_INET (a, p) -> Printf.sprintf "%s:%d" (Unix.string_of_inet_addr a) p
| Unix.ADDR_UNIX s -> s

(* A getaddrinfo stub that gives [addrs], after [delay] seconds in its
   thread, and sets [ended] at its end. *)
let stub ?(delay = 0.) ?(ended = Atomic.make false) addrs _ _ _ =
  Unix.sleepf delay;
  Atomic.set ended true;
  List.map
    (fun a ->
       { Unix.ai_family = Unix.PF_INET; ai_socktype = Unix.SOCK_STREAM; ai_protocol = 0; ai_addr = a;
         ai_canonname = "" })
    addrs

let open_fds () = Array.length (Sys.readdir "/proc/self/fd")

(* Polls [p] each 10 ms, for at most 2 s. *)
let eventually p =
  let rec loop n = if p () then true else if n = 0 then false else (Mtime.wait_for (ms 10); loop (n - 1)) in
  loop 200

(* [f port] with a port that refuses connections: a socket that is bound
   but does not listen. The socket keeps the port, thus a different
   process cannot take it during [f]. *)
let with_refused_port f =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Stdlib.Fun.protect ~finally:(fun () -> Unix.close s) @@ fun () ->
  Unix.bind s Server_support.loopback;
  match Unix.getsockname s with Unix.ADDR_INET (_, p) -> f p | _ -> fail "no port"

let main f = Affect_unix.Unix.main ~domain_count:Server_support.domain_count f

(* [f port] with a listener on [port] that accepts one connection. The
   accept stops at the end, also when [f] raises. *)
let with_listener f =
  let l = Tcp.Listener.listen Server_support.loopback in
  let port = match Tcp.Listener.addr l with Unix.ADDR_INET (_, p) -> p | _ -> 0 in
  let accepted = Fun.Async.call (fun () -> let fd, _ = Tcp.Listener.accept l in Unix.close fd) in
  Stdlib.Fun.protect
    ~finally:(fun () -> Fun.Async.cancel accepted; Tcp.Listener.close l)
    (fun () -> f port)

let resolver_tests =
  group "Resolver and connect"
    [ test "a numeric host does not start a thread" (fun () ->
          main @@ fun () ->
          let before = Atomic.get Resolver.threads_started in
          equal (list string) [ "127.0.0.1:80" ]
            (List.map show_addr (Resolver.resolve ~host:"127.0.0.1" ~port:80 ()));
          equal (list string) [ "::1:443" ] (List.map show_addr (Resolver.resolve ~host:"[::1]" ~port:443 ()));
          equal int before (Atomic.get Resolver.threads_started));
      test "a host name uses a thread" (fun () ->
          main @@ fun () ->
          let before = Atomic.get Resolver.threads_started in
          equal (list string) [ "127.0.0.1:8080" ]
            (List.map show_addr
               (Resolver.resolve ~getaddrinfo:(stub [ inet "127.0.0.1" 8080 ]) ~host:"a.example" ~port:8080 ()));
          equal int (before + 1) (Atomic.get Resolver.threads_started));
      test "a broken IPv6 host is not numeric" (fun () ->
          main @@ fun () ->
          let before = Atomic.get Resolver.threads_started in
          equal (list string) [ "127.0.0.1:80" ]
            (List.map show_addr
               (Resolver.resolve ~getaddrinfo:(stub [ inet "127.0.0.1" 80 ]) ~host:"[::1" ~port:80 ()));
          equal int (before + 1) (Atomic.get Resolver.threads_started));
      test "the read end of the pipe stays open until the thread ends" (fun () ->
          main @@ fun () ->
          let before = open_fds () and go = Atomic.make false and ended = Atomic.make false in
          let held _ _ _ =
            while not (Atomic.get go) do Unix.sleepf 0.005 done;
            Atomic.set ended true;
            []
          in
          let c = Fun.Async.call (fun () -> Resolver.resolve ~getaddrinfo:held ~host:"a.example" ~port:80 ()) in
          is_true ~msg:"the pipe is open" (eventually (fun () -> open_fds () = before + 2));
          Fun.Async.cancel c;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get c);
          (* The thread still owns the read end: it writes to it later. *)
          equal int (before + 2) (open_fds ());
          Atomic.set go true;
          is_true ~msg:"the thread ended" (eventually (fun () -> Atomic.get ended));
          is_true ~msg:"no fd leaked" (eventually (fun () -> open_fds () = before)));
      test "no address gives Error (Connect _)" (fun () ->
          main @@ fun () ->
          equal string "Caravan_client.Error: cannot connect: no address for a.example"
            (error_text (fun () -> Resolver.resolve ~getaddrinfo:(stub []) ~host:"a.example" ~port:80 ())));
      test "a cancel during the lookup leaks no file descriptor" (fun () ->
          main @@ fun () ->
          let before = open_fds () and ended = Atomic.make false in
          let c =
            Fun.Async.call (fun () ->
                Resolver.resolve ~getaddrinfo:(stub ~delay:0.3 ~ended [ inet "127.0.0.1" 80 ]) ~host:"a.example"
                  ~port:80 ())
          in
          Mtime.wait_for (ms 50);
          Fun.Async.cancel c;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get c);
          is_true ~msg:"the thread ended" (eventually (fun () -> Atomic.get ended));
          is_true ~msg:"no fd leaked" (eventually (fun () -> open_fds () = before)));
      test "other fibers run during a lookup" (fun () ->
          (* One domain: a lookup in the fiber stops the ticker. *)
          Affect_unix.Unix.main ~domain_count:1 @@ fun () ->
          let ticks = Atomic.make 0 and stop = Atomic.make false in
          let ticker =
            Fun.Async.call (fun () ->
                while not (Atomic.get stop) do
                  Mtime.wait_for (ms 10);
                  Atomic.incr ticks
                done)
          in
          ignore (Resolver.resolve ~getaddrinfo:(stub ~delay:0.3 [ inet "127.0.0.1" 80 ]) ~host:"a.example" ~port:80 ());
          let n = Atomic.get ticks in
          Atomic.set stop true;
          Fun.Async.get ticker;
          is_true ~msg:(Printf.sprintf "%d ticks" n) (n >= 10));
      test "connect tries the next address" (fun () ->
          main @@ fun () ->
          with_listener @@ fun port ->
          with_refused_port @@ fun closed ->
          Flow.close
            (Connect.connect
               ~getaddrinfo:(stub [ inet "127.0.0.1" closed; inet "127.0.0.1" port ])
               Client_config.default ~host:"a.example" ~port));
      test "all addresses refused gives Error (Connect _)" (fun () ->
          main @@ fun () ->
          with_refused_port @@ fun p1 ->
          with_refused_port @@ fun p2 ->
          equal string "Caravan_client.Error: cannot connect: Connection refused"
            (error_text (fun () ->
                 Connect.connect
                   ~getaddrinfo:(stub [ inet "127.0.0.1" p1; inet "127.0.0.1" p2 ])
                   Client_config.default ~host:"a.example" ~port:80)));
      test "the connect deadline covers the lookup" (fun () ->
          main @@ fun () ->
          let ended = Atomic.make false in
          let config = { Client_config.default with connect_timeout = ms 100 } in
          let c = Mtime.counter () in
          equal string "Caravan_client.Error: connect timeout"
            (within_2s (fun () ->
                 error_text (fun () ->
                     Connect.connect
                       ~getaddrinfo:(stub ~delay:0.5 ~ended [ inet "127.0.0.1" 80 ])
                       config ~host:"a.example" ~port:80)));
          (* The stub takes 500 ms: the lookup stops at the deadline. *)
          let took = elapsed_ms c in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took < 400.);
          is_true ~msg:"the thread ended" (eventually (fun () -> Atomic.get ended)));
      test "a cancel during the connect stops it" (fun () ->
          main @@ fun () ->
          let before = open_fds () and ended = Atomic.make false in
          let c =
            Fun.Async.call (fun () ->
                Connect.connect
                  ~getaddrinfo:(stub ~delay:0.3 ~ended [ inet "127.0.0.1" 80 ])
                  Client_config.default ~host:"a.example" ~port:80)
          in
          Mtime.wait_for (ms 50);
          Fun.Async.cancel c;
          within_2s (fun () -> raises Fun.Async.Cancelled (fun () -> Fun.Async.get c));
          is_true ~msg:"the thread ended" (eventually (fun () -> Atomic.get ended));
          is_true ~msg:"no fd leaked" (eventually (fun () -> open_fds () = before)));
      test "localhost reaches a server on 127.0.0.1" (fun () ->
          main @@ fun () ->
          with_listener @@ fun port -> Flow.close (Connect.connect Client_config.default ~host:"localhost" ~port)) ]

let () = exit (run "client" [ exchange_tests; resolver_tests ])
