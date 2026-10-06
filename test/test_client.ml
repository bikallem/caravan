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

let () = exit (run "client" [ exchange_tests ])
