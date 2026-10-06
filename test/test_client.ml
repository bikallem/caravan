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

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false
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

(* Pool *)

(* A request through a pool, as the client will make it. *)
let pooled pool writers ?(config = Client_config.default) ~port r =
  let c, _ = Pool.checkout pool ~host:"127.0.0.1" ~port in
  Exchange.run config writers c ~on_release:(fun c ~reusable -> Pool.release pool c ~reusable) r

let server_port server = match Server.local_addr server with Unix.ADDR_INET (_, p) -> p | _ -> 0

(* A server whose handler gives the id of the connection. *)
let id_handler req = Response.string (string_of_int (Conn_info.id (Option.get (Request.conn req))))

(* [f pool writers port] against a caravan server. *)
let with_pool ?config ?engine ?(handler = id_handler) f =
  Server_support.with_server ?config ?engine ~handler @@ fun server ->
  let writers = Writers.make () in
  let pool = Pool.make Client_config.default in
  Stdlib.Fun.protect ~finally:(fun () -> Pool.close pool; Writers.stop writers; Writers.wait writers)
  @@ fun () -> f pool writers (server_port server)

let get_id pool writers ~port ?(meth = Method.GET) () =
  Body.to_string (body (pooled pool writers ~port (req ~meth ~body:Body.empty "/id")))

(* A flow for the fake connects: it records its close, and its server can
   close it. *)
type fake = { closed : bool ref; server_closed : bool ref; flow : Flow.t }

let fake () =
  let closed = ref false and server_closed = ref false in
  let flow =
    Flow.make ~read:(fun _ _ _ -> 0)
      ~read_now:(fun _ _ _ -> if !server_closed then Some 0 else None)
      ~write:(fun _ _ _ -> ()) ~readable:Action.never ~shutdown_send:ignore
      ~close:(fun () -> closed := true) ~set_read_deadline:ignore ~set_write_deadline:ignore ()
  in
  { closed; server_closed; flow }

(* A pool with a fake clock and fake connects. *)
let fake_pool ?(config = Client_config.default) () =
  let clock = ref Mtime.(now ()) and made = ref [] in
  let now () = !clock in
  let connect ~host:_ ~port:_ = let f = fake () in made := f :: !made; f.flow in
  let pool = Pool.make ~now ~connect config in
  let advance span = clock := Option.get (Mtime.add_span !clock span) in
  (pool, advance, fun () -> List.rev !made)

let pool_tests =
  group "Pool"
    [ test "two requests in sequence use one connection" (fun () ->
          with_pool @@ fun pool writers port ->
          let a = get_id pool writers ~port () in
          let b = get_id pool writers ~port () in
          equal string a b);
      test "two requests at the same time use two connections" (fun () ->
          (* The handler waits until [gate] opens: the second request of a
             round opens it. Thus the two requests are in the server at
             the same time. *)
          let arrived = Atomic.make 0 and gate = Atomic.make (Cell.Once.make ()) in
          let handler req =
            let g = Atomic.get gate in
            if Atomic.fetch_and_add arrived 1 = 1 then Cell.Once.try_set g ();
            ignore
              (Action.invoke (Action.choose [ Action.map ignore (Cell.Once.get' g); Mtime.wait_for' (ms 2000) () ]));
            id_handler req
          in
          with_pool ~handler @@ fun pool writers port ->
          (* One idle connection first, with an open gate. *)
          let opened = Cell.Once.make () in
          Cell.Once.try_set opened ();
          Atomic.set gate opened;
          ignore (get_id pool writers ~port ());
          Atomic.set arrived 0;
          Atomic.set gate (Cell.Once.make ());
          let a = Fun.Async.call (fun () -> get_id pool writers ~port ()) in
          let b = Fun.Async.call (fun () -> get_id pool writers ~port ()) in
          let a = within_2s (fun () -> Fun.Async.get a) and b = within_2s (fun () -> Fun.Async.get b) in
          is_true ~msg:(a ^ " and " ^ b) (a <> b));
      test "an idle connection that the server closed is not used" (fun () ->
          let closed = Cell.Once.make () in
          let engine ctx flow handler =
            H1_server.serve ctx flow handler;
            Flow.close flow;
            Cell.Once.try_set closed ()
          in
          let config = { Server_support.config with idle_timeout = ms 50 } in
          with_pool ~config ~engine @@ fun pool writers port ->
          let a = get_id pool writers ~port () in
          within_2s (fun () -> Cell.Once.get closed);
          (* A POST, which the client never retries. *)
          let b = get_id pool writers ~port ~meth:Method.POST () in
          is_true ~msg:(a ^ " then " ^ b) (a <> b));
      test "an idle connection older than idle_timeout is closed" (fun () ->
          let pool, advance, made = fake_pool () in
          let c, _ = Pool.checkout pool ~host:"a" ~port:80 in
          Pool.release pool c ~reusable:true;
          advance (Mtime.Span.(4 * s));
          let c', kind = Pool.checkout pool ~host:"A" ~port:80 in
          is_true ~msg:"reused at 4 s" (kind = `Reused && c'.Conn.id = c.Conn.id);
          Pool.release pool c' ~reusable:true;
          advance (Mtime.Span.(6 * s));
          let _, kind = Pool.checkout pool ~host:"a" ~port:80 in
          is_true ~msg:"new at 6 s" (kind = `New);
          equal (list bool) [ true; false ] (List.map (fun f -> !(f.closed)) (made ())));
      test "max_idle_per_host keeps at most that number" (fun () ->
          let pool, _, made = fake_pool ~config:{ Client_config.default with max_idle_per_host = 2 } () in
          let cs = List.init 3 (fun _ -> fst (Pool.checkout pool ~host:"a" ~port:80)) in
          List.iter (fun c -> Pool.release pool c ~reusable:true) cs;
          equal (list bool) [ false; false; true ] (List.map (fun f -> !(f.closed)) (made ()));
          let kinds = List.init 3 (fun _ -> snd (Pool.checkout pool ~host:"a" ~port:80)) in
          equal (list bool) [ true; true; false ] (List.map (fun k -> k = `Reused) kinds);
          equal int 4 (List.length (made ())));
      test "close closes idle and used connections" (fun () ->
          let pool, _, made = fake_pool () in
          let a, _ = Pool.checkout pool ~host:"a" ~port:80 in
          let _b, _ = Pool.checkout pool ~host:"a" ~port:80 in
          Pool.release pool a ~reusable:true;
          Pool.close pool;
          equal (list bool) [ true; true ] (List.map (fun f -> !(f.closed)) (made ()));
          raises_match is_invalid_arg (fun () -> Pool.checkout pool ~host:"a" ~port:80));
      test "a connect during close is closed" (fun () ->
          let pool = ref None and flows = ref [] in
          let connect ~host:_ ~port:_ =
            let f = fake () in
            flows := f :: !flows;
            Pool.close (Option.get !pool);
            f.flow
          in
          pool := Some (Pool.make ~connect Client_config.default);
          raises_match is_invalid_arg (fun () -> Pool.checkout (Option.get !pool) ~host:"a" ~port:80);
          equal (list bool) [ true ] (List.map (fun f -> !(f.closed)) !flows)) ]

(* The pool against a model: one host, a fake clock and fake flows. Each
   program makes its pool first. *)
module Pool_model = struct
  type pool = { mutable clock : int; mutable connects : int; mutable idle : conn list }
  and conn = { pool : pool; mutable state : [ `Used | `Idle of int | `Closed ]; mutable server_closed : bool }

  let make () = { clock = 0; connects = 0; idle = [] }

  (* Idle for less than 5 s, and not closed by its server. *)
  let rec checkout p =
    match p.idle with
    | c :: rest -> (
        p.idle <- rest;
        match c.state with
        | `Idle t when p.clock - t < 5000 && not c.server_closed -> c.state <- `Used; c
        | _ -> c.state <- `Closed; checkout p)
    | [] -> p.connects <- p.connects + 1; { pool = p; state = `Used; server_closed = false }

  let release c reusable =
    let p = c.pool in
    if reusable && List.length p.idle < 2 then (c.state <- `Idle p.clock; p.idle <- c :: p.idle)
    else c.state <- `Closed

  let advance p ms = p.clock <- p.clock + ms
  let server_close c = c.server_closed <- true
  let is_closed c = c.state = `Closed
  let connects p = p.connects
  let used c = c.state = `Used
  let is_idle c = match c.state with `Idle _ -> true | _ -> false
end

module Pool_system = struct
  type pool = { pool : Pool.t; clock : Mtime.t ref; fakes : (int, fake) Hashtbl.t; connects : int ref }

  let owners : (int, pool) Hashtbl.t = Hashtbl.create 64

  let make () =
    let clock = ref Mtime.(now ()) and fakes = Hashtbl.create 8 and connects = ref 0 in
    let connect ~host:_ ~port:_ =
      incr connects;
      let f = fake () in
      (* [Conn.make] takes the next id after the connect. *)
      Hashtbl.replace fakes (Atomic.get Conn.next_id) f;
      f.flow
    in
    let config = { Client_config.default with max_idle_per_host = 2; idle_timeout = Mtime.Span.(5 * s) } in
    { pool = Pool.make ~now:(fun () -> !clock) ~connect config; clock; fakes; connects }

  let checkout p =
    let c, _ = Pool.checkout p.pool ~host:"a" ~port:80 in
    Hashtbl.replace owners c.Conn.id p;
    c

  let release (c : Conn.t) reusable = Pool.release (Hashtbl.find owners c.id).pool c ~reusable
  let advance p n = p.clock := Option.get (Mtime.add_span !(p.clock) Mtime.Span.(n * ms))
  let fake_of (c : Conn.t) = Hashtbl.find (Hashtbl.find owners c.id).fakes c.id
  let server_close c = (fake_of c).server_closed := true
  let is_closed c = !((fake_of c).closed)
  let connects p = !(p.connects)
end

let pool_stateful =
  let pool = abstract "p" ~pp:(fun ppf _ -> Format.pp_print_string ppf "pool") ~release:ignore in
  let conn =
    abstract "c"
      ~pp:(fun ppf (c : Pool_model.conn) ->
          Format.pp_print_string ppf
            (match c.state with `Used -> "used" | `Idle _ -> "idle" | `Closed -> "closed"))
      ~release:ignore
  in
  let commands =
    [ command "pool" (Gen.unit @-> makes pool) Pool_model.make Pool_system.make;
      command "checkout" (pool ^-> makes conn) Pool_model.checkout Pool_system.checkout;
      command "release" ~pre:(fun c _ -> Pool_model.used c) (conn ^-> Gen.bool @-> returns unit)
        Pool_model.release Pool_system.release;
      command "advance" (pool ^-> Gen.int_range 0 3000 @-> returns unit) Pool_model.advance
        Pool_system.advance;
      command "server closes" ~pre:Pool_model.is_idle (conn ^-> returns unit) Pool_model.server_close
        Pool_system.server_close;
      command "is closed" (conn ^-> returns bool) Pool_model.is_closed Pool_system.is_closed;
      command "connects" (pool ^-> returns int) Pool_model.connects Pool_system.connects ]
  in
  group "Pool model" [ stateful ~count:300 ~steps:30 "the pool behaves like a model" commands ]

(* The public API *)

module C = Caravan_client

let name = function Fun.Async.Cancelled -> "Cancelled" | e -> Printexc.to_string e

(* The handler of the tests: "/id" gives the id of the connection, "/conn"
   the Connection field and the id, and the others go to the handler of
   Server_support. *)
let api_handler req =
  match Request.path_only req with
  | "/id" -> id_handler req
  | "/conn" ->
      Response.string
        (Option.value ~default:"-" (Headers.find "connection" (Request.headers req))
        ^ " " ^ Body.to_string (match Response.kind (id_handler req) with Body b -> b | _ -> Body.empty))
  | _ -> Server_support.handler req

let big_config = { Server_support.config with max_body = None }

(* The result of [f], or a failure after 5 s. A whole client run gets this
   guard: a bug in its close gives a failure, not a hang. *)
let within_5s f =
  let call = Fun.Async.call f in
  let ended = Action.map (fun v -> `Ended v) (Fun.Async.get' call) in
  match Action.invoke (Action.choose [ ended; Mtime.wait_for' Mtime.Span.(5 * s) `Late ]) with
  | `Ended v -> v
  | `Late -> Fun.Async.cancel call; fail "no end after 5 s"

(* [f client url] against a caravan server. *)
let with_client ?(config = big_config) ?client_config f =
  Server_support.with_server ~config ~handler:api_handler @@ fun server ->
  within_5s @@ fun () ->
  C.run ?config:client_config @@ fun client ->
  f client (Printf.sprintf "http://127.0.0.1:%d" (server_port server))

let text resp = Body.to_string (C.body resp)
let get_text client url = text (C.get client url)

let at_port port ?(meth = Method.GET) ?(headers = []) ?body path =
  Request.make ~authority:(Printf.sprintf "127.0.0.1:%d" port) ~headers:(Headers.of_list headers) ?body meth path

(* A listener whose connections follow [scripts], one script for each
   connection, in order. [f client port accepted] runs with a client. *)
let with_scripts scripts f =
  main @@ fun () ->
  let l = Tcp.Listener.listen Server_support.loopback in
  let port = match Tcp.Listener.addr l with Unix.ADDR_INET (_, p) -> p | _ -> 0 in
  let accepted = Atomic.make 0 in
  let acceptor =
    Fun.Async.call (fun () ->
        List.iter
          (fun script ->
             let fd, _ = Tcp.Listener.accept l in
             Atomic.incr accepted;
             let flow = Tcp.flow fd in
             ignore
               (Fun.Async.call (fun () ->
                    Stdlib.Fun.protect ~finally:(fun () -> Flow.close flow) (fun () ->
                        try script flow (Buf_reader.of_flow flow) with _ -> ()))))
          scripts)
  in
  Stdlib.Fun.protect
    ~finally:(fun () -> Fun.Async.cancel acceptor; Tcp.Listener.close l)
    (fun () -> within_5s (fun () -> C.run (fun client -> f client port accepted)))

let api_tests =
  group "Caravan_client"
    [ test "GET, HEAD, 204 and a stream, with reuse" (fun () ->
          with_client @@ fun client url ->
          let id0 = get_text client (url ^ "/id") in
          equal string "hello" (get_text client (url ^ "/hello"));
          let head = C.request client (at_port (int_of_string (List.nth (String.split_on_char ':' url) 2)) ~meth:Method.HEAD "/hello") in
          equal int 200 (status head);
          equal string "" (text head);
          equal string id0 (get_text client (url ^ "/id"));
          equal int 204 (status (C.get client (url ^ "/empty")));
          equal string id0 (get_text client (url ^ "/id"));
          equal string "abcd" (get_text client (url ^ "/stream")));
      test "an upload of unknown length goes chunked, and comes back" (fun () ->
          with_client @@ fun client url ->
          let port = int_of_string (List.nth (String.split_on_char ':' url) 2) in
          let b = Body.of_writer (fun w -> List.iter (Body.Writer.write_string w) [ "one;"; "two;"; "three" ]) in
          equal string "one;two;three" (text (C.request client (at_port port ~meth:Method.POST ~body:b "/echo"))));
      test "8 MiB echoed while it uploads" (fun () ->
          with_client @@ fun client url ->
          let port = int_of_string (List.nth (String.split_on_char ':' url) 2) in
          let data = String.init (8 * 1024 * 1024) (fun i -> Char.chr (i land 0xff)) in
          let b =
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
          let got = within_2s (fun () -> text (C.request client (at_port port ~meth:Method.POST ~body:b "/echo"))) in
          is_true ~msg:"the echo is equal" (got = data));
      test "a stale connection is retried for a GET" (fun () ->
          let first flow r =
            ignore (read_request r);
            Flow.write_string flow "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\na";
            (* The next request comes, and the server closes. *)
            ignore (read_head r)
          and second flow r =
            ignore (read_request r);
            Flow.write_string flow "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\nb"
          in
          with_scripts [ first; second ] @@ fun client port accepted ->
          equal string "a" (text (C.request client (at_port port "/")));
          equal string "b" (within_2s (fun () -> text (C.request client (at_port port "/"))));
          equal int 2 (Atomic.get accepted));
      test "a stale connection is not retried for a POST" (fun () ->
          let first flow r =
            ignore (read_request r);
            Flow.write_string flow "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\na";
            ignore (read_request r)
          in
          with_scripts [ first; first ] @@ fun client port accepted ->
          equal string "a" (text (C.request client (at_port port "/")));
          equal string "Caravan_client.Error: the connection closed before the response"
            (within_2s (fun () ->
                 error_text (fun () -> C.request client (at_port port ~meth:Method.POST ~body:Body.empty "/"))));
          equal int 1 (Atomic.get accepted));
      test "a stale connection is not retried for a PUT with a body" (fun () ->
          let first flow r =
            ignore (read_request r);
            Flow.write_string flow "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\na";
            ignore (read_request r)
          in
          with_scripts [ first; first ] @@ fun client port accepted ->
          equal string "a" (text (C.request client (at_port port "/")));
          equal string "Caravan_client.Error: the connection closed before the response"
            (within_2s (fun () ->
                 error_text (fun () ->
                     C.request client (at_port port ~meth:Method.PUT ~body:(Body.of_string "abc") "/"))));
          equal int 1 (Atomic.get accepted));
      test "with_response closes an unread body" (fun () ->
          let eof = Cell.Once.make () in
          let script flow r =
            ignore (read_request r);
            Flow.write_string flow "HTTP/1.1 200 OK\r\nContent-Length: 1000000\r\n\r\nabc";
            ignore (Buf_reader.peek_eof r);
            Cell.Once.try_set eof ()
          in
          with_scripts [ script ] @@ fun client port _ ->
          within_2s (fun () -> C.with_response client (at_port port "/") ignore);
          (* The client closes the connection of the unread body. *)
          within_2s (fun () -> Cell.Once.get eof));
      test "with_response gives the exception of f" (fun () ->
          let go = Cell.Once.make () and raised = Cell.Once.make () in
          let script flow r =
            ignore (read_head r);
            Flow.write_string flow "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n";
            ignore (Buf_reader.peek_eof r)
          in
          with_scripts [ script ] @@ fun client port _ ->
          let failing =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc";
                Body.Writer.flush w;
                Cell.Once.get go;
                Cell.Once.try_set raised ();
                failwith "p")
          in
          equal string "Failure(\"f\")"
            (within_2s (fun () ->
                 error_text (fun () ->
                     C.with_response client (at_port port ~meth:Method.POST ~body:failing "/") (fun _ ->
                         Cell.Once.try_set go ();
                         Cell.Once.get raised;
                         failwith "f")))));
      test "request after close raises Invalid_argument" (fun () ->
          main @@ fun () ->
          within_2s @@ fun () ->
          C.run @@ fun client ->
          C.close client;
          raises_match is_invalid_arg (fun () -> C.request client (at_port 9 "/")));
      test "a proxy handler streams an upload to an upstream echo" (fun () ->
          main @@ fun () ->
          let upstream = Server.start ~config:big_config Server_support.handler in
          Stdlib.Fun.protect ~finally:(fun () -> Server.shutdown upstream; Server.wait upstream) @@ fun () ->
          let up = Printf.sprintf "127.0.0.1:%d" (server_port upstream) in
          within_5s @@ fun () ->
          C.run @@ fun client ->
          let forward req =
            let r =
              Request.make ~authority:up ~headers:(Request.headers req) ~body:(Request.body req)
                (Request.meth req) (Request.path req)
            in
            let resp = C.request client r in
            Response.make ~status:(Response.status resp) ~headers:(Response.headers resp) (C.body resp)
          in
          let proxy = Server.start ~config:big_config forward in
          let ok = ref false in
          Stdlib.Fun.protect
            ~finally:(fun () -> if !ok then (Server.shutdown proxy; Server.wait proxy) else Server.abort proxy)
          @@ fun () ->
          let flow = Tcp.connect (Server.local_addr proxy) in
          Stdlib.Fun.protect ~finally:(fun () -> Flow.close flow) @@ fun () ->
          Flow.write_string flow "POST /echo HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n";
          let r = Buf_reader.of_flow flow and n = 65536 in
          let send k = Flow.write_string flow (Printf.sprintf "%x\r\n%s\r\n" n (String.make n (Char.chr (65 + k)))) in
          send 0;
          let h = within_2s (fun () -> H1_parse.response_head server_limits r) in
          equal int 200 (Status.code h.status);
          let b = H1_body.chunked ~limits:server_limits r in
          let read_n () =
            let buf = Bytes.create n in
            let rec go got = if got < n then go (got + Body.read b buf got (n - got)) in
            go 0;
            Bytes.to_string buf
          in
          (* Chunk k goes only after the echo of chunk k - 1 came. *)
          for k = 0 to 15 do
            if k > 0 then send k;
            equal string (String.make n (Char.chr (65 + k))) (within_2s read_n)
          done;
          Flow.write_string flow "0\r\n\r\n";
          equal string "" (within_2s (fun () -> Body.to_string b));
          ok := true);
      test "close cancels the writers, and run returns after them" (fun () ->
          let script flow r =
            ignore (read_head r);
            Flow.write_string flow "HTTP/1.1 200 OK\r\nContent-Length: 1000000\r\n\r\n";
            ignore (Buf_reader.peek_eof r)
          in
          main @@ fun () ->
          let l = Tcp.Listener.listen Server_support.loopback in
          let port = match Tcp.Listener.addr l with Unix.ADDR_INET (_, p) -> p | _ -> 0 in
          let server =
            Fun.Async.call (fun () ->
                let fd, _ = Tcp.Listener.accept l in
                let flow = Tcp.flow fd in
                Stdlib.Fun.protect ~finally:(fun () -> Flow.close flow) (fun () ->
                    try script flow (Buf_reader.of_flow flow) with _ -> ()))
          in
          Stdlib.Fun.protect ~finally:(fun () -> Fun.Async.cancel server; Tcp.Listener.close l) @@ fun () ->
          let seen = ref "none" in
          let waiting =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc";
                Body.Writer.flush w;
                try Cell.Once.get (Cell.Once.make ())
                with e ->
                  (* Late: only a run that waits for the writers sees it. *)
                  Fun.Async.mask_cancellation (fun () -> Mtime.wait_for (ms 50));
                  seen := name e;
                  raise e)
          in
          let config = { Client_config.default with write_timeout = ms 5000 } in
          let at_return =
            within_2s (fun () ->
                C.run ~config (fun client -> ignore (C.request client (at_port port ~meth:Method.POST ~body:waiting "/")));
                !seen)
          in
          equal string "Cancelled" at_return);
      test "a cancel of the caller during the head wait stops the writer" (fun () ->
          let eof = Cell.Once.make () in
          let script _ r =
            ignore (read_head r);
            ignore (Buf_reader.peek_eof r);
            Cell.Once.try_set eof ()
          in
          with_scripts [ script ] @@ fun client port _ ->
          let wrote = Cell.Once.make () and seen = Cell.Once.make () in
          let waiting =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc";
                Body.Writer.flush w;
                Cell.Once.try_set wrote ();
                try Cell.Once.get (Cell.Once.make ()) with e -> Cell.Once.try_set seen (name e); raise e)
          in
          let caller = Fun.Async.call (fun () -> C.request client (at_port port ~meth:Method.POST ~body:waiting "/")) in
          within_2s (fun () -> Cell.Once.get wrote);
          Fun.Async.cancel caller;
          equal string "Cancelled" (within_2s (fun () -> Cell.Once.get seen));
          within_2s (fun () -> Cell.Once.get eof);
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get caller));
      test "a cancel of the caller during the body closes the connection" (fun () ->
          let eof = Cell.Once.make () in
          let script flow r =
            ignore (read_request r);
            Flow.write_string flow "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nab";
            ignore (Buf_reader.peek_eof r);
            Cell.Once.try_set eof ()
          in
          with_scripts [ script ] @@ fun client port _ ->
          let reading = Cell.Once.make () in
          let caller =
            Fun.Async.call (fun () ->
                let b = C.body (C.request client (at_port port "/")) in
                ignore (Body.read b (Bytes.create 8) 0 8);
                Cell.Once.try_set reading ();
                Body.to_string b)
          in
          within_2s (fun () -> Cell.Once.get reading);
          Fun.Async.cancel caller;
          within_2s (fun () -> Cell.Once.get eof);
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get caller));
      test "get parses URLs" (fun () ->
          let show url =
            match C.Private.parse_url url with
            | a, p -> Printf.sprintf "%s => %s %s" url a p
            | exception C.Error e -> Printf.sprintf "%s => %s" url (Printexc.to_string (C.Error e))
            | exception Invalid_argument _ -> url ^ " => Invalid_argument"
          in
          expect
            (String.concat "\n"
               (List.map show
                  [ "http://a.example"; "HTTP://a.example:8080/p?q#f"; "http://a.example?x=1"; "http://[::1]:80/";
                    "http://[::1]:8080/x"; "http://a:/"; "https://a"; "ftp://x"; "http://"; "http://u@a/";
                    "http://a:70000/"; "http://a:0/"; "http://a:8x/"; "a.example/p"; "http://a:1/";
                    "http://a:65535/"; "http://a:65536/"; "http://a:+80/" ]))
          @@ __POS_OF__ {|
            http://a.example => a.example /
            HTTP://a.example:8080/p?q#f => a.example:8080 /p?q
            http://a.example?x=1 => a.example /?x=1
            http://[::1]:80/ => [::1] /
            http://[::1]:8080/x => [::1]:8080 /x
            http://a:/ => a /
            https://a => Caravan_client.Error: unsupported: the scheme https
            ftp://x => Caravan_client.Error: unsupported: the scheme ftp
            http:// => Invalid_argument
            http://u@a/ => Invalid_argument
            http://a:70000/ => Invalid_argument
            http://a:0/ => Invalid_argument
            http://a:8x/ => Invalid_argument
            a.example/p => Invalid_argument
            http://a:1/ => a:1 /
            http://a:65535/ => a:65535 /
            http://a:65536/ => Invalid_argument
            http://a:+80/ => Invalid_argument
            |});
      test "the client and the server under load" (fun () ->
          let client_config = { Client_config.default with max_idle_per_host = 20 } in
          with_client ~client_config @@ fun client url ->
          let ids =
            List.concat
              (Fun.Async.get_all
                 (List.init 20 (fun _ -> Fun.Async.call (fun () -> List.init 25 (fun _ -> get_text client (url ^ "/id"))))))
          in
          equal int 500 (List.length ids);
          let distinct = List.sort_uniq compare ids in
          is_true ~msg:(Printf.sprintf "%d connections" (List.length distinct)) (List.length distinct <= 20));
      test "a user Connection: close is sent, and the next request uses a new connection" (fun () ->
          with_client @@ fun client url ->
          let port = int_of_string (List.nth (String.split_on_char ':' url) 2) in
          let first = text (C.request client (at_port port ~headers:[ ("Connection", "close") ] "/conn")) in
          let id1 = List.nth (String.split_on_char ' ' first) 1 in
          equal string "close" (List.hd (String.split_on_char ' ' first));
          let id2 = get_text client (url ^ "/id") in
          is_true ~msg:(id1 ^ " then " ^ id2) (id1 <> id2)) ]

let () = exit (run "client" [ exchange_tests; resolver_tests; pool_tests; pool_stateful; api_tests ])
