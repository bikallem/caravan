(* The tests of caravan.client on the simulator of weft. Each test runs in
   Sim_support: 1 seed and 1 domain, except a test whose name starts with
   "race:" (100 seeds on 4 domains). A test that checks a time runs in manual
   mode. The servers listen on 127.0.0.1, a numeric host, so that the client
   needs no DNS answer. *)

open Windtrap
open Weft
open Caravan
open Caravan.Private
open Caravan_client.Private
module Client_config = Caravan_client.Config

let server_limits = H1_server.limits Config.default

let req ?(meth = Method.GET) ?(headers = []) ?body path =
  Request.make ~authority:"a.example" ~headers:(Headers.of_list headers) ?body
    meth path

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

let body r =
  match Response.kind r with
  | Response.Body b ->
      b
  | Upgrade _ ->
      fail "an upgrade"

let status r = Status.code (Response.status r)

let error_text f =
  match f () with
  | _ ->
      "no error"
  | exception Caravan_client.Error e ->
      Printexc.to_string (Caravan_client.Error e)
  | exception e ->
      Printexc.to_string e

(* An Ivar as a signal of one event. *)
let fill iv = ignore (Ivar.try_fill iv ())

let wait iv = sync (Ivar.read iv)

let is_set iv = Option.is_some (Ivar.peek iv)

(* [check_settled sim ~msg p] lets the program run until it is quiescent, with
   no move of the time, then makes sure that [p ()] is true. *)
let check_settled sim ~msg p =
  Weft_sim.settle sim ;
  is_true ~msg (p ())

(* The bytes that the client sent on a fake flow are not in a socket. *)
let net_eio = Net.Error (Other "EIO", "write")

(* One connection: [server] is the flow of a scripted server, [run] sends a
   request on the other end. The release records [reusable], and closes a
   connection that is not reusable, as the pool does. The scripted servers and
   the callers are calls of [scope]. *)
type t =
  { sim: Weft_sim.t
  ; scope: Fiber.scope
  ; server: Flow.t
  ; run: ?config:Client_config.t -> Request.t -> Response.t
  ; releases: unit -> bool list
  ; writers: Writers.t }

(* [with_client_flow sim f] gives [f ~scope ~server ~writers ~conn ~taken] a
   client connection [conn] over a socket pair of [sim], and a scope with the
   writers of the client. [write real] replaces the write of the client flow:
   [real] is the flow of the socket. [on_write_timeout] gets each write timeout
   that the client sets. [taken] counts the bytes that the client took. At the
   end, the writers stop, and the two flows close. *)
let with_client_flow ?write ?on_write_timeout sim f =
  Sim_support.with_pair sim
  @@ fun a b ->
  let real = Tcp.flow a and server = Tcp.flow b in
  let counted =
    Server_support.counted
      ?write:(Option.map (fun w -> w real) write)
      ?on_write_timeout real
  in
  let client = counted.flow in
  Fiber.scope
  @@ fun scope ->
  let writers = Writers.make scope in
  Fun.protect ~finally:(fun () ->
      Writers.stop writers ;
      Writers.wait writers ;
      Flow.close server ;
      Flow.close client )
  @@ fun () ->
  let conn = Conn.make ~host:"a" ~port:80 client in
  f ~scope ~server ~writers ~conn ~taken:counted.read_bytes

(* [conn_in sim f] gives [f] a connection of [with_client_flow]. *)
let conn_in sim f =
  with_client_flow sim
  @@ fun ~scope ~server ~writers ~conn ~taken:_ ->
  let releases = ref [] in
  let on_release (c : Conn.t) ~reusable =
    releases := reusable :: !releases ;
    if not reusable then Flow.close c.flow
  in
  let run ?(config = Client_config.default) r =
    Exchange.run config writers conn ~on_release r
  in
  f {sim; scope; server; run; releases= (fun () -> List.rev !releases); writers}

let with_conn f = Sim_support.run @@ fun sim -> conn_in sim f

(* The scripted server, in a call: [script r] gets a reader on the server
   flow. *)
let serve t script =
  Sim_support.call ~name:"server" t.scope (fun () ->
      script (Buf_reader.of_flow t.server) )

let await = Sim_support.await

let read_head r = H1_parse.request_head server_limits r

(* Reads a request and its body. *)
let read_request r =
  let h = read_head r in
  let b =
    H1_body.body
      (H1_body.make ~limits:server_limits r (H1_parse.request_framing h))
  in
  (h, Body.to_string b)

(* Reads the body of a request with the head [h]. *)
let read_body r h =
  Body.to_string
    (H1_body.body
       (H1_body.make ~limits:server_limits r (H1_parse.request_framing h)) )

let respond t s = Flow.write_string t.server s

(* A body that writes chunks of 64 KiB until a write raises. Then it fills
   [raised] and raises again. *)
let endless raised =
  let chunk = String.make 65536 'x' in
  Body.of_writer (fun w ->
      try
        while true do
          Body.Writer.write_string w chunk ;
          Body.Writer.flush w
        done
      with e -> fill raised ; raise e )

(* [f ~scope ~server ~run ~break ~failed] with a client flow over a socket pair
   of [sim], whose writes raise [error] after [break ()]. [failed] is filled at
   the first such write. [on_write_timeout] gets each write timeout that the
   client sets, and [on_release] each release of the connection. *)
let with_breakable ?on_write_timeout ?(on_release = fun _ ~reusable:_ -> ())
    ?(failed = Ivar.create ()) error sim f =
  let broken = Atomic.make false in
  let write real buf off len =
    if Atomic.get broken then (fill failed ; raise error)
    else Flow.write real buf off len
  in
  with_client_flow ~write ?on_write_timeout sim
  @@ fun ~scope ~server ~writers ~conn ~taken:_ ->
  let run config r = Exchange.run config writers conn ~on_release r in
  f ~scope ~server ~run ~break:(fun () -> Atomic.set broken true) ~failed

(* [f ~scope ~server ~writers ~conn ~taken] with a client connection over a
   socket pair of [sim]. After a write sends its bytes, it waits for [release]
   if [hold] is true for all the bytes sent at that time. A cancel of that wait
   fills [write_cancelled]. [taken] counts the bytes that the client took. *)
let with_held_writes ?(write_cancelled = Ivar.create ()) ~hold ~release sim f =
  let sent = Buffer.create 64 in
  let write real buf off len =
    Flow.write real buf off len ;
    Buffer.add_subbytes sent buf off len ;
    if hold (Buffer.contents sent) then (
      try wait release
      with Fiber.Cancelled _ as e -> fill write_cancelled ; raise e )
  in
  with_client_flow ~write sim
  @@ fun ~scope ~server ~writers ~conn ~taken ->
  (* The held writes end before the writers stop. *)
  Fun.protect ~finally:(fun () -> fill release)
  @@ fun () -> f ~scope ~server ~writers ~conn ~taken

(* An exchange on [conn] that records its releases in [releases]. *)
let exchange ?(config = Client_config.default) writers conn releases r =
  Exchange.run config writers conn
    ~on_release:(fun _ ~reusable -> releases := reusable :: !releases)
    r

(* A source of the 3 bytes "abc". [closes] counts its closes. *)
let abc_source ?(closes = Atomic.make 0) ?(close = ignore) () =
  let sent = ref false in
  Body.of_source ~length:3
    ~close:(fun () -> Atomic.incr closes ; close ())
    (fun buf off _ ->
      if !sent then 0
      else (
        Bytes.blit_string "abc" 0 buf off 3 ;
        sent := true ;
        3 ) )

(* A request body that writes "abc", waits for [go], then raises [Failure
   "x"]. *)
let failing_after go =
  Body.of_writer (fun w ->
      Body.Writer.write_string w "abc" ;
      Body.Writer.flush w ;
      wait go ;
      failwith "x" )

(* [none_counted flow] is [flow], and a count of the calls of [read_now] that
   gave [None]. A reader makes such a call before each wait, and at each wake
   with no byte. *)
let none_counted flow =
  let nones = Atomic.make 0 in
  let read_now buf off len =
    let r = Flow.read_now flow buf off len in
    if Option.is_none r then Atomic.incr nones ;
    r
  in
  ( Flow.make ~read:(Flow.read flow) ~read_now ~write:(Flow.write flow)
      ~readable:(Flow.readable flow)
      ~shutdown_send:(fun () -> Flow.shutdown_send flow)
      ~close:(fun () -> Flow.close flow)
      ~reset:(fun () -> Flow.reset flow)
      ~set_read_deadline:(Flow.set_read_deadline flow)
      ~set_write_deadline:(Flow.set_write_deadline flow)
      ~set_read_timeout:(Flow.set_read_timeout flow)
      ~set_write_timeout:(Flow.set_write_timeout flow)
      ()
  , nones )

let exchange_tests =
  group "Exchange"
    [ test "a GET gets the response and its body" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello" )
          in
          let resp = t.run (req "/") in
          equal int 200 (status resp) ;
          equal string "hello" (Body.to_string (body resp)) ;
          await s ;
          equal (list bool) [true] (t.releases ()) )
    ; test "when a connection is reusable" (fun () ->
          let case ?(meth = Method.GET) ?(headers = []) ?(read = true)
              ?(close = false) response =
            with_conn
            @@ fun t ->
            let s =
              serve t (fun r ->
                  ignore (read_request r) ;
                  respond t response ;
                  if close then Flow.shutdown_send t.server )
            in
            let resp = t.run (req ~meth ~headers "/") in
            if read then
              ignore (error_text (fun () -> Body.to_string (body resp)))
            else if meth <> Method.HEAD then Body.close (body resp) ;
            await s ;
            match t.releases () with
            | [b] ->
                string_of_bool b
            | l ->
                Printf.sprintf "%d releases" (List.length l)
          in
          let cl = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" in
          expect
            (String.concat "\n"
               [ "CL body read => " ^ case cl
               ; "CL body closed => " ^ case ~read:false cl
               ; "chunked => "
                 ^ case
                     "HTTP/1.1 200 OK\r\n\
                      Transfer-Encoding: chunked\r\n\
                      \r\n\
                      2\r\n\
                      ok\r\n\
                      0\r\n\
                      \r\n"
               ; "until close => "
                 ^ case ~close:true "HTTP/1.1 200 OK\r\n\r\nok"
               ; "response Connection: close => "
                 ^ case
                     "HTTP/1.1 200 OK\r\n\
                      Connection: close\r\n\
                      Content-Length: 2\r\n\
                      \r\n\
                      ok"
               ; "request Connection: close => "
                 ^ case ~headers:[("Connection", "close")] cl
               ; "HTTP/1.0 with keep-alive => "
                 ^ case
                     "HTTP/1.0 200 OK\r\n\
                      Connection: keep-alive\r\n\
                      Content-Length: 2\r\n\
                      \r\n\
                      ok"
               ; "a malformed chunk => "
                 ^ case
                     "HTTP/1.1 200 OK\r\n\
                      Transfer-Encoding: chunked\r\n\
                      \r\n\
                      zz\r\n"
               ; "one byte after the body => " ^ case (cl ^ "X")
               ; "HEAD, no read => "
                 ^ case ~meth:Method.HEAD ~read:false
                     "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n" ] )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "a 100 and a 103 are skipped" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t
                  "HTTP/1.1 100 Continue\r\n\
                   \r\n\
                   HTTP/1.1 103 Early Hints\r\n\
                   Link: </a>\r\n\
                   \r\n\
                   HTTP/1.1 200 OK\r\n\
                   Content-Length: 0\r\n\
                   \r\n" )
          in
          equal int 200 (status (t.run (req "/"))) ;
          await s )
    ; test "eleven 1xx responses give Bad_response" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t
                  (String.concat ""
                     (List.init 11 (fun _ -> "HTTP/1.1 100 Continue\r\n\r\n")) ) )
          in
          equal string
            "Caravan_client.Error: bad response: 400 Bad Request (more than 10 \
             interim responses)"
            (error_text (fun () -> t.run (req "/"))) ;
          await s )
    ; test "a 413 with Connection: close stops the upload, and its body arrives"
        (fun () ->
          with_conn
          @@ fun t ->
          let raised = Ivar.create () in
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                ignore (Buf_reader.read_into r (Bytes.create 1024) 0 1024) ;
                respond t
                  "HTTP/1.1 413 Content Too Large\r\n\
                   Connection: close\r\n\
                   Content-Length: 9\r\n\
                   \r\n" ;
                wait raised ;
                respond t "too large" )
          in
          let resp = t.run (req ~meth:Method.POST ~body:(endless raised) "/") in
          equal int 413 (status resp) ;
          wait raised ;
          equal string "too large" (Body.to_string (body resp)) ;
          await s ;
          equal (list bool) [false] (t.releases ()) )
    ; test "a streamed 2xx before the end of the upload does not stop it"
        (fun () ->
          with_conn
          @@ fun t ->
          let data =
            String.init (8 * 1024 * 1024) (fun i -> Char.chr (i land 0xff))
          in
          let s =
            serve t (fun r ->
                let h = read_head r in
                respond t
                  "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n" ;
                let b =
                  H1_body.body
                    (H1_body.make ~limits:server_limits r
                       (H1_parse.request_framing h) )
                in
                let buf = Bytes.create 65536 in
                let rec echo () =
                  match Body.read b buf 0 65536 with
                  | 0 ->
                      respond t "0\r\n\r\n"
                  | n ->
                      respond t
                        (Printf.sprintf "%x\r\n%s\r\n" n
                           (Bytes.sub_string buf 0 n) ) ;
                      echo ()
                in
                echo () )
          in
          let upload =
            Body.of_writer (fun w ->
                let rec go i =
                  if i < String.length data then begin
                    let n = Int.min 65536 (String.length data - i) in
                    Body.Writer.write_string w (String.sub data i n) ;
                    Body.Writer.flush w ;
                    go (i + n)
                  end
                in
                go 0 )
          in
          let resp = t.run (req ~meth:Method.POST ~body:upload "/") in
          let got = Body.to_string (body resp) in
          is_true ~msg:"the echo is equal" (got = data) ;
          await s )
    ; test "the end of the input before the head gives Closed" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                Flow.close t.server )
          in
          equal string
            "Caravan_client.Error: the connection closed before the response"
            (error_text (fun () -> t.run (req "/"))) ;
          await s )
    ; test "the end of the input after a part of a head gives Bad_response"
        (fun () ->
          let ended_after part =
            with_conn
            @@ fun t ->
            let s =
              serve t (fun r ->
                  ignore (read_request r) ;
                  respond t part ;
                  Flow.close t.server )
            in
            let e = error_text (fun () -> t.run (req "/")) in
            await s ; e
          in
          equal string
            "Caravan_client.Error: bad response: the connection ended in a \
             response head"
            (ended_after "HTTP/1.1 20") ;
          equal string
            "Caravan_client.Error: bad response: the connection ended in a \
             response head"
            (ended_after "HTTP/1.1 100 Continue\r\n\r\n") )
    ; test "a reset before the head gives Closed" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                Flow.reset t.server )
          in
          equal string
            "Caravan_client.Error: the connection closed before the response"
            (error_text (fun () -> t.run (req "/"))) ;
          await s )
    ; test "a cancel of the caller closes the connection" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab" ;
                (* The end of the stream comes when the client closes. *)
                ignore (Buf_reader.peek_eof r) )
          in
          let reading = Ivar.create () in
          let caller =
            Sim_support.call ~name:"caller" t.scope (fun () ->
                let b = body (t.run (req "/")) in
                ignore (Body.read b (Bytes.create 8) 0 8) ;
                fill reading ;
                Body.to_string b )
          in
          wait reading ;
          Sim_support.cancel caller ;
          raises Promise.Cancelled (fun () -> await caller) ;
          equal (list bool) [false] (t.releases ()) ;
          await s )
    ; test "a 101 gives Unsupported" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t
                  "HTTP/1.1 101 Switching Protocols\r\n\
                   Upgrade: x\r\n\
                   Connection: upgrade\r\n\
                   \r\n" ;
                ignore (Buf_reader.peek_eof r) )
          in
          equal string "Caravan_client.Error: unsupported: protocol switch"
            (error_text (fun () -> t.run (req "/"))) ;
          await s ;
          equal (list bool) [false] (t.releases ()) )
    ; test "a failure of the producer goes to the caller at once" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r -> ignore (error_text (fun () -> read_request r)))
          in
          let failing =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                failwith "x" )
          in
          let config =
            {Client_config.default with response_timeout= Span.s 10}
          in
          equal string "Failure(\"x\")"
            (error_text (fun () ->
                 t.run ~config (req ~meth:Method.POST ~body:failing "/") ) ) ;
          equal (list bool) [false] (t.releases ()) ;
          await s )
    ; test "a 431 during a large request head, and its body arrives" (fun () ->
          with_conn
          @@ fun t ->
          let returned = Ivar.create () in
          let s =
            serve t (fun _ ->
                ignore (Flow.read t.server (Bytes.create 1024) 0 1024) ;
                respond t
                  "HTTP/1.1 431 Too Large\r\n\
                   Connection: close\r\n\
                   Content-Length: 7\r\n\
                   \r\n" ;
                wait returned ;
                respond t "too big" )
          in
          let resp =
            t.run
              (req ~headers:[("X-Big", String.make (8 * 1024 * 1024) 'b')] "/")
          in
          fill returned ;
          equal int 431 (status resp) ;
          equal string "too big" (Body.to_string (body resp)) ;
          await s ;
          equal (list bool) [false] (t.releases ()) )
    ; test "a truncated body gives Truncated" (fun () ->
          let truncated ~reset response =
            with_conn
            @@ fun t ->
            let s =
              serve t (fun r ->
                  ignore (read_request r) ;
                  respond t response ;
                  if reset then Flow.reset t.server else Flow.close t.server )
            in
            let resp = t.run (req "/") in
            let e = error_text (fun () -> Body.to_string (body resp)) in
            await s ; e
          in
          equal string "Caravan_client.Error: the response body is truncated"
            (truncated ~reset:false
               "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab" ) ;
          equal string "Caravan_client.Error: the response body is truncated"
            (truncated ~reset:true "HTTP/1.1 200 OK\r\n\r\nab") )
    ; test "trailers come after a chunked body" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t
                  "HTTP/1.1 200 OK\r\n\
                   Transfer-Encoding: chunked\r\n\
                   \r\n\
                   2\r\n\
                   ok\r\n\
                   0\r\n\
                   X-T: 1\r\n\
                   \r\n" )
          in
          let b = body (t.run (req "/")) in
          equal string "ok" (Body.to_string b) ;
          equal
            (list (pair string string))
            [("X-T", "1")]
            (Headers.to_list (Body.trailers b)) ;
          await s )
    ; test "an empty body releases the connection at once" (fun () ->
          let released ?(meth = Method.GET) response =
            with_conn
            @@ fun t ->
            let s =
              serve t (fun r ->
                  ignore (read_request r) ;
                  respond t response )
            in
            ignore (t.run (req ~meth "/")) ;
            await s ;
            t.releases ()
          in
          List.iter
            (fun (meth, r) -> equal (list bool) [true] (released ~meth r))
            Method.
              [ (HEAD, "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n")
              ; (GET, "HTTP/1.1 204 No Content\r\n\r\n")
              ; (GET, "HTTP/1.1 304 Not Modified\r\nContent-Length: 5\r\n\r\n")
              ; (GET, "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n") ] )
    ; test "the end of the response does not wait for the producer" (fun () ->
          let check ~response =
            with_conn
            @@ fun t ->
            let s =
              serve t (fun r ->
                  let h = read_head r in
                  respond t response ;
                  ignore (read_body r h) )
            in
            let go = Ivar.create () in
            (* The producer waits for the caller. *)
            let waiting =
              Body.of_writer (fun w ->
                  Body.Writer.write_string w "abc" ;
                  Body.Writer.flush w ;
                  wait go ;
                  Body.Writer.write_string w "def" )
            in
            let got =
              Body.to_string
                (body (t.run (req ~meth:Method.POST ~body:waiting "/")))
            in
            (* The connection goes back only after the producer. *)
            equal (list bool) [] (t.releases ()) ;
            fill go ;
            check_settled t.sim ~msg:"released after the producer" (fun () ->
                t.releases () = [true] ) ;
            await s ;
            got
          in
          equal string "ok"
            (check ~response:"HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok") ;
          equal string ""
            (check ~response:"HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n") )
    ; test
        "a producer that fails after a deferred release closes the connection"
        (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" ;
                ignore (Buf_reader.peek_eof r) )
          in
          let go = Ivar.create () in
          let failing =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                wait go ;
                failwith "late" )
          in
          equal string "ok"
            (Body.to_string
               (body (t.run (req ~meth:Method.POST ~body:failing "/"))) ) ;
          fill go ;
          check_settled t.sim ~msg:"released, not reusable" (fun () ->
              t.releases () = [false] ) ;
          await s )
    ; test "a body of length 0 with a producer does not make the response wait"
        (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                let h = read_head r in
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" ;
                ignore (read_body r h) )
          in
          let go = Ivar.create () in
          (* Content-Length: 0, but the producer runs and waits for the
             caller. *)
          let waiting =
            Body.of_writer ~length:0 (fun w -> Body.Writer.flush w ; wait go)
          in
          let st =
            Fun.protect
              ~finally:(fun () -> fill go)
              (fun () ->
                status (t.run (req ~meth:Method.POST ~body:waiting "/")) )
          in
          equal int 200 st ;
          check_settled t.sim ~msg:"released after the producer" (fun () ->
              t.releases () = [true] ) ;
          await s )
    ; test
        "a cancel during the last writes of the request closes the connection"
        (fun () ->
          (* The write of the head waits for [hold] after it sends its bytes:
             the read that ends the response body waits for the last write. *)
          let hold = Ivar.create () in
          Sim_support.run
          @@ fun sim ->
          with_held_writes ~hold:(fun _ -> true) ~release:hold sim
          @@ fun ~scope ~server ~writers ~conn ~taken:_ ->
          let releases = ref [] in
          let r = Buf_reader.of_flow server in
          let s =
            Sim_support.call ~name:"server" scope (fun () ->
                ignore (read_request r) ;
                Flow.write_string server
                  "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" )
          in
          let read_ok = Ivar.create () in
          let caller =
            Sim_support.call ~name:"caller" scope (fun () ->
                let b = body (exchange writers conn releases (req "/")) in
                let buf = Bytes.create 2 in
                let n = Body.read b buf 0 2 in
                ignore (Ivar.try_fill read_ok (Bytes.sub_string buf 0 n)) ;
                (* The end of the body: the release waits for the write. *)
                Body.read b buf 0 2 )
          in
          equal string "ok" (sync (Ivar.read read_ok)) ;
          Sim_support.cancel caller ;
          raises Promise.Cancelled (fun () -> await caller) ;
          equal (list bool) [false] !releases ;
          await s )
    ; test "the head goes before the producer runs" (fun () ->
          (* A chunked body, and a body of known length. *)
          let case length =
            with_conn
            @@ fun t ->
            let s =
              serve t (fun r ->
                  let h = read_head r in
                  respond t "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" ;
                  ignore (read_body r h) )
            in
            let go = Ivar.create () in
            (* The producer waits for the response before its first write. *)
            let waiting =
              Body.of_writer ?length (fun w ->
                  wait go ;
                  Body.Writer.write_string w "abc" )
            in
            let got =
              Fun.protect
                ~finally:(fun () -> fill go)
                (fun () ->
                  Body.to_string
                    (body (t.run (req ~meth:Method.POST ~body:waiting "/"))) )
            in
            equal string "ok" got ;
            check_settled t.sim ~msg:"released after the producer" (fun () ->
                t.releases () = [true] ) ;
            await s
          in
          case None ; case (Some 3) )
    ; test "a cancel during the close of a source is a cancel of the client"
        (fun () ->
          with_conn
          @@ fun t ->
          let closes = Atomic.make 0 and closing = Ivar.create () in
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                respond t
                  "HTTP/1.1 413 Too Large\r\n\
                   Connection: close\r\n\
                   Content-Length: 4\r\n\
                   \r\n" ;
                (* Only the cancel at the response head ends the read of the
                   source. Then the source closes. *)
                wait closing ;
                respond t "nope" )
          in
          (* The read waits for a cancel. The close waits too, thus it gets the
             cancel of the job. *)
          let source =
            Body.of_source ~length:3
              ~close:(fun () -> Atomic.incr closes ; fill closing ; sync never)
              (fun _ _ _ -> sync never)
          in
          let resp = t.run (req ~meth:Method.POST ~body:source "/") in
          equal int 413 (status resp) ;
          equal string "nope" (Body.to_string (body resp)) ;
          equal (list bool) [false] (t.releases ()) ;
          equal int 1 (Atomic.get closes) ;
          await s )
    ; test "the body closes when the writers stopped before the job" (fun () ->
          Sim_support.run
          @@ fun sim ->
          Server_support.with_flows sim
          @@ fun ~server:_ ~client ->
          Fiber.scope
          @@ fun scope ->
          let writers = Writers.make scope in
          Writers.stop writers ;
          Writers.wait writers ;
          let conn = Conn.make ~host:"a" ~port:80 client
          and releases = ref []
          and closes = Atomic.make 0 in
          let source =
            Body.of_source ~length:3
              ~close:(fun () -> Atomic.incr closes)
              (fun _ _ _ -> 0)
          in
          raises_match is_invalid_arg (fun () ->
              exchange writers conn releases
                (req ~meth:Method.POST ~body:source "/") ) ;
          equal (list bool) [false] !releases ;
          equal int 1 (Atomic.get closes) )
    ; test "an exception of the close of a source goes to the caller" (fun () ->
          (* A chunked body: the server waits for its end, which does not go,
             thus the error of the writer comes before a response. *)
          let case ~read ~close expected =
            with_conn
            @@ fun t ->
            let s =
              serve t (fun r -> try ignore (read_request r) with _ -> ())
            in
            let sent = ref false in
            let source =
              Body.of_source ~close (fun buf off _ ->
                  read () ;
                  if !sent then 0
                  else (
                    Bytes.blit_string "abc" 0 buf off 3 ;
                    sent := true ;
                    3 ) )
            in
            raises expected (fun () ->
                ignore (t.run (req ~meth:Method.POST ~body:source "/")) ) ;
            equal (list bool) [false] (t.releases ()) ;
            Flow.close t.server ;
            await s
          in
          case ~read:ignore ~close:(fun () -> failwith "close") (Failure "close") ;
          (* The exception of the read has priority. *)
          case
            ~read:(fun () -> failwith "read")
            ~close:(fun () -> failwith "close")
            (Failure "read") )
    ; test
        "a cancel during the close of a source after its reads is a cancel of \
         the client" (fun () ->
          with_conn
          @@ fun t ->
          let in_close = Ivar.create () and close_cancelled = Ivar.create () in
          (* The server answers while the close waits. It sends the body only
             after the close got its cancel: only the cancel at the response
             head ends the close. *)
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                wait in_close ;
                respond t
                  "HTTP/1.1 413 Too Large\r\n\
                   Connection: close\r\n\
                   Content-Length: 4\r\n\
                   \r\n" ;
                wait close_cancelled ;
                respond t "nope" )
          in
          let closes = Atomic.make 0 in
          let source =
            abc_source ~closes
              ~close:(fun () ->
                fill in_close ;
                try sync never
                with Fiber.Cancelled _ as e -> fill close_cancelled ; raise e )
              ()
          in
          let resp = t.run (req ~meth:Method.POST ~body:source "/") in
          equal int 413 (status resp) ;
          equal string "nope" (Body.to_string (body resp)) ;
          equal (list bool) [false] (t.releases ()) ;
          equal int 1 (Atomic.get closes) ;
          await s )
    ; test "a close of a request with no body that raises keeps the connection"
        (fun () ->
          let case close check =
            with_conn
            @@ fun t ->
            let source = Body.of_source ~length:0 ~close (fun _ _ _ -> 0) in
            raises_match check (fun () -> t.run (req ~body:source "/")) ;
            equal (list bool) [true] (t.releases ())
          in
          case
            (fun () -> failwith "close")
            (function Failure s -> s = "close" | _ -> false) ;
          (* A Cancelled of a different scope: the caller is not cancelled. *)
          case
            (fun () -> raise (Fiber.Cancelled Exit))
            (function Failure _ -> true | _ -> false) )
    ; test
        "a cancel during the close of a request with no body keeps the \
         connection" (fun () ->
          with_conn
          @@ fun t ->
          let in_close = Ivar.create () in
          let source =
            Body.of_source ~length:0
              ~close:(fun () -> fill in_close ; sync never)
              (fun _ _ _ -> 0)
          in
          let caller =
            Sim_support.call ~name:"caller" t.scope (fun () ->
                ignore (t.run (req ~body:source "/")) )
          in
          wait in_close ;
          Sim_support.cancel caller ;
          raises Promise.Cancelled (fun () -> await caller) ;
          equal (list bool) [true] (t.releases ()) )
    ; test "the close of a request with no body runs in the caller" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" )
          in
          let closer = ref [] in
          let source =
            Body.of_source ~length:0
              ~close:(fun () -> closer := Fiber.self () :: !closer)
              (fun _ _ _ -> 0)
          in
          let caller =
            Sim_support.call ~name:"caller" t.scope (fun () ->
                let st = status (t.run (req ~body:source "/")) in
                (st, Fiber.self ()) )
          in
          let st, me = await caller in
          equal int 200 st ;
          ( match !closer with
          | [c] ->
              is_true ~msg:"the caller closed" (Fiber.Id.equal c me)
          | l ->
              fail (Printf.sprintf "%d closes" (List.length l)) ) ;
          await s )
    ; test "the close of a user source does not make the response wait"
        (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" )
          in
          let go = Ivar.create () in
          (* The close of the source waits for the caller. *)
          let source = abc_source ~close:(fun () -> wait go) () in
          let got =
            Fun.protect
              ~finally:(fun () -> fill go)
              (fun () ->
                Body.to_string
                  (body (t.run (req ~meth:Method.POST ~body:source "/"))) )
          in
          equal string "ok" got ;
          check_settled t.sim ~msg:"released after the close" (fun () ->
              t.releases () = [true] ) ;
          await s )
    ; test "a cancel during the release inside run closes the connection"
        (fun () ->
          (* The write of the head waits for [hold] after it sends its bytes.
             The response has no body, thus run itself waits for the last
             write. *)
          let hold = Ivar.create () in
          let head = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" in
          Sim_support.run
          @@ fun sim ->
          with_held_writes ~hold:(fun _ -> true) ~release:hold sim
          @@ fun ~scope ~server ~writers ~conn ~taken ->
          let releases = ref [] in
          let r = Buf_reader.of_flow server in
          let s =
            Sim_support.call ~name:"server" scope (fun () ->
                ignore (read_request r) ;
                Flow.write_string server head )
          in
          let caller =
            Sim_support.call ~name:"caller" scope (fun () ->
                exchange writers conn releases (req "/") )
          in
          (* After the client took the whole head, nothing waits or checks for a
             cancel before the wait for the write. *)
          check_settled sim ~msg:"the client took the head" (fun () ->
              Atomic.get taken >= String.length head ) ;
          Sim_support.cancel caller ;
          raises Promise.Cancelled (fun () -> ignore (await caller)) ;
          equal (list bool) [false] !releases ;
          await s )
    ; test "a user source of the request body gets its close" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" )
          in
          let closes = Atomic.make 0 in
          equal int 200
            (status
               (t.run (req ~meth:Method.POST ~body:(abc_source ~closes ()) "/")) ) ;
          await s ;
          check_settled t.sim ~msg:"closed one time" (fun () ->
              Atomic.get closes = 1 ) )
    ; test "a producer that closes its own response releases the connection"
        (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n" )
          in
          let resp = ref None
          and ready = Ivar.create ()
          and seen = Ivar.create () in
          let producer =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                wait ready ;
                match Body.close (body (Option.get !resp)) with
                | () ->
                    ignore (Ivar.try_fill seen "no error")
                | exception Invalid_argument _ ->
                    ignore (Ivar.try_fill seen "Invalid_argument") )
          in
          resp := Some (t.run (req ~meth:Method.POST ~body:producer "/")) ;
          fill ready ;
          equal string "Invalid_argument" (sync (Ivar.read seen)) ;
          equal (list bool) [false] (t.releases ()) ;
          await s )
    ; test "which ends of the writer stop a read" (fun () ->
          let show phase e = string_of_bool (Exchange.stops phase e) in
          equal (list string)
            [ "false"
            ; "true"
            ; "true"
            ; "false"
            ; "false"
            ; "true"
            ; "true"
            ; "true"
            ; "true"
            ; "false" ]
            [ show `Head (Exchange.Net Flow.Closed)
            ; show `Head (Exchange.Net Flow.Timeout)
            ; show `Head Flow.Closed
            ; show `Body (Exchange.Net Flow.Timeout)
            ; show `Body Writers.Job_cancelled
            ; show `Body (Failure "x")
            ; show `Body H1_write.Length_mismatch
            ; show `Head Flow.Timeout
            ; show `Head (Fiber.Cancelled Exit)
            ; show `Head (Exchange.Net (Net.Error (Other "ETIMEDOUT", "write")))
            ] )
    ; test "a producer that raises Cancelled of a different scope gives Failure"
        (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r -> ignore (error_text (fun () -> read_request r)))
          in
          let failing =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                Fiber.scope (fun s -> Fiber.cancel s Exit ; sync never) )
          in
          equal string
            "Failure(\"Caravan_client: the request body producer raised \
             Cancelled\")"
            (error_text (fun () ->
                 t.run (req ~meth:Method.POST ~body:failing "/") ) ) ;
          await s )
    ; test "the end of the upload wakes the reader of the head one or two times"
        (fun () ->
          (* The writer fills [sent], then its result, with no wait between. The
             reader of the head makes one read with no byte before its wait, and
             one at each wake. It wakes at [sent] and at the end of the writer,
             or only at the end of the writer: 2 or 3 reads with no byte. A turn
             on a ready event gives more. The response comes 10 ms later. The
             seed selects the event of the first wake, thus 20 requests. On 4
             domains, a stale readiness can give one more wake. Thus the test
             runs on one domain, where 100 seeds give 2 or 3. *)
          Sim_support.run
          @@ fun sim ->
          with_client_flow sim
          @@ fun ~scope ~server ~writers ~conn ~taken:_ ->
          let flow, nones = none_counted conn.flow in
          let conn = Conn.make ~host:"a" ~port:80 flow and releases = ref [] in
          let r = Buf_reader.of_flow server in
          let s =
            Sim_support.call ~name:"server" scope (fun () ->
                for _ = 1 to 20 do
                  ignore (read_request r) ;
                  Time.sleep Mono (Span.ms 10) ;
                  Flow.write_string server
                    "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"
                done )
          in
          let counts =
            List.init 20 (fun _ ->
                let before = Atomic.get nones in
                let resp = exchange writers conn releases (req "/") in
                equal string "ok" (Body.to_string (body resp)) ;
                Atomic.get nones - before )
          in
          await s ;
          let text = String.concat " " (List.map string_of_int counts) in
          is_true
            ~msg:("no turn on a ready event: " ^ text)
            (List.for_all (fun n -> n = 2 || n = 3) counts) ;
          is_true ~msg:("a wake at sent: " ^ text) (List.mem 3 counts) )
    ; test "a write error of the network lets the response body arrive"
        (fun () ->
          List.iter
            (fun error ->
              Sim_support.run
              @@ fun sim ->
              with_breakable error sim
              @@ fun ~scope ~server ~run ~break ~failed ->
              let go = Ivar.create () in
              let producer =
                Body.of_writer (fun w ->
                    Body.Writer.write_string w "abc" ;
                    Body.Writer.flush w ;
                    wait go ;
                    Body.Writer.write_string w "def" ;
                    Body.Writer.flush w )
              in
              let r = Buf_reader.of_flow server in
              let s =
                Sim_support.call ~name:"server" scope (fun () ->
                    ignore (read_head r) ;
                    Flow.write_string server
                      "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nab" )
              in
              let resp =
                run Client_config.default
                  (Request.make ~authority:"a" ~body:producer Method.POST "/")
              in
              await s ;
              let b = body resp in
              let buf = Bytes.create 2 in
              equal string "ab" (Bytes.sub_string buf 0 (Body.read b buf 0 2)) ;
              (* The write error comes first, then the rest of the body. *)
              break () ;
              fill go ;
              wait failed ;
              Flow.write_string server "cd" ;
              equal string "cd" (Body.to_string b) )
            [Flow.Closed; net_eio] )
    ; test "a full response before the end of the last write releases at once"
        (fun () ->
          (* The last write sends its bytes, then waits for [release]: the
             response comes while the writer is in its last write. A request
             without a body runs no user code, and the producer of a chunked
             body returned before the last chunk. Thus the release waits for the
             writer, and the connection is reusable when the read ends. *)
          let response = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" in
          List.iter
            (fun (meth, request_body, last) ->
              let release = Ivar.create () in
              Sim_support.run
              @@ fun sim ->
              with_held_writes ~hold:last ~release sim
              @@ fun ~scope ~server ~writers ~conn ~taken ->
              let releases = ref [] in
              let r = Buf_reader.of_flow server in
              let s =
                Sim_support.call ~name:"server" scope (fun () ->
                    (* The whole request, then the response. *)
                    ignore (read_request r) ;
                    Flow.write_string server response )
              in
              let c =
                Sim_support.call ~name:"caller" scope (fun () ->
                    Body.to_string
                      (body
                         (exchange writers conn releases
                            (req ~meth ~body:(request_body ()) "/") ) ) )
              in
              check_settled sim ~msg:"the client took the response" (fun () ->
                  Atomic.get taken >= String.length response ) ;
              (* Only [release] can end the last write. *)
              is_false ~msg:"no end before the last write returns"
                (Sim_support.ended sim c) ;
              fill release ;
              equal string "ok" (await c) ;
              equal (list bool) [true] !releases ;
              await s )
            [ ( Method.GET
              , (fun () -> Body.empty)
              , fun sent -> String.ends_with ~suffix:"\r\n\r\n" sent )
            ; ( Method.POST
              , (fun () ->
                  Body.of_writer (fun w -> Body.Writer.write_string w "abc") )
              , fun sent -> String.ends_with ~suffix:"0\r\n\r\n" sent ) ] )
    ; test
        "a producer that raises Flow.Closed of a different flow is a user error"
        (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r -> ignore (error_text (fun () -> read_request r)))
          in
          let failing =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                raise Flow.Closed )
          in
          let config =
            {Client_config.default with response_timeout= Span.ms 200}
          in
          equal string "Caravan__Flow.Closed"
            (error_text (fun () ->
                 t.run ~config (req ~meth:Method.POST ~body:failing "/") ) ) ;
          await s )
    ; test "Body.close before the end stops the upload" (fun () ->
          with_conn
          @@ fun t ->
          let raised = Ivar.create () in
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n" )
          in
          let resp = t.run (req ~meth:Method.POST ~body:(endless raised) "/") in
          Body.close (body resp) ;
          wait raised ;
          equal (list bool) [false] (t.releases ()) ;
          await s )
    ; test "a Body.close after the end does nothing" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" )
          in
          let b = body (t.run (req "/")) in
          equal string "ok" (Body.to_string b) ;
          equal int 0 (Body.read b (Bytes.create 4) 0 4) ;
          Body.close b ;
          equal (list bool) [true] (t.releases ()) ;
          await s )
    ; test "a bad head from a peer that does not read" (fun () ->
          with_conn
          @@ fun t ->
          let raised = Ivar.create () in
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                respond t "HTTP/1.1 2x0 X\r\n\r\n" )
          in
          equal string
            "Caravan_client.Error: bad response: 400 Bad Request (invalid \
             status code \"2x0\")"
            (error_text (fun () ->
                 t.run (req ~meth:Method.POST ~body:(endless raised) "/") ) ) ;
          wait raised ;
          equal (list bool) [false] (t.releases ()) ;
          await s )
    ; test "a wrong length gives Invalid_argument" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r -> ignore (error_text (fun () -> read_request r)))
          in
          let pos = ref 0 in
          let short =
            Body.of_source ~length:5 (fun buf off _ ->
                if !pos = 3 then 0 else (Bytes.set buf off 'a' ; incr pos ; 1) )
          in
          equal string
            "Invalid_argument(\"Caravan_client: the request body length \
             differs from its Content-Length\")"
            (error_text (fun () ->
                 t.run (req ~meth:Method.POST ~body:short "/") ) ) ;
          await s )
    ; test "a transfer coding other than chunked gives Unsupported" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip\r\n\r\n" )
          in
          equal string
            "Caravan_client.Error: unsupported: transfer codings other than \
             chunked"
            (error_text (fun () -> t.run (req "/"))) ;
          await s )
    ; test "Body.close raises the exception of the producer" (fun () ->
          with_conn
          @@ fun t ->
          let go = Ivar.create () and raised = Ivar.create () in
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 1000000\r\n\r\n" )
          in
          let failing =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                wait go ;
                fill raised ;
                failwith "x" )
          in
          let resp = t.run (req ~meth:Method.POST ~body:failing "/") in
          fill go ;
          wait raised ;
          equal string "Failure(\"x\")"
            (error_text (fun () -> Body.close (body resp))) ;
          equal (list bool) [false] (t.releases ()) ;
          await s )
    ; test "the writer is not a child of the caller" (fun () ->
          with_conn
          @@ fun t ->
          let gate = Ivar.create () in
          let s =
            serve t (fun r ->
                let h = read_head r in
                respond t
                  "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n" ;
                let b =
                  H1_body.body
                    (H1_body.make ~limits:server_limits r
                       (H1_parse.request_framing h) )
                in
                let buf = Bytes.create 64 in
                let rec echo () =
                  match Body.read b buf 0 64 with
                  | 0 ->
                      respond t "0\r\n\r\n"
                  | n ->
                      respond t
                        (Printf.sprintf "%x\r\n%s\r\n" n
                           (Bytes.sub_string buf 0 n) ) ;
                      echo ()
                in
                echo () )
          in
          let four =
            Body.of_writer (fun w ->
                for i = 1 to 4 do
                  Body.Writer.write_string w (Printf.sprintf "part %d;" i) ;
                  Body.Writer.flush w ;
                  wait gate
                done )
          in
          let f =
            Sim_support.call ~name:"caller" t.scope (fun () ->
                t.run (req ~meth:Method.POST ~body:four "/") )
          in
          (* F ends before the upload: the writer belongs to the client. *)
          let resp =
            Fun.protect ~finally:(fun () -> fill gate) (fun () -> await f)
          in
          equal string "part 1;part 2;part 3;part 4;"
            (Body.to_string (body resp)) ;
          await s )
    ; test "race: a stop right after the start of the writers is no failure"
        (fun () ->
          (* The daemon of the writers can see the cancel of [stop] before its
             wait for [stopping]. Its scope then raises Cancelled Stopped. The
             daemon ends with no failure of [s]. *)
          Sim_support.race
          @@ fun _ ->
          (* Few orders of the domains give that case: 50 tries for each
             seed. *)
          for _ = 1 to 50 do
            Fiber.scope
            @@ fun s ->
            let writers = Writers.make s in
            Writers.stop writers ; Writers.wait writers
          done )
    ; test "stopped writers refuse a job" (fun () ->
          with_conn
          @@ fun t ->
          Writers.stop t.writers ;
          equal string
            "Invalid_argument(\"Caravan_client: the client is closed\")"
            (error_text (fun () -> t.run (req "/"))) ;
          equal (list bool) [false] (t.releases ()) )
    ; test "writers whose scope ended refuse a job" (fun () ->
          (* The end of the scope cancels the daemon of the writers, and
             [stopping] stays empty: the fork into the scope of the writers
             gives Scope_closed. *)
          Sim_support.run
          @@ fun sim ->
          Server_support.with_flows sim
          @@ fun ~server:_ ~client ->
          let writers = Fiber.scope (fun s -> Writers.make s) in
          let conn = Conn.make ~host:"a" ~port:80 client
          and releases = ref []
          and closes = Atomic.make 0 in
          let source =
            Body.of_source ~length:3
              ~close:(fun () -> Atomic.incr closes)
              (fun _ _ _ -> 0)
          in
          equal string
            "Invalid_argument(\"Caravan_client: the client is closed\")"
            (error_text (fun () ->
                 exchange writers conn releases
                   (req ~meth:Method.POST ~body:source "/") ) ) ;
          equal (list bool) [false] !releases ;
          equal int 1 (Atomic.get closes) )
    ; test "a job cancelled before its first step gives Job_cancelled"
        (fun () ->
          Sim_support.run
          @@ fun _ ->
          Fiber.scope
          @@ fun s ->
          let writers = Writers.make s in
          let closes = Atomic.make 0 in
          let j =
            Writers.job (fun () ->
                Fun.protect
                  ~finally:(fun () -> Atomic.incr closes)
                  (fun () -> Fiber.yield ()) )
          in
          (* No switch between the two calls: the job did not start. *)
          Writers.start writers j ;
          Writers.cancel j ;
          ( match Writers.await ~cancel:false j with
          | Error (Writers.Job_cancelled, _) ->
              ()
          | Ok () ->
              fail "the job ended with Ok"
          | Error (e, _) ->
              fail (Printexc.to_string e) ) ;
          equal int ~msg:"the body ran one time" 1 (Atomic.get closes) ;
          Writers.stop writers ;
          Writers.wait writers )
    ; test "a deferred release after the close of the client closes the flow"
        (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" )
          in
          let go = Ivar.create () in
          (* The producer ignores the cancel until [go]. Its next write then
             raises the cancel of its job. *)
          let late =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                Fiber.shield (fun () -> wait go) ;
                Body.Writer.write_string w "def" ;
                Body.Writer.flush w )
          in
          (* The response ends first: the release waits for the producer. *)
          equal string "ok"
            (Body.to_string
               (body (t.run (req ~meth:Method.POST ~body:late "/"))) ) ;
          await s ;
          equal (list bool) [] (t.releases ()) ;
          let fds = Weft_sim.open_fds t.sim in
          Writers.stop t.writers ;
          fill go ;
          check_settled t.sim ~msg:"released, not reusable" (fun () ->
              t.releases () = [false] ) ;
          equal int ~msg:"the client socket closed" (fds - 1)
            (Weft_sim.open_fds t.sim) )
    ; test "a malformed chunk gives Bad_response" (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t
                  "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\n" )
          in
          let resp = t.run (req "/") in
          await s ;
          equal string
            "Caravan_client.Error: bad response: 400 Bad Request (invalid \
             chunk size)"
            (error_text (fun () -> Body.to_string (body resp))) )
    ; test "a bad request raises before the exchange" (fun () ->
          with_conn
          @@ fun t ->
          raises_match is_invalid_arg (fun () -> t.run (req "/a#b")) ;
          equal (list bool) [true] (t.releases ()) ;
          Weft_sim.settle t.sim ;
          is_true ~msg:"no byte for the server"
            (Flow.read_now t.server (Bytes.create 1) 0 1 = None) )
    ; test "a cancelled reader stops also when the input is ready" (fun () ->
          (* All the body is in the socket of the client: no read waits. The
             reader takes the bytes of its buffer, then its next read raises the
             cancel. *)
          with_conn
          @@ fun t ->
          let n = 60000 in
          let s =
            serve t (fun r ->
                ignore (read_request r) ;
                respond t
                  (Printf.sprintf
                     "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n\r\n%s" n
                     (String.make n 'x') ) )
          in
          let b = body (t.run (req "/")) in
          await s ;
          Weft_sim.settle t.sim ;
          ignore (Body.read b (Bytes.create 1) 0 1) ;
          let got = ref 0 in
          raises_match
            (function Fiber.Cancelled Exit -> true | _ -> false)
            (fun () ->
              Fiber.scope (fun s ->
                  Fiber.cancel s Exit ;
                  let buf = Bytes.create 1024 in
                  while true do
                    got := !got + Body.read b buf 0 1024
                  done ) ) ;
          is_true
            ~msg:(Printf.sprintf "%d bytes after the cancel" !got)
            (!got < Conn.buffer_size) ;
          equal (list bool) [false] (t.releases ()) )
    ; test "a cancel during the wait for the last writes stops the writer first"
        (fun () ->
          let hold = Ivar.create () and write_cancelled = Ivar.create () in
          Sim_support.run
          @@ fun sim ->
          with_held_writes ~write_cancelled
            ~hold:(fun _ -> true)
            ~release:hold sim
          @@ fun ~scope ~server ~writers ~conn ~taken:_ ->
          let releases = ref [] in
          let r = Buf_reader.of_flow server in
          let s =
            Sim_support.call ~name:"server" scope (fun () ->
                ignore (read_request r) ;
                Flow.write_string server
                  "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" )
          in
          let caller =
            Sim_support.call ~name:"caller" scope (fun () ->
                Body.to_string (body (exchange writers conn releases (req "/"))) )
          in
          await s ;
          is_false ~msg:"the read waits for the last write"
            (Sim_support.ended sim caller) ;
          Sim_support.cancel caller ;
          raises Promise.Cancelled (fun () -> ignore (await caller)) ;
          (* The release waits for the end of the writer. *)
          is_true ~msg:"the writer got the cancel" (is_set write_cancelled) ;
          equal (list bool) [false] !releases )
    ; test
        "the read that ends the body raises the error of the producer one time"
        (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" ;
                ignore (Buf_reader.peek_eof r) )
          in
          let go = Ivar.create () in
          let b =
            body (t.run (req ~meth:Method.POST ~body:(failing_after go) "/"))
          in
          (* The producer fails before the read: the response and the input are
             there, so the reads do not wait. *)
          fill go ;
          Weft_sim.settle t.sim ;
          equal string "Failure(\"x\")" (error_text (fun () -> Body.to_string b)) ;
          equal string "no error" (error_text (fun () -> Body.close b)) ;
          equal (list bool) [false] (t.releases ()) ;
          await s )
    ; test "an error of the producer during a body read comes out of the read"
        (fun () ->
          with_conn
          @@ fun t ->
          let s =
            serve t (fun r ->
                ignore (read_head r) ;
                respond t "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab" ;
                ignore (Buf_reader.peek_eof r) )
          in
          let go = Ivar.create () in
          let b =
            body (t.run (req ~meth:Method.POST ~body:(failing_after go) "/"))
          in
          equal int 2 (Body.read b (Bytes.create 8) 0 8) ;
          let reader =
            Sim_support.call ~name:"reader" t.scope (fun () ->
                let read = error_text (fun () -> Body.to_string b) in
                (read, error_text (fun () -> Body.close b)) )
          in
          is_false ~msg:"the read waits" (Sim_support.ended t.sim reader) ;
          fill go ;
          equal (pair string string)
            ("Failure(\"x\")", "no error")
            (await reader) ;
          equal (list bool) [false] (t.releases ()) ;
          await s )
    ; test "a refused job gives the refusal, not the error of the close"
        (fun () ->
          Sim_support.run
          @@ fun sim ->
          Server_support.with_flows sim
          @@ fun ~server:_ ~client ->
          let writers = Fiber.scope (fun s -> Writers.make s) in
          let conn = Conn.make ~host:"a" ~port:80 client
          and releases = ref [] in
          let source =
            Body.of_source ~length:3
              ~close:(fun () -> failwith "close")
              (fun _ _ _ -> 0)
          in
          raises_match is_invalid_arg (fun () ->
              exchange writers conn releases
                (req ~meth:Method.POST ~body:source "/") ) ) ]

(* Resolver and connect *)

(* The name of the hosts of the lookup tests. *)
let lookup_host = "a.example"

(* [with_listener addr f] runs [f] with a listener on [addr] that accepts
   connections and closes them. The listener closes at the end. *)
let with_listener addr f =
  let l = Tcp.Listener.listen addr in
  Fun.protect ~finally:(fun () -> Tcp.Listener.close l)
  @@ fun () ->
  Fiber.scope
  @@ fun s ->
  Fiber.fork_daemon ~name:"acceptor" s (fun () ->
      while true do
        let c, _ = Tcp.Listener.accept l in
        Net.close c
      done ) ;
  f ()

let resolver_tests =
  group "Resolver and connect"
    [ test "a numeric host is IPv4 or IPv6, with or without brackets" (fun () ->
          List.iter
            (fun (host, address) ->
              equal ~msg:host (option string) address (Resolver.numeric host) )
            Numeric_hosts.table )
    ; test "a numeric host needs no lookup" (fun () ->
          (* The simulator has no name: a lookup gives No_entry. *)
          Sim_support.run
          @@ fun _ ->
          equal (list string) ["127.0.0.1"] (Resolver.resolve "127.0.0.1") ;
          equal (list string) ["::1"] (Resolver.resolve "[::1]") )
    ; test "a host name uses Dns" (fun () ->
          Sim_support.run
          @@ fun sim ->
          Weft_sim.set_dns sim lookup_host (Addresses ["10.0.0.1"; "10.0.0.2"]) ;
          equal (list string) ["10.0.0.1"; "10.0.0.2"]
            (Resolver.resolve lookup_host) )
    ; test "a broken IPv6 host is not numeric" (fun () ->
          Sim_support.run
          @@ fun sim ->
          Weft_sim.set_dns sim "[::1" (Addresses ["127.0.0.1"]) ;
          equal (list string) ["127.0.0.1"] (Resolver.resolve "[::1") )
    ; test "no address gives Error (Connect _)" (fun () ->
          Sim_support.run
          @@ fun _ ->
          equal string
            "Caravan_client.Error: cannot connect: no address for a.example"
            (error_text (fun () -> Resolver.resolve lookup_host)) )
    ; test "connect tries the next address" (fun () ->
          Sim_support.run
          @@ fun sim ->
          Weft_sim.set_dns sim lookup_host (Addresses ["10.0.0.9"; "10.0.0.1"]) ;
          with_listener (Net.Tcp ("10.0.0.1", 80))
          @@ fun () ->
          Flow.close
            (Connect.connect Client_config.default ~host:lookup_host ~port:80) )
    ; test "all addresses refused gives Error (Connect _)" (fun () ->
          Sim_support.run
          @@ fun sim ->
          Weft_sim.set_dns sim lookup_host (Addresses ["10.0.0.8"; "10.0.0.9"]) ;
          equal string
            "Caravan_client.Error: cannot connect: Connection refused"
            (error_text (fun () ->
                 Connect.connect Client_config.default ~host:lookup_host
                   ~port:80 ) ) )
    ; test "a cancel during the connect stops it" (fun () ->
          (* In manual mode the time does not move: only the cancel can end the
             wait for the lookup. *)
          Sim_support.manual
          @@ fun sim ->
          Weft_sim.set_dns sim lookup_host No_answer ;
          let fds = Weft_sim.open_fds sim in
          Fiber.scope
          @@ fun s ->
          let c =
            Sim_support.call ~name:"connect" s (fun () ->
                Connect.connect Client_config.default ~host:lookup_host ~port:80 )
          in
          is_false ~msg:"the lookup waits" (Sim_support.ended sim c) ;
          Sim_support.cancel c ;
          raises Promise.Cancelled (fun () -> ignore (await c)) ;
          equal int ~msg:"open fds" fds (Weft_sim.open_fds sim) )
    ; test "duplicate addresses connect one time" (fun () ->
          Sim_support.run
          @@ fun sim ->
          Weft_sim.set_dns sim lookup_host (Addresses ["10.0.0.9"; "10.0.0.9"]) ;
          equal (list string) ["10.0.0.9"] (Resolver.resolve lookup_host) )
    ; test "race: a connect at its deadline leaks no socket" (fun () ->
          (* The latency and the jitter put the end of the connect near its
             deadline: before it, at it or after it. The seed selects the delay.
             Over 10 seeds or more, the two results occur. A run with a smaller
             number of seeds (CARAVAN_SEED, or a small CARAVAN_RACE_SEEDS) does
             not do this check. *)
          let config = {Client_config.default with connect_timeout= Span.s 3} in
          let seeds = Atomic.make 0
          and flows = Atomic.make 0
          and timeouts = Atomic.make 0 in
          Sim_support.race ~guard:(Span.s 10) (fun sim ->
              Atomic.incr seeds ;
              Weft_sim.set_latency sim (Span.ms 2500) ;
              Weft_sim.net_faults sim {Weft_sim.no_faults with jitter= Span.s 1} ;
              let fds = Weft_sim.open_fds sim in
              with_listener (Net.Tcp ("10.0.0.1", 80))
              @@ fun () ->
              ( match Connect.connect config ~host:"10.0.0.1" ~port:80 with
              | flow ->
                  Flow.close flow ; Atomic.incr flows
              | exception Caravan_client.Error (Timeout `Connect) ->
                  Atomic.incr timeouts ) ;
              Weft_sim.settle sim ;
              equal int ~msg:"only the listener is open" (fds + 1)
                (Weft_sim.open_fds sim) ) ;
          if Atomic.get seeds >= 10 then
            is_true
              ~msg:
                (Printf.sprintf "%d flows and %d timeouts" (Atomic.get flows)
                   (Atomic.get timeouts) )
              (Atomic.get flows > 0 && Atomic.get timeouts > 0) )
    ; test "a connect with no free fd gives Error (Connect _)" (fun () ->
          Sim_support.run
          @@ fun sim ->
          Weft_sim.set_fd_limit sim (Some (Weft_sim.open_fds sim)) ;
          equal string
            "Caravan_client.Error: cannot connect: no resources (EMFILE)"
            (error_text (fun () ->
                 Connect.connect Client_config.default ~host:"10.0.0.1" ~port:80 )
            ) )
    ; test "a lookup error gives Error (Connect _) with its text" (fun () ->
          Sim_support.run
          @@ fun _ ->
          equal string
            "Caravan_client.Error: cannot connect: EINVAL: a NUL byte in a \
             string"
            (error_text (fun () -> Resolver.resolve "a\000b")) ) ]

(* Pool *)

(* A request through a pool, as the client makes it. *)
let pooled pool writers ?(config = Client_config.default) ~port r =
  let c, _ = Pool.checkout pool ~host:"127.0.0.1" ~port in
  Exchange.run config writers c
    ~on_release:(fun c ~reusable -> Pool.release pool c ~reusable)
    r

let server_port server =
  match Server.local_addr server with Net.Tcp (_, p) -> p | Net.Unix _ -> 0

(* A server whose handler gives the id of the connection. *)
let id_handler req =
  Response.string (string_of_int (Conn_info.id (Option.get (Request.conn req))))

(* [f pool writers port] against a caravan server, in the body of a run. *)
let with_pool ?config ?(client_config = Client_config.default) ?engine
    ?(handler = id_handler) f =
  Server_support.with_server ?config ?engine ~handler
  @@ fun server ->
  Fiber.scope
  @@ fun s ->
  let writers = Writers.make s in
  let pool = Pool.make client_config in
  Fun.protect ~finally:(fun () ->
      Pool.close pool ; Writers.stop writers ; Writers.wait writers )
  @@ fun () -> f pool writers (server_port server)

let get_id pool writers ~port ?(meth = Method.GET) () =
  Body.to_string
    (body (pooled pool writers ~port (req ~meth ~body:Body.empty "/id")))

(* A flow for the fake connects: it counts its closes, and its server can close
   it. *)
type fake = {closes: int Atomic.t; server_closed: bool Atomic.t; flow: Flow.t}

let fake () =
  let closes = Atomic.make 0 and server_closed = Atomic.make false in
  let flow =
    Flow.make
      ~read:(fun _ _ _ -> 0)
      ~read_now:(fun _ _ _ -> if Atomic.get server_closed then Some 0 else None)
      ~write:(fun _ _ _ -> ())
      ~readable:never ~shutdown_send:ignore
      ~close:(fun () -> Atomic.incr closes)
      ~set_read_deadline:ignore ~set_write_deadline:ignore ()
  in
  {closes; server_closed; flow}

let is_closed f = Atomic.get f.closes > 0

(* A pool with fake connects. [made ()] gives the fake flows in the order of the
   connects. *)
let fake_pool ?(config = Client_config.default) () =
  let made = ref [] in
  let connect ~host:_ ~port:_ =
    let f = fake () in
    made := f :: !made ;
    f.flow
  in
  let pool = Pool.make ~connect config in
  (pool, fun () -> List.rev !made)

let pool_tests =
  group "Pool"
    [ test
        "a request body that ends after the response, then the next exchange \
         on the connection" (fun () ->
          Sim_support.run
          @@ fun sim ->
          with_pool ~handler:Server_support.handler
          @@ fun pool writers port ->
          let go_on = Ivar.create () in
          let late_body =
            Body.of_writer (fun w ->
                Body.Writer.write_string w (String.make 3000 'a') ;
                Body.Writer.flush w ;
                wait go_on ;
                Body.Writer.write_string w (String.make 3000 'b') )
          in
          (* /ignore answers before it reads the body. *)
          equal string "ignored"
            (Body.to_string
               (body
                  (pooled pool writers ~port
                     (req ~meth:Method.POST ~body:late_body "/ignore") ) ) ) ;
          equal int ~msg:"the writer keeps the connection" 0
            (Pool.idle_hosts pool) ;
          fill go_on ;
          check_settled sim ~msg:"the connection is back in the pool" (fun () ->
              Pool.idle_hosts pool = 1 ) ;
          let sent = String.init 9000 (fun i -> Char.chr (97 + (i mod 26))) in
          let c, tag = Pool.checkout pool ~host:"127.0.0.1" ~port in
          is_true ~msg:"the same connection" (tag = `Reused) ;
          let resp =
            Exchange.run Client_config.default writers c
              ~on_release:(fun c ~reusable -> Pool.release pool c ~reusable)
              (req ~meth:Method.POST ~body:(Body.of_string sent) "/echo")
          in
          equal string sent (Body.to_string (body resp)) )
    ; test "two requests in sequence use one connection" (fun () ->
          Sim_support.run
          @@ fun _ ->
          with_pool
          @@ fun pool writers port ->
          let a = get_id pool writers ~port () in
          let b = get_id pool writers ~port () in
          equal string a b )
    ; test "requests in sequence always use one connection" (fun () ->
          (* The release of a request without a body never waits for the writer
             at a different time: each next request finds the connection. *)
          Sim_support.run
          @@ fun _ ->
          with_pool
          @@ fun pool writers port ->
          let ids = List.init 1000 (fun _ -> get_id pool writers ~port ()) in
          equal int 1 (List.length (List.sort_uniq compare ids)) )
    ; test "two requests at the same time use two connections" (fun () ->
          (* The handler waits until [gate] opens: the second request of a round
             opens it. Thus the two requests are in the server at the same
             time. *)
          let arrived = Atomic.make 0 and gate = Atomic.make (Ivar.create ()) in
          let handler req =
            let g = Atomic.get gate in
            if Atomic.fetch_and_add arrived 1 = 1 then fill g ;
            sync (choose [Ivar.read g; Time.after Mono (Span.s 1)]) ;
            id_handler req
          in
          Sim_support.run
          @@ fun _ ->
          with_pool ~handler
          @@ fun pool writers port ->
          (* One idle connection first, with an open gate. *)
          let opened = Ivar.create () in
          fill opened ;
          Atomic.set gate opened ;
          ignore (get_id pool writers ~port ()) ;
          Atomic.set arrived 0 ;
          Atomic.set gate (Ivar.create ()) ;
          Fiber.scope
          @@ fun s ->
          let a = Sim_support.call s (fun () -> get_id pool writers ~port ()) in
          let b = Sim_support.call s (fun () -> get_id pool writers ~port ()) in
          let a = await a and b = await b in
          is_true ~msg:(a ^ " and " ^ b) (a <> b) )
    ; test "an idle connection older than idle_timeout is closed" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let pool, made = fake_pool () in
          let idle = Client_config.default.idle_timeout in
          let c, _ = Pool.checkout pool ~host:"a" ~port:80 in
          Pool.release pool c ~reusable:true ;
          Sim_support.advance sim (Span.s 4) ;
          let c', kind = Pool.checkout pool ~host:"A" ~port:80 in
          is_true ~msg:"reused at 4 s" (kind = `Reused && c'.Conn.id = c.Conn.id) ;
          Pool.release pool c' ~reusable:true ;
          Sim_support.advance sim (Span.of_ns (Int64.pred (Span.to_ns idle))) ;
          let c', kind = Pool.checkout pool ~host:"a" ~port:80 in
          is_true ~msg:"reused at idle_timeout - 1 ns" (kind = `Reused) ;
          Pool.release pool c' ~reusable:true ;
          Sim_support.advance sim idle ;
          let _, kind = Pool.checkout pool ~host:"a" ~port:80 in
          is_true ~msg:"new at idle_timeout" (kind = `New) ;
          equal (list bool) [true; false] (List.map is_closed (made ())) )
    ; test "a connection of age idle_timeout is stale" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let pool, made = fake_pool () in
          let c, _ = Pool.checkout pool ~host:"a" ~port:80 in
          Pool.release pool c ~reusable:true ;
          Sim_support.advance sim Client_config.default.idle_timeout ;
          let _, kind = Pool.checkout pool ~host:"a" ~port:80 in
          is_true ~msg:"new at idle_timeout" (kind = `New) ;
          equal (list bool) [true; false] (List.map is_closed (made ())) )
    ; test "max_idle_per_host keeps at most that number" (fun () ->
          Sim_support.run
          @@ fun _ ->
          let pool, made =
            fake_pool
              ~config:{Client_config.default with max_idle_per_host= 2}
              ()
          in
          let cs =
            List.init 3 (fun _ -> fst (Pool.checkout pool ~host:"a" ~port:80))
          in
          List.iter (fun c -> Pool.release pool c ~reusable:true) cs ;
          equal (list bool) [false; false; true] (List.map is_closed (made ())) ;
          let kinds =
            List.init 3 (fun _ -> snd (Pool.checkout pool ~host:"a" ~port:80))
          in
          equal (list bool) [true; true; false]
            (List.map (fun k -> k = `Reused) kinds) ;
          equal int 4 (List.length (made ())) )
    ; test "close closes idle and used connections" (fun () ->
          Sim_support.run
          @@ fun _ ->
          let pool, made = fake_pool () in
          let a, _ = Pool.checkout pool ~host:"a" ~port:80 in
          let _b, _ = Pool.checkout pool ~host:"a" ~port:80 in
          Pool.release pool a ~reusable:true ;
          Pool.close pool ;
          equal (list bool) [true; true] (List.map is_closed (made ())) ;
          raises_match is_invalid_arg (fun () ->
              Pool.checkout pool ~host:"a" ~port:80 ) )
    ; test "reap closes the idle connections that are too old" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let pool, made = fake_pool () in
          let a, _ = Pool.checkout pool ~host:"a" ~port:80 in
          let b, _ = Pool.checkout pool ~host:"b" ~port:80 in
          Pool.release pool a ~reusable:true ;
          Sim_support.advance sim (Span.s 3) ;
          Pool.release pool b ~reusable:true ;
          Sim_support.advance sim (Span.s 3) ;
          Pool.reap pool ;
          equal (list bool) [true; false] (List.map is_closed (made ())) )
    ; test "the timer runs each half of idle_timeout, and each 10 ms or more"
        (fun () ->
          let interval t =
            Int64.to_int
              (Span.to_ns
                 (Caravan_client.Private.reap_interval
                    {Client_config.default with idle_timeout= t} ) )
            / 1_000_000
          in
          equal (list int) [50; 10; 10; 2500]
            [ interval (Span.ms 100)
            ; interval (Span.ms 0)
            ; interval (Span.ms 20)
            ; interval (Span.ms 5000) ] )
    ; test "the pool keeps no empty list of a host" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let pool, _ = fake_pool () in
          let a, _ = Pool.checkout pool ~host:"a" ~port:80 in
          Pool.release pool a ~reusable:true ;
          equal int 1 (Pool.idle_hosts pool) ;
          let a, _ = Pool.checkout pool ~host:"a" ~port:80 in
          equal int 0 (Pool.idle_hosts pool) ;
          Pool.release pool a ~reusable:true ;
          Sim_support.advance sim (Span.s 6) ;
          Pool.reap pool ;
          equal int 0 (Pool.idle_hosts pool) )
    ; test "a new connection after close is refused without a connect"
        (fun () ->
          Sim_support.run
          @@ fun _ ->
          let pool, made = fake_pool () in
          Pool.close pool ;
          raises_match is_invalid_arg (fun () ->
              Pool.checkout ~reuse:false pool ~host:"a" ~port:80 ) ;
          equal int 0 (List.length (made ())) )
    ; test "a connect during close is closed" (fun () ->
          Sim_support.run
          @@ fun _ ->
          let pool = ref None and flows = ref [] in
          let connect ~host:_ ~port:_ =
            let f = fake () in
            flows := f :: !flows ;
            Pool.close (Option.get !pool) ;
            f.flow
          in
          pool := Some (Pool.make ~connect Client_config.default) ;
          raises_match is_invalid_arg (fun () ->
              Pool.checkout (Option.get !pool) ~host:"a" ~port:80 ) ;
          equal (list bool) [true] (List.map is_closed !flows) )
    ; test "race: checkout, release and reap on 4 domains" (fun () ->
          (* 8 fibers on 4 domains use the pool at the same time. A sleep
             between the checkout and the release lets the other fibers run. The
             short idle timeout makes some connections stale and lets the reaper
             close some. *)
          let config =
            { Client_config.default with
              max_idle_per_host= 2
            ; idle_timeout= Span.ms 3 }
          in
          Sim_support.race
          @@ fun _ ->
          let made = Atomic.make [] in
          let rec add x =
            let l = Atomic.get made in
            if not (Atomic.compare_and_set made l (x :: l)) then add x
          in
          let connect ~host ~port:_ =
            let f = fake () in
            add (host, f) ;
            f.flow
          in
          let pool = Pool.make ~connect config in
          let worker i () =
            for step = 1 to 20 do
              let host = if (i + step) mod 2 = 0 then "a" else "b" in
              let c, _ = Pool.checkout pool ~host ~port:80 in
              Time.sleep Mono (Span.ms (((i * 7) + (step * 3)) mod 5)) ;
              Pool.release pool c ~reusable:((i + step) mod 3 <> 0) ;
              if step mod 5 = 0 then Pool.reap pool
            done
          in
          Fiber.scope (fun s ->
              List.iter await
                (List.init 8 (fun i ->
                     Sim_support.call ~name:"worker" s (worker i) ) ) ) ;
          (* All connections went back: the open ones are idle. *)
          List.iter
            (fun host ->
              let idle =
                List.length
                  (List.filter
                     (fun (h, f) -> h = host && not (is_closed f))
                     (Atomic.get made) )
              in
              is_true
                ~msg:(Printf.sprintf "%d idle connections of %s" idle host)
                (idle <= config.max_idle_per_host) )
            ["a"; "b"] ;
          Pool.close pool ;
          List.iter
            (fun (_, f) ->
              equal int ~msg:"closes of a flow" 1 (Atomic.get f.closes) )
            (Atomic.get made) )
    ; test "a stale connection leaves the connections in use" (fun () ->
          Sim_support.run
          @@ fun _ ->
          let pool, made = fake_pool () in
          let a, _ = Pool.checkout pool ~host:"a" ~port:80 in
          Pool.release pool a ~reusable:true ;
          Atomic.set (List.hd (made ())).server_closed true ;
          let _, kind = Pool.checkout pool ~host:"a" ~port:80 in
          is_true ~msg:"a new connection" (kind = `New) ;
          equal int ~msg:"in use" 1 (Pool.in_use pool) )
    ; test "a release leaves the connections in use" (fun () ->
          Sim_support.run
          @@ fun _ ->
          let pool, _ = fake_pool () in
          let a, _ = Pool.checkout pool ~host:"a" ~port:80 in
          let b, _ = Pool.checkout pool ~host:"a" ~port:80 in
          equal int 2 (Pool.in_use pool) ;
          Pool.release pool a ~reusable:true ;
          Pool.release pool b ~reusable:false ;
          equal int 0 (Pool.in_use pool) )
    ; test "close empties the pool" (fun () ->
          Sim_support.run
          @@ fun _ ->
          let pool, _ = fake_pool () in
          let a, _ = Pool.checkout pool ~host:"a" ~port:80 in
          let _b, _ = Pool.checkout pool ~host:"b" ~port:80 in
          Pool.release pool a ~reusable:true ;
          Pool.close pool ;
          equal (pair int int) ~msg:"idle hosts, in use" (0, 0)
            (Pool.idle_hosts pool, Pool.in_use pool) )
    ; test "close closes a reused connection in use" (fun () ->
          Sim_support.run
          @@ fun _ ->
          let pool, made = fake_pool () in
          let a, _ = Pool.checkout pool ~host:"a" ~port:80 in
          Pool.release pool a ~reusable:true ;
          let _, kind = Pool.checkout pool ~host:"a" ~port:80 in
          is_true ~msg:"reused" (kind = `Reused) ;
          Pool.close pool ;
          equal (list bool) [true] (List.map is_closed (made ())) ) ]

(* The pool against a model: one host, the clock of the world and fake flows.
   Each program runs in its own run of the simulator in manual mode, and makes
   its pools first. *)
module Pool_model = struct
  (* The time in ms. All the pools of a program share the clock of the world.
     The model only compares two times of one run, so the clock never goes back
     to 0. *)
  let clock = ref 0

  type pool = {mutable connects: int; mutable idle: conn list}

  and conn =
    { pool: pool
    ; mutable state: [`Used | `Idle of int | `Closed]
    ; mutable server_closed: bool }

  let make () = {connects= 0; idle= []}

  (* Idle for less than 5 s, and not closed by its server. *)
  let rec checkout p =
    match p.idle with
    | c :: rest -> (
        p.idle <- rest ;
        match c.state with
        | `Idle t when !clock - t < 5000 && not c.server_closed ->
            c.state <- `Used ;
            c
        | _ ->
            c.state <- `Closed ;
            checkout p )
    | [] ->
        p.connects <- p.connects + 1 ;
        {pool= p; state= `Used; server_closed= false}

  let release c reusable =
    let p = c.pool in
    if reusable && List.length p.idle < 2 then (
      c.state <- `Idle !clock ;
      p.idle <- c :: p.idle )
    else c.state <- `Closed

  let advance _ ms = clock := !clock + ms

  let server_close c = c.server_closed <- true

  let is_closed c = c.state = `Closed

  let connects p = p.connects

  let used c = c.state = `Used

  let is_idle c = match c.state with `Idle _ -> true | _ -> false
end

module Pool_system = struct
  (* The simulator of the program that runs. *)
  let sim : Weft_sim.t option ref = ref None

  type pool = {pool: Pool.t; fakes: (int, fake) Hashtbl.t; connects: int ref}

  let owners : (int, pool) Hashtbl.t = Hashtbl.create 64

  let make () =
    let fakes = Hashtbl.create 8 and connects = ref 0 in
    let connect ~host:_ ~port:_ =
      incr connects ;
      let f = fake () in
      (* [Conn.make] takes the next id after the connect. *)
      Hashtbl.replace fakes (Atomic.get Conn.next_id) f ;
      f.flow
    in
    let config =
      {Client_config.default with max_idle_per_host= 2; idle_timeout= Span.s 5}
    in
    {pool= Pool.make ~connect config; fakes; connects}

  let checkout p =
    let c, _ = Pool.checkout p.pool ~host:"a" ~port:80 in
    Hashtbl.replace owners c.Conn.id p ;
    c

  let release (c : Conn.t) reusable =
    Pool.release (Hashtbl.find owners c.id).pool c ~reusable

  let advance _ n = Sim_support.advance (Option.get !sim) (Span.ms n)

  let fake_of (c : Conn.t) = Hashtbl.find (Hashtbl.find owners c.id).fakes c.id

  let server_close c = Atomic.set (fake_of c).server_closed true

  let is_closed c = is_closed (fake_of c)

  let connects p = !(p.connects)
end

(* The commands come from [Stateful] of windtrap, so that [Stateful.program] and
   [Stateful.execute] take them (as in test_server_stateful.ml). [Private] has
   no stability guarantee. The flake pins windtrap. *)
module Stateful = Windtrap.Private.Stateful

let pool_commands =
  let open Stateful in
  let pool =
    abstract "p"
      ~pp:(fun ppf _ -> Format.pp_print_string ppf "pool")
      ~release:ignore
  in
  let conn =
    abstract "c"
      ~pp:(fun ppf (c : Pool_model.conn) ->
        Format.pp_print_string ppf
          ( match c.state with
          | `Used ->
              "used"
          | `Idle _ ->
              "idle"
          | `Closed ->
              "closed" ) )
      ~release:ignore
  in
  [ command "pool" (Gen.unit @-> makes pool) Pool_model.make Pool_system.make
  ; command "checkout"
      (pool ^-> makes conn)
      Pool_model.checkout Pool_system.checkout
  ; command "release"
      ~pre:(fun c _ -> Pool_model.used c)
      (conn ^-> Gen.bool @-> returns unit)
      Pool_model.release Pool_system.release
  ; command "advance"
      (pool ^-> Gen.int_range 0 3000 @-> returns unit)
      Pool_model.advance Pool_system.advance
  ; command "server closes" ~pre:Pool_model.is_idle
      (conn ^-> returns unit)
      Pool_model.server_close Pool_system.server_close
  ; command "is closed"
      (conn ^-> returns bool)
      Pool_model.is_closed Pool_system.is_closed
  ; command "connects"
      (pool ^-> returns int)
      Pool_model.connects Pool_system.connects ]

(* [execute_pool p] runs the program [p] in a new run of the simulator in manual
   mode. The command "advance" moves the time of that run. *)
let execute_pool p =
  Sim_support.manual
  @@ fun sim ->
  Pool_system.sim := Some sim ;
  Fun.protect
    ~finally:(fun () -> Pool_system.sim := None)
    (fun () -> Stateful.execute p)

let pool_stateful =
  group "Pool model"
    [ test ~tags:["prop"; "stateful"] "the pool behaves like a model" (fun () ->
          Windtrap.Private.Run.property ~count:300 ~summary:Stateful.summary
            ~prints_run:true
            (Stateful.program ~steps:30 pool_commands)
            execute_pool ) ]

(* The public API *)

module C = Caravan_client

let exn_name = function
  | Fiber.Cancelled _ ->
      "Cancelled"
  | e ->
      Printexc.to_string e

(* The Ivar that lets the second half of /two-part go. *)
let two_part_go = ref (Ivar.create ())

(* The handler of the tests: "/id" gives the id of the connection, "/conn" the
   Connection field and the id, and the others go to the handler of
   Server_support. *)
let api_handler req =
  match Request.path_only req with
  | "/id" ->
      id_handler req
  | "/two-part" ->
      (* Half of the body, then the rest when the test lets it go. *)
      let c = Option.value (Request.query req) ~default:"x" in
      let go = !two_part_go in
      Response.stream (fun w ->
          Body.Writer.write_string w
            (String.concat "" (List.init 50 (fun _ -> c))) ;
          Body.Writer.flush w ;
          wait go ;
          Body.Writer.write_string w (String.make 50 'z') )
  | "/long-head" ->
      Response.string
        ~headers:(Headers.of_list [("X-Long", String.make 6000 'h')])
        "long"
  | "/conn" ->
      Response.string
        ( Option.value ~default:"-"
            (Headers.find "connection" (Request.headers req))
        ^ " "
        ^ Body.to_string
            ( match Response.kind (id_handler req) with
            | Body b ->
                b
            | _ ->
                Body.empty ) )
  | _ ->
      Server_support.handler req

let big_config = {Server_support.config with max_body= None}

let url_of server = Printf.sprintf "http://127.0.0.1:%d" (server_port server)

(* [client_in f] calls [f client url] in [C.run], against a caravan server. Call
   it in the body of a run. *)
let client_in ?(config = big_config) ?client_config f =
  Server_support.with_server ~config ~handler:api_handler
  @@ fun server ->
  C.run ?config:client_config @@ fun client -> f client (url_of server)

let with_client ?config ?client_config f =
  Sim_support.run @@ fun _ -> client_in ?config ?client_config f

let text resp = Body.to_string (C.body resp)

(* The port of a url of [with_client]. *)
let port_of url = int_of_string (List.nth (String.split_on_char ':' url) 2)

let get_text client url = text (C.get client url)

let at_port port ?(meth = Method.GET) ?(headers = []) ?body path =
  Request.make
    ~authority:(Printf.sprintf "127.0.0.1:%d" port)
    ~headers:(Headers.of_list headers) ?body meth path

(* A port with no listener: a connect gives Refused in the simulator. *)
let refused_port = 9

(* [scripts_in ~config scripts f] runs [f client port accepted] in [C.run] with
   a client of [config]. A listener on [port] runs [scripts] on its connections,
   one script for each connection, in order. [accepted] counts the connections.
   After [C.run], the scope waits for the scripts: the end of [C.run] closes the
   connections of the client. Call it in the body of a run. *)
let scripts_in ?config scripts f =
  let l = Tcp.Listener.listen Server_support.loopback in
  let port =
    match Tcp.Listener.addr l with Net.Tcp (_, p) -> p | Net.Unix _ -> 0
  in
  let accepted = Atomic.make 0 in
  Fun.protect ~finally:(fun () -> Tcp.Listener.close l)
  @@ fun () ->
  Fiber.scope
  @@ fun s ->
  Fiber.fork_daemon ~name:"acceptor" s (fun () ->
      List.iter
        (fun script ->
          let c, _ = Tcp.Listener.accept l in
          let flow = Tcp.flow c in
          Atomic.incr accepted ;
          Fiber.fork ~name:"script" s (fun () ->
              Fun.protect
                ~finally:(fun () -> Flow.close flow)
                (fun () ->
                  try script flow (Buf_reader.of_flow flow) with _ -> () ) ) )
        scripts ) ;
  C.run ?config (fun client -> f client port accepted)

let with_scripts ?config scripts f =
  Sim_support.run @@ fun _ -> scripts_in ?config scripts f

(* A server that answers at once, does not read the body, and waits for the end
   of the connection. *)
let answer_at_once flow r =
  ignore (read_head r) ;
  Flow.write_string flow "HTTP/1.1 200 OK\r\nContent-Length: 1000000\r\n\r\n" ;
  ignore (Buf_reader.peek_eof r)

(* A request body that writes 3 bytes, then waits until [release] is filled. A
   cancel fills [cancelled], then calls [on_cancel]. *)
let held_body ?(on_cancel = ignore) ~release ~cancelled () =
  Body.of_writer (fun w ->
      Body.Writer.write_string w "abc" ;
      Body.Writer.flush w ;
      try wait release with e -> fill cancelled ; on_cancel () ; raise e )

(* [grace_after_raise ~cancel] runs [C.run] with an [f] that makes a request,
   then raises [Failure "f"]. The request body waits for [release]. In the grace
   of [run], the test cancels [run] if [cancel], else it fills [release]. [run]
   must give the exception of [f]. The result tells if the body got a cancel. *)
let grace_after_raise ~cancel =
  Sim_support.run
  @@ fun sim ->
  let release = Ivar.create ()
  and cancelled = Ivar.create ()
  and raised = Ivar.create () in
  Fiber.scope
  @@ fun s ->
  let r =
    Sim_support.call ~name:"run" s (fun () ->
        scripts_in [answer_at_once]
        @@ fun client port _ ->
        ignore
          (C.request client
             (at_port port ~meth:Method.POST
                ~body:(held_body ~release ~cancelled ())
                "/" ) ) ;
        fill raised ;
        failwith "f" )
  in
  wait raised ;
  is_false ~msg:"run waits for the body" (Sim_support.ended sim r) ;
  if cancel then Sim_support.cancel r else fill release ;
  raises (Failure "f") (fun () -> await r) ;
  is_set cancelled

let api_tests =
  group "Caravan_client"
    [ test "the body closes when request raises before the exchange" (fun () ->
          Sim_support.run
          @@ fun _ ->
          C.run
          @@ fun client ->
          let case name r =
            let closes = Atomic.make 0 in
            let body =
              Body.of_source ~length:3
                ~close:(fun () -> Atomic.incr closes)
                (fun _ _ _ -> 0)
            in
            ( match C.request client (r body) with
            | _ ->
                fail (name ^ ": a response")
            | exception (C.Error _ | Invalid_argument _) ->
                () ) ;
            equal ~msg:name int 1 (Atomic.get closes)
          in
          let authority = Printf.sprintf "127.0.0.1:%d" refused_port in
          case "refused" (fun body ->
              Request.make ~authority ~body Method.POST "/" ) ;
          case "https" (fun body ->
              Request.make ~scheme:"https" ~authority ~body Method.POST "/" ) ;
          case "no authority" (fun body -> Request.make ~body Method.POST "/") ;
          case "bad path" (fun body ->
              Request.make ~authority ~body Method.POST "/a#b" ) )
    ; test
        "a body writer that its producer keeps cannot reach the next exchange"
        (fun () ->
          with_client
          @@ fun client url ->
          let port = port_of url in
          let post produce =
            at_port port ~meth:Method.POST ~body:(Body.of_writer produce)
              "/echo"
          in
          let id0 = get_text client (url ^ "/id") in
          let kept = ref None in
          equal string "one"
            (text
               (C.request client
                  (post (fun w ->
                       kept := Some w ;
                       Body.Writer.write_string w "one" ) ) ) ) ;
          let late = ref "" and nested = ref "" in
          let second =
            text
              (C.request client
                 (post (fun w ->
                      Body.Writer.write_string w "hello" ;
                      ( match
                          Body.Writer.write_string (Option.get !kept) "XXXX"
                        with
                      | () ->
                          late := "went through"
                      | exception Invalid_argument _ ->
                          late := "refused" ) ;
                      (* The stopped writer given to a nested writer body. *)
                      match
                        Body.write_to
                          (Body.of_writer (fun w2 ->
                               Body.Writer.write_string w2 "YYYY" ) )
                          (Option.get !kept)
                      with
                      | () ->
                          nested := "went through"
                      | exception Invalid_argument _ ->
                          nested := "refused" ) ) )
          in
          equal string "hello" second ;
          equal string "refused" !late ;
          equal string "refused" !nested ;
          equal string ~msg:"one connection" id0 (get_text client (url ^ "/id")) )
    ; test "a response that arrives while request bytes wait in the buffer"
        (fun () ->
          with_client
          @@ fun client url ->
          let port = port_of url in
          let go_on = Ivar.create () in
          let x = String.make 100 'x'
          and y = String.make 100 'y'
          and z = String.make 100 'z' in
          let produce w =
            Body.Writer.write_string w x ;
            Body.Writer.flush w ;
            (* Not flushed: these bytes wait in the write buffer while the
               response comes in. *)
            Body.Writer.write_string w y ;
            wait go_on ;
            Body.Writer.write_string w z
          in
          (* /echo sends its head with the first bytes of the echo. *)
          let resp =
            C.request client
              (at_port port ~meth:Method.POST ~body:(Body.of_writer produce)
                 "/echo" )
          in
          fill go_on ;
          equal string (x ^ y ^ z) (text resp) )
    ; test "two exchanges hold request bytes in their buffers at the same time"
        (fun () ->
          (* Each producer leaves bytes unsent in its write buffer and waits. A
             buffer that two connections share mixes the two requests. *)
          with_client
          @@ fun client url ->
          let port = port_of url in
          Fiber.scope
          @@ fun s ->
          let echo c ~waiting ~go_on =
            Sim_support.call ~name:"echo" s (fun () ->
                let first = String.make 50 c and second = String.make 50 'z' in
                let produce w =
                  Body.Writer.write_string w first ;
                  fill waiting ;
                  wait go_on ;
                  Body.Writer.write_string w second
                in
                text
                  (C.request client
                     (at_port port ~meth:Method.POST
                        ~body:(Body.of_writer produce) "/echo" ) ) )
          in
          let wa = Ivar.create () and wb = Ivar.create () in
          let go_on = Ivar.create () in
          let a = echo 'a' ~waiting:wa ~go_on in
          wait wa ;
          let b = echo 'b' ~waiting:wb ~go_on in
          wait wb ;
          fill go_on ;
          equal string (String.make 50 'a' ^ String.make 50 'z') (await a) ;
          equal string (String.make 50 'b' ^ String.make 50 'z') (await b) )
    ; test "two responses wait half read in their buffers at the same time"
        (fun () ->
          (* Each response is half read when the other one comes in. A read
             buffer that two connections share mixes the two responses. *)
          with_client
          @@ fun client url ->
          two_part_go := Ivar.create () ;
          let a = C.get client (url ^ "/two-part?a") in
          let b = C.get client (url ^ "/two-part?b") in
          fill !two_part_go ;
          equal string (String.make 50 'a' ^ String.make 50 'z') (text a) ;
          equal string (String.make 50 'b' ^ String.make 50 'z') (text b) )
    ; test "a response head larger than the read buffer" (fun () ->
          with_client
          @@ fun client url ->
          let id0 = get_text client (url ^ "/id") in
          List.iter
            (fun _ ->
              let resp = C.get client (url ^ "/long-head") in
              equal int 6000
                (String.length
                   (Option.value ~default:""
                      (Headers.find "X-Long" (Response.headers resp)) ) ) ;
              equal string "long" (text resp) )
            [1; 2] ;
          equal string "hello" (get_text client (url ^ "/hello")) ;
          equal string ~msg:"one connection" id0 (get_text client (url ^ "/id")) )
    ; test "two exchanges at the same time on two connections" (fun () ->
          with_client
          @@ fun client url ->
          let port = port_of url in
          Fiber.scope
          @@ fun s ->
          let echo c =
            Sim_support.call ~name:"echo" s (fun () ->
                let sent = String.make 6000 c in
                let got =
                  text
                    (C.request client
                       (at_port port ~meth:Method.POST
                          ~body:
                            (Body.of_writer (fun w ->
                                 String.iteri
                                   (fun i ch ->
                                     Body.Writer.write_string w
                                       (String.make 1 ch) ;
                                     if i mod 500 = 0 then (
                                       Body.Writer.flush w ; Fiber.yield () ) )
                                   sent ) )
                          "/echo" ) )
                in
                equal string sent got )
          in
          let a = echo 'a' and b = echo 'b' in
          await a ; await b )
    ; test "sequential exchanges with large heads and bodies" (fun () ->
          with_client
          @@ fun client url ->
          let port = port_of url in
          List.iter
            (fun n ->
              let body =
                String.init n (fun i -> Char.chr (65 + ((i + n) mod 26)))
              in
              let headers = [("X-Long", String.make 5000 'h')] in
              equal string body
                (text
                   (C.request client
                      (at_port port ~meth:Method.POST ~headers
                         ~body:(Body.of_string body) "/echo" ) ) ) )
            [4097; 1; 5000; 4096] )
    ; test "GET, HEAD, 204 and a stream, with reuse" (fun () ->
          with_client
          @@ fun client url ->
          let id0 = get_text client (url ^ "/id") in
          equal string "hello" (get_text client (url ^ "/hello")) ;
          let head =
            C.request client (at_port (port_of url) ~meth:Method.HEAD "/hello")
          in
          equal int 200 (status head) ;
          equal string "" (text head) ;
          equal string id0 (get_text client (url ^ "/id")) ;
          equal int 204 (status (C.get client (url ^ "/empty"))) ;
          equal string id0 (get_text client (url ^ "/id")) ;
          equal string "abcd" (get_text client (url ^ "/stream")) )
    ; test "an upload of unknown length goes chunked, and comes back" (fun () ->
          with_client
          @@ fun client url ->
          let b =
            Body.of_writer (fun w ->
                List.iter (Body.Writer.write_string w) ["one;"; "two;"; "three"] )
          in
          equal string "one;two;three"
            (text
               (C.request client
                  (at_port (port_of url) ~meth:Method.POST ~body:b "/echo") ) ) )
    ; test "8 MiB echoed while it uploads" (fun () ->
          with_client
          @@ fun client url ->
          let data =
            String.init (8 * 1024 * 1024) (fun i -> Char.chr (i land 0xff))
          in
          let b =
            Body.of_writer (fun w ->
                let rec go i =
                  if i < String.length data then begin
                    let n = Int.min 65536 (String.length data - i) in
                    Body.Writer.write_string w (String.sub data i n) ;
                    Body.Writer.flush w ;
                    go (i + n)
                  end
                in
                go 0 )
          in
          let got =
            text
              (C.request client
                 (at_port (port_of url) ~meth:Method.POST ~body:b "/echo") )
          in
          is_true ~msg:"the echo is equal" (got = data) )
    ; test "a stale connection is retried for a GET" (fun () ->
          let first flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\na" ;
            (* The next request comes, and the server closes. *)
            ignore (read_head r)
          and second flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\nb"
          in
          with_scripts [first; second]
          @@ fun client port accepted ->
          equal string "a" (text (C.request client (at_port port "/"))) ;
          equal string "b" (text (C.request client (at_port port "/"))) ;
          equal int 2 (Atomic.get accepted) )
    ; test "a stale connection is retried for a PUT with an empty body"
        (fun () ->
          let first flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\na" ;
            ignore (read_request r)
          and second flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\nb"
          in
          with_scripts [first; second]
          @@ fun client port accepted ->
          equal string "a" (text (C.request client (at_port port "/"))) ;
          equal string "b"
            (text
               (C.request client
                  (at_port port ~meth:Method.PUT ~body:(Body.of_string "") "/") ) ) ;
          equal int 2 (Atomic.get accepted) )
    ; test "a new connection that closes is not retried" (fun () ->
          let closes _ r = ignore (read_head r) in
          with_scripts [closes; closes]
          @@ fun client port accepted ->
          equal string
            "Caravan_client.Error: the connection closed before the response"
            (error_text (fun () -> C.request client (at_port port "/"))) ;
          equal int 1 (Atomic.get accepted) )
    ; test "a stale connection is not retried for a POST" (fun () ->
          let first flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\na" ;
            ignore (read_request r)
          in
          with_scripts [first; first]
          @@ fun client port accepted ->
          equal string "a" (text (C.request client (at_port port "/"))) ;
          equal string
            "Caravan_client.Error: the connection closed before the response"
            (error_text (fun () ->
                 C.request client
                   (at_port port ~meth:Method.POST ~body:Body.empty "/") ) ) ;
          equal int 1 (Atomic.get accepted) )
    ; test "a stale connection is not retried for a PUT with a body" (fun () ->
          let first flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\na" ;
            ignore (read_request r)
          in
          with_scripts [first; first]
          @@ fun client port accepted ->
          equal string "a" (text (C.request client (at_port port "/"))) ;
          equal string
            "Caravan_client.Error: the connection closed before the response"
            (error_text (fun () ->
                 C.request client
                   (at_port port ~meth:Method.PUT ~body:(Body.of_string "abc")
                      "/" ) ) ) ;
          equal int 1 (Atomic.get accepted) )
    ; test "with_response closes an unread body" (fun () ->
          let eof = Ivar.create () in
          let script flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1000000\r\n\r\nabc" ;
            ignore (Buf_reader.peek_eof r) ;
            fill eof
          in
          with_scripts [script]
          @@ fun client port _ ->
          C.with_response client (at_port port "/") ignore ;
          (* The client closes the connection of the unread body. *)
          wait eof )
    ; test "with_response gives the exception of f" (fun () ->
          let go = Ivar.create () and raised = Ivar.create () in
          let script flow r =
            ignore (read_head r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n" ;
            ignore (Buf_reader.peek_eof r)
          in
          with_scripts [script]
          @@ fun client port _ ->
          let failing =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                wait go ;
                fill raised ;
                failwith "p" )
          in
          equal string "Failure(\"f\")"
            (error_text (fun () ->
                 C.with_response client
                   (at_port port ~meth:Method.POST ~body:failing "/") (fun _ ->
                     fill go ; wait raised ; failwith "f" ) ) ) )
    ; test "close stops a request body at once" (fun () ->
          let script flow r =
            ignore (read_head r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" ;
            ignore (Buf_reader.peek_eof r)
          in
          with_scripts [script]
          @@ fun client port _ ->
          let seen = Ivar.create () in
          let waiting =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                try sync never
                with e ->
                  ignore (Ivar.try_fill seen (exn_name e)) ;
                  raise e )
          in
          equal int 200
            (status
               (C.request client
                  (at_port port ~meth:Method.POST ~body:waiting "/") ) ) ;
          C.close client ;
          equal string "Cancelled" (sync (Ivar.read seen)) )
    ; test "a bad path raises before a connect" (fun () ->
          Sim_support.run
          @@ fun _ ->
          C.run
          @@ fun client ->
          (* A connect to this port gives Error (Connect _). *)
          equal string "Invalid_argument"
            ( match C.request client (at_port refused_port "/a#b") with
            | _ ->
                "no error"
            | exception Invalid_argument _ ->
                "Invalid_argument"
            | exception e ->
                Printexc.to_string e ) )
    ; test "request after close raises Invalid_argument" (fun () ->
          Sim_support.run
          @@ fun _ ->
          C.run
          @@ fun client ->
          C.close client ;
          raises_match is_invalid_arg (fun () ->
              C.request client (at_port refused_port "/") ) )
    ; test "a proxy handler streams an upload to an upstream echo" (fun () ->
          Sim_support.run
          @@ fun _ ->
          Server_support.with_server ~config:big_config
            ~handler:Server_support.handler
          @@ fun upstream ->
          let up = Printf.sprintf "127.0.0.1:%d" (server_port upstream) in
          C.run
          @@ fun client ->
          let forward req =
            let r =
              Request.make ~authority:up ~headers:(Request.headers req)
                ~body:(Request.body req) (Request.meth req) (Request.path req)
            in
            let resp = C.request client r in
            Response.make ~status:(Response.status resp)
              ~headers:(Response.headers resp) (C.body resp)
          in
          Server_support.with_server ~config:big_config ~handler:forward
          @@ fun proxy ->
          let flow = Tcp.connect (Server.local_addr proxy) in
          Fun.protect ~finally:(fun () -> Flow.close flow)
          @@ fun () ->
          Flow.write_string flow
            "POST /echo HTTP/1.1\r\n\
             Host: x\r\n\
             Transfer-Encoding: chunked\r\n\
             \r\n" ;
          let r = Buf_reader.of_flow flow and n = 65536 in
          let send k =
            Flow.write_string flow
              (Printf.sprintf "%x\r\n%s\r\n" n
                 (String.make n (Char.chr (65 + k))) )
          in
          send 0 ;
          let h = H1_parse.response_head server_limits r in
          equal int 200 (Status.code h.status) ;
          let b = H1_body.chunked ~limits:server_limits r in
          let read_n () =
            let buf = Bytes.create n in
            let rec go got =
              if got < n then go (got + Body.read b buf got (n - got))
            in
            go 0 ; Bytes.to_string buf
          in
          (* Chunk k goes only after the echo of chunk k - 1 came. *)
          for k = 0 to 15 do
            if k > 0 then send k ;
            equal string (String.make n (Char.chr (65 + k))) (read_n ())
          done ;
          Flow.write_string flow "0\r\n\r\n" ;
          equal string "" (Body.to_string b) )
    ; test "a cancel of the caller during the head wait stops the writer"
        (fun () ->
          let eof = Ivar.create () in
          let script _ r =
            ignore (read_head r) ;
            ignore (Buf_reader.peek_eof r) ;
            fill eof
          in
          with_scripts [script]
          @@ fun client port _ ->
          let wrote = Ivar.create () and seen = Ivar.create () in
          let waiting =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                fill wrote ;
                try sync never
                with e ->
                  ignore (Ivar.try_fill seen (exn_name e)) ;
                  raise e )
          in
          Fiber.scope
          @@ fun s ->
          let caller =
            Sim_support.call ~name:"caller" s (fun () ->
                C.request client
                  (at_port port ~meth:Method.POST ~body:waiting "/") )
          in
          wait wrote ;
          Sim_support.cancel caller ;
          equal string "Cancelled" (sync (Ivar.read seen)) ;
          wait eof ;
          raises Promise.Cancelled (fun () -> ignore (await caller)) )
    ; test "a cancel of the caller during the body closes the connection"
        (fun () ->
          let eof = Ivar.create () in
          let script flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nab" ;
            ignore (Buf_reader.peek_eof r) ;
            fill eof
          in
          with_scripts [script]
          @@ fun client port _ ->
          let reading = Ivar.create () in
          Fiber.scope
          @@ fun s ->
          let caller =
            Sim_support.call ~name:"caller" s (fun () ->
                let b = C.body (C.request client (at_port port "/")) in
                ignore (Body.read b (Bytes.create 8) 0 8) ;
                fill reading ;
                Body.to_string b )
          in
          wait reading ;
          Sim_support.cancel caller ;
          wait eof ;
          raises Promise.Cancelled (fun () -> ignore (await caller)) )
    ; test "get parses URLs" (fun () ->
          let show url =
            match C.Private.parse_url url with
            | a, p ->
                Printf.sprintf "%s => %s %s" url a p
            | exception C.Error e ->
                Printf.sprintf "%s => %s" url (Printexc.to_string (C.Error e))
            | exception Invalid_argument _ ->
                url ^ " => Invalid_argument"
          in
          expect
            (String.concat "\n"
               (List.map show
                  [ "http://a.example"
                  ; "HTTP://a.example:8080/p?q#f"
                  ; "http://a.example?x=1"
                  ; "http://[::1]:80/"
                  ; "http://[::1]:8080/x"
                  ; "http://a:/"
                  ; "https://a"
                  ; "ftp://x"
                  ; "http://"
                  ; "http://u@a/"
                  ; "http://a:70000/"
                  ; "http://a:0/"
                  ; "http://a:8x/"
                  ; "a.example/p"
                  ; "http://a:1/"
                  ; "http://a:65535/"
                  ; "http://a:65536/"
                  ; "http://a:+80/"
                  ; "http://a:9/"
                  ; "http://[::1/" ] ) )
          @@ __POS_OF__
               {|
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
            http://a:9/ => a:9 /
            http://[::1/ => Invalid_argument
            |} )
    ; test "the client and the server under load" (fun () ->
          let client_config =
            {Client_config.default with max_idle_per_host= 20}
          in
          with_client ~client_config
          @@ fun client url ->
          let ids =
            Fiber.scope (fun s ->
                List.concat_map await
                  (List.init 20 (fun _ ->
                       Sim_support.call ~name:"loader" s (fun () ->
                           List.init 25 (fun _ -> get_text client (url ^ "/id")) ) )
                  ) )
          in
          equal int 500 (List.length ids) ;
          let distinct = List.sort_uniq compare ids in
          is_true
            ~msg:(Printf.sprintf "%d connections" (List.length distinct))
            (List.length distinct <= 20) )
    ; test
        "a user Connection: close is sent, and the next request uses a new \
         connection" (fun () ->
          with_client
          @@ fun client url ->
          let first =
            text
              (C.request client
                 (at_port (port_of url)
                    ~headers:[("Connection", "close")]
                    "/conn" ) )
          in
          let id1 = List.nth (String.split_on_char ' ' first) 1 in
          equal string "close" (List.hd (String.split_on_char ' ' first)) ;
          let id2 = get_text client (url ^ "/id") in
          is_true ~msg:(id1 ^ " then " ^ id2) (id1 <> id2) )
    ; test "race: the client and the server under load" (fun () ->
          Sim_support.race
          @@ fun _ ->
          client_in
          @@ fun client url ->
          let ids =
            Fiber.scope (fun s ->
                List.concat_map await
                  (List.init 4 (fun _ ->
                       Sim_support.call ~name:"loader" s (fun () ->
                           List.init 5 (fun _ -> get_text client (url ^ "/id")) ) )
                  ) )
          in
          equal int 20 (List.length ids) ;
          let distinct = List.sort_uniq compare ids in
          is_true
            ~msg:(Printf.sprintf "%d connections" (List.length distinct))
            (List.length distinct <= 4) )
    ; test "a client that is not closed ends with its scope" (fun () ->
          Sim_support.run
          @@ fun _ ->
          Server_support.with_server ~handler:api_handler
          @@ fun server ->
          let got =
            Fiber.scope (fun s ->
                let client = C.make s in
                get_text client (url_of server ^ "/hello") )
          in
          equal string "hello" got )
    ; test "the end of the scope closes the idle connections" (fun () ->
          Sim_support.run
          @@ fun sim ->
          Server_support.with_server ~handler:api_handler
          @@ fun server ->
          let fds = Weft_sim.open_fds sim in
          Fiber.scope (fun s ->
              let client = C.make s in
              equal string "hello" (get_text client (url_of server ^ "/hello")) ) ;
          check_settled sim ~msg:"only the listener is open" (fun () ->
              Weft_sim.open_fds sim = fds ) )
    ; test "run in a cancelled fiber raises Cancelled" (fun () ->
          Sim_support.run
          @@ fun _ ->
          let called = ref false in
          raises_match
            (function Fiber.Cancelled Exit -> true | _ -> false)
            (fun () ->
              Fiber.scope (fun s ->
                  Fiber.cancel s Exit ;
                  C.run (fun _ -> called := true) ) ) ;
          is_false ~msg:"f ran" !called )
    ; test
        "race: a request after a cancel of the caller of run raises Cancelled"
        (fun () ->
          (* The cancel of [s] reaches the scope of the writers of the client.
             The next request does no sync before its checkout and the start of
             its writer. On a different domain, the daemon of the writers can
             close the pool first. *)
          Sim_support.race
          @@ fun _ ->
          Server_support.with_server ~handler:api_handler
          @@ fun server ->
          let url = url_of server ^ "/hello" and second = ref "no request" in
          ( match
              Fiber.scope (fun s ->
                  C.run (fun client ->
                      equal string "hello" (get_text client url) ;
                      Fiber.cancel s Exit ;
                      second :=
                        match C.get client url with
                        | _ ->
                            "a response"
                        | exception e ->
                            exn_name e ) )
            with
          | () ->
              fail "run returned"
          | exception Fiber.Cancelled Exit ->
              () ) ;
          equal string "Cancelled" !second )
    ; test "with_response closes the body when f raises" (fun () ->
          let eof = Ivar.create () in
          let script flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1000000\r\n\r\nabc" ;
            ignore (Buf_reader.peek_eof r) ;
            fill eof
          in
          with_scripts [script]
          @@ fun client port _ ->
          raises (Failure "f") (fun () ->
              C.with_response client (at_port port "/") (fun _ -> failwith "f") ) ;
          (* The client closes the connection of the unread body. *)
          wait eof )
    ; test "a close during the head wait on a reused connection gives Closed"
        (fun () ->
          (* The connection closes, and the client is closed: no retry. *)
          let script flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\na" ;
            ignore (read_request r) ;
            ignore (Buf_reader.peek_eof r)
          in
          Sim_support.run
          @@ fun sim ->
          scripts_in [script]
          @@ fun client port _ ->
          equal string "a" (text (C.request client (at_port port "/"))) ;
          Fiber.scope
          @@ fun s ->
          let second =
            Sim_support.call ~name:"second" s (fun () ->
                error_text (fun () -> C.request client (at_port port "/")) )
          in
          is_false ~msg:"the head wait" (Sim_support.ended sim second) ;
          C.close client ;
          equal string
            "Caravan_client.Error: the connection closed before the response"
            (await second) )
    ; test "run gives the grace to the bodies also when f raises" (fun () ->
          is_false ~msg:"the body got no cancel"
            (grace_after_raise ~cancel:false) )
    ; test "a cancel in the grace after f raised gives the exception of f"
        (fun () ->
          is_true ~msg:"the body got the cancel"
            (grace_after_raise ~cancel:true) )
    ; test "the body of an upgrade response is empty" (fun () ->
          equal string ""
            (Body.to_string
               (C.body (Response.upgrade ~status:Status.ok (fun _ -> ()))) ) )
    ; test
        "a request that raises before the exchange gives its error, not the \
         error of the close" (fun () ->
          Sim_support.run
          @@ fun _ ->
          C.run
          @@ fun client ->
          let body =
            Body.of_source ~length:3
              ~close:(fun () -> failwith "close")
              (fun _ _ _ -> 0)
          in
          raises_match is_invalid_arg (fun () ->
              C.request client (Request.make ~body Method.POST "/") ) ) ]

(* The client and exact time. Each test runs in manual mode. *)

let ( +: ) = Sim_support.( +: )

(* [d] minus 1 ns. *)
let minus_ns d = Span.of_ns (Int64.pred (Span.to_ns d))

(* The spans of the client in these tests. Server_support has different spans,
   with other names. *)
let client_connect = Span.s 3

let client_response = Span.s 5

let client_read = Span.s 7

let client_write = Span.s 11

let client_idle = Span.s 13

let client_grace = Span.s 17

let timed =
  { Client_config.default with
    connect_timeout= client_connect
  ; response_timeout= client_response
  ; read_timeout= client_read
  ; write_timeout= client_write
  ; idle_timeout= client_idle
  ; close_grace= client_grace }

(* [test ~sim ~server ~t0 c] with an exchange in a call: [exchange run] runs
   [run] on the client flow of [with_breakable], with the spans of [timed]. The
   call gives the result of [exchange], or the text of its error. With
   [~broken:true], each write of the client raises EIO. [failed],
   [on_write_timeout] and [on_release] go to [with_breakable]. *)
let with_timed_exchange ?(broken = false) ?failed ?on_write_timeout ?on_release
    ~exchange test =
  Sim_support.manual
  @@ fun sim ->
  with_breakable ?on_write_timeout ?on_release ?failed net_eio sim
  @@ fun ~scope ~server ~run ~break ~failed:_ ->
  if broken then break () ;
  let t0 = Time.now Mono in
  let c =
    Sim_support.call ~name:"exchange" scope (fun () ->
        match exchange (fun r -> run timed r) with
        | v ->
            v
        | exception Caravan_client.Error e ->
            Printexc.to_string (Caravan_client.Error e) )
  in
  test ~sim ~server ~t0 c

let timed_tests =
  group "Client and exact time"
    [ test "a lookup that does not end gives a connect timeout at the deadline"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          Weft_sim.set_dns sim lookup_host No_answer ;
          let t0 = Time.now Mono in
          Fiber.scope
          @@ fun s ->
          let c =
            Sim_support.call ~name:"connect" s (fun () ->
                error_text (fun () ->
                    Connect.connect timed ~host:lookup_host ~port:80 ) )
          in
          equal string "Caravan_client.Error: connect timeout"
            (Sim_support.ends_at sim ~t0 client_connect c) )
    ; test "a close grace that overflows the clock does not cancel the bodies"
        (fun () ->
          (* The grace is the largest span. After 1 s, the end of the grace is
             out of the range of the Mono clock: the grace never ends. The body
             ends 1 h after the end of [f]. *)
          let config =
            {Client_config.default with close_grace= Span.of_ns Int64.max_int}
          in
          Sim_support.manual
          @@ fun sim ->
          Sim_support.advance sim (Span.s 1) ;
          let late flow r =
            ignore (read_head r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" ;
            ignore (Buf_reader.peek_eof r)
          in
          let returned = Ivar.create () and t_end = ref (Time.now Mono) in
          Fiber.scope
          @@ fun s ->
          let r =
            Sim_support.call ~name:"run" s (fun () ->
                scripts_in ~config [late]
                @@ fun client port _ ->
                let slow =
                  Body.of_writer (fun w ->
                      Body.Writer.write_string w "abc" ;
                      Body.Writer.flush w ;
                      Time.sleep Mono (Span.s 3600) )
                in
                ignore
                  (C.request client
                     (at_port port ~meth:Method.POST ~body:slow "/") ) ;
                t_end := Time.now Mono ;
                fill returned )
          in
          wait returned ;
          Sim_support.ends_at sim ~t0:!t_end (Span.s 3600) r )
    ; test "a response 1 ns before the response timeout is taken" (fun () ->
          with_timed_exchange ~exchange:(fun run ->
              Printf.sprintf "status %d" (status (run (req "/"))) )
          @@ fun ~sim ~server ~t0 c ->
          ignore (read_head (Buf_reader.of_flow server)) ;
          (* The deadline starts at the end of the request. *)
          Sim_support.advance_to sim (t0 +: minus_ns client_response) ;
          Flow.write_string server
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" ;
          equal string "status 200" (await c) )
    ; test "no response at the response timeout gives Timeout `Response"
        (fun () ->
          with_timed_exchange ~exchange:(fun run ->
              Printf.sprintf "status %d" (status (run (req "/"))) )
          @@ fun ~sim ~server ~t0 c ->
          ignore (read_head (Buf_reader.of_flow server)) ;
          equal string "Caravan_client.Error: response timeout"
            (Sim_support.ends_at sim ~t0 client_response c) )
    ; test "a body read that waits until the read timeout gives Timeout `Read"
        (fun () ->
          let releases = ref [] in
          let on_release _ ~reusable = releases := reusable :: !releases in
          with_timed_exchange ~on_release ~exchange:(fun run ->
              let b = body (run (req "/")) in
              let n = Body.read b (Bytes.create 8) 0 8 in
              Printf.sprintf "%d bytes, then %s" n
                (error_text (fun () -> Body.to_string b)) )
          @@ fun ~sim ~server ~t0 c ->
          ignore (read_head (Buf_reader.of_flow server)) ;
          (* The head and the first 2 bytes in one write: the first read gives
             the 2 bytes, and the next read waits from t0. A byte 1 ns before
             its read timeout ends that read, and the next read waits from that
             time. *)
          Flow.write_string server
            "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab" ;
          let t1 = t0 +: minus_ns client_read in
          Sim_support.advance_to sim t1 ;
          Flow.write_string server "c" ;
          equal string "2 bytes, then Caravan_client.Error: read timeout"
            (Sim_support.ends_at sim ~t0:t1 client_read c) ;
          (* The rest of the body did not come: the connection closes. *)
          equal (list bool) [false] !releases )
    ; test "a write that waits until the write timeout gives Timeout `Write"
        (fun () ->
          let raised = Ivar.create () in
          with_timed_exchange ~exchange:(fun run ->
              Printf.sprintf "status %d"
                (status
                   (run (req ~meth:Method.POST ~body:(endless raised) "/")) ) )
          @@ fun ~sim ~server:_ ~t0 c ->
          (* The server never reads: the first write that waits starts at t0. *)
          equal string "Caravan_client.Error: write timeout"
            (Sim_support.ends_at sim ~t0 client_write c) ;
          is_true ~msg:"the producer got the error" (is_set raised) )
    ; test "a second blocked write gets a new write timeout" (fun () ->
          (* Two writes of 100 KiB each. The window of the simulator is 64 KiB.
             The first write waits from t0. At t0 + 10 s the server reads 64
             KiB: the first write ends, and the second write waits from that
             time. *)
          let part = String.make (100 * 1024) 'x' in
          let two_parts =
            Body.of_writer
              ~length:(2 * String.length part)
              (fun w ->
                Body.Writer.write_string w part ;
                Body.Writer.flush w ;
                Body.Writer.write_string w part ;
                Body.Writer.flush w )
          in
          with_timed_exchange ~exchange:(fun run ->
              Printf.sprintf "status %d"
                (status (run (req ~meth:Method.POST ~body:two_parts "/"))) )
          @@ fun ~sim ~server ~t0 c ->
          let t1 = t0 +: Span.s 10 in
          Sim_support.advance_to sim t1 ;
          ignore (Server_support.read_exactly server (64 * 1024)) ;
          equal string "Caravan_client.Error: write timeout"
            (Sim_support.ends_at sim ~t0:t1 client_write c) )
    ; test "a wake by the end of the writer keeps the read deadline" (fun () ->
          (* The read of the body waits from t0. At t0 + 3 s the writer ends:
             the read wakes, and waits again with the same deadline. *)
          let gate = Ivar.create () in
          let gated =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                wait gate )
          in
          with_timed_exchange ~exchange:(fun run ->
              let b = body (run (req ~meth:Method.POST ~body:gated "/")) in
              let n = Body.read b (Bytes.create 8) 0 8 in
              Printf.sprintf "%d bytes, then %s" n
                (error_text (fun () -> Body.to_string b)) )
          @@ fun ~sim ~server ~t0 c ->
          ignore (read_head (Buf_reader.of_flow server)) ;
          Flow.write_string server
            "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nab" ;
          Sim_support.advance_to sim (t0 +: Span.s 3) ;
          fill gate ;
          equal string "2 bytes, then Caravan_client.Error: read timeout"
            (Sim_support.ends_at sim ~t0 client_read c) )
    ; test
        "a write error before the end of the request starts the response \
         deadline" (fun () ->
          (* A GET with no body on the wire, and a POST with a source. *)
          List.iter
            (fun (meth, length) ->
              let failed = Ivar.create () and closes = Atomic.make 0 in
              let source =
                Body.of_source ~length
                  ~close:(fun () -> Atomic.incr closes)
                  (fun _ _ _ -> 0)
              in
              with_timed_exchange ~broken:true ~failed ~exchange:(fun run ->
                  Printf.sprintf "status %d"
                    (status (run (req ~meth ~body:source "/"))) )
              @@ fun ~sim ~server:_ ~t0 c ->
              (* The write of the head fails. The writer ends with the error
                 before [sent]: the reader stamps the end of the request. *)
              equal string "Caravan_client.Error: response timeout"
                (Sim_support.ends_at sim ~t0 client_response c) ;
              is_true ~msg:"the write failed" (is_set failed) ;
              equal int 1 (Atomic.get closes) )
            [(Method.GET, 0); (Method.POST, 3)] )
    ; test "run cancels the bodies at the close grace" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let release = Ivar.create ()
          and cancelled = Ivar.create ()
          and late = Ivar.create ()
          and returned = Ivar.create () in
          (* After its cancel, the body waits for [late]. *)
          let on_cancel () = Fiber.shield (fun () -> wait late) in
          let t_end = ref (Time.now Mono) in
          Fiber.scope
          @@ fun s ->
          let r =
            Sim_support.call ~name:"run" s (fun () ->
                scripts_in ~config:timed [answer_at_once]
                @@ fun client port _ ->
                ignore
                  (C.request client
                     (at_port port ~meth:Method.POST
                        ~body:(held_body ~on_cancel ~release ~cancelled ())
                        "/" ) ) ;
                t_end := Time.now Mono ;
                fill returned )
          in
          wait returned ;
          Sim_support.holds_from sim ~t0:!t_end client_grace ~what:"the cancel"
            (fun () -> is_set cancelled ) ;
          (* Only [late] can end the body: run returns after it. *)
          is_false ~msg:"run waits for the cancelled body"
            (Sim_support.ended sim r) ;
          fill late ;
          await r )
    ; test "run waits for a request body that goes after its response"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let received = Ivar.create () in
          (* The server answers at once, then reads the whole body. *)
          let script flow r =
            let h = read_head r in
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" ;
            ignore
              (Ivar.try_fill received (error_text (fun () -> read_body r h)))
          in
          let got = ref ""
          and gate = Ivar.create ()
          and returned = Ivar.create () in
          (* The rest of the body goes after the response, when the test opens
             [gate]. *)
          let late =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "part1;" ;
                Body.Writer.flush w ;
                wait gate ;
                List.iter
                  (fun part ->
                    Body.Writer.write_string w part ;
                    Body.Writer.flush w )
                  ["part2;"; "part3"] ;
                got := "part1;part2;part3" )
          in
          let t0 = Time.now Mono in
          Fiber.scope
          @@ fun s ->
          let r =
            Sim_support.call ~name:"run" s (fun () ->
                scripts_in ~config:timed [script]
                @@ fun client port _ ->
                equal int 200
                  (status
                     (C.request client
                        (at_port port ~meth:Method.POST ~body:late "/") ) ) ;
                fill returned )
          in
          wait returned ;
          is_false ~msg:"run waits for the body" (Sim_support.ended sim r) ;
          fill gate ;
          await r ;
          (* [run] returned after the producer ended, not after a cancel. *)
          equal Server_support.span ~msg:"no time passed" Span.zero
            (Time.diff Mono (Time.now Mono) t0) ;
          equal string "part1;part2;part3" !got ;
          equal string "no error" (sync (Ivar.read received)) )
    ; test "the response deadline starts at the end of the upload" (fun () ->
          let gate = Ivar.create () in
          let gated =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                wait gate ;
                Body.Writer.write_string w "def" ;
                Body.Writer.flush w )
          in
          with_timed_exchange ~exchange:(fun run ->
              Printf.sprintf "status %d"
                (status (run (req ~meth:Method.POST ~body:gated "/"))) )
          @@ fun ~sim ~server ~t0 c ->
          Fun.protect ~finally:(fun () -> fill gate)
          @@ fun () ->
          (* The server reads the head first: the time of the head is not the
             start of the deadline. *)
          ignore (read_head (Buf_reader.of_flow server)) ;
          let t1 = t0 +: Span.s 5 in
          Sim_support.advance_to sim t1 ;
          fill gate ;
          Sim_support.advance_to sim (t1 +: minus_ns client_response) ;
          Flow.write_string server
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" ;
          equal string "status 200" (await c) )
    ; test "a response head that comes slowly gives Timeout `Response"
        (fun () ->
          with_timed_exchange ~exchange:(fun run ->
              Printf.sprintf "status %d" (status (run (req "/"))) )
          @@ fun ~sim ~server ~t0 c ->
          ignore (read_head (Buf_reader.of_flow server)) ;
          (* The reader takes each part, and waits again until the same
             deadline. *)
          List.iter
            (fun part ->
              Sim_support.advance sim (Span.s 1) ;
              Flow.write_string server part )
            ["H"; "TTP/1.1 2"] ;
          equal string "Caravan_client.Error: response timeout"
            (Sim_support.ends_at sim ~t0 client_response c) )
    ; test "an idle connection that the server closed is not used" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          let closed = Ivar.create () in
          let engine ctx flow handler =
            H1_server.serve ctx flow handler ;
            Flow.close flow ;
            fill closed
          in
          (* The idle timeout of the client is longer than the one of the
             server. *)
          with_pool ~config:Server_support.timed_config ~client_config:timed
            ~engine
          @@ fun pool writers port ->
          let a = get_id pool writers ~port () in
          (* After the response, the server waits for the next request until its
             idle timeout. *)
          Sim_support.advance sim Server_support.idle_i ;
          is_true ~msg:"the server closed" (is_set closed) ;
          (* A POST, which the client never retries. *)
          let b = get_id pool writers ~port ~meth:Method.POST () in
          is_true ~msg:(a ^ " then " ^ b) (a <> b) )
    ; test
        "the reaper closes an idle connection at the first tick after \
         idle_timeout" (fun () ->
          (* [run] makes the reaper at t0. It ticks each 6.5 s, half of
             [client_idle]. The release occurs at t0 + 1 s, thus the connection
             is young at the second tick (t0 + 13 s). *)
          Sim_support.manual
          @@ fun sim ->
          let eof = Ivar.create () in
          let script flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\na" ;
            ignore (Buf_reader.peek_eof r) ;
            fill eof
          in
          let t0 = Time.now Mono in
          scripts_in ~config:timed [script]
          @@ fun client port _ ->
          Sim_support.advance sim (Span.s 1) ;
          equal string "a" (text (C.request client (at_port port "/"))) ;
          (* No request uses the host again: only the reaper closes it. *)
          Sim_support.holds_from sim ~t0 (Span.ms 19500)
            ~what:"the end of the stream" (fun () -> is_set eof ) )
    ; test "a request during the grace of run raises before a connect"
        (fun () ->
          let script flow r =
            ignore (read_head r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" ;
            ignore (Buf_reader.peek_eof r)
          in
          Sim_support.manual
          @@ fun sim ->
          Fiber.scope
          @@ fun outer ->
          let returned = Ivar.create () in
          let late =
            scripts_in ~config:timed [script; script]
            @@ fun client port accepted ->
            let go = Ivar.create () in
            let waiting =
              Body.of_writer (fun w ->
                  Body.Writer.write_string w "abc" ;
                  Body.Writer.flush w ;
                  wait go )
            in
            equal int 200
              (status
                 (C.request client
                    (at_port port ~meth:Method.POST ~body:waiting "/") ) ) ;
            (* A fiber of the outer scope makes a request after f returns,
               during the grace, then lets the first body end. On 1 domain, run
               starts its grace before that fiber runs. *)
            let late =
              Sim_support.call ~name:"late" outer (fun () ->
                  wait returned ;
                  Fun.protect ~finally:(fun () -> fill go)
                  @@ fun () ->
                  match C.request client (at_port port "/") with
                  | _ ->
                      "no error"
                  | exception Invalid_argument _ ->
                      (* A connect, if any, has time to arrive. *)
                      Weft_sim.settle sim ;
                      Printf.sprintf "Invalid_argument, %d accepted"
                        (Atomic.get accepted)
                  | exception e ->
                      Printexc.to_string e )
            in
            fill returned ; late
          in
          equal string "Invalid_argument, 1 accepted" (await late) )
    ; test "a stale connection is not retried during the grace of run"
        (fun () ->
          let second_head = Ivar.create () and stopped = Ivar.create () in
          (* Connection 1 keeps the grace open: its request body goes after the
             response. *)
          let first flow r =
            ignore (read_head r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" ;
            ignore (Buf_reader.peek_eof r)
          (* Connection 2 answers one GET, then closes during the grace without
             a response to the second GET. *)
          and second flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\nb" ;
            ignore (read_head r) ;
            fill second_head ;
            wait stopped
          (* Only a retry connects a third time. *)
          and third flow r =
            ignore (read_request r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\nc"
          in
          Sim_support.manual
          @@ fun sim ->
          Fiber.scope
          @@ fun outer ->
          let late =
            scripts_in ~config:timed [first; second; third]
            @@ fun client port accepted ->
            let go = Ivar.create () in
            let waiting =
              Body.of_writer (fun w ->
                  Body.Writer.write_string w "abc" ;
                  Body.Writer.flush w ;
                  wait go )
            in
            equal int 200
              (status
                 (C.request client
                    (at_port port ~meth:Method.POST ~body:waiting "/") ) ) ;
            (* Connection 1 is in use: this GET makes connection 2. *)
            equal string "b" (text (C.request client (at_port port "/"))) ;
            let late =
              Sim_support.call ~name:"late" outer (fun () ->
                  Fun.protect ~finally:(fun () -> fill go)
                  @@ fun () ->
                  let result =
                    error_text (fun () ->
                        text (C.request client (at_port port "/")) )
                  in
                  (* A connect, if any, has time to arrive. *)
                  Weft_sim.settle sim ;
                  Printf.sprintf "%s, %d accepted" result (Atomic.get accepted) )
            in
            (* The GET goes on connection 2 before f returns. After f, run
               starts its grace before the server closes connection 2. *)
            wait second_head ;
            Fiber.fork outer (fun () -> fill stopped) ;
            late
          in
          equal string
            "Caravan_client.Error: the connection closed before the response, \
             2 accepted"
            (await late) )
    ; test "a cancel during the grace of run raises Cancelled" (fun () ->
          let script flow r =
            ignore (read_head r) ;
            Flow.write_string flow
              "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" ;
            ignore (Buf_reader.peek_eof r)
          in
          Sim_support.manual
          @@ fun sim ->
          let cancelled = Ivar.create () and returned = Ivar.create () in
          let release = Ivar.create () in
          let t0 = Time.now Mono in
          Fiber.scope
          @@ fun s ->
          let caller =
            Sim_support.call ~name:"run" s (fun () ->
                scripts_in ~config:timed [script]
                @@ fun client port _ ->
                ignore
                  (C.request client
                     (at_port port ~meth:Method.POST
                        ~body:(held_body ~release ~cancelled ())
                        "/" ) ) ;
                fill returned )
          in
          wait returned ;
          (* run waits in its grace. *)
          is_false ~msg:"run is in its grace" (Sim_support.ended sim caller) ;
          Sim_support.cancel caller ;
          raises Promise.Cancelled (fun () -> await caller) ;
          is_true ~msg:"the body got the cancel" (is_set cancelled) ;
          equal Server_support.span ~msg:"at once" Span.zero
            (Time.diff Mono (Time.now Mono) t0) ) ]

let () =
  exit
    (Windtrap.run "client"
       [ exchange_tests
       ; resolver_tests
       ; pool_tests
       ; pool_stateful
       ; api_tests
       ; timed_tests ] )
