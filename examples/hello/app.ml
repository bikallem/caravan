(* A small caravan server: plain and streamed responses, the configuration of
   the server, its stop, and a service that runs with it.

   Run dune exec examples/hello/main.exe, then send these requests:

   curl -i 127.0.0.1:8080/hello

   curl -i --data-binary 'some text' 127.0.0.1:8080/echo

   curl -N 127.0.0.1:8080/stream

   curl -i 127.0.0.1:8080/slow

   The body of the next request is larger than max_body, thus it gets 413:

   head -c 2000000 /dev/zero | curl -i --data-binary @- 127.0.0.1:8080/echo

   Press Ctrl-C while curl -N 127.0.0.1:8080/stream runs: the stream ends at
   once with a last line, and the server stops. *)

open Weft
open Caravan

(* The server configuration: start from the default, and change what you need. A
   request body larger than 1 MiB gets 413. A request in progress gets 5 s to
   finish when the server stops. *)
let config =
  {Config.default with max_body= Some (1024 * 1024); shutdown_grace= Span.s 5}

let requests_text n =
  if n = 1 then "1 request" else Printf.sprintf "%d requests" n

(* The start of the reporter service. It must not block: it forks a fiber that
   logs the count each minute. A daemon fiber does not keep its scope open: the
   end of the scope of the server cancels it. *)
let reporter_start ~log requests sw =
  Fiber.fork_daemon sw (fun () ->
      while true do
        Time.sleep Mono (Span.s 60) ;
        log (requests_text (Atomic.get requests))
      done )

(* The stop of the reporter service: it logs the total. *)
let reporter_stop ~log requests ~grace:_ =
  log (requests_text (Atomic.get requests) ^ " in total")

(* The event of the stop of the server. A request from the server always has a
   connection. *)
let stopping req =
  match Request.conn req with Some c -> Conn_info.stopping c | None -> never

(* Five lines, 200 ms apart. A flush sends each line at once. When the server
   starts to stop, the stream ends at once with a last line, so that the client
   gets a complete body before the end of the grace. *)
let stream req =
  let stop = stopping req in
  Response.stream (fun w ->
      let rec line i =
        Body.Writer.write_string w (Printf.sprintf "event %d\n" i) ;
        Body.Writer.flush w ;
        if i < 5 then
          match
            sync
              (choose
                 [ wrap (Time.after Mono (Span.ms 200)) (fun () -> `Next)
                 ; wrap stop (fun () -> `Stop) ] )
          with
          | `Next ->
              line (i + 1)
          | `Stop ->
              Body.Writer.write_string w "the server stops\n"
      in
      line 1 )

(* Streams the request body back as it arrives. With the length of the request
   body, the response has a Content-Length.

   A response with bytes of the client must always have its own Content-Type and
   X-Content-Type-Options: nosniff. Never copy the Content-Type of the request:
   if not, a browser can run the bytes as HTML or script. *)
let echo req =
  let body = Request.body req in
  let headers =
    Headers.of_list
      [ ("content-type", "application/octet-stream")
      ; ("x-content-type-options", "nosniff") ]
  in
  Response.stream ~headers ?length:(Body.length body) (fun w ->
      Body.write_to body w )

let routes req =
  match Request.path_only req with
  | "/hello" ->
      Response.string "Hello from caravan!\n"
  | "/echo" ->
      echo req
  | "/stream" ->
      stream req
  | "/slow" ->
      Time.sleep Mono (Span.s 5) ;
      Response.string "Sorry for the wait.\n"
  | _ ->
      Handler.not_found req

(* [make ~log] gives a new count of the requests that reach the handler, the
   handler, and the reporter service of this count. [log] gets the lines of the
   reporter. A service starts before the server and stops after it (see
   Caravan.Server.run ~services). *)
type t = {requests: int Atomic.t; handler: Handler.t; reporter: Server.service}

let make ~log =
  let requests = Atomic.make 0 in
  let count : Handler.middleware =
   fun next req -> Atomic.incr requests ; next req
  in
  { requests
  ; handler= Handler.compose [count] routes
  ; reporter=
      Server.service
        ~start:(reporter_start ~log requests)
        ~stop:(reporter_stop ~log requests) }
