(* A small caravan server.

   dune exec examples/hello.exe, then try: curl -i localhost:8080/hello curl -i
   --data-binary 'some text' localhost:8080/echo curl -N localhost:8080/stream
   curl -i localhost:8080/slow *)

open Weft
open Caravan

let handler req =
  match Request.path_only req with
  | "/hello" ->
      Response.string "Hello from caravan!\n"
  | "/echo" ->
      (* Streams the request body back as it arrives. *)
      let body = Request.body req in
      Response.stream ?length:(Body.length body) (fun w -> Body.write_to body w)
  | "/stream" ->
      Response.stream (fun w ->
          for i = 1 to 5 do
            Body.Writer.write_string w (Printf.sprintf "event %d\n" i) ;
            Body.Writer.flush w ;
            Time.sleep Mono (Span.ms 200)
          done )
  | "/slow" ->
      Time.sleep Mono (Span.s 5) ;
      Response.string "Sorry for the wait.\n"
  | _ ->
      Handler.not_found req

let main () =
  Caravan_run.run handler ~on_start:(fun server ->
      Format.printf "Listening on http://%a (Ctrl-C to stop)@."
        Conn_info.pp_addr (Server.local_addr server) )

let () = if !Sys.interactive then () else main ()
