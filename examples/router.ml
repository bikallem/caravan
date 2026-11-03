(* A server with routes.

   Run dune exec examples/router.exe, then send these requests:

   curl -i localhost:8080/users/42

   curl -i -X DELETE localhost:8080/users/42

   curl -i localhost:8080/static/css/site.css

   The second request gets 405 with an Allow field. *)

open Caravan
module R = Caravan_router

(* Prints the template of the route that the response comes from. *)
let log : Handler.middleware =
 fun next req ->
  let resp = next req in
  Format.printf "%a %s -> %a@." Method.pp (Request.meth req)
    (Option.value (Response.find_attr R.template resp) ~default:"-")
    Status.pp (Response.status resp) ;
  resp

let app =
  R.handler
    [ R.get "/" (fun _ -> Response.string "home\n")
    ; R.group "/users"
        [ R.get "" (fun _ -> Response.string "all users\n")
        ; R.get "/:id" (fun req ->
              Response.string ("user " ^ R.param "id" req ^ "\n") ) ]
    ; R.get "/static/**" (fun req ->
          (* The rest is not a safe file path: a segment can contain "/" or
             "..". This handler only shows the segments. *)
          Response.string
            ( "segments "
            ^ String.concat ", " (List.map (Printf.sprintf "%S") (R.rest req))
            ^ "\n" ) ) ]

let main () = Server.run (module Weft_os) (Handler.compose [log] app)

let () = if !Sys.interactive then () else main ()
