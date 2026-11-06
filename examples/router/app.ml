(* A server with routes: methods, parameters, a group, and the answers of the
   router to requests that match no route.

   Run dune exec examples/router/main.exe, then send these requests:

   curl -i 127.0.0.1:8080/users/42

   curl -i 127.0.0.1:8080/users

   curl -i -X POST 127.0.0.1:8080/users

   curl -i -X PUT 127.0.0.1:8080/users/42

   curl -i -X PATCH 127.0.0.1:8080/users/42

   curl -i -X DELETE 127.0.0.1:8080/users/42

   curl -i 127.0.0.1:8080/users/42/posts/7

   curl -i 127.0.0.1:8080/static/css/site.css

   curl -i -X BREW 127.0.0.1:8080/method

   curl -i -X PURGE 127.0.0.1:8080/cache

   The route / has only GET. Thus DELETE gets 405, and OPTIONS gets 204. The two
   responses have the field Allow: GET, HEAD, OPTIONS.

   curl -i -X DELETE 127.0.0.1:8080/

   curl -i -X OPTIONS 127.0.0.1:8080/

   A HEAD request goes to the GET route, and the response has no body:

   curl -I 127.0.0.1:8080/users/42

   A path that matches no route gets the not_found handler of this example:

   curl -i 127.0.0.1:8080/nope

   A path with a segment "." or ".." gets 400. Curl removes these segments, thus
   it needs --path-as-is to send them:

   curl -i --path-as-is 127.0.0.1:8080/static/../users *)

open Caravan
module R = Caravan_router

(* Logs the template of the route that gave the response. This middleware wraps
   the router, thus it reads the template on the response. The line has the path
   with no query, because a query can contain a secret. *)
let log_routes ~log : Handler.middleware =
 fun next req ->
  let resp = next req in
  log
    (Printf.sprintf "%s %s -> %d (route %s)"
       (Method.to_string (Request.meth req))
       (Request.path_only req)
       (Status.code (Response.status resp))
       (Option.value (Response.find_attr R.template resp) ~default:"-") ) ;
  resp

(* The middleware of the group /users. It adds Cache-Control: no-store, thus a
   cache does not keep the user data. It runs only for the routes of the group,
   and not for a 405 of the router. *)
let no_store : Handler.middleware =
 fun next req ->
  let resp = next req in
  Response.with_headers
    (Headers.set "cache-control" "no-store" (Response.headers resp))
    resp

(* One handler for two routes. find_param tells if the route has an id. *)
let users req =
  match R.find_param "id" req with
  | Some id ->
      Response.string ("user " ^ id ^ "\n")
  | None ->
      Response.string "all users\n"

(* [act verb] is a handler that tells what it did to the user. Use param only in
   the routes that have the parameter: else it raises Invalid_argument. *)
let act verb req = Response.string (verb ^ " user " ^ R.param "id" req ^ "\n")

(* params gives all parameters of the route, in the order of the pattern. *)
let user_post req =
  Response.string
    ( String.concat " " (List.map (fun (k, v) -> k ^ "=" ^ v) (R.params req))
    ^ "\n" )

(* rest gives the segments that ** matched. A segment can contain "/" or "..",
   from %2F. Thus the rest is not a safe file path: this handler only shows the
   segments. *)
let static req =
  Response.string
    ( "segments "
    ^ String.concat ", " (List.map (Printf.sprintf "%S") (R.rest req))
    ^ "\n" )

let not_found req =
  Response.string ~status:Status.not_found
    ("No route for " ^ Request.path_only req ^ "\n")

let make ~log =
  Handler.compose
    [log_routes ~log]
    (R.handler ~not_found
       [ R.get "/" (fun _ -> Response.string "home\n")
       ; R.group ~middleware:[no_store] "/users"
           [ R.get "" users
           ; R.post "" (fun _ ->
                 Response.string ~status:Status.created "created a user\n" )
           ; R.get "/:id" users
           ; R.put "/:id" (act "replaced")
           ; R.patch "/:id" (act "changed")
           ; R.delete "/:id" (act "deleted")
           ; R.get "/:id/posts/:post" user_post ]
       ; R.get "/static/**" static
       ; (* any gets all methods of /method. *)
         R.any "/method" (fun req ->
             Response.string
               ("method " ^ Method.to_string (Request.meth req) ^ "\n") )
       ; (* An extension method. *)
         R.route (Method.Other "PURGE") "/cache" (fun _ ->
             Response.string "cache purged\n" ) ] )
