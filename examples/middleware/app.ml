(* Middleware that give typed attributes to a handler, and get them back.

   Run dune exec examples/middleware/main.exe, then send these requests. A known
   token gets 200 and the name of its user:

   curl -i -H 'Authorization: Bearer 7f3kQ9xVb2Lm' 127.0.0.1:8080/hello

   A request without a token, or with an unknown token, gets 401:

   curl -i 127.0.0.1:8080/hello

   curl -i -H 'Authorization: Bearer ann' 127.0.0.1:8080/hello

   The responses from /hello have the field X-Route: /hello. The server logs a
   line for each request. *)

open Caravan

(* The tokens and their users. A token is a random secret: it does not contain
   the name of its user. The table lookup and the parse of the Authorization
   field are only for the example. A real application uses
   Caravan_auth.Api_key.scheme (a lookup by the SHA-256 hash of the token, not a
   compare of plaintext tokens), or Caravan_auth.Jwt. *)
let tokens = [("7f3kQ9xVb2Lm", "ann"); ("Wq8cT1zRn5Hd", "bob")]

(* Keys of attributes. Make them at the top level, with a type annotation. *)
let user : string Attrs.key = Attrs.key ~name:"user" ()

let route : string Attrs.key = Attrs.key ~name:"route" ()

(* [bearer v] is the token of the field value "Bearer <token>". The scheme is
   case-insensitive (RFC 9110 section 11.1), thus "bearer" is also correct. *)
let bearer v =
  match String.index_opt v ' ' with
  | Some i ->
      let scheme = String.sub v 0 i
      and token = String.sub v (i + 1) (String.length v - i - 1) in
      if String.lowercase_ascii scheme = "bearer" && token <> "" then Some token
      else None
  | None ->
      None

(* Gives the user of a known bearer token to the handlers after it. A request
   without a known token goes on without a user. *)
let auth : Handler.middleware =
 fun next req ->
  let token =
    Option.bind (Headers.find "authorization" (Request.headers req)) bearer
  in
  match Option.bind token (fun t -> List.assoc_opt t tokens) with
  | Some name ->
      next (Request.with_attr user name req)
  | None ->
      next req

(* The outer middleware. It gets the response after the handler, reads the route
   attribute, puts it in the field X-Route, and logs a line. A response without
   the attribute gets no X-Route.

   The line has the user attribute of the request of trace. It is always "-": an
   outer middleware does not see the attributes that an inner middleware adds.
   The line has the path with no query, because a query can contain a secret. *)
let trace ~log : Handler.middleware =
 fun next req ->
  let resp = next req in
  let route = Response.find_attr route resp in
  log
    (Printf.sprintf "%s %s -> %d (user %s, route %s)"
       (Method.to_string (Request.meth req))
       (Request.path_only req)
       (Status.code (Response.status resp))
       (Option.value (Request.find_attr user req) ~default:"-")
       (Option.value route ~default:"-") ) ;
  match route with
  | Some r ->
      Response.with_headers
        (Headers.set "x-route" r (Response.headers resp))
        resp
  | None ->
      resp

(* The handler reads the user that auth gave. It puts its route on the response,
   for trace. *)
let hello req =
  let resp =
    match Request.find_attr user req with
    | Some name ->
        Response.string ("Hello, " ^ name ^ "!\n")
    | None ->
        Response.string ~status:Status.unauthorized
          ~headers:(Headers.of_list [("www-authenticate", "Bearer")])
          "Send a known token.\n"
  in
  Response.with_attr route "/hello" resp

let routes req =
  match Request.path_only req with
  | "/hello" ->
      hello req
  | _ ->
      Handler.not_found req

(* The first middleware of the list is the outermost: trace runs before auth,
   and gets the response after it. *)
let make ~log = Handler.compose [trace ~log; auth] routes
