(* Middleware that give typed attributes to a handler, and get them back.

   Run dune exec examples/middleware.exe. A request with a token gets 200:

   curl -i -H 'Authorization: Bearer ann' localhost:8080/hello

   A request without a token gets 401:

   curl -i localhost:8080/hello *)

open Caravan

let user : string Attrs.key = Attrs.key ~name:"user" ()

let route : string Attrs.key = Attrs.key ~name:"route" ()

(* Gives the user of the bearer token to the handlers after it. A real check
   examines the token. *)
let auth : Handler.middleware =
 fun next req ->
  match Headers.find "authorization" (Request.headers req) with
  | Some v when String.starts_with ~prefix:"Bearer " v ->
      let name = String.sub v 7 (String.length v - 7) in
      next (Request.with_attr user name req)
  | _ ->
      next req

(* Prints the route that the handler gives back in its response. *)
let log : Handler.middleware =
 fun next req ->
  let resp = next req in
  Format.printf "%a %s -> %a@." Method.pp (Request.meth req)
    (Option.value (Response.find_attr route resp) ~default:"-")
    Status.pp (Response.status resp) ;
  resp

let hello req =
  let resp =
    match Request.find_attr user req with
    | Some name ->
        Response.string ("Hello, " ^ name ^ "!\n")
    | None ->
        Response.string ~status:Status.unauthorized "Who are you?\n"
  in
  Response.with_attr route "/hello" resp

let main () = Server.run (module Weft_os) (Handler.compose [log; auth] hello)

let () = if !Sys.interactive then () else main ()
