(* The tests of the middleware example. Each test runs the handler through the
   real server and client of caravan, in the simulator of weft. *)

open Windtrap
open Caravan
module T = Caravan_testing
module App = Example_middleware.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

(* [run f] runs [f] with a handler that collects the log lines. It gives the
   result of [f] and the log lines, in their order. *)
let run f =
  let logs = ref [] in
  let log line = logs := line :: !logs in
  let v = T.run (App.make ~log) f in
  (v, List.rev !logs)

(* [get ?auth path] is the reply to a GET of [path], with the field
   Authorization: [auth] if [auth] is given. *)
let get ?auth path =
  let headers =
    match auth with
    | Some a ->
        Headers.of_list [("authorization", a)]
    | None ->
        Headers.empty
  in
  fst (run (fun t -> T.get ~headers t path))

let x_route (r : T.reply) = Headers.find "x-route" r.headers

let () =
  exit
  @@ Windtrap.run "middleware"
       [ test "a known token gives 200 and the name of its user" (fun () ->
             let r = get ~auth:"Bearer 7f3kQ9xVb2Lm" "/hello" in
             equal status Status.ok r.status ;
             equal string "Hello, ann!\n" r.body ;
             equal (option string) (Some "/hello") (x_route r) )
       ; test "the scheme is case-insensitive: bearer is also correct"
           (fun () ->
             let r = get ~auth:"bearer 7f3kQ9xVb2Lm" "/hello" in
             equal status Status.ok r.status ;
             equal string "Hello, ann!\n" r.body )
       ; test "the other token gives the other user" (fun () ->
             let r = get ~auth:"Bearer Wq8cT1zRn5Hd" "/hello" in
             equal string "Hello, bob!\n" r.body )
       ; test "no token gives 401, and the handler sets X-Route" (fun () ->
             let r = get "/hello" in
             equal status Status.unauthorized r.status ;
             equal (option string) (Some "Bearer")
               (Headers.find "www-authenticate" r.headers) ;
             equal (option string) (Some "/hello") (x_route r) )
       ; test "the name of a user is not a token: 401" (fun () ->
             let r = get ~auth:"Bearer ann" "/hello" in
             equal status Status.unauthorized r.status )
       ; test "a token without the Bearer scheme gives 401" (fun () ->
             let r = get ~auth:"7f3kQ9xVb2Lm" "/hello" in
             equal status Status.unauthorized r.status )
       ; test "a response without the route attribute has no X-Route" (fun () ->
             let r = get ~auth:"Bearer 7f3kQ9xVb2Lm" "/nope" in
             equal status Status.not_found r.status ;
             equal (option string) None (x_route r) )
       ; test "the log has a line for each request" (fun () ->
             let (), logs =
               run (fun t ->
                   let ann =
                     Headers.of_list [("authorization", "Bearer 7f3kQ9xVb2Lm")]
                   in
                   ignore (T.get ~headers:ann t "/hello") ;
                   ignore (T.get t "/hello") ;
                   ignore (T.get t "/nope?token=secret") )
             in
             (* trace is outside auth, thus it sees no user. *)
             equal (list string)
               [ "GET /hello -> 200 (user -, route /hello)"
               ; "GET /hello -> 401 (user -, route /hello)"
               ; "GET /nope -> 404 (user -, route -)" ]
               logs ) ]
