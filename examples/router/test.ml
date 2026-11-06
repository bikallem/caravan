(* The tests of the router example. Each test runs the handler through the real
   server and client of caravan, in the simulator of weft. *)

open Windtrap
open Caravan
module T = Caravan_testing
module App = Example_router.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

(* [run f] runs [f] with a handler that collects the log lines. It gives the
   result of [f] and the log lines, in their order. *)
let run f =
  let logs = ref [] in
  let log line = logs := line :: !logs in
  let v = T.run (App.make ~log) f in
  (v, List.rev !logs)

(* [fetch meth path] is the reply to a request with [meth] for [path]. *)
let fetch meth path = fst (run (fun t -> T.fetch t (Request.make meth path)))

let header name (r : T.reply) = Headers.find name r.headers

(* [expect ~code body r] checks that [r] has the status [code] (200 by default)
   and the body [body]. *)
let expect ?(code = Status.ok) body (r : T.reply) =
  equal ~msg:"status" status code r.status ;
  equal ~msg:"body" string body r.body

let () =
  exit
  @@ Windtrap.run "router"
       [ test "GET /users/42 gives the user" (fun () ->
             expect "user 42\n" (fetch GET "/users/42") )
       ; test "GET /users has no id: all users" (fun () ->
             expect "all users\n" (fetch GET "/users") )
       ; test "POST /users gives 201" (fun () ->
             expect ~code:Status.created "created a user\n"
               (fetch POST "/users") )
       ; test "PUT, PATCH and DELETE go to their routes" (fun () ->
             expect "replaced user 42\n" (fetch PUT "/users/42") ;
             expect "changed user 42\n" (fetch PATCH "/users/42") ;
             expect "deleted user 42\n" (fetch DELETE "/users/42") )
       ; test "param raises Invalid_argument without the parameter" (fun () ->
             raises_match
               (function Invalid_argument _ -> true | _ -> false)
               (fun () -> App.act "deleted" (Request.make DELETE "/users")) )
       ; test "params gives the parameters in the order of the pattern"
           (fun () -> expect "id=42 post=7\n" (fetch GET "/users/42/posts/7") )
       ; test "rest gives the segments of **" (fun () ->
             expect "segments \"css\", \"site.css\"\n"
               (fetch GET "/static/css/site.css") )
       ; test "a segment of the rest can contain / and .., from %2F" (fun () ->
             expect "segments \"a/../secret\"\n"
               (fetch GET "/static/a%2F..%2Fsecret") )
       ; test "any gets all methods" (fun () ->
             expect "method BREW\n" (fetch (Other "BREW") "/method") ;
             expect "method GET\n" (fetch GET "/method") )
       ; test "PURGE /cache goes to the route of the extension method"
           (fun () -> expect "cache purged\n" (fetch (Other "PURGE") "/cache") )
       ; test "DELETE / gives 405 and Allow" (fun () ->
             let r = fetch DELETE "/" in
             equal status Status.method_not_allowed r.status ;
             equal (option string) (Some "GET, HEAD, OPTIONS")
               (header "allow" r) )
       ; test "OPTIONS / gives 204 and Allow" (fun () ->
             let r = fetch OPTIONS "/" in
             equal status Status.no_content r.status ;
             equal (option string) (Some "GET, HEAD, OPTIONS")
               (header "allow" r) )
       ; test "HEAD goes to the GET route, with no body" (fun () ->
             let r, logs =
               run (fun t -> T.fetch t (Request.make HEAD "/users/42"))
             in
             expect "" r ;
             equal (list string)
               ["HEAD /users/42 -> 200 (route /users/:id)"]
               logs )
       ; test "an unknown path gets the not_found of the example" (fun () ->
             expect ~code:Status.not_found "No route for /nope\n"
               (fetch GET "/nope") )
       ; test "a segment . or .. gives 400" (fun () ->
             equal status Status.bad_request
               (fetch GET "/static/../users").status ;
             equal status Status.bad_request (fetch GET "/static/./x").status )
       ; test "the group middleware adds Cache-Control: no-store" (fun () ->
             equal (option string) (Some "no-store")
               (header "cache-control" (fetch GET "/users/42")) ;
             equal (option string) (Some "no-store")
               (header "cache-control" (fetch GET "/users")) )
       ; test "the group middleware runs only in the group" (fun () ->
             equal (option string) None (header "cache-control" (fetch GET "/")) ;
             equal (option string) None
               (header "cache-control" (fetch GET "/static/a")) )
       ; test "the group middleware does not run for a 405" (fun () ->
             let r = fetch DELETE "/users" in
             equal status Status.method_not_allowed r.status ;
             equal (option string) None (header "cache-control" r) )
       ; test "the log sees the template of the route, and no query" (fun () ->
             let (), logs =
               run (fun t ->
                   ignore (T.get t "/users/42?token=secret") ;
                   ignore (T.get t "/static/css/site.css") ;
                   ignore (T.get t "/nope") )
             in
             equal (list string)
               [ "GET /users/42 -> 200 (route /users/:id)"
               ; "GET /static/css/site.css -> 200 (route /static/**)"
               ; "GET /nope -> 404 (route -)" ]
               logs ) ]
