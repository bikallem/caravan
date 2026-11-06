(* The tests of the limits example. Each test runs the handler through the real
   server and client of caravan, in the simulator of weft. Two tests call a
   handler directly: the test of the buckets and the test of Set-Cookie. Each
   run makes a new application, thus each run has full buckets of the rate
   limit. *)

open Windtrap
open Weft
open Caravan
module T = Caravan_testing
module App = Example_limits.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

(* The fields of the curl lines: $O, $M and $H of the header comment, and the
   others. *)
let o = ("Origin", "https://app.example")

let m = ("Access-Control-Request-Method", "PUT")

let h = ("Access-Control-Request-Headers", "content-type")

let other = ("Origin", "https://other.example")

let cross_site = ("Sec-Fetch-Site", "cross-site")

let call ?(fields = []) t meth path =
  T.fetch t (Request.make ~headers:(Headers.of_list fields) meth path)

(* [once ~fields meth path] is the reply to one request in its own run. *)
let once ?fields meth path =
  T.run (App.make ()) (fun t -> call ?fields t meth path)

let header name (r : T.reply) = Headers.find name r.headers

(* The CORS fields of [r], with their names in lower case, sorted. *)
let cors_fields (r : T.reply) =
  Headers.to_list r.headers
  |> List.map (fun (k, v) -> (String.lowercase_ascii k, v))
  |> List.filter (fun (k, _) -> String.starts_with ~prefix:"access-control-" k)
  |> List.sort compare

let fields = Testable.(list (pair string string))

let reply ?(code = Status.ok) body (r : T.reply) =
  equal ~msg:"status" status code r.status ;
  equal ~msg:"body" string body r.body

let () =
  exit
  @@ Windtrap.run "limits"
       [ test "a preflight from the permitted origin: 204 and the CORS fields"
           (fun () ->
             let r = once ~fields:[o; m; h] OPTIONS "/notes" in
             (* The routes have no OPTIONS: the handler did not run. *)
             reply ~code:Status.no_content "" r ;
             equal fields
               [ ("access-control-allow-credentials", "true")
               ; ("access-control-allow-headers", "content-type")
               ; ("access-control-allow-methods", "PUT")
               ; ("access-control-allow-origin", "https://app.example")
               ; ("access-control-max-age", "600") ]
               (cors_fields r) )
       ; test "a preflight from another origin: 204 with no CORS field"
           (fun () ->
             let r = once ~fields:[other; m] OPTIONS "/notes" in
             reply ~code:Status.no_content "" r ;
             equal fields [] (cors_fields r) )
       ; test "a request from the permitted origin gets the CORS fields"
           (fun () ->
             let r = once ~fields:[o] GET "/notes" in
             reply "notes\n" r ;
             equal fields
               [ ("access-control-allow-credentials", "true")
               ; ("access-control-allow-origin", "https://app.example")
               ; ("access-control-expose-headers", "Retry-After") ]
               (cors_fields r) ;
             equal (option string) (Some "Origin") (header "vary" r) )
       ; test "another origin gets no CORS field, but the handler runs"
           (fun () ->
             let r = once ~fields:[other] GET "/notes" in
             reply "notes\n" r ;
             equal fields [] (cors_fields r) )
       ; test "the sixth request in one second: 429 with Retry-After: 1"
           (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 for n = 1 to 5 do
                   reply "home\n" (call t GET (Printf.sprintf "/?n=%d" n))
                 done ;
                 let r = call t GET "/?n=6" in
                 equal status Status.too_many_requests r.status ;
                 equal (option string) (Some "1") (header "retry-after" r) ;
                 (* One second later, the bucket has one token. *)
                 T.advance t (Span.s 1) ;
                 reply "home\n" (call t GET "/?n=7") ;
                 equal status Status.too_many_requests
                   (call t GET "/?n=8").status ) )
       ; test "after 5 s with no request, the bucket is full again" (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 for _ = 1 to 6 do
                   ignore (call t GET "/")
                 done ;
                 T.advance t (Span.s 5) ;
                 for n = 1 to 5 do
                   reply "home\n" (call t GET (Printf.sprintf "/?n=%d" n))
                 done ;
                 equal status Status.too_many_requests
                   (call t GET "/?n=6").status ) )
       ; test "a 429 gets the CORS fields: the page can read Retry-After"
           (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 for _ = 1 to 5 do
                   ignore (call ~fields:[o] t GET "/")
                 done ;
                 let r = call ~fields:[o] t GET "/" in
                 equal status Status.too_many_requests r.status ;
                 equal (option string) (Some "https://app.example")
                   (header "access-control-allow-origin" r) ;
                 equal (option string) (Some "Retry-After")
                   (header "access-control-expose-headers" r) ) )
       ; test "a preflight takes no token" (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 for _ = 1 to 6 do
                   equal status Status.no_content
                     (call ~fields:[o; m; h] t OPTIONS "/notes").status
                 done ;
                 for _ = 1 to 5 do
                   reply "home\n" (call t GET "/")
                 done ) )
       ; test "each client address has its own bucket" (fun () ->
             (* The client of the simulator has one address. Thus the test calls
                the handler directly, with a connection of its own. *)
             let app = App.make () in
             (* [from ip id] is a GET of / from [ip], on the connection [id]. *)
             let from ip id =
               let conn =
                 Conn_info.make ~id
                   ~peer:(Net.Tcp (ip, 4000))
                   ~local:(Net.Tcp ("127.0.0.1", 80))
                   ()
               in
               Response.status (app (Request.make ~conn GET "/"))
             in
             T.run ~manual:true app (fun _ ->
                 (* Each request has its own connection, but the key is the
                    address: the requests share one bucket. *)
                 for id = 1 to 5 do
                   equal status Status.ok (from "10.0.0.2" id)
                 done ;
                 equal status Status.too_many_requests (from "10.0.0.2" 6) ;
                 equal status Status.ok (from "10.0.0.3" 7) ) )
       ; test "/report serves one request at a time: the second gets 503"
           (fun () ->
             T.run ~manual:true (App.make ()) (fun t ->
                 let first = T.background t (fun () -> call t GET "/report") in
                 T.settle t ;
                 equal status Status.service_unavailable
                   (call t GET "/report").status ;
                 (* /report takes 5 s: after 4 s, the first still runs. *)
                 T.advance t (Span.s 4) ;
                 equal status Status.service_unavailable
                   (call t GET "/report").status ;
                 T.advance t (Span.s 1) ;
                 reply "report ready\n" (Promise.await first) ;
                 (* The slot is free again. *)
                 let third = T.background t (fun () -> call t GET "/report") in
                 T.advance t (Span.s 5) ;
                 reply "report ready\n" (Promise.await third) ) )
       ; test "a cross-origin POST gets 403" (fun () ->
             let r = once ~fields:[cross_site] POST "/notes" in
             equal status Status.forbidden r.status )
       ; test "a cross-origin POST from the trusted origin gets 200" (fun () ->
             reply "note saved\n" (once ~fields:[cross_site; o] POST "/notes") )
       ; test "a cross-origin POST to /webhook gets 200" (fun () ->
             reply "webhook received\n"
               (once ~fields:[cross_site] POST "/webhook") ;
             reply "webhook received\n"
               (once ~fields:[cross_site] POST "/webhook?id=7") )
       ; test "a POST with no Sec-Fetch-Site and no Origin gets 200" (fun () ->
             reply "note saved\n" (once POST "/notes") )
       ; test "a GET of / gets Cache-Control: public, max-age=60" (fun () ->
             let r = once GET "/" in
             reply "home\n" r ;
             equal (option string) (Some "public, max-age=60")
               (header "cache-control" r) )
       ; test "a request with a cookie gets private, max-age=60" (fun () ->
             let r = once ~fields:[("Cookie", "session=abc")] GET "/" in
             equal (option string) (Some "private, max-age=60")
               (header "cache-control" r) )
       ; test "Authorization gives the private form, X-Api-Key does not"
           (fun () ->
             let r = once ~fields:[("Authorization", "Bearer abc")] GET "/" in
             equal (option string) (Some "private, max-age=60")
               (header "cache-control" r) ;
             let r = once ~fields:[("X-Api-Key", "abc")] GET "/" in
             equal (option string) (Some "public, max-age=60")
               (header "cache-control" r) )
       ; test "a response with Set-Cookie gets the private form" (fun () ->
             (* No route of the example sets a cookie: the test gives the value
                of the example to a handler that sets one. *)
             let handler _ =
               Response.string
                 ~headers:(Headers.of_list [("Set-Cookie", "session=abc")])
                 "home\n"
             in
             let r =
               Caravan_middleware.Cache_control.middleware App.public handler
                 (Request.make GET "/")
             in
             equal (option string) (Some "private, max-age=60")
               (Headers.find "cache-control" (Response.headers r)) )
       ; test "/notes keeps its own value, no-store" (fun () ->
             equal (option string) (Some "no-store")
               (header "cache-control" (once GET "/notes")) ) ]
