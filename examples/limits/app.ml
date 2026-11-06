(* CORS, limits, a protection against cross-origin requests, and cache headers,
   for an API of the pages of https://app.example. The limits are a rate limit
   and a concurrency limit.

   Run dune exec examples/limits/main.exe. First, set these shell variables:

   O=Origin:https://app.example M=Access-Control-Request-Method:PUT
   H=Access-Control-Request-Headers:content-type

   A preflight from the permitted origin gets 204 with the CORS fields, and the
   handler does not run:

   curl -i -X OPTIONS -H $O -H $M -H $H 127.0.0.1:8080/notes

   A preflight from another origin gets 204 with no CORS field:

   curl -i -X OPTIONS -H Origin:https://other.example -H $M 127.0.0.1:8080/notes

   A request from the permitted origin gets the CORS fields: its page can read
   the response and Retry-After. A request from another origin gets no CORS
   field. CORS is not access control: the handler runs for the two origins.

   curl -i -H $O 127.0.0.1:8080/notes

   curl -i -H Origin:https://other.example 127.0.0.1:8080/notes

   Each client has a burst of 5 requests, and one more request each second. Thus
   the sixth request in one second gets 429 with Retry-After: 1. The earlier
   requests took tokens: wait 5 s, thus the bucket is full again. Then curl
   sends 6 requests:

   curl -si '127.0.0.1:8080/?n=[1-6]' | grep -Ei '^(http|retry)'

   /report takes 5 s, and serves only one request at a time. Thus a second
   request during the first gets 503:

   curl -s 127.0.0.1:8080/report & sleep 1; curl -i 127.0.0.1:8080/report

   A browser tells in Sec-Fetch-Site that a request is cross-origin. A
   cross-origin POST gets 403, but not from the trusted origin, and not to
   /webhook:

   curl -i -X POST -H Sec-Fetch-Site:cross-site 127.0.0.1:8080/notes

   curl -i -X POST -H Sec-Fetch-Site:cross-site -H $O 127.0.0.1:8080/notes

   curl -i -X POST -H Sec-Fetch-Site:cross-site 127.0.0.1:8080/webhook

   A POST from curl has no Sec-Fetch-Site and no Origin: it does not come from a
   browser, thus it gets 200:

   curl -i -X POST 127.0.0.1:8080/notes

   A GET of / gets Cache-Control: public, max-age=60. A request with a cookie
   gets the private form, private, max-age=60. /notes keeps its own value,
   no-store:

   curl -i 127.0.0.1:8080/

   curl -i -b session=abc 127.0.0.1:8080/

   curl -i 127.0.0.1:8080/notes *)

open Weft
open Caravan
open Caravan_middleware

let app_origin = "https://app.example"

(* The pages of app_origin can send PUT with a JSON body, and with the cookies
   of the user (~credentials:true). Retry-After is in ~expose_headers, thus the
   code of a page can read it on a 429. *)
let cors =
  Cors.middleware ~origins:(Only [app_origin]) ~allow_methods:[PUT]
    ~allow_headers:["content-type"] ~expose_headers:["Retry-After"]
    ~credentials:true ~max_age:600 ()

(* Put the CORS origins with credentials in ~trusted_origins too. A webhook gets
   POSTs from other sites: ~exempt lets them in. Compare the path with no query.

   Cross_origin does not protect the webhook. Thus the webhook must verify the
   signature of the sender (an HMAC of the body), and must not use the session
   of the user. *)
let cross_origin =
  Cross_origin.middleware ~trusted_origins:[app_origin]
    ~exempt:(fun req -> Request.path_only req = "/webhook")
    ()

(* A shared cache can keep the public pages for 60 s. Give a shared value only
   to public content, for example in a router group. A response for one user
   gets the private form only for a request with Cookie or Authorization, or for
   a response with Set-Cookie. Another field, for example X-Api-Key, does not
   change the form. *)
let public = Cache_control.make ~scope:`Public ~max_age:60 ()

(* The notes are data of the user: no cache keeps them. The middleware does not
   change the Cache-Control of a response. *)
let no_store = Cache_control.make ~no_store:true ()

let notes _req =
  Response.string
    ~headers:
      (Headers.of_list [("Cache-Control", Cache_control.to_string no_store)])
    "notes\n"

let slow_report _req =
  Time.sleep Mono (Span.s 5) ;
  Response.string "report ready\n"

let routes ~limited_report req =
  match (Request.meth req, Request.path_only req) with
  | GET, "/" ->
      Response.string "home\n"
  | GET, "/notes" ->
      notes req
  | POST, "/notes" ->
      Response.string "note saved\n"
  | PUT, "/notes" ->
      Response.string "notes replaced\n"
  | POST, "/webhook" ->
      Response.string "webhook received\n"
  | GET, "/report" ->
      limited_report req
  | _ ->
      Handler.not_found req

(* The middleware are in the order of the README. Cors is first, thus a 429 gets
   the CORS fields, and a preflight takes no token of the rate limit. Rate_limit
   counts the requests of each client address. Behind a proxy, put Forwarded
   first, as the proxy_headers example does. *)
let make () =
  let limited_report = Concurrency_limit.middleware ~limit:1 () slow_report in
  Handler.compose
    [ cors
    ; (* ~max_keys (100_000 by default) limits the memory of the buckets: see
         caravan_middleware.mli. *)
      Rate_limit.middleware ~burst:5 ~every:(Span.s 1) ()
    ; cross_origin
    ; Cache_control.middleware public ]
    (routes ~limited_report)
