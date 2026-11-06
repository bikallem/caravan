(* The tests of the forms example. Each request of the header comment of app.ml
   has a test here, with the same request. *)

open Windtrap
open Caravan
module T = Caravan_testing
module App = Example_forms.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

(* [request ~headers path form] is a POST of the urlencoded [form], as curl -d
   sends it. *)
let request ?(headers = []) path form =
  Request.with_form form
    (Request.make ~headers:(Headers.of_list headers) Method.POST path)

(* [raw_request ~content_type path body] is a POST of the bytes [body]. *)
let raw_request ~content_type path body =
  Request.make
    ~headers:(Headers.of_list [("Content-Type", content_type)])
    ~body:(Body.of_string body) Method.POST path

let get path = T.run App.handler (fun t -> T.get t path)

let fetch req = T.run App.handler (fun t -> T.fetch t req)

let () =
  exit
  @@ Windtrap.run "forms"
       [ test "GET / gives the HTML page with the two forms" (fun () ->
             let r = get "/" in
             equal status Status.ok r.status ;
             equal (option string) (Some "text/html; charset=utf-8")
               (Headers.find "content-type" r.headers) ;
             contains ~sub:"<form action=\"/search\">" r.body ;
             contains ~sub:"name=\"tag\" value=\"web\"" r.body ;
             contains ~sub:"name=\"tag\" value=\"http\"" r.body ;
             contains ~sub:"<form method=\"post\" action=\"/subscribe\">" r.body )
       ; test "GET /search?q=a+b gives \"a b\": + is a space" (fun () ->
             let r = get "/search?q=a+b" in
             equal status Status.ok r.status ;
             equal string "search: a b\ntags: \nexact: false\n" r.body )
       ; test "Form.find gives the first value of a name" (fun () ->
             let r = get "/search?q=a&q=b" in
             equal string "search: a\ntags: \nexact: false\n" r.body )
       ; test "the tags come in order, and exact is there" (fun () ->
             let r = get "/search?q=ocaml&tag=web&tag=http&exact=on" in
             equal status Status.ok r.status ;
             equal string "search: ocaml\ntags: web, http\nexact: true\n" r.body )
       ; test "GET /search with no q gives 400" (fun () ->
             let r = get "/search" in
             equal status Status.bad_request r.status ;
             equal string "The search needs q.\n" r.body )
       ; test "GET /search with an empty q gives 400" (fun () ->
             let r = get "/search?q=&tag=web" in
             equal status Status.bad_request r.status )
       ; test "GET /hello?name=ada gives the name" (fun () ->
             let r = get "/hello?name=ada" in
             equal string "Hello, ada!\n" r.body )
       ; test "GET /hello with no name gives the default" (fun () ->
             let r = get "/hello" in
             equal string "Hello, guest!\n" r.body )
       ; test "POST /subscribe with an urlencoded email gives the email"
           (fun () ->
             let r =
               fetch (request "/subscribe" [("email", "ada@example.com")])
             in
             equal status Status.ok r.status ;
             equal string "Subscribed: ada@example.com\n" r.body )
       ; test "POST /subscribe with no email gives 400" (fun () ->
             let r = fetch (request "/subscribe" [("name", "ada")]) in
             equal status Status.bad_request r.status ;
             equal string "The form needs email.\n" r.body )
       ; test "a JSON body gives 415" (fun () ->
             let r =
               fetch
                 (raw_request ~content_type:"application/json" "/subscribe" "{}")
             in
             equal status Status.unsupported_media_type r.status ;
             equal string "Unsupported_media_type\n" r.body )
       ; test "a body larger than 1 KiB gives 413" (fun () ->
             let r =
               fetch
                 (raw_request ~content_type:Form.media_type "/subscribe"
                    (String.make 2000 '\000') )
             in
             equal status Status.content_too_large r.status ;
             equal string "Too_large\n" r.body )
       ; test "a body of 1 KiB is permitted" (fun () ->
             let email = String.make (1024 - String.length "email=") 'x' in
             let r = fetch (request "/subscribe" [("email", email)]) in
             equal status Status.ok r.status )
       ; test "a POST from another site gives 403" (fun () ->
             let r =
               fetch
                 (request
                    ~headers:[("Sec-Fetch-Site", "cross-site")]
                    "/subscribe"
                    [("email", "ada@example.com")] )
             in
             equal status Status.forbidden r.status )
       ; test "a POST from the same origin gets through" (fun () ->
             let r =
               fetch
                 (request
                    ~headers:[("Sec-Fetch-Site", "same-origin")]
                    "/subscribe"
                    [("email", "ada@example.com")] )
             in
             equal status Status.ok r.status )
       ; test "GET /subscribe gives 404: the route takes only POST" (fun () ->
             let r = get "/subscribe?email=ada@example.com" in
             equal status Status.not_found r.status ) ]
