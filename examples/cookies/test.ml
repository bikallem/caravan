(* The tests of the cookies example. Each request of the header comment of
   app.ml has a test here, with the same request. *)

open Windtrap
open Caravan
module T = Caravan_testing
module App = Example_cookies.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let handler = App.make ~log:ignore

(* A small cookie jar, as curl -c jar -b jar keeps it: the name and value pairs
   of the client. [send t jar req] sends [req] with a Cookie field of the jar,
   then keeps the cookies of the Set-Cookie fields of the response. An empty
   value removes a cookie, as Cookie.expired sends it. The jar reads no other
   attribute. *)
let send t jar req =
  let req =
    match !jar with
    | [] ->
        req
    | pairs ->
        let field =
          String.concat "; " (List.map (fun (n, v) -> n ^ "=" ^ v) pairs)
        in
        Request.with_headers
          (Headers.add "Cookie" field (Request.headers req))
          req
  in
  let r = T.fetch t req in
  List.iter
    (fun field ->
      match Cookie.parse_set_cookie field with
      | Some (n, "") ->
          jar := List.remove_assoc n !jar
      | Some (n, v) ->
          jar := (n, v) :: List.remove_assoc n !jar
      | None ->
          () )
    (Headers.find_all "set-cookie" r.headers) ;
  r

(* [request ~headers ~cookie ~form meth path] is a request. [cookie] is the
   value of a Cookie field. [form] is an urlencoded body, as curl -d sends
   it. *)
let request ?(headers = []) ?cookie ?form meth path =
  let headers =
    match cookie with Some c -> ("Cookie", c) :: headers | None -> headers
  in
  let req = Request.make ~headers:(Headers.of_list headers) meth path in
  match form with Some f -> Request.with_form f req | None -> req

(* [dark_request ()] is the request of curl -d t=dark 127.0.0.1:8080/theme. *)
let dark_request ?headers () =
  request ?headers ~form:[("t", "dark")] POST "/theme"

let fetch req = T.run handler (fun t -> T.fetch t req)

let set_cookies (r : T.reply) = Headers.find_all "set-cookie" r.headers

let () =
  exit
  @@ Windtrap.run "cookies"
       [ test "POST /theme with t=dark sets a __Host- cookie" (fun () ->
             let r = fetch (dark_request ()) in
             equal status Status.ok r.status ;
             equal string "theme: dark\n" r.body ;
             equal (list string)
               [ "__Host-theme=dark; Max-Age=2592000; Path=/; Secure; \
                  HttpOnly; SameSite=Lax" ]
               (set_cookies r) )
       ; test "with the jar, GET / shows the theme of the cookie" (fun () ->
             let r =
               T.run handler (fun t ->
                   let jar = ref [] in
                   ignore (send t jar (dark_request ())) ;
                   send t jar (request GET "/") )
             in
             equal string "theme: dark\n" r.body )
       ; test "with the jar, GET /cookies gives the names" (fun () ->
             let r =
               T.run handler (fun t ->
                   let jar = ref [] in
                   ignore (send t jar (dark_request ())) ;
                   send t jar (request GET "/cookies") )
             in
             equal string "__Host-theme\n" r.body )
       ; test "GET /cookies gives no value" (fun () ->
             let r =
               fetch
                 (request ~cookie:"__Host-theme=dark; sid=secret" GET "/cookies")
             in
             equal string "__Host-theme\nsid\n" r.body )
       ; test "POST /theme/clear removes the cookie, then / gives the default"
           (fun () ->
             let clear, after =
               T.run handler (fun t ->
                   let jar = ref [] in
                   ignore (send t jar (dark_request ())) ;
                   let clear = send t jar (request POST "/theme/clear") in
                   (clear, send t jar (request GET "/")) )
             in
             equal (list string)
               [ "__Host-theme=; Expires=Thu, 01 Jan 1970 00:00:00 GMT; \
                  Max-Age=0; Path=/; Secure; HttpOnly; SameSite=Lax" ]
               (set_cookies clear) ;
             equal string "theme: light\n" after.body )
       ; test "POST /theme with t=blue gives 400 and no cookie" (fun () ->
             let r = fetch (request ~form:[("t", "blue")] POST "/theme") in
             equal status Status.bad_request r.status ;
             equal (list string) [] (set_cookies r) )
       ; test "POST /theme with a JSON body gives 415" (fun () ->
             let r =
               fetch
                 (Request.make
                    ~headers:
                      (Headers.of_list [("Content-Type", "application/json")])
                    ~body:(Body.of_string {|{"t":"dark"}|})
                    POST "/theme" )
             in
             equal status Status.unsupported_media_type r.status )
       ; test "GET / with no cookie gives the default" (fun () ->
             let r = fetch (request GET "/") in
             equal string "theme: light\n" r.body )
       ; test "GET / with an unknown value gives the default" (fun () ->
             let r = fetch (request ~cookie:"__Host-theme=blue" GET "/") in
             equal string "theme: light\n" r.body )
       ; test "a POST from another site gives 403 and no cookie" (fun () ->
             let cross = [("Sec-Fetch-Site", "cross-site")] in
             let r = fetch (dark_request ~headers:cross ()) in
             equal status Status.forbidden r.status ;
             equal (list string) [] (set_cookies r) ;
             let r = fetch (request ~headers:cross POST "/theme/clear") in
             equal status Status.forbidden r.status ;
             equal (list string) [] (set_cookies r) )
       ; test "a GET changes no cookie: GET /theme gives 404" (fun () ->
             let r = fetch (request GET "/theme?t=dark") in
             equal status Status.not_found r.status ;
             equal (list string) [] (set_cookies r) )
       ; test "the log line gives the length of the value, not the value"
           (fun () ->
             let logs = ref [] in
             let log line = logs := line :: !logs in
             ignore
               (T.run (App.make ~log) (fun t -> T.fetch t (dark_request ()))) ;
             equal (list string)
               [ "set __Host-theme=<4 bytes>; Max-Age=2592000; Path=/; Secure; \
                  HttpOnly; SameSite=Lax" ]
               !logs ) ]
