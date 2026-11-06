(* Cookies: the client keeps its theme in a cookie. A POST sets the cookie and
   another POST clears it. A GET shows the theme and the names of the cookies.

   Run dune exec examples/cookies/main.exe, then send these requests. curl keeps
   the cookies in the file jar:

   curl -i -c jar -b jar -d t=dark 127.0.0.1:8080/theme

   curl -i -c jar -b jar 127.0.0.1:8080/

   curl -i -c jar -b jar 127.0.0.1:8080/cookies

   curl -i -c jar -b jar -X POST 127.0.0.1:8080/theme/clear

   curl -i -d t=blue 127.0.0.1:8080/theme

   The cookie has Secure, but curl sends it to 127.0.0.1 also over http. Chrome
   and Firefox keep a Secure cookie over http for localhost, but some browsers
   do not keep it for 127.0.0.1. Thus in a browser, use
   http://localhost:8080/. *)

open Caravan

(* The cookie of the theme. Its value comes from Cookie.with_value. A __Host-
   name needs Secure, Path=/ and no Domain: Cookie.make refuses it with no
   ~secure:true. A browser then keeps the cookie only from this host. The cookie
   lives 30 days. The defaults give HttpOnly (a script cannot read the cookie)
   and SameSite=Lax. *)
let theme = Cookie.make ~secure:true ~max_age:(30 * 24 * 3600) "__Host-theme" ""

let themes = ["light"; "dark"]

(* The client can send any value. Validate a cookie as you validate all other
   input. A request with no theme cookie, or with an unknown value, gets the
   default. *)
let show req =
  let t =
    match Request.cookie theme.name req with
    | Some t when List.mem t themes ->
        t
    | _ ->
        "light"
  in
  Response.string (Printf.sprintf "theme: %s\n" t)

(* The names of all the cookies of the request, one on each line. The response
   has no values: a value in a response lets a script read an HttpOnly
   cookie. *)
let names req =
  Response.string
    (String.concat "" (List.map (fun (n, _) -> n ^ "\n") (Request.cookies req)))

(* A GET must not change state, thus only a POST sets or clears the cookie.
   Cookie.pp shows the length of the value, not the value: thus a log line never
   shows a secret, for example a session ID. *)
let set ~log req =
  match Request.form ~max_bytes:1024 req with
  | Error e ->
      Response.string ~status:(Form.error_status e)
        (Format.asprintf "%a\n" Form.pp_error e)
  | Ok form -> (
    match Form.find "t" form with
    | Some t when List.mem t themes ->
        let c = Cookie.with_value t theme in
        log (Format.asprintf "set %a" Cookie.pp c) ;
        Response.string (Printf.sprintf "theme: %s\n" t)
        |> Response.add_cookie c
    | _ ->
        Response.string ~status:Status.bad_request "t: light or dark\n" )

(* Cookie.expired gives the cookie that removes the theme from the client: the
   same name and path, an empty value and Max-Age=0. *)
let clear _req =
  Response.string "theme: light\n" |> Response.add_cookie (Cookie.expired theme)

let routes ~log req =
  match (Request.meth req, Request.path_only req) with
  | GET, "/" ->
      show req
  | GET, "/cookies" ->
      names req
  | POST, "/theme" ->
      set ~log req
  | POST, "/theme/clear" ->
      clear req
  | _ ->
      Handler.not_found req

(* [make ~log] is the handler. [log] gets a line for each cookie that the server
   sets. Cross_origin refuses a POST that a browser sends from another site,
   with 403: thus another site cannot change the theme (CSRF). *)
let make ~log =
  Handler.compose [Caravan_middleware.Cross_origin.middleware ()] (routes ~log)
