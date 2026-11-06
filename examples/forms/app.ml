(* Queries and forms: the pairs of a query, an HTML page with two forms, and a
   form body with a limit.

   Run dune exec examples/forms/main.exe, then open http://127.0.0.1:8080/ in a
   browser, or send these requests:

   curl -i '127.0.0.1:8080/search?q=a+b'

   curl -i '127.0.0.1:8080/search?q=ocaml&tag=web&tag=http&exact=on'

   curl -i 127.0.0.1:8080/search

   curl -i '127.0.0.1:8080/hello?name=ada'

   curl -i -d email=ada@example.com 127.0.0.1:8080/subscribe

   curl -i -H 'Content-Type: application/json' -d '{}' 127.0.0.1:8080/subscribe

   The body of the next request is larger than 1 KiB, thus it gets 413:

   head -c 2000 /dev/zero | curl -i --data-binary @- 127.0.0.1:8080/subscribe *)

open Caravan

let bad_request text = Response.string ~status:Status.bad_request text

(* The page at /, with caravan.html. The search form has no method, thus the
   browser sends a GET with the fields in the query. Each checked box gives one
   more pair: two tags give tag=web&tag=http. *)
let page =
  let open Caravan_html in
  let checkbox name value label =
    El.label
      [ El.input ~at:[At.type_ "checkbox"; At.name name; At.value value] ()
      ; txt label ]
  in
  list
    [ doctype
    ; El.html
        ~at:[At.lang "en"]
        [ El.head [El.meta ~at:[At.v "charset" "utf-8"] (); El.title "Forms"]
        ; El.body
            [ El.h1 [txt "Search"]
            ; El.form
                ~at:[At.action "/search"]
                [ El.label
                    [txt "Words "; El.input ~at:[At.name "q"; At.required] ()]
                ; checkbox "tag" "web" " Web"
                ; checkbox "tag" "http" " HTTP"
                ; checkbox "exact" "on" " Exact words"
                ; El.button [txt "Search"] ]
            ; El.h1 [txt "Subscribe"]
            ; El.form
                ~at:[At.method_ "post"; At.action "/subscribe"]
                [ El.label
                    [ txt "Email "
                    ; El.input
                        ~at:[At.type_ "email"; At.name "email"; At.required]
                        () ]
                ; El.button [txt "Subscribe"] ] ] ] ]

(* Form.find gives the first value of a name, Form.find_all gives all of them in
   order, and Form.mem tells if the name is there. A query with no q, or with an
   empty q, gets 400. *)
let search req =
  let params = Request.query_params req in
  match Form.find "q" params with
  | None | Some "" ->
      bad_request "The search needs q.\n"
  | Some q ->
      let tags = Form.find_all "tag" params in
      let exact = Form.mem "exact" params in
      Response.string
        (Printf.sprintf "search: %s\ntags: %s\nexact: %b\n" q
           (String.concat ", " tags) exact )

(* Form.find_text finds one value in the text of the query, with no list of the
   pairs. A query with no name gets the default. *)
let hello req =
  let name = Option.bind (Request.query req) (Form.find_text "name") in
  Response.string
    (Printf.sprintf "Hello, %s!\n" (Option.value ~default:"guest" name))

(* Request.form reads the body: call it one time and keep the result. The limit
   is 1 KiB: a larger body gets 413, and a body that is not urlencoded, for
   example JSON, gets 415. Form.error_status gives the status of each error. *)
let subscribe req =
  match Request.form ~max_bytes:1024 req with
  | Error e ->
      Response.string ~status:(Form.error_status e)
        (Format.asprintf "%a\n" Form.pp_error e)
  | Ok form -> (
    match Form.find "email" form with
    | None | Some "" ->
        bad_request "The form needs email.\n"
    | Some email ->
        Response.string (Printf.sprintf "Subscribed: %s\n" email) )

let routes req =
  match (Request.meth req, Request.path_only req) with
  | GET, "/" ->
      Caravan_html.response page
  | GET, "/search" ->
      search req
  | GET, "/hello" ->
      hello req
  | POST, "/subscribe" ->
      subscribe req
  | _ ->
      Handler.not_found req

(* Cross_origin refuses a POST that a browser sends from another site, with 403:
   a protection against CSRF. Thus you can copy this POST handler, with its
   Cross_origin middleware, for a form that changes state. A client that is not
   a browser, for example curl, sends no Sec-Fetch-Site and no Origin field, and
   gets through. *)
let handler =
  Handler.compose [Caravan_middleware.Cross_origin.middleware ()] routes
