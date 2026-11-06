(* A page of caravan.html: elements, attributes, text, data for a script, and
   htmx.

   Run dune exec examples/html/main.exe, then open http://127.0.0.1:8080/ in a
   browser, or send these requests:

   curl -i '127.0.0.1:8080/?name=Ada'

   caravan.html escapes the data of a user. The name <b> comes back as text, and
   a javascript: link becomes about:invalid#caravan:

   curl -i '127.0.0.1:8080/?name=%3Cb%3E&link=javascript:alert(1)'

   The page uses htmx (https://htmx.org) for the link "More". The example does
   not load htmx from another host. To use it, download htmx.min.js of htmx 2
   (see https://htmx.org/docs/#installing) to examples/html/htmx.min.js, and
   start the example from the root of the repository. Then the page loads it
   from /htmx.min.js. Without the file, the page does not load htmx, and the
   link opens /more as a full page. With htmx, the link gets only a fragment,
   and htmx puts it into the page:

   curl -i 127.0.0.1:8080/more

   curl -i -H 'HX-Request: true' 127.0.0.1:8080/more *)

open Caravan
open Caravan_html

let tasks = [("Write the page", true); ("Test the page", false)]

(* All classes of an element join, and an empty class drops. At.none adds no
   attribute. *)
let task (name, done_) =
  El.li
    ~at:
      [At.classes ["task"; (if done_ then "done" else "")]; At.data "task" name]
    [ El.input
        ~at:[At.type_ "checkbox"; (if done_ then At.checked else At.none)]
        ()
    ; txt name ]

(* The text of El.style and El.script is not escaped: give it only text of your
   code. *)
let css = ".done { text-decoration: line-through; }"

let script =
  {|const data = JSON.parse(document.getElementById("data").textContent);
document.title = "Tasks of " + data.name;|}

(* A full page. It loads htmx only if the server has it ([htmx] is true). The
   defer attribute has no value: At.bool makes it. *)
let layout ~htmx content =
  list
    [ doctype
    ; El.html
        ~at:[At.lang "en"]
        [ El.head
            [ El.meta ~at:[At.v "charset" "utf-8"] ()
            ; El.title "caravan.html"
            ; El.style css
            ; ( if htmx then
                  El.script ~at:[At.src "/htmx.min.js"; At.bool "defer"] ""
                else empty ) ]
        ; El.body content ] ]

let more = El.p [txt "More text from the server."]

(* The page for the name and the link of a user. txtf escapes the name. The
   escape hatch unsafe_raw does not escape: give it only trusted markup of your
   code, never data of a user. At.href escapes the link. At.href permits only
   http, https, mailto, tel and relative URLs: another scheme becomes
   about:invalid#caravan. The JSON for the script goes into El.json_script,
   which escapes it: thus a name cannot close the script element.

   At.v refuses onclick, because its value runs as script. Use At.unsafe only
   for a value of your code. With a strict Content-Security-Policy, an inline
   script needs a nonce or a hash. An inline handler, as this onclick, needs
   'unsafe-hashes' and its hash: use addEventListener in a script. *)
let home ~htmx ~name ~link =
  layout ~htmx
    [ El.h1 [txtf "Hello, %s!" name]
    ; ( match link with
      | None ->
          empty
      | Some url ->
          El.p [txt "Your link: "; El.a ~at:[At.href url] [txt url]] )
    ; El.ul (List.map task tasks)
    ; El.div
        ~at:[At.id "more"; At.aria "live" "polite"]
        [ El.a
            ~at:
              [At.href "/more"; At.v "hx-get" "/more"; At.v "hx-target" "#more"]
            [txt "More"] ]
    ; El.button
        ~at:[At.type_ "button"; At.unsafe "onclick" "window.print()"]
        [txt "Print"]
    ; (* Any JSON library works: El.json_script takes the JSON text. *)
      El.json_script
        ~at:[At.id "data"]
        (Yojson.Safe.to_string (`Assoc [("name", `String name)]))
    ; El.script script ]

(* [make ~htmx] serves the page. [htmx] is the text of htmx.min.js, if any. *)
let make ~htmx req =
  let query = Request.query_params req in
  match (Request.meth req, Request.path_only req) with
  | GET, "/" ->
      let name = Option.value ~default:"world" (Form.find "name" query) in
      response
        (home ~htmx:(Option.is_some htmx) ~name ~link:(Form.find "link" query))
  | GET, "/more" ->
      (* htmx sends HX-Request: true. A browser without htmx gets a page. The
         Vary field tells caches that the response changes with this field. *)
      let headers = Headers.of_list [("vary", "HX-Request")] in
      if Headers.find "hx-request" (Request.headers req) = Some "true" then
        response ~headers more
      else
        response ~headers
          (layout ~htmx:(Option.is_some htmx)
             [more; El.a ~at:[At.href "/"] [txt "Back"]] )
  | GET, "/htmx.min.js" -> (
    match htmx with
    | Some js ->
        Response.string ~content_type:"text/javascript; charset=utf-8" js
    | None ->
        Handler.not_found req )
  | _ ->
      Handler.not_found req
