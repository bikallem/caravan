(* The tests of the html example. Each request of the header comment of app.ml
   has a test here, with the same request. *)

open Windtrap
open Caravan
open Caravan_html
module T = Caravan_testing
module App = Example_html.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

(* A GET of [path]. [htmx] is the text of htmx.min.js, none by default. *)
let get ?htmx ?headers path =
  T.run (App.make ~htmx) (fun t -> T.get ?headers t path)

let content_type (r : T.reply) = Headers.find "content-type" r.headers

let () =
  exit
  @@ Windtrap.run "html"
       [ test "GET /?name=Ada gives the page in UTF-8" (fun () ->
             let r = get "/?name=Ada" in
             equal status Status.ok r.status ;
             equal (option string) (Some "text/html; charset=utf-8")
               (content_type r) ;
             starts_with ~prefix:"<!DOCTYPE html><html lang=\"en\">" r.body ;
             contains ~sub:"<h1>Hello, Ada!</h1>" r.body )
       ; test "the name <b> comes back as text" (fun () ->
             let r = get "/?name=%3Cb%3E&link=javascript:alert(1)" in
             contains ~sub:"<h1>Hello, &lt;b&gt;!</h1>" r.body ;
             not_contains ~sub:"<b>" r.body )
       ; test "a javascript: link becomes about:invalid#caravan" (fun () ->
             let r = get "/?name=%3Cb%3E&link=javascript:alert(1)" in
             contains
               ~sub:{|<a href="about:invalid#caravan">javascript:alert(1)</a>|}
               r.body )
       ; test "the href escapes the link" (fun () ->
             let r = get "/?link=%22%3E%3Cb%3E" in
             contains ~sub:{|<a href="&quot;&gt;&lt;b&gt;">|} r.body )
       ; test "At.href keeps http, https, mailto, tel and relative URLs"
           (fun () ->
             List.iter
               (fun link ->
                 let r = get ("/?link=" ^ link) in
                 contains ~sub:(Printf.sprintf {|<a href="%s">|} link) r.body )
               [ "http://example.com/"
               ; "https://example.com/"
               ; "mailto:ada@example.com"
               ; "tel:555-0100"
               ; "/more" ] )
       ; test "At.href changes another scheme to about:invalid#caravan"
           (fun () ->
             let r = get "/?link=data:text/html,x" in
             contains ~sub:{|<a href="about:invalid#caravan">|} r.body )
       ; test "the JSON for the script has the name, escaped" (fun () ->
             let r = get "/?name=%3C%2Fscript%3E" in
             (* The JSON escapes of < and >. \092 is a backslash. *)
             let lt = "\092u003c" and gt = "\092u003e" in
             contains
               ~sub:
                 ( {|<script type="application/json" id="data">{"name":"|} ^ lt
                 ^ "/script" ^ gt ^ {|"}</script>|} )
               r.body )
       ; test "the text of El.style and El.script is not escaped" (fun () ->
             let r = get "/" in
             contains ~sub:("<style>" ^ App.css ^ "</style>") r.body ;
             contains ~sub:("<script>" ^ App.script ^ "</script>") r.body )
       ; test "a task joins its classes and drops the empty class" (fun () ->
             equal string
               {|<li class="task done" data-task="Write the page"><input type="checkbox" checked>Write the page</li>|}
               (to_string (App.task ("Write the page", true))) ;
             equal string
               {|<li class="task" data-task="Test the page"><input type="checkbox">Test the page</li>|}
               (to_string (App.task ("Test the page", false))) )
       ; test "At.v refuses onclick, and the page has the onclick of At.unsafe"
           (fun () ->
             raises_match (Exn.invalid_arg ~sub:"onclick") (fun () ->
                 At.v "onclick" "window.print()" ) ;
             contains ~sub:{|<button type="button" onclick="window.print()">|}
               (get "/").body )
       ; test "with the file, the page loads /htmx.min.js with defer" (fun () ->
             let script = {|<script src="/htmx.min.js" defer></script>|} in
             contains ~sub:script (get ~htmx:"/* htmx */" "/").body ;
             contains ~sub:script (get ~htmx:"/* htmx */" "/more").body )
       ; test "without the file, the page does not load htmx" (fun () ->
             not_contains ~sub:"htmx.min.js" (get "/").body ;
             not_contains ~sub:"htmx.min.js" (get "/more").body )
       ; test "the page links to /more" (fun () ->
             let r = get "/" in
             contains
               ~sub:
                 {|<div id="more" aria-live="polite"><a href="/more" hx-get="/more" hx-target="#more">More</a></div>|}
               r.body )
       ; test "GET /more without htmx gives a full page" (fun () ->
             let r = get "/more" in
             equal status Status.ok r.status ;
             equal (option string) (Some "HX-Request")
               (Headers.find "vary" r.headers) ;
             starts_with ~prefix:"<!DOCTYPE html>" r.body ;
             contains ~sub:"<p>More text from the server.</p>" r.body )
       ; test "GET /more from htmx gives only the fragment" (fun () ->
             let r =
               get ~headers:(Headers.of_list [("hx-request", "true")]) "/more"
             in
             equal status Status.ok r.status ;
             equal (option string) (Some "text/html; charset=utf-8")
               (content_type r) ;
             equal (option string) (Some "HX-Request")
               (Headers.find "vary" r.headers) ;
             equal string "<p>More text from the server.</p>" r.body )
       ; test "without the file, GET /htmx.min.js gives 404" (fun () ->
             equal status Status.not_found (get "/htmx.min.js").status )
       ; test "with the file, GET /htmx.min.js gives it" (fun () ->
             let r = get ~htmx:"/* htmx */" "/htmx.min.js" in
             equal status Status.ok r.status ;
             equal (option string) (Some "text/javascript; charset=utf-8")
               (content_type r) ;
             equal string "/* htmx */" r.body ) ]
