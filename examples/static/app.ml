(* Static files: a middleware that serves a directory at a prefix, a directory
   on a route of the router, and one file.

   Run dune exec examples/static/main.exe from the root of the repository. It
   serves the directory examples/static/public, or the directory in the
   environment variable STATIC_ROOT. Then send these requests:

   curl -i 127.0.0.1:8080/assets/app.css

   The response has ETag, Last-Modified and X-Content-Type-Options: nosniff.
   Copy the ETag, with its quotes, into If-None-Match. Then the response is 304
   with no body:

   curl -i -H 'If-None-Match: <the ETag>' 127.0.0.1:8080/assets/app.css

   A range gives 206 with the first 4 bytes, and a range out of the file gives
   416:

   curl -i -H 'Range: bytes=0-3' 127.0.0.1:8080/assets/app.css

   curl -i -H 'Range: bytes=1000-' 127.0.0.1:8080/assets/app.css

   A directory gets its index.html. The path of a directory with no / at the end
   gets 301 to the path with a /:

   curl -i 127.0.0.1:8080/assets/

   curl -i 127.0.0.1:8080/assets/docs

   The directory has a file .env, but the server does not send it:

   curl -i 127.0.0.1:8080/assets/.env

   A path with a segment .. gets 400 from the router, also with %2e%2e for the
   dots. --path-as-is tells curl to send the .. with no change:

   curl -i --path-as-is 127.0.0.1:8080/assets/../app.ml

   A segment with %2f (a /) gets 404:

   curl -i 127.0.0.1:8080/files/..%2fapp.ml

   The route /files/** serves the same files, and the route /favicon.ico serves
   one file:

   curl -i 127.0.0.1:8080/files/docs/notes.md

   curl -i 127.0.0.1:8080/favicon.ico *)

open Caravan
module R = Caravan_router

(* An index.html names the other files, and these names can change. Thus a
   browser must ask again for an index.html each time (no-cache). It can keep
   the other files for one hour. *)
let cache_control rel =
  if Filename.basename rel = "index.html" then Some "no-cache"
  else Some "max-age=3600"

(* content_type knows the usual extensions. Add the types that it does not know,
   for example Markdown. *)
let content_type rel =
  if Filename.extension rel = ".md" then "text/markdown; charset=utf-8"
  else Caravan_static.content_type rel

(* Put only public files in root. The server follows symbolic links, also to a
   file out of root. Never serve a directory that clients write to. The rule for
   dot files is a second protection: a dot file in root is not a place for
   secrets. Thus the example keeps ~dotfiles at false, the default: a path with
   a segment that starts with a dot gets 404. *)
let make ~root =
  Handler.compose
    [ (* The files of root at the paths under /assets. The middleware gives the
         other requests to the router. A path that the rules refuse (for example
         .env) also goes to the router, thus it gets 404. *)
      Caravan_static.middleware ~prefix:"/assets" ~cache_control ~content_type
        ~root () ]
    (R.handler
       [ (* The same files on a route of the router. For the cost of the two,
            read "The cost" in caravan_static.mli. A refused path gets 404. *)
         R.get "/files/**" (fun req ->
             Caravan_static.dir ~cache_control ~content_type ~root (R.rest req)
               req )
       ; (* One file. Its type comes from its extension: image/x-icon. The path
            joins with '/', as caravan.static does: Windows accepts it, and the
            simulator of the tests has only '/'. *)
         R.get "/favicon.ico"
           (Caravan_static.file ~cache_control:"max-age=86400"
              (root ^ "/favicon.ico") ) ] )
