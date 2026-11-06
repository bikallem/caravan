(* The tests of the static example. They send the requests of the curl lines of
   app.ml. Each test in the simulator first copies the files of public/ into the
   file system of the simulator, with Weft.File and Weft.Dir. The test of a
   symbolic link runs on the OS. *)

open Windtrap
open Caravan
module T = Caravan_testing
module App = Example_static.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

(* The files of public/, as (path under public/, contents). The test reads them
   from the disk before the simulator starts. *)
let files =
  let rec walk rel =
    let path = Filename.concat "public" rel in
    if Sys.is_directory path then
      Sys.readdir path |> Array.to_list
      |> List.concat_map (fun name ->
          walk (if rel = "" then name else rel ^ "/" ^ name) )
    else [(rel, In_channel.with_open_bin path In_channel.input_all)]
  in
  walk ""

let file rel = List.assoc rel files

(* The root of the files in the simulator. *)
let root = "/srv/public"

let rec mkdirs dir =
  if dir <> "/" then (
    mkdirs (Filename.dirname dir) ;
    try Weft.Dir.mkdir dir with Weft.Dir.Error (Exists, _) -> () )

(* [copy ()] writes [files] under [root], in the simulator. *)
let copy () =
  List.iter
    (fun (rel, data) ->
      let path = Filename.concat root rel in
      mkdirs (Filename.dirname path) ;
      let f = Weft.File.openfile [Write; Create; Truncate] path in
      Weft.File.write f data ; Weft.File.close f )
    files

(* [run f] runs [f] with a server of the example, after [copy]. The file
   /srv/app.ml is out of the root: a request must never get it. *)
let run f =
  T.run (App.make ~root) (fun t ->
      copy () ;
      let f' = Weft.File.openfile [Write; Create; Truncate] "/srv/app.ml" in
      Weft.File.write f' "out of the root\n" ;
      Weft.File.close f' ;
      f t )

(* [get ?headers path] is the reply to a GET of [path] with [headers]. *)
let get ?(headers = []) path =
  run (fun t -> T.get ~headers:(Headers.of_list headers) t path)

let header name (r : T.reply) = Headers.find name r.headers

let has name value r =
  equal ~msg:name (option string) (Some value) (header name r)

let () =
  exit
  @@ Windtrap.run "static"
       [ test "GET /assets/app.css gives the file, ETag, Last-Modified, nosniff"
           (fun () ->
             let r = get "/assets/app.css" in
             equal status Status.ok r.status ;
             equal string (file "app.css") r.body ;
             has "content-type" "text/css; charset=utf-8" r ;
             has "x-content-type-options" "nosniff" r ;
             has "cache-control" "max-age=3600" r ;
             is_true ~msg:"ETag" (Option.is_some (header "etag" r)) ;
             is_true ~msg:"Last-Modified"
               (Option.is_some (header "last-modified" r)) )
       ; test "If-None-Match with the ETag gives 304 with no body" (fun () ->
             let r =
               run (fun t ->
                   let etag =
                     Option.get (header "etag" (T.get t "/assets/app.css"))
                   in
                   T.get t
                     ~headers:(Headers.of_list [("If-None-Match", etag)])
                     "/assets/app.css" )
             in
             equal status Status.not_modified r.status ;
             equal string "" r.body )
       ; test "Range: bytes=0-3 gives 206 with the first 4 bytes" (fun () ->
             let r = get ~headers:[("Range", "bytes=0-3")] "/assets/app.css" in
             equal status Status.partial_content r.status ;
             equal string (String.sub (file "app.css") 0 4) r.body ;
             has "content-range"
               (Printf.sprintf "bytes 0-3/%d" (String.length (file "app.css")))
               r )
       ; test "a range out of the file gives 416" (fun () ->
             let size = String.length (file "app.css") in
             is_true ~msg:"the file is shorter than 1000 bytes" (size < 1000) ;
             let r =
               get ~headers:[("Range", "bytes=1000-")] "/assets/app.css"
             in
             equal status Status.range_not_satisfiable r.status ;
             has "content-range" (Printf.sprintf "bytes */%d" size) r )
       ; test "GET /assets/ gives index.html with no-cache" (fun () ->
             let r = get "/assets/" in
             equal status Status.ok r.status ;
             equal string (file "index.html") r.body ;
             has "content-type" "text/html; charset=utf-8" r ;
             has "cache-control" "no-cache" r )
       ; test "GET /assets/docs gives 301 to docs/" (fun () ->
             let r = get "/assets/docs" in
             equal status Status.moved_permanently r.status ;
             has "location" "docs/" r )
       ; test "GET /assets/docs/ gives docs/index.html with no-cache" (fun () ->
             let r = get "/assets/docs/" in
             equal status Status.ok r.status ;
             equal string (file "docs/index.html") r.body ;
             has "cache-control" "no-cache" r )
       ; test "GET /assets/.env gives 404, but the file is in the root"
           (fun () ->
             is_true ~msg:".env is in public/" (List.mem_assoc ".env" files) ;
             let r = get "/assets/.env" in
             equal status Status.not_found r.status ;
             is_false ~msg:"no secret in the body"
               (String.equal (file ".env") r.body) )
       ; test "GET /assets/../app.ml gives 400, also with %2e%2e" (fun () ->
             equal status Status.bad_request (get "/assets/../app.ml").status ;
             equal status Status.bad_request
               (get "/assets/%2e%2e/app.ml").status )
       ; test "GET /files/..%2fapp.ml gives 404" (fun () ->
             equal status Status.not_found (get "/files/..%2fapp.ml").status )
       ; test "GET /assets/docs/notes.md gives the file as text/markdown"
           (fun () ->
             let r = get "/assets/docs/notes.md" in
             equal status Status.ok r.status ;
             equal string (file "docs/notes.md") r.body ;
             has "content-type" "text/markdown; charset=utf-8" r )
       ; test "GET /files/docs/notes.md gives the file as text/markdown"
           (fun () ->
             let r = get "/files/docs/notes.md" in
             equal status Status.ok r.status ;
             equal string (file "docs/notes.md") r.body ;
             has "content-type" "text/markdown; charset=utf-8" r ;
             has "cache-control" "max-age=3600" r )
       ; test "GET /files/.env gives 404" (fun () ->
             equal status Status.not_found (get "/files/.env").status )
       ; test "GET /favicon.ico gives the file as image/x-icon" (fun () ->
             let r = get "/favicon.ico" in
             equal status Status.ok r.status ;
             equal string (file "favicon.ico") r.body ;
             has "content-type" "image/x-icon" r ;
             has "cache-control" "max-age=86400" r )
       ; test "the server follows a symbolic link out of the root" (fun () ->
             (* On the OS: the simulator has no symbolic links. The link
                public/link.txt points to ../secret.txt. *)
             let dir = Filename.temp_dir "static" "" in
             let root = Filename.concat dir "public" in
             let secret = Filename.concat dir "secret.txt" in
             let link = Filename.concat root "link.txt" in
             Sys.mkdir root 0o700 ;
             Out_channel.with_open_bin secret (fun oc ->
                 output_string oc "secret\n" ) ;
             Unix.symlink "../secret.txt" link ;
             let cleanup () =
               List.iter Sys.remove [link; secret] ;
               List.iter Sys.rmdir [root; dir]
             in
             let r =
               Fun.protect ~finally:cleanup
               @@ fun () ->
               Weft.run
                 (module Weft_os)
                 (fun () ->
                   match
                     Response.kind
                       (App.make ~root (Request.make GET "/assets/link.txt"))
                   with
                   | Body b ->
                       Body.to_string b
                   | Upgrade _ ->
                       "" )
             in
             equal string "secret\n" r ) ]
