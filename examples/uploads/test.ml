(* The tests of the uploads example. Each request of the header comment of
   app.ml has a test here, with the same request. Request.with_multipart makes
   the body that curl -F sends. *)

open Windtrap
open Caravan
module T = Caravan_testing
module App = Example_uploads.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

(* A part as curl -F 'name=data;filename=...' sends it. *)
let file ?(content_type = "text/plain") name filename data =
  { Multipart.name
  ; filename= Some filename
  ; content_type= Some content_type
  ; data }

(* A part as curl -F name=data sends it. *)
let field name data = {Multipart.name; filename= None; content_type= None; data}

(* [request ~headers path fields] is a POST of the multipart [fields], as curl
   -F sends it. *)
let request ?(headers = []) path fields =
  Request.with_multipart fields
    (Request.make ~headers:(Headers.of_list headers) Method.POST path)

let fetch req = T.run App.handler (fun t -> T.fetch t req)

(* The file big.bin of head -c 2000000 /dev/zero > big.bin, as curl -F
   file=@big.bin sends it. *)
let big =
  file ~content_type:"application/octet-stream" "file" "big.bin"
    (String.make 2_000_000 '\000')

(* A multipart body with a file of [n] zero bytes, which it makes chunk by
   chunk: the test also keeps no file in memory. A body of Body.of_writer is not
   possible here: the handler reads the body with Body.read, which refuses a
   writer body. *)
let zeros_body n =
  let head =
    "--b\r\n\
     Content-Disposition: form-data; name=\"file\"; filename=\"zeros\"\r\n\
     \r\n"
  and tail = "\r\n--b--\r\n" in
  let h = String.length head in
  let total = h + n + String.length tail in
  let pos = ref 0 in
  Body.of_source ~length:total (fun buf off len ->
      let p = !pos in
      let k =
        if p < h then (
          (* The head of the part. *)
          let k = min len (h - p) in
          Bytes.blit_string head p buf off k ;
          k )
        else if p < h + n then (
          (* The zero bytes of the file. *)
          let k = min len (h + n - p) in
          Bytes.fill buf off k '\000' ;
          k )
        else
          (* The end of the body. At the end, k is 0. *)
          let k = min len (total - p) in
          Bytes.blit_string tail (p - h - n) buf off k ;
          k
      in
      pos := p + k ;
      k )

(* The largest size of the heap since the start, in bytes. *)
let heap_top () = (Gc.quick_stat ()).top_heap_words * (Sys.word_size / 8)

let () =
  exit
  @@ Windtrap.run "uploads"
       [ test "GET / gives the page with two forms for files" (fun () ->
             let r = T.run App.handler (fun t -> T.get t "/") in
             equal status Status.ok r.status ;
             equal (option string) (Some "text/html; charset=utf-8")
               (Headers.find "content-type" r.headers) ;
             in_order
               ~subs:
                 [ {|action="/upload" enctype="multipart/form-data"|}
                 ; {|action="/profile" enctype="multipart/form-data"|} ]
               r.body )
       ; test "POST /upload with two files gives their names and sizes"
           (fun () ->
             let r =
               fetch
                 (request "/upload"
                    [file "file" "a.txt" "hello"; file "file" "b.txt" "bye"] )
             in
             equal status Status.ok r.status ;
             equal string "a.txt: 5 bytes\nb.txt: 3 bytes\n" r.body )
       ; test "POST /upload ignores the parts that are not files of the field"
           (fun () ->
             let r =
               fetch
                 (request "/upload"
                    [ field "note" "x"
                    ; file "other" "c.txt" "x"
                    ; file "file" "a.txt" "hello" ] )
             in
             equal string "a.txt: 5 bytes\n" r.body )
       ; test "POST /upload gives the file names as the client sent them"
           (fun () ->
             let r =
               fetch
                 (request "/upload"
                    [ file "file" "../../etc/passwd" "x"
                    ; file "file" "/etc/passwd" "x" ] )
             in
             equal string "../../etc/passwd: 1 bytes\n/etc/passwd: 1 bytes\n"
               r.body )
       ; test "POST /upload counts a file of 2 MB" (fun () ->
             let r = fetch (request "/upload" [big]) in
             equal status Status.ok r.status ;
             equal string "big.bin: 2000000 bytes\n" r.body )
       ; test "POST /upload keeps no file in memory" (fun () ->
             (* The handler with no server: the limit max_body of the server
                does not apply. The heap of the program grows by less than 16
                MiB for a file of 64 MiB. *)
             let n = 64 * 1024 * 1024 in
             let req =
               Request.make
                 ~headers:
                   (Headers.of_list
                      [("content-type", "multipart/form-data; boundary=b")] )
                 ~body:(zeros_body n) Method.POST "/upload"
             in
             let before = heap_top () in
             let body =
               match Response.kind (App.handler req) with
               | Body b ->
                   Body.to_string b
               | Upgrade _ ->
                   fail "not a body"
             in
             equal string (Printf.sprintf "zeros: %d bytes\n" n) body ;
             is_true (heap_top () - before < 16 * 1024 * 1024) )
       ; test "POST /upload with more than 3 parts gives 413" (fun () ->
             let r =
               fetch
                 (request "/upload"
                    [field "a" "1"; field "b" "2"; field "c" "3"; field "d" "4"] )
             in
             equal status Status.content_too_large r.status ;
             equal string "Too_many_parts\n" r.body )
       ; test "POST /profile with a name and a photo gives them" (fun () ->
             let r =
               fetch
                 (request "/profile"
                    [field "name" "Ada"; file "photo" "ada.png" "PNG"] )
             in
             equal status Status.ok r.status ;
             equal string "name: Ada\nphoto: 3 bytes\n" r.body )
       ; test "POST /profile with a photo of 2 MB gives 413" (fun () ->
             let r =
               fetch
                 (request "/profile"
                    [field "name" "Ada"; {big with name= "photo"}] )
             in
             equal status Status.content_too_large r.status )
       ; test "POST /profile without a photo or a name gives 400" (fun () ->
             let r = fetch (request "/profile" [field "name" "Ada"]) in
             equal status Status.bad_request r.status ;
             let r =
               fetch (request "/profile" [file "photo" "ada.png" "PNG"])
             in
             equal status Status.bad_request r.status )
       ; test "a POST from another site gives 403" (fun () ->
             let cross = [("Sec-Fetch-Site", "cross-site")] in
             let r =
               fetch
                 (request ~headers:cross "/upload" [file "file" "a.txt" "x"])
             in
             equal status Status.forbidden r.status ;
             let r =
               fetch (request ~headers:cross "/profile" [field "name" "x"])
             in
             equal status Status.forbidden r.status )
       ; test "a form that is not multipart gives 415" (fun () ->
             let urlencoded path =
               Request.with_form
                 [("name", "Ada")]
                 (Request.make Method.POST path)
             in
             let r = fetch (urlencoded "/profile") in
             equal status Status.unsupported_media_type r.status ;
             let r = fetch (urlencoded "/upload") in
             equal status Status.unsupported_media_type r.status ) ]
