(* File uploads: an upload page, a streamed parse of files, and a parse in
   memory of a small form.

   Run dune exec examples/uploads/main.exe, then open http://127.0.0.1:8080/ in
   a browser, or send these requests:

   curl -i 127.0.0.1:8080/

   curl -i -F 'file=hello;filename=a.txt' -F 'file=bye;filename=b.txt' \
   127.0.0.1:8080/upload

   curl -i -F name=Ada -F 'photo=PNG;filename=ada.png' 127.0.0.1:8080/profile

   Make a file of 2 MB. It goes to /upload, but /profile keeps at most 1 MiB in
   memory, thus it gives 413:

   head -c 2000000 /dev/zero > /tmp/big.bin

   curl -i -F file=@/tmp/big.bin 127.0.0.1:8080/upload

   curl -i -F name=Ada -F photo=@/tmp/big.bin 127.0.0.1:8080/profile

   More than 3 parts give 413, and a body that is not multipart gives 415:

   curl -i -F a=1 -F b=2 -F c=3 -F d=4 127.0.0.1:8080/upload

   curl -i --data name=Ada 127.0.0.1:8080/profile *)

open Caravan
open Caravan_html

(* The upload page. A form that sends files must have the enctype
   multipart/form-data. *)
let page =
  let form ~action children =
    El.form
      ~at:
        [ At.method_ "post"
        ; At.action action
        ; At.v "enctype" "multipart/form-data" ]
      children
  in
  let file ~name ~label at =
    El.p
      [ El.label
          [txt label; El.input ~at:([At.type_ "file"; At.name name] @ at) ()] ]
  in
  list
    [ doctype
    ; El.html
        ~at:[At.lang "en"]
        [ El.head [El.meta ~at:[At.v "charset" "utf-8"] (); El.title "Uploads"]
        ; El.body
            [ El.h1 [txt "Uploads"]
            ; form ~action:"/upload"
                [ file ~name:"file" ~label:"Files (at most 3): " [At.multiple]
                ; El.button [txt "Upload"] ]
            ; form ~action:"/profile"
                [ El.p
                    [ El.label
                        [ txt "Name: "
                        ; El.input ~at:[At.name "name"; At.required] () ] ]
                ; file ~name:"photo" ~label:"Photo: " [At.required]
                ; El.button [txt "Send"] ] ] ] ]

let error e =
  Response.string ~status:(Multipart.error_status e)
    (Format.asprintf "%a\n" Multipart.pp_error e)

(* Streams the parts, and gives the name and the size of each file of the field
   "file". The handler ignores the other parts. Read the body of a part only in
   the callback. The reads go into one buffer of 64 KiB, thus no file stays in
   memory.

   The client gives the file name. It is not a safe path: it can be
   "../../etc/passwd" or an absolute path. Do not use it as the path of a file
   on the server: make a name of your own.

   The client also gives the type of a file. A real application examines the
   bytes. It serves a stored upload with its own Content-Type and
   X-Content-Type-Options: nosniff, or with Content-Disposition: attachment. *)
let upload req =
  let buf = Bytes.create 65536 in
  let rec size body n =
    match Body.read body buf 0 (Bytes.length buf) with
    | 0 ->
        n
    | k ->
        size body (n + k)
  in
  let add files part =
    match (Multipart.name part, Multipart.filename part) with
    | "file", Some filename ->
        (filename, size (Multipart.body part) 0) :: files
    | _ ->
        files
  in
  match Request.multipart ~max_parts:3 req ~init:[] add with
  | Ok files ->
      Response.string
        (String.concat ""
           (List.rev_map
              (fun (name, n) -> Printf.sprintf "%s: %d bytes\n" name n)
              files ) )
  | Error e ->
      error e

(* Keeps all parts in memory, at most 1 MiB, and gives the name and the size of
   the photo. Without a name or a photo, the response is 400. *)
let profile req =
  match Request.multipart_form ~max_bytes:(1024 * 1024) req with
  | Error e ->
      error e
  | Ok fields -> (
    match (Multipart.find "name" fields, Multipart.find "photo" fields) with
    | Some {filename= None; data= name; _}, Some {filename= Some _; data; _} ->
        Response.string
          (Printf.sprintf "name: %s\nphoto: %d bytes\n" name
             (String.length data) )
    | _ ->
        Response.string ~status:Status.bad_request "Send a name and a photo.\n"
    )

let routes req =
  match (Request.meth req, Request.path_only req) with
  | GET, "/" ->
      response page
  | POST, "/upload" ->
      upload req
  | POST, "/profile" ->
      profile req
  | _ ->
      Handler.not_found req

(* Cross_origin refuses a POST that a browser sends from another site, with 403:
   a protection against CSRF. curl sends no Sec-Fetch-Site and no Origin field,
   and gets through. *)
let handler =
  Handler.compose [Caravan_middleware.Cross_origin.middleware ()] routes
