(* Compression: gzip and deflate for the responses, and the decode of request
   bodies that the client compressed.

   Run dune exec examples/compress/main.exe. First, set these shell variables:

   U=127.0.0.1:8080/upload G=Content-Encoding:gzip

   A page of more than 2 KiB gets gzip, Vary: Accept-Encoding and a weak ETag.
   --compressed tells curl to decode the body:

   curl -i --compressed -H 'Accept-Encoding: gzip' 127.0.0.1:8080/report

   A client of deflate gets deflate, with another weak ETag:

   curl -i --compressed -H 'Accept-Encoding: deflate' 127.0.0.1:8080/report

   A small page is not compressed:

   curl -i -H 'Accept-Encoding: gzip' 127.0.0.1:8080/hello

   The account page has a secret and text of the client. Thus it is not
   compressed (BREACH):

   curl -i -H 'Accept-Encoding: gzip' '127.0.0.1:8080/account?name=ada'

   The handler gets a compressed request body decoded. These 10240 bytes get
   "10240 bytes":

   head -c 10240 /dev/zero | gzip | curl -i -H $G --data-binary @- $U

   A body of more than 64 KiB gets 413, with or without gzip. A body that does
   not decode gets 400, and another coding gets 415:

   head -c 100000 /dev/zero | gzip | curl -i -H $G --data-binary @- $U

   head -c 100000 /dev/zero | curl -i --data-binary @- $U

   curl -i -H $G --data-binary 'not gzip' $U

   curl -i -H Content-Encoding:br --data-binary x $U

   An exception of a handler gets the page of Error_page (500), and the server
   prints a report:

   curl -i 127.0.0.1:8080/boom *)

open Caravan
module R = Caravan_router

(* A text of more than 2 KiB. The middleware compresses only bodies of 2 KiB or
   more (min_size): for a smaller body, the gain is small. *)
let report_text =
  String.concat ""
    (List.init 100 (fun i -> Printf.sprintf "Line %d of the report.\n" i))

(* The compressed bytes are not the bytes of the report. Thus the middleware
   makes the ETag weak. The ETag of gzip is W/"report-1", and the ETag of
   deflate is W/"report-1-deflate". *)
let report_page _ =
  Response.string
    ~headers:(Headers.of_list [("ETag", {|"report-1"|})])
    report_text

(* BREACH: the size of a compressed page tells an attacker if the text of the
   client is similar to the secret. With many requests, the attacker can find
   the secret, also over HTTPS. Thus a page with a secret and text of the client
   must not be compressed: the attribute skip stops the middleware.

   With nosniff, a browser does not read the text of the client as HTML. With
   no-store, no cache keeps the secret. *)
let account req =
  let name =
    Option.value (Form.find "name" (Request.query_params req)) ~default:"guest"
  in
  let headers =
    Headers.of_list
      [("X-Content-Type-Options", "nosniff"); ("Cache-Control", "no-store")]
  in
  Response.string ~headers
    (Printf.sprintf "Hello %s.\nYour API key is sk_example_1234.\n\n%s" name
       report_text )
  |> Response.with_attr Caravan_compress.skip ()

(* decompress limits only a compressed body. The server limits each body to
   max_body (8 MiB by default). Thus the handler reads at most 64 KiB, with or
   without a coding: more gives 413. *)
let upload req =
  match Body.to_string ~max:(64 * 1024) (Request.body req) with
  | body ->
      Response.string (Printf.sprintf "%d bytes\n" (String.length body))
  | exception Body.Too_large ->
      Response.string ~status:Status.content_too_large "Too large\n"

(* [report] gets a line for each exception of a handler. *)
let make ~report =
  let report req e _bt =
    report (Printf.sprintf "%s: %s" (Request.path req) (Printexc.to_string e))
  in
  Handler.compose
    [ Caravan_middleware.Error_page.middleware ~report ()
    ; (* ~level (0 to 9, 4 by default) trades CPU time for size: measure with
         your pages. *)
      Caravan_compress.middleware ()
    ; (* After Error_page: thus a body that does not decode gets 400, not the
         500 of Error_page. Error_page reports no error for it. *)
      Caravan_compress.decompress ~max_bytes:(64 * 1024) () ]
    (R.handler
       [ R.get "/report" report_page
       ; R.get "/hello" (fun _ -> Response.string "Hello!\n")
       ; R.get "/account" account
       ; R.post "/upload" upload
       ; R.get "/boom" (fun _ -> failwith "the handler failed") ] )
