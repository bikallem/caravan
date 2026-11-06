(* The tests of the compress example. They send the requests of the curl lines
   of app.ml. *)

open Windtrap
open Caravan
module T = Caravan_testing
module App = Example_compress.App

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

(* {1 Requests} *)

(* [run f] runs [f] with a server of the example. It gives the result of [f] and
   the reports of Error_page. *)
let run f =
  let reports = ref [] in
  let v = T.run (App.make ~report:(fun l -> reports := l :: !reports)) f in
  (v, List.rev !reports)

let get ?(headers = []) path =
  fst (run (fun t -> T.get ~headers:(Headers.of_list headers) t path))

(* [upload ~coding body] is the reply to a POST of [body] to /upload, with
   Content-Encoding: [coding] if any, and the reports. *)
let upload ?coding body =
  let fields = Option.map (fun c -> ("Content-Encoding", c)) coding in
  run (fun t ->
      T.fetch t
        (Request.make
           ~headers:(Headers.of_list (Option.to_list fields))
           ~body:(Body.of_string body) Method.POST "/upload" ) )

let header name (r : T.reply) = Headers.find name r.headers

let has name value r = equal ~msg:name (option string) value (header name r)

let gzip_accepted = [("Accept-Encoding", "gzip")]

(* {1 gzip}

   The test makes and decodes gzip bodies with decompress (not with
   caravan.compress), as the gzip program and curl --compressed do. *)

let buffer () = De.bigstring_create De.io_buffer_size

(* [refill s] gives the bytes of [s] to decompress, one buffer at a time. *)
let refill s =
  let pos = ref 0 in
  fun buf ->
    let n = min (String.length s - !pos) (Bigarray.Array1.dim buf) in
    for k = 0 to n - 1 do
      Bigarray.Array1.set buf k s.[!pos + k]
    done ;
    pos := !pos + n ;
    n

let flush b buf len =
  for k = 0 to len - 1 do
    Buffer.add_char b (Bigarray.Array1.get buf k)
  done

(* [gzip s] is [s] in the gzip format. *)
let gzip s =
  let b = Buffer.create 1024 in
  Gz.Higher.compress
    ~w:(De.Lz77.make_window ~bits:15)
    ~q:(De.Queue.create 0x1000) ~refill:(refill s) ~flush:(flush b) ()
    (Gz.Higher.configuration Gz.Unix (fun () -> 0l))
    (buffer ()) (buffer ()) ;
  Buffer.contents b

(* [gunzip s] is the text of the gzip data [s]. *)
let gunzip s =
  let b = Buffer.create 1024 in
  match
    Gz.Higher.uncompress ~refill:(refill s) ~flush:(flush b) (buffer ())
      (buffer ())
  with
  | Ok _ ->
      Buffer.contents b
  | Error (`Msg m) ->
      failwith m

let reports = Testable.(list string)

let zeros n = String.make n '\000'

(* [gzipped n] is the reply to an upload of [n] zero bytes, in gzip. *)
let gzipped n = upload ~coding:"gzip" (gzip (zeros n))

let () =
  exit
  @@ Windtrap.run "compress"
       [ test "GET /report with gzip: gzip, Vary, a weak ETag, the text"
           (fun () ->
             is_true ~msg:"the report has more than 2 KiB"
               (String.length App.report_text > 2048) ;
             let r = get ~headers:gzip_accepted "/report" in
             equal status Status.ok r.status ;
             has "content-encoding" (Some "gzip") r ;
             has "vary" (Some "Accept-Encoding") r ;
             has "etag" (Some {|W/"report-1"|}) r ;
             equal string App.report_text (gunzip r.body) )
       ; test "GET /report with deflate: deflate and another weak ETag"
           (fun () ->
             let r = get ~headers:[("Accept-Encoding", "deflate")] "/report" in
             equal status Status.ok r.status ;
             has "content-encoding" (Some "deflate") r ;
             has "etag" (Some {|W/"report-1-deflate"|}) r )
       ; test "GET /hello with gzip: not compressed" (fun () ->
             let r = get ~headers:gzip_accepted "/hello" in
             equal status Status.ok r.status ;
             has "content-encoding" None r ;
             equal string "Hello!\n" r.body )
       ; test "GET /account?name=ada with gzip: not compressed (skip)"
           (fun () ->
             let r = get ~headers:gzip_accepted "/account?name=ada" in
             equal status Status.ok r.status ;
             has "content-encoding" None r ;
             (* The page is large: only skip stops the compression. *)
             is_true ~msg:"more than 2 KiB" (String.length r.body > 2048) ;
             is_true ~msg:"the text of the client"
               (String.starts_with ~prefix:"Hello ada.\n" r.body) ;
             has "x-content-type-options" (Some "nosniff") r ;
             has "cache-control" (Some "no-store") r )
       ; test "a gzip body of 10240 bytes gets to the handler as 10240 bytes"
           (fun () ->
             let r, rs = gzipped 10240 in
             equal status Status.ok r.status ;
             equal string "10240 bytes\n" r.body ;
             equal reports [] rs )
       ; test "a gzip body that decodes to 64 KiB passes, one more byte: 413"
           (fun () ->
             let r, _ = gzipped (64 * 1024) in
             equal string "65536 bytes\n" r.body ;
             let r, rs = gzipped ((64 * 1024) + 1) in
             equal status Status.content_too_large r.status ;
             equal reports [] rs )
       ; test "a gzip body that decodes to more than 64 KiB gives 413"
           (fun () ->
             let r, rs = gzipped 100000 in
             equal status Status.content_too_large r.status ;
             equal reports [] rs )
       ; test "a body of more than 64 KiB with no coding gives 413" (fun () ->
             let r, _ = upload (zeros (64 * 1024)) in
             equal string "65536 bytes\n" r.body ;
             let r, _ = upload (zeros ((64 * 1024) + 1)) in
             equal status Status.content_too_large r.status ;
             let r, rs = upload (zeros 100000) in
             equal status Status.content_too_large r.status ;
             equal reports [] rs )
       ; test "a body that does not decode gives 400, and no report" (fun () ->
             let r, rs = upload ~coding:"gzip" "not gzip" in
             equal status Status.bad_request r.status ;
             equal reports [] rs )
       ; test "Content-Encoding: br gives 415" (fun () ->
             let r, rs = upload ~coding:"br" "x" in
             equal status Status.unsupported_media_type r.status ;
             has "accept-encoding" (Some "gzip, deflate") r ;
             equal reports [] rs )
       ; test "an exception: 500, the page of Error_page, and one report"
           (fun () ->
             let r, rs = run (fun t -> T.get t "/boom") in
             equal status Status.internal_server_error r.status ;
             equal string "Internal Server Error\n" r.body ;
             equal reports ["/boom: Failure(\"the handler failed\")"] rs ) ]
