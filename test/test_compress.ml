(* The tests of caravan.compress. The compressed bytes are decoded with
   decompress, and compared with the bytes of the handler. *)

open Windtrap
open Caravan
module C = Caravan_compress

(* {1 Decoders} *)

let decode_with decode flush dst_rem d =
  let size = 65536 in
  let o = De.bigstring_create size in
  let b = Buffer.create 1024 in
  let take d =
    for k = 0 to size - dst_rem d - 1 do
      Buffer.add_char b (Bigarray.Array1.get o k)
    done
  in
  let rec go d =
    match decode d with
    | `Await _ ->
        failwith "await"
    | `Flush d ->
        take d ;
        go (flush d)
    | `End d ->
        take d ; Buffer.contents b
    | `Malformed e ->
        failwith e
  in
  go (d o)

let gunzip s =
  decode_with Gz.Inf.decode Gz.Inf.flush Gz.Inf.dst_rem (fun o ->
      Gz.Inf.decoder (`String s) ~o )

let inflate s =
  decode_with Zl.Inf.decode Zl.Inf.flush Zl.Inf.dst_rem (fun o ->
      Zl.Inf.decoder (`String s) ~o ~allocate:(fun bits ->
          De.make_window ~bits ) )

(* {1 Helpers} *)

let text n =
  String.init n (fun i ->
      "the quick brown fox jumps over the lazy dog\n".[i mod 44] )

(* [drain r] is the bytes, the count of flushes and the trailers of the body of
   [r], written out as an engine does. *)
let drain r =
  match Response.kind r with
  | Response.Body body ->
      let b = Buffer.create 1024 and flushes = ref 0 in
      let w =
        Body.Writer.make
          ~write:(fun buf off len -> Buffer.add_subbytes b buf off len)
          ~flush:(fun () -> incr flushes)
      in
      Body.write_to body w ;
      (Buffer.contents b, !flushes, Body.Writer.trailers w)
  | Response.Upgrade _ ->
      fail "an upgrade"

let bytes_of r =
  let s, _, _ = drain r in
  s

let field name r = Headers.find name (Response.headers r)

let fields name r = Headers.find_all name (Response.headers r)

let req ?(meth = Method.GET) ?version ?(ae = Some "gzip") ?(fields = []) () =
  let fields =
    match ae with Some v -> ("Accept-Encoding", v) :: fields | None -> fields
  in
  Request.make ?version ~headers:(Headers.of_list fields) meth "/"

let html ?(status = Status.ok) ?(headers = []) ?(content_type = "text/html")
    ?(size = 2048) () _ =
  Response.string ~status
    ~headers:(Headers.of_list (("Content-Type", content_type) :: headers))
    (text size)

let run ?(mw = C.middleware ()) ?(request = req ()) h = mw h request

let compressed r = field "content-encoding" r

(* {1 The negotiation} *)

let negotiation_tests =
  let coding =
    Testable.make
      ~pp:(fun ppf -> function
        | Some C.Gzip ->
            Format.pp_print_string ppf "gzip"
        | Some C.Deflate ->
            Format.pp_print_string ppf "deflate"
        | None ->
            Format.pp_print_string ppf "identity" )
      ~equal:( = )
  in
  group "Negotiation"
    [ test "the choice of a coding" (fun () ->
          List.iter
            (fun (values, expected) ->
              equal
                ~msg:(String.concat " | " values)
                coding expected (C.negotiate values) )
            [ ([], None)
            ; ([""], None)
            ; (["gzip"], Some C.Gzip)
            ; (["deflate"], Some C.Deflate)
            ; (["GZIP"], Some C.Gzip)
            ; (["x-gzip"], Some C.Gzip)
            ; (["gzip, deflate, br"], Some C.Gzip)
            ; (["deflate, gzip"], Some C.Gzip)
            ; (["gzip;q=0.5, deflate"], Some C.Deflate)
            ; (["gzip;q=0.5"; "deflate;q=0.6"], Some C.Deflate)
            ; (["gzip; Q=0.5 , deflate;q=0.501"], Some C.Deflate)
            ; (["gzip;q=0"], None)
            ; (["gzip;q=0, *"], Some C.Deflate)
            ; (["*"], Some C.Gzip)
            ; (["*;q=0.1, deflate;q=0.2"], Some C.Deflate)
            ; (["identity;q=0, *;q=0"], None)
            ; (["identity"], None)
            ; (["br"], None)
            ; (["gzip;q=1.000"], Some C.Gzip)
            ; (["gzip;q=0."; "deflate;q=0.001"], Some C.Deflate)
            ; (["gzip;q=0.999, deflate;q=1"], Some C.Deflate)
            ; (["gzip;q=0, gzip"], None)
            ; (["gzip;q=1, deflate"], Some C.Gzip)
            ; (["deflate;q=1, gzip"], Some C.Gzip)
            ; (["deflate;q=1.0, gzip"], Some C.Gzip)
            ; (["gzip;q=05, deflate"], None)
            ; (["gzip;q=0.5x, deflate"], None)
            ; (["gzip, gzip;q=0"], Some C.Gzip)
            ; ([", ,gzip,"], Some C.Gzip)
            ; (["\tgzip\t"], Some C.Gzip)
              (* x-gzip is gzip: its first element wins. *)
            ; (["x-gzip;q=0, gzip"], None) (* A bad element: no coding. *)
            ; (["gzip;q=1.001"], None)
            ; (["gzip;q=2"], None)
            ; (["gzip;q=.5"], None)
            ; (["gzip;q=0.5000"], None)
            ; (["gzip;q=x"], None)
            ; (["gzip;q="], None)
            ; (["gzip;q"], None)
            ; (["gzip;level=1"], None)
            ; (["gzip;q=0.5;x=1"], None)
            ; (["gzip q=0.5"], None)
            ; ([";q=1"], None)
            ; (["gzip;q=0.5"; "de@flate"], None) ] ) ]

(* {1 Compressed responses} *)

let compress_tests =
  group "Compressed responses"
    [ test "a text response with gzip" (fun () ->
          let r = run (html ()) in
          equal (option string) (Some "gzip") (compressed r) ;
          equal (option string) None (field "content-length" r) ;
          equal (list string) ["Accept-Encoding"] (fields "vary" r) ;
          let s = bytes_of r in
          equal string (text 2048) (gunzip s) ;
          (* No modification time in the gzip header. *)
          equal string "\000\000\000\000" (String.sub s 4 4) ;
          (* The default level is 4. *)
          let big = html ~size:65536 () in
          equal string
            (bytes_of (run ~mw:(C.middleware ~level:4 ()) big))
            (bytes_of (run big)) )
    ; test "deflate, and the level" (fun () ->
          let request = req ~ae:(Some "deflate") () in
          let size level coding =
            let request = req ~ae:(Some coding) () in
            String.length
              (bytes_of
                 (run ~mw:(C.middleware ~level ()) ~request
                    (html ~size:65536 ()) ) )
          in
          let r = run ~request (html ()) in
          equal (option string) (Some "deflate") (compressed r) ;
          equal string (text 2048) (inflate (bytes_of r)) ;
          List.iter
            (fun coding ->
              is_true ~msg:coding (size 0 coding > size 9 coding) ;
              is_true ~msg:coding (size 0 coding > 65536) )
            ["gzip"; "deflate"] ;
          let r = run ~mw:(C.middleware ~level:9 ()) ~request (html ()) in
          equal string (text 2048) (inflate (bytes_of r)) )
    ; test "the responses that are not compressed" (fun () ->
          (* [unchanged label h]: the middleware gives the response of [h]
             itself. *)
          let unchanged ?mw ?request label h =
            let original = ref None in
            let r =
              run ?mw ?request (fun req ->
                  let r = h req in
                  original := Some r ;
                  r )
            in
            is_true ~msg:label
              (match !original with Some o -> r == o | None -> false) ;
            r
          in
          (* [not_compressed label h]: no coding, but a Vary. *)
          let not_compressed ?request label h =
            let r = run ?request h in
            equal ~msg:label (option string) None (compressed r) ;
            equal ~msg:label (list string) ["Accept-Encoding"] (fields "vary" r)
          in
          let with_headers headers = html ~headers () in
          ignore (unchanged "upgrade" (fun _ -> Response.upgrade ignore)) ;
          (* An upgrade with a compressible type, and streams. *)
          ignore
            (unchanged "upgrade with a type" ~mw:(C.middleware ~streams:true ())
               (fun _ ->
                 Response.with_headers
                   (Headers.of_list [("Content-Type", "text/html")])
                   (Response.upgrade ignore) ) ) ;
          ignore
            (unchanged "CONNECT"
               ~request:(req ~meth:Method.CONNECT ())
               (html ()) ) ;
          ignore
            (unchanged "HTTP/1.0"
               ~request:(req ~version:Version.Http_1_0 ())
               (html ()) ) ;
          ignore
            (unchanged "skip" (fun req ->
                 Response.with_attr C.skip () (html () req) ) ) ;
          List.iter
            (fun (label, headers) ->
              ignore (unchanged label (with_headers headers)) )
            [ ("content-encoding", [("Content-Encoding", "br")])
            ; ("content-range", [("Content-Range", "bytes 0-1/2048")])
            ; ("no-transform", [("Cache-Control", "public, No-Transform")])
            ; ("content-digest", [("Content-Digest", "sha-256=:x:")])
            ; ("repr-digest", [("Repr-Digest", "sha-256=:x:")])
            ; ("content-md5", [("Content-MD5", "x")])
            ; ("a digest trailer", [("Trailer", "X-A, Content-Digest")]) ] ;
          List.iter
            (fun code ->
              ignore
                (unchanged (string_of_int code)
                   (html ~status:(Status.v code) ()) ) )
            [204; 205; 206] ;
          List.iter
            (fun ct ->
              let r = unchanged ct (html ~content_type:ct ()) in
              equal ~msg:ct (list string) [] (fields "vary" r) )
            ["image/png"; "text/event-stream"; "application/octet-stream"] ;
          let r =
            unchanged "no type" (fun _ ->
                let r = Response.string (text 2048) in
                Response.with_headers
                  (Headers.remove "content-type" (Response.headers r))
                  r )
          in
          equal (list string) [] (fields "vary" r) ;
          not_compressed "identity" ~request:(req ~ae:None ()) (html ()) ;
          not_compressed "br" ~request:(req ~ae:(Some "br") ()) (html ()) )
    ; test "the media types" (fun () ->
          List.iter
            (fun (ct, expected) ->
              equal ~msg:ct bool expected
                (Option.is_some (compressed (run (html ~content_type:ct ())))) )
            [ ("text/plain; charset=utf-8", true)
            ; ("TEXT/CSS", true)
            ; ("application/json", true)
            ; ("application/javascript", true)
            ; ("application/xml", true)
            ; ("image/svg+xml", true)
            ; ("application/problem+json", true)
            ; ("application/atom+xml", true)
            ; ("application/wasm", false)
            ; ("text/event-stream; charset=utf-8", false) ] ;
          let mw =
            C.middleware ~compressible:(fun t -> t = "application/wasm") ()
          in
          is_some
            (compressed (run ~mw (html ~content_type:"application/wasm" ()))) ;
          is_none (compressed (run ~mw (html ()))) )
    ; test "the size limits" (fun () ->
          let mw = C.middleware ~min_size:100 ~max_size:200 () in
          List.iter
            (fun (size, expected) ->
              let r = run ~mw (html ~size ()) in
              equal ~msg:(string_of_int size) bool expected
                (Option.is_some (compressed r)) ;
              (* The decision does not depend on the request: no Vary. *)
              if not expected then
                equal ~msg:(string_of_int size) (list string) []
                  (fields "vary" r) )
            [(99, false); (100, true); (200, true); (201, false)] ;
          (* The defaults: 2048 bytes and 8 MiB. *)
          is_none (compressed (run (html ~size:2047 ()))) ;
          is_some (compressed (run (html ~size:2048 ()))) ;
          is_some (compressed (run (html ~size:(8 * 1024 * 1024) ()))) ;
          is_none (compressed (run (html ~size:((8 * 1024 * 1024) + 1) ()))) )
    ; test "Vary" (fun () ->
          let vary headers = fields "vary" (run (html ~headers ())) in
          equal (list string)
            ["Origin"; "Accept-Encoding"]
            (vary [("Vary", "Origin")]) ;
          equal (list string)
            ["origin, accept-ENCODING"]
            (vary [("Vary", "origin, accept-ENCODING")]) ;
          equal (list string)
            ["Origin"; "accept-encoding"]
            (vary [("Vary", "Origin"); ("Vary", "accept-encoding")]) ;
          equal (list string) ["*"] (vary [("Vary", "*")]) ;
          (* Identity for a compressible type: Vary. *)
          let r = run ~request:(req ~ae:None ()) (html ()) in
          equal (list string) ["Accept-Encoding"] (fields "vary" r) )
    ; test "the ETags become weak" (fun () ->
          let etags headers = fields "etag" (run (html ~headers ())) in
          equal (list string) [{|W/"x"|}] (etags [("ETag", {|"x"|})]) ;
          equal (list string) [{|W/"x"|}] (etags [("ETag", {|W/"x"|})]) ;
          equal (list string) ["x"] (etags [("ETag", "x")]) ;
          equal (list string) [{|"x|}] (etags [("ETag", {|"x|})]) ;
          equal (list string) [{|x"|}] (etags [("ETag", {|x"|})]) ;
          equal (list string) [{|W/""|}] (etags [("ETag", {|""|})]) ;
          equal (list string) [{|W/"a"|}; {|W/"b"|}]
            (etags [("ETag", {|"a"|}); ("ETag", {|W/"b"|})]) ;
          (* No Accept-Ranges. *)
          let r = run (html ~headers:[("Accept-Ranges", "bytes")] ()) in
          equal (option string) None (field "accept-ranges" r) )
    ; test "a 304 gets the ETag and the Vary of its 200" (fun () ->
          let r304 ?(headers = []) () _ =
            Response.empty ~status:Status.not_modified
              ~headers:(Headers.of_list headers) ()
          in
          let r =
            run
              (r304
                 ~headers:
                   [ ("Content-Type", "text/html")
                   ; ("Content-Length", "4000")
                   ; ("ETag", {|"x"|}) ]
                 () )
          in
          equal (list string) [{|W/"x"|}] (fields "etag" r) ;
          equal (list string) ["Accept-Encoding"] (fields "vary" r) ;
          (* A 304 is never compressed. *)
          equal (option string) None (compressed r) ;
          (* No coding: the strong ETag, and the Vary. *)
          let r =
            run ~request:(req ~ae:None ())
              (r304
                 ~headers:
                   [ ("Content-Type", "text/html")
                   ; ("Content-Length", "4000")
                   ; ("ETag", {|"x"|}) ]
                 () )
          in
          equal (list string) [{|"x"|}] (fields "etag" r) ;
          equal (list string) ["Accept-Encoding"] (fields "vary" r) ;
          (* No type, a small file, no length: no change. *)
          List.iter
            (fun (label, headers) ->
              let r = run (r304 ~headers:(("ETag", {|"x"|}) :: headers) ()) in
              equal ~msg:label (list string) [{|"x"|}] (fields "etag" r) ;
              equal ~msg:label (list string) [] (fields "vary" r) )
            [ ("no type", [("Content-Length", "4000")])
            ; ( "small"
              , [("Content-Type", "text/html"); ("Content-Length", "2047")] )
            ; ("no length", [("Content-Type", "text/html")]) ] ;
          (* deflate: its own tag. *)
          let r =
            run
              ~request:(req ~ae:(Some "deflate") ())
              (r304
                 ~headers:
                   [ ("Content-Type", "text/html")
                   ; ("Content-Length", "4000")
                   ; ("ETag", {|"x"|}) ]
                 () )
          in
          equal (list string) [{|W/"x-deflate"|}] (fields "etag" r) )
    ; test "a writer body with many writes and flushes" (fun () ->
          let data = text 400_000 in
          let h _ =
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_writer ~length:400_000 (fun w ->
                   let rec go i =
                     if i < 400_000 then (
                       let n = min 997 (400_000 - i) in
                       Body.Writer.write_string w (String.sub data i n) ;
                       Body.Writer.flush w ;
                       go (i + n) )
                   in
                   go 0 ) )
          in
          List.iter
            (fun (coding, decode) ->
              let s, flushes, _ =
                drain (run ~request:(req ~ae:(Some coding) ()) h)
              in
              equal ~msg:coding string data (decode s) ;
              (* Each flush of the handler reaches the writer. *)
              (* One flush for each write of 997 bytes. *)
              is_true ~msg:coding (flushes >= (400_000 + 996) / 997) )
            [("gzip", gunzip); ("deflate", inflate)] )
    ; test "one write of 1 MiB, and an empty body" (fun () ->
          let data = text (1024 * 1024) in
          let h _ =
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_writer ~length:(String.length data) (fun w ->
                   Body.Writer.write_string w data ) )
          in
          equal string data (gunzip (bytes_of (run h))) ;
          (* Writes of one byte. *)
          let h _ =
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_writer ~length:3001 (fun w ->
                   Body.Writer.write_string w "a" ;
                   Body.Writer.write_string w (text 2999) ;
                   Body.Writer.write_string w "z" ) )
          in
          equal string ("a" ^ text 2999 ^ "z") (gunzip (bytes_of (run h))) ;
          let empty _ =
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_writer (fun _ -> ()))
          in
          let mw = C.middleware ~min_size:0 ~streams:true () in
          let r = run ~mw empty in
          equal (option string) (Some "gzip") (compressed r) ;
          equal string "" (gunzip (bytes_of r)) )
    ; test "streams only with ~streams:true" (fun () ->
          let stream _ =
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_writer (fun w -> Body.Writer.write_string w (text 5000)))
          in
          let r = run stream in
          equal (option string) None (compressed r) ;
          equal (list string) [] (fields "vary" r) ;
          let r = run ~mw:(C.middleware ~streams:true ()) stream in
          equal (option string) (Some "gzip") (compressed r) ;
          equal string (text 5000) (gunzip (bytes_of r)) )
    ; test "the trailers of a source" (fun () ->
          let data = text 3000 in
          let pos = ref 0 in
          let h _ =
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_source ~length:3000
                 ~trailers:(fun () -> Headers.of_list [("X-Sum", "1")])
                 (fun buf off len ->
                   let n = min len (3000 - !pos) in
                   Bytes.blit_string data !pos buf off n ;
                   pos := !pos + n ;
                   n ) )
          in
          let s, _, t = drain (run h) in
          equal string data (gunzip s) ;
          equal (option string) (Some "1") (Headers.find "x-sum" t) )
    ; test "an exception of the body goes out" (fun () ->
          let h _ =
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_writer ~length:5000 (fun w ->
                   Body.Writer.write_string w (text 100) ;
                   failwith "body" ) )
          in
          raises_match
            (function Failure m -> m = "body" | _ -> false)
            (fun () -> ignore (drain (run h))) )
    ; test "a declared length that the producer does not write" (fun () ->
          let h n _ =
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_writer ~length:3000 (fun w ->
                   Body.Writer.write_string w (text n) ) )
          in
          List.iter
            (fun n ->
              raises_match ~msg:(string_of_int n)
                (function Failure _ -> true | _ -> false)
                (fun () -> ignore (drain (run (h n)))) )
            [2999; 3001] ;
          equal string (text 3000) (gunzip (bytes_of (run (h 3000)))) )
    ; test "HEAD gets the fields" (fun () ->
          let r = run ~request:(req ~meth:Method.HEAD ()) (html ()) in
          equal (option string) (Some "gzip") (compressed r) ;
          equal (option string) None (field "content-length" r) )
    ; test "the old body is closed at each end" (fun () ->
          let closes = ref 0 in
          let h ?(fail = false) () _ =
            let pos = ref 0 in
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_source ~length:3000
                 ~close:(fun () -> incr closes)
                 (fun buf off len ->
                   if fail then failwith "read" ;
                   let n = min len (3000 - !pos) in
                   Bytes.blit_string (text 3000) !pos buf off n ;
                   pos := !pos + n ;
                   n ) )
          in
          ignore (bytes_of (run (h ()))) ;
          equal ~msg:"written" int 1 !closes ;
          ( match Response.kind (run (h ())) with
          | Response.Body b ->
              Body.close b
          | Response.Upgrade _ ->
              fail "an upgrade" ) ;
          equal ~msg:"abandoned" int 2 !closes ;
          (try ignore (bytes_of (run (h ~fail:true ()))) with Failure _ -> ()) ;
          equal ~msg:"failed" int 3 !closes ;
          (* Each end frees the slot of its encoder. *)
          let mw = C.middleware ~max_active:1 () in
          ( match Response.kind (run ~mw (h ())) with
          | Response.Body b ->
              Body.close b
          | Response.Upgrade _ ->
              fail "an upgrade" ) ;
          ( try ignore (bytes_of (run ~mw (h ~fail:true ())))
            with Failure _ -> () ) ;
          is_some ~msg:"free" (compressed (run ~mw (h ()))) )
    ; test "max_active" (fun () ->
          let mw = C.middleware ~max_active:1 () in
          let first = run ~mw (html ()) in
          equal (option string) (Some "gzip") (compressed first) ;
          (* The first body is not written out: its encoder is busy. *)
          let second = run ~mw (html ~headers:[("ETag", {|"e"|})] ()) in
          equal (option string) None (compressed second) ;
          equal (list string) ["Accept-Encoding"] (fields "vary" second) ;
          (* The ETag of a compressed response: one validator for a cache. *)
          equal (list string) [{|W/"e"|}] (fields "etag" second) ;
          ignore (bytes_of first) ;
          is_some (compressed (run ~mw (html ()))) )
    ; test "random bytes and each level" (fun () ->
          let random =
            String.init 70_000 (fun i ->
                Char.chr (((i * 7919) + (i * i)) mod 251) )
          in
          List.iter
            (fun level ->
              List.iter
                (fun (coding, decode) ->
                  let mw = C.middleware ~level () in
                  let h _ =
                    Response.string
                      ~headers:
                        (Headers.of_list [("Content-Type", "text/plain")])
                      random
                  in
                  let r = run ~mw ~request:(req ~ae:(Some coding) ()) h in
                  equal
                    ~msg:(coding ^ string_of_int level)
                    string random
                    (decode (bytes_of r)) )
                [("gzip", gunzip); ("deflate", inflate)] )
            [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] )
    ; test "a longer body stops at once" (fun () ->
          let written = ref 0 in
          let h _ =
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_writer ~length:3000 (fun w ->
                   Body.Writer.write_string w (text 3000) ;
                   written := 3000 ;
                   Body.Writer.write_string w (text (1024 * 1024)) ;
                   written := !written + (1024 * 1024) ) )
          in
          raises_match
            (function Failure _ -> true | _ -> false)
            (fun () -> ignore (drain (run h))) ;
          equal int 3000 !written )
    ; test "the encoder yields to the other fibers" (fun () ->
          let big = html ~size:65536 () in
          Sim_support.run (fun _ ->
              let ticks = ref 0 and finished = ref false in
              Weft.Fiber.scope (fun scope ->
                  Weft.Fiber.fork scope (fun () ->
                      while not !finished do
                        incr ticks ; Weft.Fiber.yield ()
                      done ) ;
                  ignore (bytes_of (run big)) ;
                  finished := true ) ;
              (* 64 KiB in blocks of 4 KiB: about 16 yields. *)
              is_true ~msg:(string_of_int !ticks) (!ticks >= 8) ;
              (* 20000 writes of one byte: a yield for each block, not for each
                 write. *)
              let small _ =
                Response.make
                  ~headers:(Headers.of_list [("Content-Type", "text/plain")])
                  (Body.of_writer ~length:20000 (fun w ->
                       for _ = 1 to 20000 do
                         Body.Writer.write_string w "a"
                       done ) )
              in
              let ticks = ref 0 and finished = ref false in
              let bytes = ref "" in
              Weft.Fiber.scope (fun scope ->
                  Weft.Fiber.fork scope (fun () ->
                      while not !finished do
                        incr ticks ; Weft.Fiber.yield ()
                      done ) ;
                  bytes := bytes_of (run small) ;
                  finished := true ) ;
              is_true ~msg:(string_of_int !ticks) (!ticks <= 10) ;
              equal string (String.make 20000 'a') (gunzip !bytes) ) )
    ; test "a body of whole blocks, and digest trailers" (fun () ->
          let h n _ =
            Response.make
              ~headers:(Headers.of_list [("Content-Type", "text/plain")])
              (Body.of_writer ~length:n (fun w ->
                   Body.Writer.write_string w (text n) ;
                   Body.Writer.set_trailers w
                     (Headers.of_list
                        [("Content-Digest", "sha-256=:x:"); ("X-A", "1")] ) ) )
          in
          List.iter
            (fun n ->
              let s, _, t = drain (run (h n)) in
              equal ~msg:(string_of_int n) string (text n) (gunzip s) ;
              equal (option string) None (Headers.find "content-digest" t) ;
              equal (option string) (Some "1") (Headers.find "x-a" t) )
            [4096; 8192; 4097] )
    ; test "deflate tags in If-None-Match" (fun () ->
          (* The handler gives back the field that it got. *)
          let echo req =
            Response.string
              ~headers:(Headers.of_list [("Content-Type", "image/png")])
              (Option.value ~default:"-"
                 (Headers.find "if-none-match" (Request.headers req)) )
          in
          let seen ae inm =
            let fields = [("If-None-Match", inm)] in
            let r = run ~request:(req ~ae:(Some ae) ~fields ()) echo in
            bytes_of r
          in
          equal string {|W/"a", "b-deflate", W/"c", *|}
            (seen "deflate" {|W/"a-deflate", "b-deflate", W/"c", *|}) ;
          (* Only for a client of deflate. *)
          equal string {|W/"a-deflate"|} (seen "gzip" {|W/"a-deflate"|}) ;
          equal string "-"
            (bytes_of (run ~request:(req ~ae:(Some "deflate") ()) echo)) )
    ; test "the default max_active is 64" (fun () ->
          let mw = C.middleware () in
          let held = List.init 64 (fun _ -> run ~mw (html ())) in
          List.iter (fun r -> is_some (compressed r)) held ;
          is_none (compressed (run ~mw (html ()))) ;
          List.iter (fun r -> ignore (bytes_of r)) held ;
          is_some (compressed (run ~mw (html ()))) )
    ; test "bad setups" (fun () ->
          List.iter
            (fun (label, f) ->
              raises_match ~msg:label
                (function Invalid_argument _ -> true | _ -> false)
                (fun () -> ignore (f () : Handler.middleware)) )
            [ ("min_size", fun () -> C.middleware ~min_size:(-1) ())
            ; ("max_size", fun () -> C.middleware ~min_size:10 ~max_size:9 ())
            ; ("max_active", fun () -> C.middleware ~max_active:0 ())
            ; ("level -1", fun () -> C.middleware ~level:(-1) ())
            ; ("level 10", fun () -> C.middleware ~level:10 ()) ] ;
          ignore
            ( C.middleware ~min_size:0 ~max_size:0 ~max_active:1 ~level:0 ()
              : Handler.middleware ) ;
          ignore (C.middleware ~level:9 () : Handler.middleware) ) ]

(* {1 On the wire, with static files} *)

let root = "/srv"

let write path data =
  (try Weft.Dir.mkdir root with Weft.Dir.Error (Exists, _) -> ()) ;
  let f = Weft.File.openfile [Write; Create; Truncate] path in
  Weft.File.write f data ; Weft.File.close f

let wire_tests =
  group "On the wire"
    [ test "a static file, then a 304 with the weak ETag" (fun () ->
          (* One encoder: each request checks that the one before freed it. *)
          let app =
            C.middleware ~max_active:1 ()
              (Caravan_static.middleware ~root () (fun _ ->
                   Response.string ~status:Status.not_found "" ) )
          in
          Caravan_testing.run app (fun t ->
              write (root ^ "/a.txt") (text 4000) ;
              let get fields =
                Caravan_testing.fetch t
                  (Request.make ~headers:(Headers.of_list fields) Method.GET
                     "/a.txt" )
              in
              let r = get [("Accept-Encoding", "gzip")] in
              equal int 200 (Status.code r.status) ;
              equal (option string) (Some "gzip")
                (Headers.find "content-encoding" r.headers) ;
              equal string (text 4000) (gunzip r.body) ;
              let etag = Option.get (Headers.find "etag" r.headers) in
              is_true ~msg:"weak" (String.starts_with ~prefix:"W/" etag) ;
              let r =
                get [("Accept-Encoding", "gzip"); ("If-None-Match", etag)]
              in
              equal int 304 (Status.code r.status) ;
              equal (option string) (Some etag) (Headers.find "etag" r.headers) ;
              equal (option string) (Some "Accept-Encoding")
                (Headers.find "vary" r.headers) ;
              (* The engine sends no Content-Type with a 304. *)
              equal (option string) None (Headers.find "content-type" r.headers) ;
              let r = get [] in
              equal string (text 4000) r.body ;
              (* A range: no coding, the strong ETag. *)
              let r =
                get [("Accept-Encoding", "gzip"); ("Range", "bytes=0-9")]
              in
              equal int 206 (Status.code r.status) ;
              equal (option string) None
                (Headers.find "content-encoding" r.headers) ;
              equal string (String.sub (text 4000) 0 10) r.body ;
              (* A small file: no coding, and its 304 keeps the strong ETag. *)
              write (root ^ "/small.txt") (text 500) ;
              let small fields =
                Caravan_testing.fetch t
                  (Request.make ~headers:(Headers.of_list fields) Method.GET
                     "/small.txt" )
              in
              let r = small [("Accept-Encoding", "gzip")] in
              equal (option string) None
                (Headers.find "content-encoding" r.headers) ;
              let etag = Option.get (Headers.find "etag" r.headers) in
              is_false ~msg:"strong" (String.starts_with ~prefix:"W/" etag) ;
              let r =
                small [("Accept-Encoding", "gzip"); ("If-None-Match", etag)]
              in
              equal int 304 (Status.code r.status) ;
              equal (option string) (Some etag) (Headers.find "etag" r.headers) ;
              equal (option string) None (Headers.find "vary" r.headers) ;
              (* HEAD: the fields of the GET, no body, and the slot is free. *)
              let r =
                Caravan_testing.fetch t
                  (Request.make
                     ~headers:(Headers.of_list [("Accept-Encoding", "gzip")])
                     Method.HEAD "/a.txt" )
              in
              equal (option string) (Some "gzip")
                (Headers.find "content-encoding" r.headers) ;
              equal string "" r.body ;
              (* deflate: its own weak ETag, and a 304 with it. *)
              let r = get [("Accept-Encoding", "deflate")] in
              equal (option string) (Some "deflate")
                (Headers.find "content-encoding" r.headers) ;
              equal string (text 4000) (inflate r.body) ;
              let etag = Option.get (Headers.find "etag" r.headers) in
              is_true ~msg:etag (String.ends_with ~suffix:"-deflate\"" etag) ;
              let r =
                get [("Accept-Encoding", "deflate"); ("If-None-Match", etag)]
              in
              equal int 304 (Status.code r.status) ;
              equal (option string) (Some etag) (Headers.find "etag" r.headers) ;
              (* The slot is free after all of these. *)
              let r = get [("Accept-Encoding", "gzip")] in
              equal (option string) (Some "gzip")
                (Headers.find "content-encoding" r.headers) ) ) ]

let () =
  Sim_support.run (fun _ -> ()) ;
  exit (Windtrap.run "compress" [negotiation_tests; compress_tests; wire_tests])
