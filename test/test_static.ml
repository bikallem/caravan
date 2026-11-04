(* The tests of caravan.static. Most tests call the handler with a request in
   the simulator, with no server: the file system of the simulator holds the
   files. The tests of the bytes on the wire use Caravan_testing. *)

open Windtrap
open Caravan
module S = Caravan_static
module T = Caravan_testing
module File = Weft.File

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let root = "/srv"

(* [mkdirs path] makes each directory of [path] that is not there. *)
let mkdirs path =
  let parts = List.filter (( <> ) "") (String.split_on_char '/' path) in
  ignore
    (List.fold_left
       (fun dir p ->
         let d = dir ^ "/" ^ p in
         (try Weft.Dir.mkdir d with Weft.Dir.Error (Exists, _) -> ()) ;
         d )
       "" parts )

let write path data =
  mkdirs (Filename.dirname path) ;
  let f = File.openfile [Write; Create; Truncate] path in
  File.write f data ; File.close f

(* [site files] writes each [(rel, data)] under [root]. *)
let site files =
  mkdirs root ;
  List.iter (fun (rel, data) -> write (root ^ "/" ^ rel) data) files

(* The POSIX time of the tests: 1700000000.5 s, a time with a fraction. *)
let t0 = Weft.Time.posix_of_unix_s 1_700_000_000.5

let imf s = Caravan.Private.Date.to_imf (Weft.Time.posix_of_unix_s s)

(* [sim f] runs [f] in the simulator at [t0]. *)
let sim f = Sim_support.run (fun s -> Weft_sim.set_posix s t0 ; f s)

let body_of r =
  match Response.kind r with
  | Response.Body b ->
      b
  | Response.Upgrade _ ->
      fail "an upgrade"

let body r = Body.to_string (body_of r)

let close r = Body.close (body_of r)

let field name r = Headers.find name (Response.headers r)

let req ?(meth = Method.GET) ?(headers = []) path =
  Request.make ~headers:(Headers.of_list headers) meth path

(* [get ?meth ?headers h path] calls [h], and gives the response and its
   body. *)
let get ?meth ?headers h path =
  let r = h (req ?meth ?headers path) in
  (r, body r)

let check_status ~msg code r =
  equal ~msg status (Status.v code) (Response.status r)

(* The marker of the next handler of the middleware. *)
let next _ = Response.string ~status:(Status.v 299) "next"

let mw ?prefix ?index ?dotfiles ?content_type ?cache_control () =
  S.middleware ?prefix ?index ?dotfiles ?content_type ?cache_control ~root ()
    next

(* [served h path text]: [h] gives 200 and [text] to a GET of [path]. *)
let served ?headers h path text =
  let r, b = get ?headers h path in
  check_status ~msg:path 200 r ;
  equal ~msg:path string text b

let passes h path =
  let r, b = get h path in
  check_status ~msg:path 299 r ;
  equal ~msg:path string "next" b

let fds s = Weft_sim.open_fds s

(* The handler of [dir] for the path of a request, as a route [/**] gives it. *)
let dir_of () req =
  let p = Request.path_only req in
  let segs = match String.split_on_char '/' p with "" :: l -> l | l -> l in
  S.dir ~root segs req

let content_type_tests =
  group "Media types"
    [ test "content_type knows the common extensions" (fun () ->
          List.iter
            (fun (name, t) -> equal ~msg:name string t (S.content_type name))
            [ ("a.html", "text/html; charset=utf-8")
            ; ("a.htm", "text/html; charset=utf-8")
            ; ("a.css", "text/css; charset=utf-8")
            ; ("a.js", "text/javascript; charset=utf-8")
            ; ("a.mjs", "text/javascript; charset=utf-8")
            ; ("a.json", "application/json; charset=utf-8")
            ; ("a.map", "application/json; charset=utf-8")
            ; ("a.txt", "text/plain; charset=utf-8")
            ; ("a.csv", "text/csv; charset=utf-8")
            ; ("a.xml", "application/xml; charset=utf-8")
            ; ("a.svg", "image/svg+xml; charset=utf-8")
            ; ("a.png", "image/png")
            ; ("a.jpg", "image/jpeg")
            ; ("a.jpeg", "image/jpeg")
            ; ("a.gif", "image/gif")
            ; ("a.webp", "image/webp")
            ; ("a.avif", "image/avif")
            ; ("a.ico", "image/x-icon")
            ; ("a.woff", "font/woff")
            ; ("a.woff2", "font/woff2")
            ; ("a.ttf", "font/ttf")
            ; ("a.otf", "font/otf")
            ; ("a.wasm", "application/wasm")
            ; ("a.pdf", "application/pdf")
            ; ("a.zip", "application/zip")
            ; ("a.tar.gz", "application/gzip")
            ; ("a.mp4", "video/mp4")
            ; ("a.webm", "video/webm")
            ; ("a.mp3", "audio/mpeg")
            ; ("a.ogg", "audio/ogg")
            ; ("a.wav", "audio/wav") ] )
    ; test "content_type has no case and takes the last segment" (fun () ->
          equal string "text/html; charset=utf-8" (S.content_type "A.HTML") ;
          equal string "image/png" (S.content_type "x/y.Png") ;
          equal string "application/octet-stream" (S.content_type "README") ;
          equal string "application/octet-stream" (S.content_type "a.d/b") ;
          equal string "application/octet-stream" (S.content_type "a.bin") ;
          equal string "application/octet-stream" (S.content_type "") ) ]

let file_tests =
  group "file"
    [ test "a GET gives the file, its type and its validators" (fun () ->
          sim (fun _ ->
              site [("a.txt", "hello")] ;
              let r, b = get (S.file (root ^ "/a.txt")) "/x" in
              check_status ~msg:"status" 200 r ;
              equal string "hello" b ;
              equal (option string) (Some "text/plain; charset=utf-8")
                (field "content-type" r) ;
              equal (option string) (Some "nosniff")
                (field "x-content-type-options" r) ;
              equal (option string)
                (Some (imf 1_700_000_000.))
                (field "last-modified" r) ;
              (* The size and the time in ns, in hex. *)
              equal (option string)
                (Some (Printf.sprintf "\"5-%Lx\"" 1_700_000_000_500_000_000L))
                (field "etag" r) ;
              equal (option string) None (field "cache-control" r) ;
              equal (option int) (Some 5) (Body.length (body_of r)) ) )
    ; test "content_type and cache_control set the fields" (fun () ->
          sim (fun _ ->
              site [("a.txt", "x")] ;
              let r, _ =
                get
                  (S.file ~content_type:"text/x" ~cache_control:"max-age=60"
                     (root ^ "/a.txt") )
                  "/"
              in
              equal (option string) (Some "text/x") (field "content-type" r) ;
              equal (option string) (Some "max-age=60")
                (field "cache-control" r) ) )
    ; test "a large file comes in order" (fun () ->
          sim (fun _ ->
              let data =
                String.init ((3 * 16384) + 5) (fun i -> Char.chr (i mod 251))
              in
              site [("big.bin", data)] ;
              let _, b = get (S.file (root ^ "/big.bin")) "/" in
              equal int (String.length data) (String.length b) ;
              is_true ~msg:"the bytes" (String.equal data b) ) )
    ; test "an empty file has a length of 0" (fun () ->
          sim (fun _ ->
              site [("e", "")] ;
              let r, b = get (S.file (root ^ "/e")) "/" in
              check_status ~msg:"status" 200 r ;
              equal string "" b ;
              equal (option int) (Some 0) (Body.length (body_of r)) ) )
    ; test "no file, a directory and a file as a directory give 404" (fun () ->
          sim (fun _ ->
              site [("a.txt", "x"); ("d/i", "y")] ;
              List.iter
                (fun p ->
                  let r, _ = get (S.file p) "/" in
                  check_status ~msg:p 404 r )
                [root ^ "/none"; root ^ "/d"; root ^ "/a.txt/b"] ) )
    ; test "another method gives 405 with Allow" (fun () ->
          sim (fun _ ->
              site [("a.txt", "x")] ;
              List.iter
                (fun h ->
                  let r, _ = get ~meth:Method.POST h "/a.txt" in
                  check_status ~msg:"405" 405 r ;
                  equal (option string) (Some "GET, HEAD") (field "allow" r) )
                [S.file (root ^ "/a.txt"); dir_of ()] ) )
    ; test "the fd closes at the end of the body and on a close" (fun () ->
          sim (fun s ->
              site [("a.txt", "hello")] ;
              let n = fds s in
              let r = S.file (root ^ "/a.txt") (req "/") in
              equal int (n + 1) (fds s) ;
              ignore (body r) ;
              equal int n (fds s) ;
              let r = S.file (root ^ "/a.txt") (req "/") in
              close r ;
              equal int n (fds s) ;
              (* A read of 1 byte, then a close. *)
              let r = S.file (root ^ "/a.txt") (req "/") in
              ignore (Body.read (body_of r) (Bytes.create 1) 0 1) ;
              close r ;
              equal int n (fds s) ) )
    ; test "the body stops at the size of the fstat" (fun () ->
          sim (fun _ ->
              site [("a.txt", "hello")] ;
              let r = S.file (root ^ "/a.txt") (req "/") in
              let f = File.openfile [Write; Append] (root ^ "/a.txt") in
              File.write f " world" ;
              File.close f ;
              equal string "hello" (body r) ) )
    ; test "the body ends early if the file gets shorter" (fun () ->
          sim (fun s ->
              site [("a.txt", "hello")] ;
              let n = fds s in
              let r = S.file (root ^ "/a.txt") (req "/") in
              let f = File.openfile [Write] (root ^ "/a.txt") in
              File.truncate f 2 ;
              File.close f ;
              equal string "he" (body r) ;
              equal int n (fds s) ) )
    ; test "an error of the open other than a missing file raises" (fun () ->
          (* EMFILE is not a missing file: the handler does not give 404. *)
          sim (fun s ->
              site [("a.txt", "hello")] ;
              Weft_sim.set_fd_limit s (Some (Weft_sim.open_fds s)) ;
              raises_match
                (function
                  | File.Error (No_resources "EMFILE", _) -> true | _ -> false )
                (fun () -> S.file (root ^ "/a.txt") (req "/")) ;
              Weft_sim.set_fd_limit s None ) )
    ; test "a cancel during the open or the fstat closes the fd" (fun () ->
          (* Each file operation takes up to 10 ms. A timeout of 1 to 30 ms
             cancels the open, the fstat, or neither. *)
          sim (fun s ->
              site [("a.txt", "hello")] ;
              Weft_sim.set_blocking_cost s (Weft.Span.ms 10) ;
              let n = fds s in
              for ms = 1 to 30 do
                ( match
                    Weft.Fiber.with_timeout Mono (Weft.Span.ms ms) (fun () ->
                        S.file (root ^ "/a.txt") (req "/") )
                  with
                | r ->
                    close r
                | exception Weft.Fiber.Timeout ->
                    () ) ;
                Weft.Time.sleep Mono (Weft.Span.ms 50) ;
                equal ~msg:(string_of_int ms) int n (fds s)
              done ) ) ]

(* [pre ?meth ?file ?cache_control headers] is the response of [file] for
   [root/file] (default [a.txt]), with [headers], and its body. *)
let pre ?meth ?(file = "a.txt") ?cache_control headers =
  get ?meth ~headers (S.file ?cache_control (root ^ "/" ^ file)) "/"

let code (r, _) = Status.code (Response.status r)

let etag () = Option.get (field "etag" (fst (pre [])))

let codes l = List.map (fun h -> code (pre h)) l

let precondition_tests =
  group "Preconditions"
    [ test "If-None-Match: a weak compare" (fun () ->
          sim (fun _ ->
              site [("a.txt", "hello")] ;
              let e = etag () in
              equal (list int)
                [304; 304; 304; 200; 304; 200; 304]
                (codes
                   [ [("If-None-Match", e)]
                   ; [("If-None-Match", "W/" ^ e)]
                   ; [("If-None-Match", "*")]
                   ; [("If-None-Match", "\"other\"")]
                   ; [("If-None-Match", "\"x\", " ^ e)]
                   ; [("If-None-Match", "bad")]
                   ; [("If-None-Match", "\"x\""); ("If-None-Match", e)] ] ) ) )
    ; test "a list with a syntax error matches nothing" (fun () ->
          sim (fun _ ->
              site [("a.txt", "hello")] ;
              let e = etag () in
              let inm v = [("If-None-Match", v)] in
              equal (list int)
                [200; 200; 200; 200; 200; 200; 200; 200; 304; 304]
                (codes
                   [ inm "\"abc"
                   ; inm ("\"a b\", " ^ e)
                   ; inm (e ^ ", W")
                   ; inm (e ^ ", W/")
                   ; inm ("X/" ^ e)
                   ; inm ("W" ^ e)
                   ; inm (e ^ "x")
                   ; inm (e ^ " \"x\"")
                   ; inm (e ^ " ,")
                   ; inm (", ," ^ e) ] ) ) )
    ; test "If-Match: a strong compare" (fun () ->
          sim (fun _ ->
              site [("a.txt", "hello")] ;
              let e = etag () in
              equal (list int)
                [200; 412; 200; 412; 200; 412]
                (codes
                   [ [("If-Match", e)]
                   ; [("If-Match", "W/" ^ e)]
                   ; [("If-Match", "*")]
                   ; [("If-Match", "\"other\"")]
                   ; [("If-Match", ",\t\"x\" , " ^ e ^ " ,")]
                   ; [("If-Match", e ^ " x")] ] ) ) )
    ; test "a 304 has the ETag, no Last-Modified and no body" (fun () ->
          sim (fun s ->
              site [("a.txt", "hello")] ;
              let e = etag () in
              let n = fds s in
              List.iter
                (fun meth ->
                  let ((r, b) as reply) = pre ~meth [("If-None-Match", e)] in
                  equal int 304 (code reply) ;
                  equal string "" b ;
                  equal (option string) (Some e) (field "etag" r) ;
                  equal (option string) None (field "last-modified" r) )
                [Method.GET; Method.HEAD] ;
              let r =
                S.file ~cache_control:"no-cache" (root ^ "/a.txt")
                  (req ~headers:[("If-None-Match", e)] "/")
              in
              equal (option string) (Some "no-cache") (field "cache-control" r) ;
              ignore (body r) ;
              equal int n (fds s) ) )
    ; test "If-Modified-Since compares whole seconds" (fun () ->
          sim (fun s ->
              site [("a.txt", "hello")] ;
              (* The file is from 1700000000.5. The time of the response is 100
                 s later. *)
              Weft_sim.set_posix s (Weft.Time.posix_of_unix_s 1_700_000_100.) ;
              let ims v = [("If-Modified-Since", v)] in
              equal (list int)
                [304; 200; 304; 200; 200; 200; 304]
                (codes
                   [ ims (imf 1_700_000_000.)
                   ; ims (imf 1_699_999_999.)
                   ; ims (imf 1_700_000_050.)
                   ; ims (imf 1_700_000_101.)
                   ; ims "not a date"
                   ; ims (imf 1_700_000_000.) @ ims (imf 1_700_000_000.)
                   ; ims (imf 1_700_000_100.) ] ) ) )
    ; test "If-Modified-Since takes the three formats" (fun () ->
          sim (fun s ->
              (* The file is from 1994-11-06 08:49:37 UTC. *)
              Weft_sim.set_posix s (Weft.Time.posix_of_unix_s 784111777.) ;
              site [("a.txt", "hello")] ;
              Weft_sim.set_posix s t0 ;
              let ims v = [("If-Modified-Since", v)] in
              equal (list int) [304; 304; 304; 200]
                (codes
                   [ ims "Sun, 06 Nov 1994 08:49:37 GMT"
                   ; ims "Sunday, 06-Nov-94 08:49:37 GMT"
                   ; ims "Sun Nov  6 08:49:37 1994"
                   ; ims "Sunday, 06-Nov-94 08:49:36 GMT" ] ) ) )
    ; test "an RFC 850 year is at most 50 years after now" (fun () ->
          sim (fun s ->
              (* The file is from 1970, and the year of t0 is 2023. "73" is
                 2073, 50 years after 2023: a date after now, thus ignored. "74"
                 is 1974. *)
              Weft_sim.set_posix s (Weft.Time.posix_of_unix_s 1000.) ;
              site [("a.txt", "hello")] ;
              Weft_sim.set_posix s t0 ;
              let ims v = [("If-Modified-Since", v)] in
              equal (list int) [200; 304]
                (codes
                   [ ims "Sunday, 01-Jan-73 00:00:00 GMT"
                   ; ims "Tuesday, 01-Jan-74 00:00:00 GMT" ] ) ;
              (* In 2060, "11" is 2011, and "05" is 2105, not more than 50 years
                 after: a date after now, thus ignored. *)
              Weft_sim.set_posix s (Weft.Time.posix_of_unix_s 2_853_000_000.) ;
              equal (list int) [304; 200]
                (codes
                   [ ims "Sunday, 01-Jan-11 00:00:00 GMT"
                   ; ims "Monday, 01-Jan-05 00:00:00 GMT" ] ) ;
              (* "10" is 2110, 50 years after 2060, not more: thus not 2010. *)
              equal (list int) [200]
                (codes [ims "Friday, 01-Jan-10 00:00:00 GMT"]) ;
              (* An If-Unmodified-Since of 2105 passes. 1905 gives 412. *)
              equal (list int) [200]
                (codes
                   [[("If-Unmodified-Since", "Monday, 01-Jan-05 00:00:00 GMT")]] ) ) )
    ; test "If-None-Match comes before If-Modified-Since" (fun () ->
          sim (fun _ ->
              site [("a.txt", "hello")] ;
              let e = etag () in
              equal (list int) [200; 304]
                (codes
                   [ [ ("If-None-Match", "\"other\"")
                     ; ("If-Modified-Since", imf 1_700_000_000.) ]
                   ; [ ("If-None-Match", e)
                     ; ("If-Modified-Since", imf 1_600_000_000.) ] ] ) ) )
    ; test "If-Unmodified-Since, and If-Match before it" (fun () ->
          sim (fun _ ->
              site [("a.txt", "hello")] ;
              let e = etag () in
              let ius v = ("If-Unmodified-Since", v) in
              equal (list int) [412; 200; 200; 200; 412]
                (codes
                   [ [ius (imf 1_699_999_999.)]
                   ; [ius (imf 1_700_000_000.)]
                   ; [ius "bad"]
                   ; [("If-Match", e); ius (imf 1_699_999_999.)]
                   ; [("If-Match", "\"x\""); ius (imf 1_700_000_000.)] ] ) ) )
    ; test "a date before 1970 rounds down to a whole second" (fun () ->
          sim (fun s ->
              (* The file is from -86400.5 s, thus its second is -86401 s. The
                 date of -86401 s is a whole second. *)
              Weft_sim.set_posix s (Weft.Time.posix_of_unix_s (-86_400.5)) ;
              site [("a.txt", "hello")] ;
              Weft_sim.set_posix s t0 ;
              let ius v = [("If-Unmodified-Since", v)] in
              equal (list int) [200; 412]
                (codes [ius (imf (-86_401.)); ius (imf (-86_402.))]) ) )
    ; test "a 412 comes before a 304" (fun () ->
          sim (fun _ ->
              site [("a.txt", "hello")] ;
              let e = etag () in
              equal (list int) [412]
                (codes [[("If-Match", "\"x\""); ("If-None-Match", e)]]) ) )
    ; test "a time after the response gives the time of the response" (fun () ->
          sim (fun s ->
              Weft_sim.set_posix s (Weft.Time.posix_of_unix_s 1_800_000_000.) ;
              site [("a.txt", "hello")] ;
              Weft_sim.set_posix s (Weft.Time.posix_of_unix_s 1_700_000_000.) ;
              let ((r, _) as reply) = pre [] in
              equal int 200 (code reply) ;
              equal (option string)
                (Some (imf 1_700_000_000.))
                (field "last-modified" r) ;
              equal (list int) [304]
                (codes [[("If-Modified-Since", imf 1_700_000_000.)]]) ) )
    ; (* The model: each element of the list is an entity tag or empty, with OWS
         around it. ["e"] is the tag of the file. *)
      (let e = Printf.sprintf "\"5-%Lx\"" 1_700_000_000_500_000_000L in
       let elements =
         Gen.of_list
           [ ("\"a\"", false)
           ; ("W/\"a\"", false)
           ; (e, true)
           ; ("W/" ^ e, true)
           ; ("", false)
           ; ("\"\"", false)
           ; ("\"!#~\x80\"", false) ]
       in
       let ows = Gen.of_list [""; " "; "\t"; " \t "] in
       let gen =
         Gen.list ~size:(Gen.int_range 0 4) (Gen.triple ows elements ows)
       in
       prop ~count:200 "the parse of entity tags agrees with a model" gen
         (fun l ->
           let v =
             (* A field value has no OWS at its ends. *)
             String.trim
               (String.concat ","
                  (List.map (fun (a, (t, _), b) -> a ^ t ^ b) l) )
           in
           let expected =
             if List.exists (fun (_, (_, m), _) -> m) l then 304 else 200
           in
           sim (fun _ ->
               site [("a.txt", "hello")] ;
               equal ~msg:v int expected (code (pre [("If-None-Match", v)])) ) )
      ) ]

let dir_tests =
  group "dir and middleware"
    [ test "the paths under the root" (fun () ->
          sim (fun _ ->
              site [("a.txt", "a"); ("css/b.css", "b"); ("x y.txt", "c")] ;
              let h = mw () in
              served h "/a.txt" "a" ;
              served h "/css/b.css" "b" ;
              served h "/x%20y.txt" "c" ;
              passes h "/none" ;
              passes h "/a.txt/" ;
              passes h "/a.txt/b" ;
              passes h "/a%zz" ;
              let r, _ = get (mw ()) "/css/b.css" in
              equal (option string) (Some "text/css; charset=utf-8")
                (field "content-type" r) ) )
    ; test "refused paths go to the next handler" (fun () ->
          (* Each path names a file that is there: the simulator walks "..", and
             drops "." and empty names. Thus only a rule refuses it. *)
          let cases =
            [ ("/../srv/a.txt", "a.txt")
            ; ("/%2e%2e/srv/a.txt", "a.txt")
            ; ("/.%2e/srv/a.txt", "a.txt")
            ; ("/./a.txt", "a.txt")
            ; ("/d/%2F/a.txt", "d/a.txt")
            ; ("/d%2fa.txt", "d/a.txt")
            ; ("/%2Fsrv%2Fa.txt", "srv/a.txt")
            ; ("//a.txt", "a.txt")
            ; ("/d//a.txt", "d/a.txt")
            ; ("//evil.com", "evil.com")
            ; ("/\\evil.com", "\\evil.com")
            ; ("/b%5Cc", "b\\c")
            ; ("/b:c", "b:c")
            ; ("/b%3Ac", "b:c")
            ; ("/%01a", "\x01a")
            ; ("/%1Fa", "\x1fa")
            ; ("/%7Fa", "\x7fa")
            ; ("/CON", "CON")
            ; ("/con", "con")
            ; ("/COM1.txt", "COM1.txt")
            ; ("/con.tar.gz", "con.tar.gz")
            ; ("/CON%20.txt", "CON .txt")
            ; ("/lpt%C2%B9", "lpt\xc2\xb9")
            ; ("/COM%C2%B2", "COM\xc2\xb2")
            ; ("/COM%C2%B3", "COM\xc2\xb3")
            ; ("/COM0", "COM0")
            ; ("/LPT9", "LPT9")
            ; ("/AUX", "AUX")
            ; ("/NUL", "NUL")
            ; ("/PRN", "PRN")
            ; ("/CONIN$", "CONIN$")
            ; ("/conout$", "conout$")
            ; ("/a.", "a.")
            ; ("/a%20", "a ")
            ; ("/GIT~1", "GIT~1")
            ; ("/~1", "~1")
            ; ("/%E2%80%8C.git", "\xe2\x80\x8c.git")
            ; ("/a%E2%80%8F", "a\xe2\x80\x8f")
            ; ("/a%E2%80%AA", "a\xe2\x80\xaa")
            ; ("/a%E2%80%AE", "a\xe2\x80\xae")
            ; ("/a%E2%81%AA", "a\xe2\x81\xaa")
            ; ("/a%E2%81%AF", "a\xe2\x81\xaf")
            ; ("/%EF%BB%BFa", "\xef\xbb\xbfa")
            ; ("/a~0", "a~0")
            ; ("/a~9", "a~9")
            ; ("/ab~1c", "ab~1c")
            ; ("/git~9/config", "git~9/config")
            ; ("/.git/config", ".git/config")
            ; ("/.env", ".env")
            ; ("/%FF", "\xff")
            ; ("/" ^ String.make 256 'n', String.make 256 'n') ]
          in
          sim (fun _ ->
              site (List.map (fun (_, f) -> (f, "secret")) cases) ;
              let h = mw () in
              List.iter (fun (p, _) -> passes h p) cases ;
              (* The rule of the dots is the only rule that dotfiles changes. *)
              let h = mw ~dotfiles:true () in
              List.iter
                (fun (p, _) ->
                  if p <> "/.git/config" && p <> "/.env" then passes h p )
                cases ;
              (* Bad % sequences. *)
              List.iter (passes h) ["/a.txt%4"; "/a.txt%"; "/%zza"; "/%a"] ;
              (* Names near the rules are served. *)
              site
                [ ("COM10", "c")
                ; ("COMX", "c")
                ; ("CONX", "c")
                ; ("COM", "c")
                ; ("~a", "t")
                ; ("a~b1", "t")
                ; ("a~", "t")
                ; ("..a", "d")
                ; ("\xc3\xa9", "u")
                ; ("\xe2\x80\x8b", "z")
                ; ("\xe2\x80\x90", "z")
                ; ("\xe2\x80\xa9", "z")
                ; ("\xe2\x80\xaf", "z")
                ; ("\xe2\x81\xa9", "z")
                ; ("\xe2\x81\xb0", "z")
                ; ("\xef\xbb\xbe", "z")
                ; ("\xe2\x82\xac", "z")
                ; ("\x7e~", "u")
                ; (String.make 255 'n', "l") ] ;
              served h "/COM10" "c" ;
              served h "/COMX" "c" ;
              served h "/CONX" "c" ;
              served h "/COM" "c" ;
              served h "/~a" "t" ;
              served h "/a~" "t" ;
              served h "/a~b1" "t" ;
              served h "/..a" "d" ;
              served h "/%C3%A9" "u" ;
              served h "/%c3%a9" "u" ;
              List.iter
                (fun p -> served h p "z")
                [ "/%E2%80%8B"
                ; "/%E2%80%90"
                ; "/%E2%80%A9"
                ; "/%E2%80%AF"
                ; "/%E2%81%A9"
                ; "/%E2%81%B0"
                ; "/%EF%BB%BE"
                ; "/%E2%82%AC" ] ;
              served h "/%7E%7e" "u" ;
              served h ("/" ^ String.make 255 'n') "l" ) )
    ; test "dotfiles:true serves a name with a dot" (fun () ->
          sim (fun _ ->
              site [(".well-known/x", "w")] ;
              passes (mw ()) "/.well-known/x" ;
              served (mw ~dotfiles:true ()) "/.well-known/x" "w" ;
              passes (mw ~dotfiles:true ()) "/../srv/.well-known/x" ) )
    ; test "dir gives 404 for a refused path" (fun () ->
          sim (fun _ ->
              site [("a.txt", "a"); (".env", "s")] ;
              List.iter
                (fun p ->
                  let r, _ = get (dir_of ()) p in
                  check_status ~msg:p 404 r )
                ["/.env"; "/none"; "/a.txt/"] ;
              served (dir_of ()) "/a.txt" "a" ;
              (* A segment with a '/' from a router. *)
              let r = S.dir ~root ["x/../a.txt"] (req "/") in
              check_status ~msg:"slash" 404 r ) )
    ; test "a directory with an index file" (fun () ->
          sim (fun _ ->
              site [("d/index.html", "i"); ("index.html", "top")] ;
              let h = mw () in
              served h "/d/" "i" ;
              served h "/" "top" ;
              let r, _ = get h "/d?q=1" in
              check_status ~msg:"301" 301 r ;
              equal (option string) (Some "d/?q=1") (field "location" r) ;
              let r, _ = get h "/d" in
              equal (option string) (Some "d/") (field "location" r) ;
              passes (mw ~index:None ()) "/d/" ;
              passes (mw ~index:None ()) "/d" ;
              site [("x y/index.html", "s")] ;
              let r, _ = get h "/x%20y" in
              equal (option string) (Some "x%20y/") (field "location" r) ;
              (* A path of a request with no '/': the whole path is the last
                 segment. *)
              let r = S.dir ~root ["d"] (req "d") in
              equal (option string) (Some "d/") (field "location" r) ;
              (* With no segment, the path of the request tells the '/'. A route
                 "/" with [dir ~root []] gets the index, not a 301 to itself. *)
              served (S.dir ~root []) "/" "top" ;
              let r, _ = get (S.dir ~root []) "/docs" in
              equal (option string) (Some "docs/") (field "location" r) ) )
    ; test "the fds of a directory close" (fun () ->
          sim (fun s ->
              site [("d/index.html", "i"); ("e/a", "a")] ;
              let n = fds s in
              let h = mw () in
              List.iter
                (fun p ->
                  ignore (get h p) ;
                  equal ~msg:p int n (fds s) )
                ["/d"; "/d/"; "/e"; "/e/"; "/"] ;
              let r, _ = get (S.file (root ^ "/d")) "/" in
              check_status ~msg:"file of a directory" 404 r ;
              equal int n (fds s) ) )
    ; test "a directory with no index file goes to the next handler" (fun () ->
          sim (fun _ ->
              site [("api/a.txt", "a"); ("d/index.html/x", "x")] ;
              let h = mw () in
              passes h "/api" ;
              passes h "/api/" ;
              (* An index that is a directory. *)
              passes h "/d/" ;
              passes h "/d" ) )
    ; test "an index of another name" (fun () ->
          sim (fun _ ->
              site [("d/main.txt", "m")] ;
              served (mw ~index:(Some "main.txt") ()) "/d/" "m" ) )
    ; test "the prefix" (fun () ->
          sim (fun _ ->
              site [("a.txt", "a"); ("index.html", "i")] ;
              let h = mw ~prefix:"/static" () in
              served h "/static/a.txt" "a" ;
              served h "/static/" "i" ;
              passes h "/staticx/a.txt" ;
              passes h "/staticxa.txt" ;
              passes h "/a.txt" ;
              passes h "/stat" ;
              let r, _ = get h "/static?v=2" in
              check_status ~msg:"301" 301 r ;
              equal (option string) (Some "static/?v=2") (field "location" r) ;
              passes (mw ()) "" ;
              List.iter
                (fun prefix ->
                  raises_match ~msg:prefix
                    (function Invalid_argument _ -> true | _ -> false)
                    (fun () ->
                      ignore (S.middleware ~prefix ~root () : Handler.middleware) ) )
                ["static"; "/static/"; ""] ) )
    ; test "the middleware gives other methods to the next handler" (fun () ->
          sim (fun _ ->
              site [("a.txt", "a")] ;
              let r, _ = get ~meth:Method.POST (mw ()) "/a.txt" in
              check_status ~msg:"post" 299 r ;
              let r, b = get ~meth:Method.HEAD (mw ()) "/a.txt" in
              check_status ~msg:"head" 200 r ;
              equal string "a" b ) )
    ; test "types and cache get the path under the root" (fun () ->
          sim (fun _ ->
              site [("d/index.html", "i"); ("css/a.css", "c")] ;
              let seen = ref [] in
              let cache rel =
                seen := rel :: !seen ;
                if rel = "d/index.html" then Some "no-cache" else None
              in
              let types rel = "t/" ^ rel in
              let h = mw ~cache_control:cache ~content_type:types () in
              let r, _ = get h "/d/" in
              equal (option string) (Some "no-cache") (field "cache-control" r) ;
              equal (option string) (Some "t/d/index.html")
                (field "content-type" r) ;
              let r, _ = get h "/css/a.css" in
              equal (option string) None (field "cache-control" r) ;
              equal (option string) (Some "t/css/a.css")
                (field "content-type" r) ;
              equal (list string) ["css/a.css"; "d/index.html"] !seen ) )
    ; test "a callback that raises closes the fd" (fun () ->
          sim (fun s ->
              site [("a.txt", "a")] ;
              let n = fds s in
              List.iter
                (fun h ->
                  raises_match
                    (function Failure _ -> true | _ -> false)
                    (fun () -> ignore (get h "/a.txt")) ;
                  equal int n (fds s) )
                [ mw ~content_type:(fun _ -> failwith "type") ()
                ; mw ~cache_control:(fun _ -> failwith "cache") () ] ;
              (* A value that Headers refuses also raises in the protect. *)
              raises_match
                (function Invalid_argument _ -> true | _ -> false)
                (fun () ->
                  ignore
                    (get (mw ~cache_control:(fun _ -> Some "a\nb") ()) "/a.txt") ) ;
              equal int n (fds s) ) )
    ; test "an empty root raises" (fun () ->
          let is_invalid = function Invalid_argument _ -> true | _ -> false in
          raises_match is_invalid (fun () ->
              ignore (S.middleware ~root:"" () : Handler.middleware) ) ;
          sim (fun _ ->
              raises_match is_invalid (fun () ->
                  ignore (S.dir ~root:"" ["a"] (req "/a")) ) ) )
    ; test "dir with a route of the router" (fun () ->
          sim (fun _ ->
              site [("a.txt", "a"); ("index.html", "i")] ;
              let h =
                Caravan_router.(
                  handler
                    [get "/static/**" (fun req -> S.dir ~root (rest req) req)] )
              in
              served h "/static/a.txt" "a" ;
              served h "/static/" "i" ;
              let r, _ = get h "/static" in
              equal (option string) (Some "static/") (field "location" r) ) )
    ; (* An invariant, not a copy of the rules: the secret files are next to the
         root and at the top. *)
      (let pieces =
         [ ".."
         ; "%2e%2e"
         ; ".%2e"
         ; "%2F"
         ; "\\"
         ; "%5C"
         ; ""
         ; "~1"
         ; "secret"
         ; "srv"
         ; "a"
         ; "."
         ; "srv%2Fsecret"
         ; "srvsecret" ]
       in
       let gen =
         Gen.map
           (fun l -> "/" ^ String.concat "/" l)
           (Gen.list ~size:(Gen.int_range 1 6) (Gen.of_list pieces))
       in
       prop ~count:300 "no path under the root serves a file out of it" gen
         (fun p ->
           sim (fun _ ->
               site [("a/x", "fine")] ;
               write "/secret" "SECRET" ;
               write "/srvsecret" "SECRET" ;
               write "/a/secret" "SECRET" ;
               let _, b = get (mw ~dotfiles:true ()) p in
               is_false ~msg:p (String.equal b "SECRET") ) ) ) ]

(* The file of the range tests: 10 bytes, at [t0]. *)
let digits = "0123456789"

let range_site () = site [("r.txt", digits)]

(* [r_pre] is [pre] for the file of the range tests. *)
let r_pre ?meth ?cache_control headers =
  pre ?meth ~file:"r.txt" ?cache_control headers

let r_etag () = Option.get (field "etag" (fst (r_pre [])))

(* [range_of ?headers v] is the status, the body and the Content-Range of
   [Range: v]. *)
let range_of ?(headers = []) v =
  let ((r, b) as reply) = r_pre (("Range", v) :: headers) in
  (code reply, b, field "content-range" r)

let range_result = Testable.(triple int string (option string))

let full = (200, digits, None)

let range_tests =
  group "Ranges"
    [ test "one range gives 206 with its bytes and fields" (fun () ->
          sim (fun _ ->
              range_site () ;
              let r =
                S.file ~cache_control:"max-age=1" (root ^ "/r.txt")
                  (req ~headers:[("Range", "bytes=0-3")] "/")
              in
              check_status ~msg:"206" 206 r ;
              equal (option int) (Some 4) (Body.length (body_of r)) ;
              equal string "0123" (body r) ;
              List.iter
                (fun (n, v) -> equal ~msg:n (option string) v (field n r))
                [ ("content-range", Some "bytes 0-3/10")
                ; ("accept-ranges", Some "bytes")
                ; ("content-type", Some "text/plain; charset=utf-8")
                ; ("cache-control", Some "max-age=1")
                ; ("x-content-type-options", Some "nosniff")
                ; ("last-modified", Some (imf 1_700_000_000.)) ] ;
              is_some ~msg:"etag" (field "etag" r) ) )
    ; test "the forms of a range and the clamp of the end" (fun () ->
          sim (fun _ ->
              range_site () ;
              equal (list range_result)
                [ (206, "789", Some "bytes 7-9/10")
                ; (206, "789", Some "bytes 7-9/10")
                ; (206, "56789", Some "bytes 5-9/10")
                ; (206, digits, Some "bytes 0-9/10")
                ; (206, "0", Some "bytes 0-0/10")
                ; (206, "9", Some "bytes 9-9/10")
                ; (206, digits, Some "bytes 0-9/10")
                ; (206, digits, Some "bytes 0-9/10")
                ; (206, "34", Some "bytes 3-4/10") ]
                (List.map
                   (fun v -> range_of v)
                   [ "bytes=7-"
                   ; "bytes=-3"
                   ; "bytes=5-100"
                   ; "bytes=-100"
                   ; "BYTES=0-0"
                   ; "Bytes=9-9"
                   ; "bytes=0-99999999999999999999"
                   ; "bytes=-99999999999999999999"
                   ; "bytes=0000000000000000000003-4" ] ) ) )
    ; test "a range that is not satisfiable gives 416" (fun () ->
          sim (fun _ ->
              range_site () ;
              List.iter
                (fun v ->
                  let ((r, b) as reply) =
                    r_pre ~cache_control:"immutable" [("Range", v)]
                  in
                  equal ~msg:v int 416 (code reply) ;
                  equal ~msg:v string "" b ;
                  equal ~msg:v (option string) (Some "bytes */10")
                    (field "content-range" r) ;
                  List.iter
                    (fun n ->
                      equal ~msg:(v ^ " " ^ n) (option string) None (field n r) )
                    [ "cache-control"
                    ; "etag"
                    ; "content-type"
                    ; "accept-ranges"
                    ; "last-modified" ] )
                [ "bytes=10-"
                ; "bytes=10-20"
                ; "bytes=-0"
                ; "bytes=99999999999999999999-"
                ; "bytes=10-11, 20-" ] ) )
    ; test "an incorrect Range field gives the full file" (fun () ->
          sim (fun _ ->
              range_site () ;
              List.iter
                (fun v -> equal ~msg:v range_result full (range_of v))
                [ "bytes=3-1"
                ; "bytes=a-b"
                ; "bytes=1-2-3"
                ; "items=0-1"
                ; "bytes 0-1"
                ; "bytes="
                ; "bytes=,"
                ; "bytes=-"
                ; "bytes=1"
                ; "bytes=--1"
                ; "bytes=+1-2"
                ; "bytes=1-+2"
                ; "bytesx=0-1"
                ; "byte=0-1"
                ; "=0-1"
                ; "bytes = 0-0"
                ; "bytes=0 - 0"
                ; "bytes=0-1,3-1"
                ; "bytes=0-1,x"
                ; "bytes=99999999999999999999-99999999999999999998"
                ; "bytes=10-9"
                ; (* More than 100 elements: 100 commas, and one satisfiable
                     range. *)
                  "bytes=0-0" ^ String.make 99 ',' ^ ",20-30"
                ; "bytes=0-0" ^ String.make 100 ',' ] ;
              (* 100 elements, with 99 commas. *)
              equal range_result
                (206, "0", Some "bytes 0-0/10")
                (range_of ("bytes=0-0" ^ String.make 99 ',')) ;
              (* The order of two large numbers. *)
              equal range_result
                (206, "9", Some "bytes 9-9/10")
                (range_of "bytes=09-99999999999999999999") ;
              equal range_result
                (416, "", Some "bytes */10")
                (range_of "bytes=99999999999999999998-99999999999999999999") ) )
    ; test "OWS and empty elements of the list" (fun () ->
          sim (fun _ ->
              range_site () ;
              List.iter
                (fun v ->
                  equal ~msg:v range_result
                    (206, "0", Some "bytes 0-0/10")
                    (range_of v) )
                ["bytes=0-0 ,"; "bytes=,\t0-0"; "bytes=0-0, ,"; "bytes=, 0-0"] ) )
    ; test "more than one satisfiable range gives the full file" (fun () ->
          sim (fun _ ->
              range_site () ;
              equal (list range_result)
                [full; (206, "01", Some "bytes 0-1/10"); full]
                (List.map
                   (fun v -> range_of v)
                   ["bytes=0-1,5-6"; "bytes=0-1, 20-30"; "bytes=0-1,0-1"] ) ) )
    ; test "two Range lines give the full file" (fun () ->
          sim (fun _ ->
              range_site () ;
              equal range_result full
                (range_of ~headers:[("Range", "bytes=0-1")] "bytes=0-1") ) )
    ; test "HEAD and an empty file ignore Range" (fun () ->
          sim (fun _ ->
              range_site () ;
              site [("e", "")] ;
              (* A direct call keeps the body of a HEAD: the server drops it. *)
              let ((r, _) as reply) =
                r_pre ~meth:Method.HEAD [("Range", "bytes=0-3")]
              in
              equal int 200 (code reply) ;
              equal (option int) (Some 10) (Body.length (body_of r)) ;
              equal (option string) None (field "content-range" r) ;
              equal (option string) (Some "bytes") (field "accept-ranges" r) ;
              List.iter
                (fun v ->
                  let r, b =
                    get ~headers:[("Range", v)] (S.file (root ^ "/e")) "/"
                  in
                  check_status ~msg:v 200 r ;
                  equal ~msg:v string "" b ;
                  equal ~msg:v (option string) None (field "content-range" r) )
                ["bytes=0-"; "bytes=-5"; "bytes=0-0"] ) )
    ; test "If-Range" (fun () ->
          sim (fun s ->
              range_site () ;
              let e = r_etag () in
              let lm = imf 1_700_000_000. in
              (* The response is 10 s after the change of the file. *)
              Weft_sim.set_posix s (Weft.Time.posix_of_unix_s 1_700_000_010.) ;
              let ir v = [("If-Range", v)] in
              let p = (206, "0123", Some "bytes 0-3/10") in
              List.iter
                (fun (msg, headers, expected) ->
                  equal ~msg range_result expected
                    (range_of ~headers "bytes=0-3") )
                [ ("the ETag", ir e, p)
                ; ("a weak ETag", ir ("W/" ^ e), full)
                ; ("another tag", ir "\"x\"", full)
                ; ("*", ir "*", full)
                ; ("a list", ir ("\"x\", " ^ e), full)
                ; ("a list after", ir (e ^ ","), full)
                ; ("the date", ir lm, p)
                ; ("another date", ir (imf 1_699_999_999.), full)
                ; ("a later date", ir (imf 1_700_000_005.), full)
                ; ("garbage", ir "garbage", full)
                ; ("two lines", ir e @ ir e, full) ] ;
              (* If-Range comes before the satisfiable check. *)
              equal range_result full
                (range_of ~headers:(ir "\"x\"") "bytes=10-") ;
              equal range_result
                (416, "", Some "bytes */10")
                (range_of ~headers:(ir e) "bytes=10-") ;
              (* The date is strong only if it is before the second of the
                 response. *)
              Weft_sim.set_posix s (Weft.Time.posix_of_unix_s 1_700_000_000.9) ;
              equal range_result full (range_of ~headers:(ir lm) "bytes=0-3") ) )
    ; test "the preconditions come before the range" (fun () ->
          sim (fun _ ->
              range_site () ;
              let e = r_etag () in
              let status_of h = code (r_pre (("Range", "bytes=0-3") :: h)) in
              equal (list int) [304; 304; 412; 412]
                [ status_of [("If-None-Match", e)]
                ; status_of [("If-Modified-Since", imf 1_700_000_000.)]
                ; status_of [("If-Match", "\"x\"")]
                ; status_of [("If-Unmodified-Since", imf 1_699_999_999.)] ] ;
              let r, _ = r_pre [("Range", "bytes=0-3"); ("If-None-Match", e)] in
              equal (option string) None (field "accept-ranges" r) ;
              equal (option string) None (field "content-range" r) ;
              let r, _ = r_pre [("If-Match", "\"x\"")] in
              equal (option string) None (field "accept-ranges" r) ) )
    ; test "a range of a large file" (fun () ->
          sim (fun _ ->
              let data =
                String.init ((3 * 16384) + 5) (fun i -> Char.chr (i mod 251))
              in
              site [("big", data)] ;
              let r, b =
                get
                  ~headers:[("Range", "bytes=16380-32780")]
                  (S.file (root ^ "/big"))
                  "/"
              in
              check_status ~msg:"206" 206 r ;
              is_true ~msg:"the bytes"
                (String.equal (String.sub data 16380 16401) b) ) )
    ; test "the fds of a 206 and a 416 close" (fun () ->
          sim (fun s ->
              range_site () ;
              let n = fds s in
              ignore (r_pre [("Range", "bytes=0-3")]) ;
              equal int n (fds s) ;
              ignore (r_pre [("Range", "bytes=10-")]) ;
              equal int n (fds s) ;
              close
                (S.file (root ^ "/r.txt")
                   (req ~headers:[("Range", "bytes=1-2")] "/") ) ;
              equal int n (fds s) ) )
    ; (let gen =
         Gen.triple (Gen.int_range 0 40)
           (Gen.of_list [`From; `From_to; `Suffix])
           (Gen.pair (Gen.int_range 0 45) (Gen.int_range 0 45))
       in
       prop ~count:300 "a range agrees with a model" gen
         (fun (size, form, (a, b)) ->
           let data = String.init size (fun i -> Char.chr (65 + (i mod 26))) in
           let v, model =
             match form with
             | `From ->
                 ( Printf.sprintf "bytes=%d-" a
                 , if a < size then `Part (a, size - 1) else `None )
             | `From_to ->
                 ( Printf.sprintf "bytes=%d-%d" a b
                 , if b < a then `Full
                   else if a < size then `Part (a, min b (size - 1))
                   else `None )
             | `Suffix ->
                 ( Printf.sprintf "bytes=-%d" a
                 , if a > 0 then `Part (max 0 (size - a), size - 1) else `None
                 )
           in
           (* An empty file ignores Range. *)
           let model = if size = 0 then `Full else model in
           sim (fun _ ->
               site [("p", data)] ;
               let r, body =
                 get ~headers:[("Range", v)] (S.file (root ^ "/p")) "/"
               in
               let got =
                 (Status.code (Response.status r), body, field "content-range" r)
               in
               let expected =
                 match model with
                 | `Full ->
                     (200, data, None)
                 | `None ->
                     (416, "", Some (Printf.sprintf "bytes */%d" size))
                 | `Part (f, l) ->
                     ( 206
                     , String.sub data f (l - f + 1)
                     , Some (Printf.sprintf "bytes %d-%d/%d" f l size) )
               in
               equal ~msg:v range_result expected got ) ) ) ]

let wire_tests =
  group "On the wire"
    [ test "a GET and a HEAD through the server, and the fds" (fun () ->
          let app = S.middleware ~root () Handler.not_found in
          T.run app (fun t ->
              site [("a.txt", "hello")] ;
              let r = T.get t "/a.txt" in
              equal status Status.ok r.status ;
              equal string "hello" r.body ;
              T.settle t ;
              let n = fds (T.sim t) in
              for _ = 1 to 20 do
                ignore (T.get t "/a.txt")
              done ;
              let r = T.fetch t (Request.make Method.HEAD "/a.txt") in
              equal string "" r.body ;
              equal (option string) (Some "5")
                (Headers.find "content-length" r.headers) ;
              (* A body that the client closes before the end. *)
              T.with_response t (Request.make Method.GET "/a.txt") (fun _ -> ()) ;
              (* The close of the body closes the connection. One more request
                 opens a connection again, as at the first count. *)
              ignore (T.get t "/a.txt") ;
              T.settle t ;
              equal int n (fds (T.sim t)) ) )
    ; test "a 304 and a 412 through the server" (fun () ->
          let app = S.middleware ~root () Handler.not_found in
          T.run app (fun t ->
              site [("a.txt", "hello")] ;
              let e =
                Option.get (Headers.find "etag" (T.get t "/a.txt").headers)
              in
              T.settle t ;
              let n = fds (T.sim t) in
              let r =
                T.get
                  ~headers:(Headers.of_list [("If-None-Match", e)])
                  t "/a.txt"
              in
              equal status Status.not_modified r.status ;
              equal (option string) None (Headers.find "content-type" r.headers) ;
              let r =
                T.get
                  ~headers:(Headers.of_list [("If-Match", "\"x\"")])
                  t "/a.txt"
              in
              equal status Status.precondition_failed r.status ;
              T.settle t ;
              equal int n (fds (T.sim t)) ) )
    ; test "a 206 and a 416 through the server, then a request" (fun () ->
          let app = S.middleware ~root () Handler.not_found in
          T.run app (fun t ->
              site [("r.txt", digits)] ;
              let get v =
                T.get ~headers:(Headers.of_list [("Range", v)]) t "/r.txt"
              in
              let r = get "bytes=2-4" in
              equal status Status.partial_content r.status ;
              equal string "234" r.body ;
              equal (option string) (Some "3")
                (Headers.find "content-length" r.headers) ;
              equal (option string) (Some "bytes 2-4/10")
                (Headers.find "content-range" r.headers) ;
              let r = get "bytes=10-" in
              equal status Status.range_not_satisfiable r.status ;
              equal string "" r.body ;
              equal (option string) (Some "bytes */10")
                (Headers.find "content-range" r.headers) ;
              (* The framing of the 416 lets the next request work. *)
              let r = get "bytes=-1" in
              equal string "9" r.body ;
              equal (list string) []
                (List.map
                   (fun (e, _) -> Printexc.to_string e)
                   (T.take_errors t) ) ) )
    ; test "a file that gets shorter ends the connection" (fun () ->
          let data = String.make 100_000 'x' in
          let app req =
            let r = S.file (root ^ "/big") req in
            let f = File.openfile [Write] (root ^ "/big") in
            File.truncate f 10 ; File.close f ; r
          in
          T.run app (fun t ->
              site [("big", data)] ;
              T.settle t ;
              let n = fds (T.sim t) in
              ( match T.get t "/" with
              | _ ->
                  fail "a short body"
              | exception Caravan_client.Error _ ->
                  () ) ;
              T.settle t ;
              equal (list string)
                [Printexc.to_string Caravan.Private.H1_write.Length_mismatch]
                (List.map
                   (fun (e, _) -> Printexc.to_string e)
                   (T.take_errors t) ) ;
              equal int n (fds (T.sim t)) ) ) ]

let () =
  exit
    (Windtrap.run "static"
       [ content_type_tests
       ; file_tests
       ; precondition_tests
       ; dir_tests
       ; range_tests
       ; wire_tests ] )
