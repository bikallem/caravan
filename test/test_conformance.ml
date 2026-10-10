(* Conformance rules from the cispa/http-conformance suite (commit 24f75be;
   Copyright (c) 2023 Jannis Rautenstrauch, MIT license: see
   LICENSE.http-conformance). The tests send the inputs of the upstream rules to
   the HTTP/1.1 engine on in-memory connections, and check the bytes that it
   writes against RFC 9110 and RFC 9112. The plan is
   caravan-http-conformance.md. *)

open Windtrap
open Caravan

(* The fixed time of the Date fields: Sun, 06 Nov 1994 08:49:37 GMT. *)
let config =
  { Config.default with
    date_header= true
  ; posix_time= Fake_time.posix (Fake_time.make ())
  ; on_error= (fun _ _ -> ()) }

let date = "Sun, 06 Nov 1994 08:49:37 GMT"

let serve ?(fixtures = true) inputs =
  Memory_server.serve ~config (Conformance_app.handler ~fixtures) inputs

let serve_one ?fixtures input = List.hd (serve ?fixtures [input])

let request ?(v = "HTTP/1.1") ?(headers = []) meth path =
  Printf.sprintf "%s %s %s\r\nHost: localhost:8080\r\n%s\r\n" meth path v
    (String.concat "" (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") headers))

let framing = function
  | Wire.No_body ->
      "no body"
  | Length n ->
      Printf.sprintf "length %d" n
  | Chunked ->
      "chunked"
  | Close_delimited ->
      "close-delimited"
  | Tunnel ->
      "tunnel"

(* The responses of [out] in one line each, then the rest and the end. The Date
   field shows as "date" if it has the fixed time. *)
let describe ~meth (out : Memory_server.output) =
  match Wire.parse ~meth out with
  | Error e ->
      "error: " ^ e
  | Ok t ->
      let field (n, v) =
        if String.lowercase_ascii n = "date" && v = date then n
        else Printf.sprintf "%s: %s" n v
      in
      let response (r : Wire.response) =
        Printf.sprintf "%s | %s | %s %s" r.status_line
          (String.concat ", " (List.map field r.fields))
          (framing r.framing)
          ( if r.body = Conformance_app.page then "<page>"
            else Printf.sprintf "%S" r.body )
      in
      String.concat "\n" (List.map response t.responses)
      ^ Printf.sprintf "\nrest %S%s%s" t.rest
          (if t.fin then " [FIN]" else "")
          (if t.reset then " [RST]" else "")

let output ?(fin = false) ?(reset = false) written =
  {Memory_server.written; fin; reset}

let wire_tests =
  group "Wire"
    [ test "a Content-Length body, then the rest" (fun () ->
          expect
            (describe ~meth:"GET"
               (output ~fin:true
                  "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nX: a\r\n\r\nhiextra" ) )
          @@ __POS_OF__
               {|
          HTTP/1.1 200 OK | Content-Length: 2, X: a | length 2 "hi"
          rest "extra" [FIN]
          |} )
    ; test "a chunked body with trailers" (fun () ->
          expect
            (describe ~meth:"GET"
               (output
                  "HTTP/1.1 200 OK\r\n\
                   Transfer-Encoding: chunked\r\n\
                   \r\n\
                   2;x=y\r\n\
                   he\r\n\
                   3\r\n\
                   llo\r\n\
                   0\r\n\
                   T: 1\r\n\
                   \r\n" ) )
          @@ __POS_OF__
               {|
          HTTP/1.1 200 OK | Transfer-Encoding: chunked | chunked "hello"
          rest ""
          |} )
    ; test "no body for HEAD, 204 and 304" (fun () ->
          let one meth status =
            describe ~meth
              (output
                 (Printf.sprintf "HTTP/1.1 %s\r\nContent-Length: 3\r\n\r\nabc"
                    status ) )
          in
          expect
            (String.concat "\n"
               [ one "HEAD" "200 OK"
               ; one "GET" "204 No Content"
               ; one "GET" "304 Not Modified" ] )
          @@ __POS_OF__
               {|
          HTTP/1.1 200 OK | Content-Length: 3 | no body ""
          rest "abc"
          HTTP/1.1 204 No Content | Content-Length: 3 | no body ""
          rest "abc"
          HTTP/1.1 304 Not Modified | Content-Length: 3 | no body ""
          rest "abc"
          |} )
    ; test "an interim 100, then the final response" (fun () ->
          expect
            (describe ~meth:"POST"
               (output
                  "HTTP/1.1 100 Continue\r\n\
                   \r\n\
                   HTTP/1.1 200 OK\r\n\
                   Content-Length: 0\r\n\
                   \r\n" ) )
          @@ __POS_OF__
               {|
          HTTP/1.1 100 Continue |  | no body ""
          HTTP/1.1 200 OK | Content-Length: 0 | length 0 ""
          rest ""
          |} )
    ; test "a 101 and a 2xx to CONNECT start a tunnel" (fun () ->
          expect
            (String.concat "\n"
               [ describe ~meth:"GET"
                   (output
                      "HTTP/1.1 101 Switching Protocols\r\n\
                       Upgrade: x\r\n\
                       \r\n\
                       bytes" )
               ; describe ~meth:"CONNECT"
                   (output "HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\nabc") ] )
          @@ __POS_OF__
               {|
          HTTP/1.1 101 Switching Protocols | Upgrade: x | tunnel ""
          rest "bytes"
          HTTP/1.1 200 OK | Content-Length: 3 | tunnel ""
          rest "abc"
          |} )
    ; test "a body with no length ends with the connection" (fun () ->
          expect
            (String.concat "\n"
               [ describe ~meth:"GET"
                   (output ~fin:true "HTTP/1.1 200 OK\r\n\r\nall of it")
               ; describe ~meth:"GET"
                   (output
                      "HTTP/1.0 200 OK\r\nTransfer-Encoding: gzip\r\n\r\nzz" )
               ] )
          @@ __POS_OF__
               {|
          HTTP/1.1 200 OK |  | close-delimited "all of it"
          rest "" [FIN]
          HTTP/1.0 200 OK | Transfer-Encoding: gzip | close-delimited "zz"
          rest ""
          |} )
    ; test "field lines keep their order, case and duplicates" (fun () ->
          let t =
            Result.get_ok
              (Wire.parse ~meth:"GET"
                 (output
                    "HTTP/1.1 200 OK\r\n\
                     Date: a\r\n\
                     x:  1 \t\r\n\
                     DATE: b\r\n\
                     Content-Length: 0\r\n\
                     \r\n" ) )
          in
          let r = Wire.final t in
          equal
            (list (pair string string))
            [("Date", "a"); ("x", "1"); ("DATE", "b"); ("Content-Length", "0")]
            r.fields ;
          equal (list string) ["a"; "b"] (Wire.find_all "date" r) ;
          equal (option string) (Some "1") (Wire.find "X" r) )
    ; test "a bare CR or LF stays in its line" (fun () ->
          let raw =
            "HTTP/1.1 200 OK\r\nA: 1\rB: 2\nC\r\nContent-Length: 0\r\n\r\n"
          in
          let r =
            Wire.final (Result.get_ok (Wire.parse ~meth:"GET" (output raw)))
          in
          equal string raw r.head ;
          equal
            (list (pair string string))
            [("A", "1\rB: 2\nC"); ("Content-Length", "0")]
            r.fields )
    ; test "malformed responses are errors" (fun () ->
          let one s = describe ~meth:"GET" (output s) in
          expect
            (String.concat "\n"
               [ one "HTTP/1.1 200 OK\r\nContent-Length: 1\r\n"
               ; one "HTTP/1.1 200\r\n\r\n"
               ; one "HTTP/1.1 2000 OK\r\n\r\n"
               ; one "HTTP/1.1 200 OK\r\nNo colon\r\n\r\n"
               ; one "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nabc"
               ; one
                   "HTTP/1.1 200 OK\r\n\
                    Content-Length: 1\r\n\
                    Content-Length: 2\r\n\
                    \r\n\
                    ab"
               ; one "HTTP/1.1 200 OK\r\nContent-Length: +1\r\n\r\na"
               ; one
                   "HTTP/1.1 200 OK\r\n\
                    Transfer-Encoding: chunked\r\n\
                    \r\n\
                    5\r\n\
                    ab"
               ; one
                   "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nz\r\n"
               ] )
          @@ __POS_OF__
               {|
          error: no complete head
          error: malformed status line "HTTP/1.1 200"
          error: malformed status line "HTTP/1.1 2000 OK"
          error: field line without a colon "No colon"
          error: short body
          error: bad Content-Length values
          error: bad Content-Length values
          error: chunked body: short chunk
          error: chunked body: bad size "z"
          |} )
    ]

let memory_server_tests =
  group "Memory_server"
    [ test "serves a GET with the fixed Date" (fun () ->
          let out = serve_one (request "GET" "/") in
          equal string
            ( "HTTP/1.1 200 OK\r\nDate: " ^ date
            ^ "\r\nContent-Type: text/html\r\nContent-Length: 153\r\n\r\n"
            ^ Conformance_app.page )
            out.written ;
          is_true ~msg:"no FIN on a kept connection" (not out.fin) )
    ; test "an engine error closes the connection" (fun () ->
          expect (describe ~meth:"GET" (serve_one "GET / HTTP/1.1\r\n\r\n"))
          @@ __POS_OF__
               {|
            HTTP/1.1 400 Bad Request | Date, Content-Type: text/plain; charset=utf-8, Content-Length: 12, Connection: close | length 12 "Bad Request\n"
            rest "" [FIN]
            |} )
    ; test "each input is its own connection" (fun () ->
          let outs =
            serve [request "GET" "/missing"; request "HEAD" "/missing"]
          in
          equal (list string)
            ["HTTP/1.1 404 Not Found"; "HTTP/1.1 404 Not Found"]
            (List.map2
               (fun meth out ->
                 (Wire.final (Result.get_ok (Wire.parse ~meth out))).status_line )
               ["GET"; "HEAD"] outs ) ) ]

(* One line for each request: what the application gives. *)
let app ?fixtures requests =
  let outs = serve ?fixtures (List.map snd requests) in
  String.concat "\n"
    (List.map2
       (fun (meth, input) out ->
         let line = List.hd (String.split_on_char '\r' input) in
         line ^ " => " ^ describe ~meth out )
       requests outs )

let app_tests =
  group "Conformance_app"
    [ test "the methods" (fun () ->
          let r meth = (meth, request meth "/") in
          expect
            (app
               [ r "ABC"
               ; ("CONNECT", request "CONNECT" "localhost:8080")
               ; r "TRACE"
               ; r "POST"
               ; r "PUT"
               ; r "PATCH"
               ; r "DELETE"
               ; r "OPTIONS"
               ; r "HEAD" ] )
          @@ __POS_OF__
               {|
            ABC / HTTP/1.1 => HTTP/1.1 501 Not Implemented | Date, Content-Type: text/plain; charset=utf-8, Content-Length: 15 | length 15 "not implemented"
            rest ""
            CONNECT localhost:8080 HTTP/1.1 => HTTP/1.1 200 OK | Date | tunnel ""
            rest ""
            TRACE / HTTP/1.1 => HTTP/1.1 405 Method Not Allowed | Date, Allow: GET, HEAD, OPTIONS, Content-Type: text/plain; charset=utf-8, Content-Length: 18 | length 18 "method not allowed"
            rest ""
            POST / HTTP/1.1 => HTTP/1.1 405 Method Not Allowed | Date, Allow: GET, HEAD, OPTIONS, Content-Type: text/plain; charset=utf-8, Content-Length: 18 | length 18 "method not allowed"
            rest ""
            PUT / HTTP/1.1 => HTTP/1.1 405 Method Not Allowed | Date, Allow: GET, HEAD, OPTIONS, Content-Type: text/plain; charset=utf-8, Content-Length: 18 | length 18 "method not allowed"
            rest ""
            PATCH / HTTP/1.1 => HTTP/1.1 405 Method Not Allowed | Date, Allow: GET, HEAD, OPTIONS, Content-Type: text/plain; charset=utf-8, Content-Length: 18 | length 18 "method not allowed"
            rest ""
            DELETE / HTTP/1.1 => HTTP/1.1 405 Method Not Allowed | Date, Allow: GET, HEAD, OPTIONS, Content-Type: text/plain; charset=utf-8, Content-Length: 18 | length 18 "method not allowed"
            rest ""
            OPTIONS / HTTP/1.1 => HTTP/1.1 204 No Content | Date, Allow: GET, HEAD, OPTIONS | no body ""
            rest ""
            HEAD / HTTP/1.1 => HTTP/1.1 200 OK | Date, Content-Type: text/html, Content-Length: 153 | no body ""
            rest ""
            |} )
    ; test "the preconditions of /" (fun () ->
          let r ?(meth = "GET") name value =
            (meth, request ~headers:[(name, value)] meth "/")
          in
          expect
            (app
               [ r "If-None-Match" "*"
               ; r ~meth:"HEAD" "If-None-Match" "*"
               ; r ~meth:"OPTIONS" "If-None-Match" "*"
               ; r "If-None-Match" "\"abc\""
               ; r "If-Match" "*"
               ; r "If-Match" "\"abc\""
               ; r "If-Modified-Since" date ] )
          @@ __POS_OF__
               {|
            GET / HTTP/1.1 => HTTP/1.1 304 Not Modified | Date | no body ""
            rest ""
            HEAD / HTTP/1.1 => HTTP/1.1 304 Not Modified | Date | no body ""
            rest ""
            OPTIONS / HTTP/1.1 => HTTP/1.1 412 Precondition Failed | Date, Content-Length: 0 | length 0 ""
            rest ""
            GET / HTTP/1.1 => HTTP/1.1 200 OK | Date, Content-Type: text/html, Content-Length: 153 | length 153 <page>
            rest ""
            GET / HTTP/1.1 => HTTP/1.1 200 OK | Date, Content-Type: text/html, Content-Length: 153 | length 153 <page>
            rest ""
            GET / HTTP/1.1 => HTTP/1.1 412 Precondition Failed | Date, Content-Length: 0 | length 0 ""
            rest ""
            GET / HTTP/1.1 => HTTP/1.1 200 OK | Date, Content-Type: text/html, Content-Length: 153 | length 153 <page>
            rest ""
            |} )
    ; test "the fixtures" (fun () ->
          let r ?(headers = []) path = ("GET", request ~headers "GET" path) in
          expect
            (app
               [ r "/fixture/no-content"
               ; r "/fixture/not-modified"
               ; r "/fixture/reset-content"
               ; r "/fixture/stream"
               ; r "/fixture/upgrade-required"
               ; r
                   ~headers:
                     [("Connection", "upgrade"); ("Upgrade", "example/1, b/2")]
                   "/fixture/upgrade"
               ; r "/fixture/upgrade"
               ; r "/fixture/fail"
               ; r "/missing" ] )
          @@ __POS_OF__
               {|
            GET /fixture/no-content HTTP/1.1 => HTTP/1.1 204 No Content | Date | no body ""
            rest ""
            GET /fixture/not-modified HTTP/1.1 => HTTP/1.1 304 Not Modified | Date | no body ""
            rest ""
            GET /fixture/reset-content HTTP/1.1 => HTTP/1.1 205 Reset Content | Date, Content-Type: text/plain; charset=utf-8, Content-Length: 5 | length 5 "hello"
            rest ""
            GET /fixture/stream HTTP/1.1 => HTTP/1.1 200 OK | Date, Content-Type: text/plain, Transfer-Encoding: chunked | chunked "hello"
            rest ""
            GET /fixture/upgrade-required HTTP/1.1 => HTTP/1.1 426 Upgrade Required | Date, Content-Type: text/plain; charset=utf-8, Content-Length: 16 | length 16 "upgrade required"
            rest ""
            GET /fixture/upgrade HTTP/1.1 => HTTP/1.1 101 Switching Protocols | Date, Upgrade: example/1, Connection: Upgrade | tunnel ""
            rest ""
            GET /fixture/upgrade HTTP/1.1 => HTTP/1.1 200 OK | Date, Content-Type: text/plain; charset=utf-8, Content-Length: 10 | length 10 "no upgrade"
            rest ""
            GET /fixture/fail HTTP/1.1 => HTTP/1.1 500 Internal Server Error | Date, Content-Type: text/plain; charset=utf-8, Content-Length: 22, Connection: close | length 22 "Internal Server Error\n"
            rest "" [FIN]
            GET /missing HTTP/1.1 => HTTP/1.1 404 Not Found | Date, Content-Type: text/plain; charset=utf-8, Content-Length: 9 | length 9 "not found"
            rest ""
            |} )
    ; test "without fixtures, the fixture paths are not found" (fun () ->
          equal string "HTTP/1.1 404 Not Found"
            (Wire.final
               (Result.get_ok
                  (Wire.parse ~meth:"GET"
                     (serve_one ~fixtures:false (request "GET" "/fixture/fail")) ) ) )
              .status_line ) ]

(* The DIRECT rules (plan 5.1). Upstream sends each to the base URL and to a
   random path. *)

let random_path = "/q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6"

let paths = ["/"; random_path]

let host = "Host: localhost:8080\r\n"

(* The request of upstream [build_request]: the request line, then Host, Accept,
   Accept-Encoding, Cache-Control, User-Agent, Connection, the additional
   fields, and the empty line. *)
let direct ?(meth = "GET") ?(host = host)
    ?(connection = "Connection: keep-alive\r\n") ?(additional = "") path =
  Printf.sprintf
    "%s %s HTTP/1.1\r\n\
     %sAccept: */*\r\n\
     Accept-Encoding: gzip, deflate, br\r\n\
     Cache-Control: no-store\r\n\
     User-Agent: test\r\n\
     %s%s\r\n"
    meth path host connection additional

let parsed ~meth out =
  match Wire.parse ~meth out with
  | Ok t ->
      t
  | Error e ->
      failf "the response is malformed: %s in %S" e out.Memory_server.written

(* The status codes of the responses, and the extra bytes if any. *)
let codes ~meth out =
  let t = parsed ~meth out in
  String.concat " "
    (List.map (fun (r : Wire.response) -> string_of_int r.code) t.responses)
  ^ if t.rest = "" then "" else Printf.sprintf " + %S" t.rest

(* One line for each labeled input: its path, label and codes. *)
let outcomes ?(meth = "GET") inputs =
  let outs = serve (List.map (fun (_, _, input) -> input) inputs) in
  String.concat "\n"
    (List.map2
       (fun (path, label, _) out ->
         Printf.sprintf "%s %s => %s" path label (codes ~meth out) )
       inputs outs )

let on_paths f = List.concat_map f paths

let direct_tests =
  group "DIRECT rules"
    [ test
        "reject_fields_contaning_cr_lf_nul: 400 for NUL, CR or LF in a value \
         (RFC 9110 5.5)" (fun () ->
          (* RFC 9112 2.2 permits a recipient to take a bare LF as a line end,
             and then this head ends early. Caravan refuses it, and the test
             asserts this stricter 400. *)
          expect
            (outcomes
               (on_paths (fun path ->
                    List.map
                      (fun (label, x) ->
                        ( path
                        , label
                        , direct ~additional:("Invalid: a" ^ x ^ "\r\n") path ) )
                      [("NUL", "\000"); ("CR", "\r"); ("LF", "\n")] ) ) )
          @@ __POS_OF__
               {|
          / NUL => 400
          / CR => 400
          / LF => 400
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 NUL => 400
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 CR => 400
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 LF => 400
          |} )
    ; test
        "code_400_after_bad_host_request: 400 for no Host, two Hosts or an \
         invalid Host (RFC 9112 3.2)" (fun () ->
          let inv = "inv: a\r\ninv: ab\r\n" in
          expect
            (outcomes
               (on_paths (fun path ->
                    List.map
                      (fun (label, host) ->
                        (path, label, direct ~host ~additional:inv path) )
                      [ ("no Host", "")
                      ; ("two Hosts", host ^ host)
                      ; ("Host ###", "Host: ###\r\n") ] ) ) )
          @@ __POS_OF__
               {|
          / no Host => 400
          / two Hosts => 400
          / Host ### => 400
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 no Host => 400
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 two Hosts => 400
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 Host ### => 400
          |} )
    ; test
        "allow_crlf_start: an empty line before the request line is ignored \
         (RFC 9112 2.2)" (fun () ->
          expect
            (outcomes
               (on_paths (fun path ->
                    [(path, "CRLF first", "\r\n" ^ direct path)] ) ) )
          @@ __POS_OF__
               {|
          / CRLF first => 200
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 CRLF first => 404
          |} )
    ; test
        "reject_msgs_with_whitespace_between_startline_and_first_header_field: \
         400 (RFC 9112 2.2)" (fun () ->
          (* Upstream puts a space before each field line. *)
          let indent req =
            let lines = String.split_on_char '\n' req in
            let lines =
              List.filteri (fun i _ -> i < List.length lines - 2) lines
            in
            String.concat "\n " lines ^ "\n\r\n"
          in
          expect
            (outcomes
               (on_paths (fun path ->
                    [(path, "indented fields", indent (direct path))] ) ) )
          @@ __POS_OF__
               {|
          / indented fields => 400
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 indented fields => 400
          |} )
    ; test
        "code_400_if_msg_with_whitespace_between_header_field_and_colon: 400 \
         (RFC 9112 5.1)" (fun () ->
          expect
            (outcomes
               (on_paths (fun path ->
                    [ ( path
                      , "Space : Ohno"
                      , direct ~additional:"Space : Ohno\r\n" path ) ] ) ) )
          @@ __POS_OF__
               {|
          / Space : Ohno => 400
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 Space : Ohno => 400
          |} )
    ; test
        "content_head_request: no content in a HEAD response (RFC 9110 9.3.2)"
        (fun () ->
          expect
            (outcomes ~meth:"HEAD"
               (on_paths (fun path ->
                    [(path, "HEAD", direct ~meth:"HEAD" path)] ) ) )
          @@ __POS_OF__
               {|
          / HEAD => 200
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 HEAD => 404
          |} )
    ; xfail
        ~reason:
          "G3: caravan sends the 101 with no 100 when the request has no body"
        (test
           "continue_before_upgrade: 100 before 101 for Upgrade and Expect \
            100-continue, no body (RFC 9110 7.8)" (fun () ->
             (* Upstream sends this to its URLs. The fixture path makes the
                application switch protocols. *)
             let input =
               direct ~connection:"Connection: upgrade\r\n"
                 ~additional:"Upgrade: https\r\nExpect: 100-continue\r\n"
                 "/fixture/upgrade"
             in
             equal string "100 101" (codes ~meth:"GET" (serve_one input)) ) )
    ; test
        "continue_before_upgrade: with a body that the handler does not read, \
         the result is 500 (not an upstream case)" (fun () ->
          (* No 101 goes out, so RFC 9110 7.8 does not apply. This records the
             behavior until the issue of the plan is fixed. *)
          let input =
            direct ~connection:"Connection: upgrade\r\n"
              ~additional:
                "Upgrade: https\r\n\
                 Expect: 100-continue\r\n\
                 Content-Length: 5\r\n"
              "/fixture/upgrade"
            ^ "hello"
          in
          equal string "500" (codes ~meth:"GET" (serve_one input)) )
    ; test
        "content_length_2XX_connect: no Content-Length in a 2xx to CONNECT \
         (RFC 9110 8.6)" (fun () ->
          let r =
            Wire.final
              (parsed ~meth:"CONNECT"
                 (serve_one
                    "CONNECT localhost:8080 HTTP/1.1\r\n\
                     Host: localhost:8080\r\n\
                     \r\n" ) )
          in
          equal int 200 r.code ;
          equal (list string) [] (Wire.find_all "Content-Length" r) )
    ; test
        "transfer_encoding_2XX_connect: no Transfer-Encoding in a 2xx to \
         CONNECT (RFC 9112 6.1)" (fun () ->
          let r =
            Wire.final
              (parsed ~meth:"CONNECT"
                 (serve_one
                    "CONNECT localhost:8080 HTTP/1.1\r\n\
                     Host: localhost:8080\r\n\
                     \r\n" ) )
          in
          equal int 200 r.code ;
          equal (list string) [] (Wire.find_all "Transfer-Encoding" r) )
    ; test
        "code_501_unknown_methods: 501 for an unknown method (RFC 9110 9.1, an \
         application rule)" (fun () ->
          expect
            (outcomes
               (on_paths (fun path -> [(path, "ABC", direct ~meth:"ABC" path)])) )
          @@ __POS_OF__
               {|
          / ABC => 501
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 ABC => 501
          |} )
    ; test
        "code_405_blocked_methods: 405 for TRACE, 400 for CONNECT with a path \
         (RFC 9110 9.1, 9.3.6)" (fun () ->
          (* Upstream wants 405 also for CONNECT with a path. The target of
             CONNECT must be host:port (RFC 9112 3.2.3), so 400 is correct. *)
          expect
            (outcomes
               (on_paths (fun path ->
                    [ (path, "TRACE", direct ~meth:"TRACE" path)
                    ; (path, "CONNECT", direct ~meth:"CONNECT" path) ] ) ) )
          @@ __POS_OF__
               {|
          / TRACE => 405
          / CONNECT => 400
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 TRACE => 405
          /q7w2e9r4t1y6u3i8o5p0a2s7d4f9g1h6 CONNECT => 400
          |} )
    ]

let () =
  exit
    (run "conformance"
       [wire_tests; memory_server_tests; app_tests; direct_tests] )
