(* Conformance rules from the cispa/http-conformance suite (commit 24f75be;
   Copyright (c) 2023 Jannis Rautenstrauch, MIT license: see
   LICENSE.http-conformance). The tests send the inputs of the upstream rules to
   the HTTP/1.1 engine on in-memory connections, and check the bytes that it
   writes against RFC 9110 and RFC 9112. A comment names each verdict that
   differs from the upstream rule. *)

open Windtrap
open Caravan

(* The errors that the engine reported, for example a handler that raised. *)
let errors = ref []

(* The fixed time of the Date fields: Sun, 06 Nov 1994 08:49:37 GMT. *)
let config =
  { Config.default with
    date_header= true
  ; posix_time= Fake_time.posix (Fake_time.make ())
  ; on_error= (fun e _ -> errors := Printexc.to_string e :: !errors) }

let date = "Sun, 06 Nov 1994 08:49:37 GMT"

(* Only /fixture/fail makes the engine report an error. *)
let expected_error = Printexc.to_string (Failure "the fixture fails")

let serve ?(fixtures = true) inputs =
  errors := [] ;
  let outs =
    Memory_server.serve ~config (Conformance_app.handler ~fixtures) inputs
  in
  ( match List.filter (fun e -> e <> expected_error) !errors with
  | [] ->
      ()
  | es ->
      failf "the server reported: %s" (String.concat "; " (List.rev es)) ) ;
  outs

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
               ; one
                   "HTTP/1.1 200 OK\r\n\
                    Transfer-Encoding: chunked\r\n\
                    \r\n\
                    1_0\r\n"
               ; one
                   "HTTP/1.1 200 OK\r\n\
                    Transfer-Encoding: chunked\r\n\
                    Content-Length: 5\r\n\
                    \r\n\
                    0\r\n\
                    \r\n" ] )
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
          error: chunked body: bad size "1_0"
          error: both Transfer-Encoding and Content-Length
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
            GET /fixture/reset-content HTTP/1.1 => HTTP/1.1 205 Reset Content | Date, Content-Type: text/plain; charset=utf-8, Content-Length: 0 | length 0 ""
            rest ""
            GET /fixture/stream HTTP/1.1 => HTTP/1.1 200 OK | Date, Content-Type: text/plain, Transfer-Encoding: chunked | chunked "hello"
            rest ""
            GET /fixture/upgrade-required HTTP/1.1 => HTTP/1.1 426 Upgrade Required | Date, Upgrade: example/1, Content-Type: text/plain; charset=utf-8, Content-Length: 16, Connection: Upgrade | length 16 "upgrade required"
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
    ; test
        "continue_before_upgrade: 100 before 101 for Upgrade and Expect \
         100-continue, no body (RFC 9110 7.8)" (fun () ->
          (* Upstream sends this to its URLs. The fixture path makes the
             application switch protocols. *)
          let input =
            direct ~connection:"Connection: upgrade\r\n"
              ~additional:"Upgrade: https\r\nExpect: 100-continue\r\n"
              "/fixture/upgrade"
          in
          equal string "100 101" (codes ~meth:"GET" (serve_one input)) )
    ; test
        "continue_before_upgrade: 100 before 101 also with a body that the \
         handler does not read (not an upstream case)" (fun () ->
          (* Before #23 was fixed, the result was 500. *)
          let input =
            direct ~connection:"Connection: upgrade\r\n"
              ~additional:
                "Upgrade: https\r\n\
                 Expect: 100-continue\r\n\
                 Content-Length: 5\r\n"
              "/fixture/upgrade"
            ^ "hello"
          in
          equal string "100 101" (codes ~meth:"GET" (serve_one input)) )
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

(* The probe matrix (plan 4.3). It is a function, not a lazy value: mutation
   testing starts each mutant from the state of a dry run, and a forced lazy
   value would hide the mutant. *)

type probe = {label: string; meth: string; v10: bool; close: bool; input: string}

(* The 10 header sets of upstream. Sets 6 and 7 use the fixed time. *)
let header_sets =
  [ (1, [])
  ; (2, [("upgrade-insecure-requests", "1")])
  ; (3, [("Range", "bytes=0-10")])
  ; (4, [("If-Match", "*")])
  ; (5, [("If-None-Match", "*")])
  ; (6, [("If-Modified-Since", date)])
  ; (7, [("If-Range", date)])
  ; (8, [("Early-Data", "1")])
  ; (9, [("Early-Data", "Abc")])
  ; (10, [("Connection", "close")]) ]

let probe_methods =
  ["GET"; "HEAD"; "POST"; "OPTIONS"; "DELETE"; "PUT"; "TRACE"; "PATCH"; "ABC"]

(* A request as httpx sends it through mitmproxy: Host, Content-Length for POST,
   PUT and PATCH, Accept, Accept-Encoding, Connection (except in set 10, where
   Connection: close comes last), user-agent, cache-control, then the field of
   the set. *)
let httpx_request ?(v = "HTTP/1.1") meth path set =
  let extra = List.assoc set header_sets in
  let close = set = 10 in
  let fields =
    [("Host", "localhost:8080")]
    @ ( if List.mem meth ["POST"; "PUT"; "PATCH"] then [("Content-Length", "0")]
        else [] )
    @ [("Accept", "*/*"); ("Accept-Encoding", "gzip, deflate, br")]
    @ (if close then [] else [("Connection", "keep-alive")])
    @ [("user-agent", "TODO"); ("cache-control", "no-store")]
    @ extra
  in
  Printf.sprintf "%s %s %s\r\n%s\r\n" meth path v
    (String.concat "" (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") fields))

let probe ?(v10 = false) ?(close = false) label meth input =
  {label; meth; v10; close; input}

let fixture_paths =
  [ "/fixture/no-content"
  ; "/fixture/not-modified"
  ; "/fixture/reset-content"
  ; "/fixture/stream"
  ; "/fixture/upgrade-required"
  ; "/fixture/fail" ]

let probes () =
  let upstream =
    List.concat_map
      (fun (path, path_label) ->
        List.concat_map
          (fun meth ->
            List.map
              (fun (set, _) ->
                probe ~close:(set = 10)
                  (Printf.sprintf "%s set %d %s" meth set path_label)
                  meth
                  (httpx_request meth path set) )
              header_sets )
          probe_methods )
      [("/", "/"); (random_path, "random")]
  in
  let fixtures =
    List.concat_map
      (fun path ->
        List.map
          (fun meth ->
            probe (meth ^ " " ^ path) meth (httpx_request meth path 1) )
          ["GET"; "HEAD"] )
      fixture_paths
  in
  upstream @ fixtures
  @ [ probe "upgrade" "GET"
        (request
           ~headers:[("Connection", "upgrade"); ("Upgrade", "example/1")]
           "GET" "/fixture/upgrade" )
    ; probe ~v10:true "1.0 GET /" "GET"
        (httpx_request ~v:"HTTP/1.0" "GET" "/" 1)
    ; probe ~v10:true "1.0 GET /fixture/stream" "GET"
        (httpx_request ~v:"HTTP/1.0" "GET" "/fixture/stream" 1)
    ; probe "CONNECT" "CONNECT"
        "CONNECT localhost:8080 HTTP/1.1\r\nHost: localhost:8080\r\n\r\n" ]

(* Each probe with what the server wrote, parsed. *)
let matrix () =
  let ps = probes () in
  let outs = serve (List.map (fun p -> p.input) ps) in
  List.map2 (fun p out -> (p, parsed ~meth:p.meth out)) ps outs

(* [rule ~applies ~must check] runs [check] on each response of the matrix that
   [applies] selects. Each probe in [must] must give at least one such response,
   so that the rule cannot pass with nothing to check. *)
let rule ?(applies = fun _ _ -> true) ~must check =
  let units =
    List.concat_map
      (fun (p, (t : Wire.t)) ->
        List.filter_map
          (fun r -> if applies p r then Some (p, t, r) else None)
          t.responses )
      (matrix ())
  in
  List.iter
    (fun label ->
      if not (List.exists (fun (p, _, _) -> p.label = label) units) then
        failf "the rule does not apply to the probe %S" label )
    must ;
  let violations =
    List.filter_map
      (fun (p, t, (r : Wire.response)) ->
        match check p t r with
        | Ok () ->
            None
        | Error m ->
            Some (Printf.sprintf "%s: %s: %s" p.label r.status_line m) )
      units
  in
  if violations <> [] then
    failf "%d violations:\n%s" (List.length violations)
      (String.concat "\n" violations)

let check ok fmt = Printf.ksprintf (fun m -> if ok then Ok () else Error m) fmt

let absent name _ _ r =
  check
    (Wire.find_all name r = [])
    "%s: %s" name
    (String.concat ", " (Wire.find_all name r))

let is_status codes _ (r : Wire.response) = List.mem r.code codes

let is_tchar = function
  | '!'
  | '#'
  | '$'
  | '%'
  | '&'
  | '\''
  | '*'
  | '+'
  | '-'
  | '.'
  | '^'
  | '_'
  | '`'
  | '|'
  | '~'
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z' ->
      true
  | _ ->
      false

let is_token s = s <> "" && String.for_all is_tchar s

let is_ows c = c = ' ' || c = '\t'

let is_digit c = c >= '0' && c <= '9'

(* A small parser for the field grammars: a position in a string. *)
let skip_ows s i =
  let i = ref i in
  while !i < String.length s && is_ows s.[!i] do
    incr i
  done ;
  !i

let token_at s i =
  let j = ref i in
  while !j < String.length s && is_tchar s.[!j] do
    incr j
  done ;
  if !j = i then None else Some !j

(* quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE *)
let quoted_at s i =
  let n = String.length s in
  if i >= n || s.[i] <> '"' then None
  else
    let rec go j =
      if j >= n then None
      else
        match s.[j] with
        | '"' ->
            Some (j + 1)
        | '\\'
          when j + 1 < n
               && (s.[j + 1] = '\t' || s.[j + 1] >= ' ')
               && s.[j + 1] <> '\x7f' ->
            go (j + 2)
        | c
          when c = '\t' || c = ' ' || c = '!'
               || (c >= '#' && c <= '[')
               || (c >= ']' && c <= '~')
               || c >= '\x80' ->
            go (j + 1)
        | _ ->
            None
    in
    go (i + 1)

(* The parameters after a media type or a transfer coding, then the end: -
   media-type (RFC 9110 5.6.6): *( OWS ";" OWS [ token "=" ( token /
   quoted-string ) ] ). A parameter can be empty, and there is no space around
   "=". - transfer-coding (RFC 9112 7): *( OWS ";" OWS token BWS "=" BWS ( token
   / quoted-string ) ). [bws] permits the spaces around "=", and no parameter is
   empty. *)
let rec params ~bws s i =
  let n = String.length s in
  let i' = skip_ows s i in
  if i' = n then i = n
  else if s.[i'] <> ';' then false
  else
    let p = skip_ows s (i' + 1) in
    if (not bws) && (p = n || s.[p] = ';') then p = n || params ~bws s p
    else
      let ws j = if bws then skip_ows s j else j in
      match token_at s p with
      | None ->
          false
      | Some j -> (
          let j = ws j in
          if j >= n || s.[j] <> '=' then false
          else
            let k = ws (j + 1) in
            match
              match token_at s k with Some e -> Some e | None -> quoted_at s k
            with
            | Some e ->
                params ~bws s e
            | None ->
                false )

(* 1#element: elements separated by OWS "," OWS, none empty. It splits at each
   comma, also in a quoted-string: caravan writes no quoted-string in these
   fields. *)
let list_of element s =
  List.for_all
    (fun e -> e <> "" && element e)
    (List.map String.trim (String.split_on_char ',' s))

let is_transfer_coding c =
  match token_at c 0 with Some i -> params ~bws:true c i | None -> false

let is_media_type s =
  match token_at s 0 with
  | Some i when i < String.length s && s.[i] = '/' -> (
    match token_at s (i + 1) with
    | Some j ->
        params ~bws:false s j
    | None ->
        false )
  | _ ->
      false

(* IMF-fixdate = day-name "," SP 2DIGIT SP month SP 4DIGIT SP 2DIGIT ":" 2DIGIT
   ":" 2DIGIT SP "GMT" (RFC 9110 5.6.7) *)
let is_imf_fixdate s =
  let digits i n = String.for_all is_digit (String.sub s i n) in
  String.length s = 29
  && List.mem (String.sub s 0 3)
       ["Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun"]
  && String.sub s 3 2 = ", "
  && digits 5 2
  && s.[7] = ' '
  && List.mem (String.sub s 8 3)
       [ "Jan"
       ; "Feb"
       ; "Mar"
       ; "Apr"
       ; "May"
       ; "Jun"
       ; "Jul"
       ; "Aug"
       ; "Sep"
       ; "Oct"
       ; "Nov"
       ; "Dec" ]
  && s.[11] = ' '
  && digits 12 4
  && s.[16] = ' '
  && digits 17 2
  && s.[19] = ':'
  && digits 20 2
  && s.[22] = ':'
  && digits 23 2
  && String.sub s 25 4 = " GMT"
  (* The ranges of the day, the hour, the minute and the second (60 for a leap
     second). The test does not compute the weekday of the date. *)
  &&
  let n i = int_of_string (String.sub s i 2) in
  n 5 >= 1 && n 5 <= 31 && n 17 <= 23 && n 20 <= 59 && n 23 <= 60

(* The names that upstream permits to occur more than one time. *)
let repeatable =
  [ "set-cookie"
  ; "content-language"
  ; "vary"
  ; "server-timing"
  ; "link"
  ; "accept-ch"
  ; "cache-control"
  ; "p3p"
  ; "content-security-policy"
  ; "connection"
  ; "referrer-policy"
  ; "via"
  ; "alt-svc"
  ; "content-security-policy-report-only"
  ; "accept-ranges"
  ; "allow"
  ; "www-authenticate"
  ; "pragma" ]

(* Whether the Connection fields of [r] have the option [o]. *)
let has_option o r =
  List.exists
    (fun c ->
      List.exists
        (fun x -> String.lowercase_ascii (String.trim x) = o)
        (String.split_on_char ',' c) )
    (Wire.find_all "Connection" r)

let not_interim _ (r : Wire.response) = r.code >= 200 || r.code = 101

let with_field name _ r = Wire.find_all name r <> []

let grammar_tests =
  group "grammar checks of the probe rules"
    [ test "media-type (RFC 9110 8.3.1, 5.6.6)" (fun () ->
          let valid =
            [ "text/plain"
            ; "text/plain;"
            ; "text/plain; charset=utf-8"
            ; "text/plain; charset=utf-8;"
            ; "text/plain;;a=b"
            ; "text/plain ; a=\"q;\\\"\"" ]
          and invalid =
            [ "text"
            ; "text/"
            ; "/plain"
            ; "text/plain; charset"
            ; "text/plain; a = b"
            ; "text/plain; a=\"open"
            ; "text/plain x" ]
          in
          equal (list string) []
            (List.filter (fun s -> not (is_media_type s)) valid) ;
          equal (list string) [] (List.filter is_media_type invalid) )
    ; test "transfer-coding (RFC 9112 7)" (fun () ->
          let valid = ["chunked"; "gzip, chunked"; "x ; a = b"; "x;a=\"q\""]
          and invalid = ["chunked;"; "chunked, "; "x; a"; ", chunked"] in
          equal (list string) []
            (List.filter (fun s -> not (list_of is_transfer_coding s)) valid) ;
          equal (list string) []
            (List.filter (list_of is_transfer_coding) invalid) )
    ; test "IMF-fixdate (RFC 9110 5.6.7)" (fun () ->
          let valid = [date; "Wed, 31 Dec 1999 23:59:60 GMT"]
          and invalid =
            [ "Sun, 6 Nov 1994 08:49:37 GMT"
            ; "Sun, 00 Nov 1994 08:49:37 GMT"
            ; "Sun, 06 Nov 1994 24:00:00 GMT"
            ; "Sun, 06 Nov 1994 08:60:37 GMT"
            ; "Sunday, 06-Nov-94 08:49:37 GMT"
            ; "Sun Nov  6 08:49:37 1994"
            ; "Sun, 06 nov 1994 08:49:37 GMT" ]
          in
          equal (list string) []
            (List.filter (fun s -> not (is_imf_fixdate s)) valid) ;
          equal (list string) [] (List.filter is_imf_fixdate invalid) ) ]

let probe_tests =
  group "probe rules"
    [ test
        "duplicate_fields: no field occurs two times, except the fields of a \
         list (RFC 9110 5.3)" (fun () ->
          (* Upstream gives only "unclear" for a repeated Content-Length, Date
             or Transfer-Encoding. The test refuses them. *)
          rule ~must:["GET set 1 /"] (fun _ _ r ->
              let names =
                List.map (fun (n, _) -> String.lowercase_ascii n) r.fields
              in
              let repeated =
                List.sort_uniq compare
                  (List.filter
                     (fun n ->
                       List.length (List.filter (String.equal n) names) > 1
                       && not (List.mem n repeatable) )
                     names )
              in
              check (repeated = []) "repeated %s" (String.concat ", " repeated) ) )
    ; test
        "content_length_1XX_204: no Content-Length in a 1xx or 204 (RFC 9110 \
         8.6)" (fun () ->
          rule
            ~applies:(fun _ r -> r.code < 200 || r.code = 204)
            ~must:["GET /fixture/no-content"; "OPTIONS set 1 /"; "upgrade"]
            (absent "Content-Length") )
    ; test
        "no_transfer_encoding_1xx_204: no Transfer-Encoding in a 1xx or 204 \
         (RFC 9112 6.1)" (fun () ->
          rule
            ~applies:(fun _ r -> r.code < 200 || r.code = 204)
            ~must:["GET /fixture/no-content"; "OPTIONS set 1 /"; "upgrade"]
            (absent "Transfer-Encoding") )
    ; test "send_upgrade_101: a 101 has Upgrade (RFC 9110 7.8)" (fun () ->
          rule ~applies:(is_status [101]) ~must:["upgrade"] (fun _ _ r ->
              check (Wire.find "Upgrade" r <> None) "no Upgrade" ) )
    ; test
        "date_header_required: caravan sends Date on each response that is not \
         1xx (RFC 9110 6.6.1)" (fun () ->
          (* RFC 9110 6.6.1 requires Date on 2xx, 3xx and 4xx, and permits it on
             5xx. Upstream does not examine 5xx. Caravan sends Date on 5xx too,
             and the test asserts this. *)
          rule
            ~applies:(fun _ r -> r.code >= 200)
            ~must:["GET set 1 /"; "GET /fixture/fail"]
            (fun _ _ r -> check (Wire.find "Date" r <> None) "no Date") )
    ; test "date_grammar: Date is an IMF-fixdate (RFC 9110 5.6.7)" (fun () ->
          (* Upstream also accepts the obsolete forms, but a sender must use
             IMF-fixdate. *)
          rule ~applies:(with_field "Date") ~must:["GET set 1 /"] (fun _ _ r ->
              let ds = Wire.find_all "Date" r in
              check
                (List.for_all is_imf_fixdate ds)
                "Date %s" (String.concat ", " ds) ) )
    ; test "content_length_grammar: Content-Length is 1*DIGIT (RFC 9110 8.6)"
        (fun () ->
          (* Upstream uses the Python int(), which also accepts "+14". *)
          rule ~applies:(with_field "Content-Length") ~must:["GET set 1 /"]
            (fun _ _ r ->
              let ls = Wire.find_all "Content-Length" r in
              check
                (List.for_all
                   (fun l -> l <> "" && String.for_all is_digit l)
                   ls )
                "Content-Length %s" (String.concat ", " ls) ) )
    ; test "connection_grammar: Connection is 1#token (RFC 9110 7.6.1)"
        (fun () ->
          rule ~applies:(with_field "Connection")
            ~must:["GET set 10 /"; "upgrade"] (fun _ _ r ->
              let cs = Wire.find_all "Connection" r in
              check
                (List.for_all (list_of is_token) cs)
                "Connection %s" (String.concat ", " cs) ) )
    ; test
        "transfer_encoding_grammar: Transfer-Encoding is 1#transfer-coding \
         (RFC 9112 6.1)" (fun () ->
          rule ~applies:(with_field "Transfer-Encoding")
            ~must:["GET /fixture/stream"] (fun _ _ r ->
              let ts = Wire.find_all "Transfer-Encoding" r in
              check
                (List.for_all (list_of is_transfer_coding) ts)
                "Transfer-Encoding %s" (String.concat ", " ts) ) )
    ; test
        "content_type_grammar: each Content-Type is a media-type (RFC 9110 8.3)"
        (fun () ->
          rule ~applies:(with_field "Content-Type")
            ~must:["GET set 1 /"; "GET /fixture/fail"] (fun _ _ r ->
              let ts = Wire.find_all "Content-Type" r in
              check
                (List.for_all is_media_type ts)
                "Content-Type %s" (String.concat ", " ts) ) )
    ; test
        "content_type_header_required: a response with content has \
         Content-Type (RFC 9110 8.3)" (fun () ->
          rule
            ~applies:(fun _ r -> r.body <> "")
            ~must:["GET set 1 /"; "GET /fixture/fail"]
            (fun _ _ r ->
              check (Wire.find "Content-Type" r <> None) "no Content-Type" ) )
    ; test
        "transfer_encoding_http11: no Transfer-Encoding in a response to \
         HTTP/1.0 (RFC 9112 6.1)" (fun () ->
          (* Upstream never sends HTTP/1.0, so this rule always passes there. *)
          rule
            ~applies:(fun p _ -> p.v10)
            ~must:["1.0 GET /"; "1.0 GET /fixture/stream"]
            (absent "Transfer-Encoding") )
    ; test
        "close_option_in_final_response: the response to Connection: close has \
         close and the server closes (RFC 9112 9.6)" (fun () ->
          (* Upstream examines only the field (a SHOULD). The test also asserts
             that the server closes (a MUST). *)
          rule
            ~applies:(fun p r -> p.close && not_interim p r)
            ~must:["GET set 10 /"; "ABC set 10 random"]
            (fun _ t r ->
              (* Upstream needs the field to be exactly "close". The test
                 accepts "close" in a list, for example "Upgrade, close" on a
                 426. *)
              let close = has_option "close" r in
              check (close && t.fin) "close option %b, half-close %b" close
                t.fin ) )
    ; test "no_bare_cr: no CR without LF in a head (RFC 9112 2.2)" (fun () ->
          rule ~must:["GET set 1 /"] (fun _ _ r ->
              let bare = ref false in
              String.iteri
                (fun i c ->
                  if
                    c = '\r'
                    && (i + 1 >= String.length r.head || r.head.[i + 1] <> '\n')
                  then bare := true )
                r.head ;
              check (not !bare) "bare CR" ) )
    ; test
        "code_204_no_additional_content: no bytes after a 204 head (RFC 9110 \
         15.3.5)" (fun () ->
          (* mitmproxy reads no body for a 204, so upstream cannot see the
             bytes. *)
          rule ~applies:(is_status [204]) ~must:["GET /fixture/no-content"]
            (fun _ t _ -> check (t.rest = "") "then %S" t.rest ) )
    ; test "code_304_no_content: no bytes after a 304 head (RFC 9110 15.4.5)"
        (fun () ->
          rule ~applies:(is_status [304])
            ~must:["GET /fixture/not-modified"; "GET set 5 /"] (fun _ t _ ->
              check (t.rest = "") "then %S" t.rest ) )
    ; test
        "code_205_no_content_allowed: a 205 has no content (RFC 9110 15.3.6, \
         RFC 9112 6.3)" (fun () ->
          rule
            ~applies:(fun p r -> r.code = 205 && p.meth <> "HEAD")
            ~must:["GET /fixture/reset-content"]
            (fun _ t r ->
              let framed =
                match r.framing with
                | Length 0 | Chunked ->
                    true
                | Close_delimited ->
                    t.fin || t.reset
                | _ ->
                    false
              in
              check (r.body = "" && framed) "%s %S" (framing r.framing) r.body ) )
    ; test
        "send_upgrade_426: a 426 has Upgrade and the upgrade option (RFC 9110 \
         15.5.22, 7.8)" (fun () ->
          rule ~applies:(is_status [426])
            ~must:["GET /fixture/upgrade-required"] (fun _ _ r ->
              (* Upstream examines only Upgrade. RFC 9110 7.8 also requires the
                 upgrade option. *)
              let upgrade_option = has_option "upgrade" r in
              check
                (Wire.find "Upgrade" r <> None && upgrade_option)
                "Upgrade %s, Connection %s"
                (Option.value (Wire.find "Upgrade" r) ~default:"none")
                (String.concat ", " (Wire.find_all "Connection" r)) ) )
    ; test
        "field syntax: each name is a token and no value has NUL, CR or LF \
         (RFC 9110 5.1, 5.5)" (fun () ->
          (* The HTTP/1 form of the HTTP/2 rules of upstream. *)
          rule ~must:["GET set 1 /"] (fun _ _ r ->
              let bad =
                List.filter
                  (fun (n, v) ->
                    (not (is_token n))
                    || String.exists
                         (fun c -> c = '\000' || c = '\r' || c = '\n')
                         v )
                  r.fields
              in
              check (bad = []) "bad fields %s"
                (String.concat ", "
                   (List.map (fun (n, v) -> Printf.sprintf "%S: %S" n v) bad) ) ) )
    ]

(* The multi rules (plan 5.3) compare the final responses of two probes. *)

(* [pair_rule ~pairs ~must check] runs [check] on the final responses of each
   pair of probe labels that [applies] selects. Each label in [must] must be the
   first label of a pair that applies. *)
let pair_rule ?(applies = fun _ _ -> true) ~pairs ~must check =
  let m = matrix () in
  let final label =
    match List.find_opt (fun (p, _) -> p.label = label) m with
    | Some (_, t) ->
        (t, Wire.final t)
    | None ->
        failf "no probe %S" label
  in
  let units =
    List.filter_map
      (fun (a, b) ->
        let ((_, ra) as x) = final a and ((_, rb) as y) = final b in
        if applies ra rb then Some (a, b, x, y) else None )
      pairs
  in
  List.iter
    (fun label ->
      if not (List.exists (fun (a, _, _, _) -> a = label) units) then
        failf "the rule does not apply to the probe %S" label )
    must ;
  let violations =
    List.filter_map
      (fun (a, b, x, y) ->
        match check x y with
        | Ok () ->
            None
        | Error m ->
            Some (Printf.sprintf "%s / %s: %s" a b m) )
      units
  in
  if violations <> [] then
    failf "%d violations:\n%s" (List.length violations)
      (String.concat "\n" violations)

let path_labels = ["/"; "random"]

(* HEAD with GET: the same header set and path, and each fixture. *)
let head_get_pairs =
  List.concat_map
    (fun path ->
      List.map
        (fun (set, _) ->
          ( Printf.sprintf "HEAD set %d %s" set path
          , Printf.sprintf "GET set %d %s" set path ) )
        header_sets )
    path_labels
  @ List.map (fun path -> ("HEAD " ^ path, "GET " ^ path)) fixture_paths

let names (r : Wire.response) ~except =
  List.sort_uniq compare
    (List.filter
       (fun n -> not (List.mem n except))
       (List.map (fun (n, _) -> String.lowercase_ascii n) r.fields) )

let multi_tests =
  group "multi rules"
    [ test
        "content_length_same_head_get: the Content-Length of a HEAD is that of \
         the GET (RFC 9110 8.6)" (fun () ->
          pair_rule ~pairs:head_get_pairs
            ~applies:(fun h _ -> Wire.find "Content-Length" h <> None)
            ~must:["HEAD set 1 /"]
            (fun (_, h) (_, g) ->
              let cl r = Wire.find "Content-Length" r in
              check
                (cl h = cl g)
                "HEAD %s, GET %s"
                (Option.value (cl h) ~default:"none")
                (Option.value (cl g) ~default:"none") ) )
    ; test
        "head_get_headers: a HEAD has the status and the field names of the \
         GET (RFC 9110 9.3.2)" (fun () ->
          let except =
            ["content-length"; "vary"; "transfer-encoding"; "content-encoding"]
          in
          pair_rule ~pairs:head_get_pairs ~must:(List.map fst head_get_pairs)
            (fun (_, h) (_, g) ->
              check
                (h.code = g.code && names h ~except = names g ~except)
                "HEAD %d %s, GET %d %s" h.code
                (String.concat " " (names h ~except))
                g.code
                (String.concat " " (names g ~except)) ) )
    ; test
        "content_length_same_304_200: no 304 has Content-Length (RFC 9110 8.6)"
        (fun () ->
          (* Upstream compares the Content-Length of a 304 with that of a 200.
             Caravan writes none on a 304, so the test asserts this. *)
          rule ~applies:(is_status [304])
            ~must:["GET /fixture/not-modified"; "GET set 5 /"]
            (absent "Content-Length") )
    ; test
        "code_304_headers: a 304 has the Date, Cache-Control, ETag, Expires, \
         Content-Location and Vary of the 200 (RFC 9110 15.4.5)" (fun () ->
          let fields =
            [ "date"
            ; "cache-control"
            ; "etag"
            ; "expires"
            ; "content-location"
            ; "vary" ]
          in
          let pairs =
            List.concat_map
              (fun meth ->
                List.concat_map
                  (fun path ->
                    List.map
                      (fun set ->
                        ( Printf.sprintf "%s set %d %s" meth set path
                        , Printf.sprintf "%s set 1 %s" meth path ) )
                      [4; 5; 6; 7] )
                  path_labels )
              ["GET"; "HEAD"]
          in
          pair_rule ~pairs
            ~applies:(fun r304 r200 -> r304.code = 304 && r200.code = 200)
            ~must:["GET set 5 /"; "HEAD set 5 /"]
            (fun (_, r304) (_, r200) ->
              let missing =
                List.filter
                  (fun n -> Wire.find n r200 <> None && Wire.find n r304 = None)
                  fields
              in
              check (missing = []) "missing %s" (String.concat ", " missing) ) )
    ]

let () =
  exit
    (run "conformance"
       [ wire_tests
       ; memory_server_tests
       ; app_tests
       ; direct_tests
       ; grammar_tests
       ; probe_tests
       ; multi_tests ] )
