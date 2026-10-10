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

let () = exit (run "conformance" [wire_tests; memory_server_tests; app_tests])
