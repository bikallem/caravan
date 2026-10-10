open Windtrap
open Caravan

let meth = Testable.make ~pp:Method.pp ~equal:Method.equal

let version = Testable.structural ~pp:Version.pp

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

let standard_methods =
  Method.[GET; HEAD; POST; PUT; DELETE; CONNECT; OPTIONS; TRACE; PATCH]

let tchars =
  List.of_seq
    (String.to_seq
       "!#$%&'*+-.^_`|~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" )

let token = Gen.string_of ~size:(Gen.int_range 1 16) (Gen.of_list tchars)

let method_tests =
  group "Method"
    [ test "standard names parse to their constructors" (fun () ->
          List.iter
            (fun m -> equal meth m (Method.of_string (Method.to_string m)))
            standard_methods ;
          is_true (Method.of_string "GET" = Method.GET) )
    ; test "names are case-sensitive" (fun () ->
          is_true (Method.of_string "get" = Method.Other "get") )
    ; test "rejects non-tokens" (fun () ->
          List.iter
            (fun s -> raises_match is_invalid_arg (fun () -> Method.of_string s))
            [""; "GE T"; "GET\r"; "GET\n"; "G\000T"; "(GET)"] )
    ; test "extension methods keep their name" (fun () ->
          equal string "PURGE" (Method.to_string (Method.of_string "PURGE")) )
    ; test "equal compares names" (fun () ->
          is_true (Method.equal (Method.Other "GET") Method.GET) ;
          is_true (Method.compare (Method.Other "GET") Method.GET = 0) )
    ; test "safe and idempotent" (fun () ->
          is_true (Method.is_safe Method.GET) ;
          is_true (not (Method.is_safe Method.POST)) ;
          is_true (Method.is_idempotent Method.PUT) ;
          is_true (not (Method.is_idempotent Method.PATCH)) )
    ; prop "to_string (of_string s) = s for any token" token (fun s ->
          equal string s (Method.to_string (Method.of_string s)) ) ]

let version_tests =
  group "Version"
    [ test "round trips" (fun () ->
          List.iter
            (fun v ->
              equal (option version) (Some v)
                (Version.of_string (Version.to_string v)) )
            Version.[Http_1_0; Http_1_1; Http_2; Http_3] )
    ; test "unknown strings" (fun () ->
          List.iter
            (fun s -> is_none ~pp:Version.pp (Version.of_string s))
            [""; "HTTP/1.2"; "http/1.1"; "HTTP/2.0"; "HTTP/1.1 "] )
    ; test "equal" (fun () ->
          is_true (Version.equal Version.Http_2 Version.Http_2) ;
          is_true (not (Version.equal Version.Http_1_0 Version.Http_1_1)) )
    ; test "ordered oldest to newest" (fun () ->
          is_true (Version.compare Version.Http_1_0 Version.Http_1_1 < 0) ;
          is_true (Version.compare Version.Http_2 Version.Http_3 < 0) ) ]

let status_tests =
  group "Status"
    [ test "rejects codes outside [100;599]" (fun () ->
          raises_match is_invalid_arg (fun () -> Status.v 99) ;
          raises_match is_invalid_arg (fun () -> Status.v 600) )
    ; test "known reasons" (fun () ->
          equal string "Not Found" (Status.reason Status.not_found) ;
          equal string "Content Too Large"
            (Status.reason Status.content_too_large) ;
          equal string "" (Status.reason (Status.v 299)) )
    ; test "pp" (fun () ->
          expect (Format.asprintf "%a" Status.pp Status.ok)
          @@ __POS_OF__ {|200 OK|} ;
          expect (Format.asprintf "%a" Status.pp (Status.v 299))
          @@ __POS_OF__ {|299|} )
    ; test "registered reason phrases" (fun () ->
          let registered =
            List.filter_map
              (fun n ->
                let s = Status.v n in
                if Status.reason s = "" then None
                else Some (Format.asprintf "%a" Status.pp s) )
              (List.init 500 (fun i -> i + 100))
          in
          expect (String.concat "\n" registered)
          @@ __POS_OF__
               {|
            100 Continue
            101 Switching Protocols
            103 Early Hints
            200 OK
            201 Created
            202 Accepted
            203 Non-Authoritative Information
            204 No Content
            205 Reset Content
            206 Partial Content
            300 Multiple Choices
            301 Moved Permanently
            302 Found
            303 See Other
            304 Not Modified
            307 Temporary Redirect
            308 Permanent Redirect
            400 Bad Request
            401 Unauthorized
            402 Payment Required
            403 Forbidden
            404 Not Found
            405 Method Not Allowed
            406 Not Acceptable
            407 Proxy Authentication Required
            408 Request Timeout
            409 Conflict
            410 Gone
            411 Length Required
            412 Precondition Failed
            413 Content Too Large
            414 URI Too Long
            415 Unsupported Media Type
            416 Range Not Satisfiable
            417 Expectation Failed
            421 Misdirected Request
            422 Unprocessable Content
            426 Upgrade Required
            428 Precondition Required
            429 Too Many Requests
            431 Request Header Fields Too Large
            451 Unavailable For Legal Reasons
            500 Internal Server Error
            501 Not Implemented
            502 Bad Gateway
            503 Service Unavailable
            504 Gateway Timeout
            505 HTTP Version Not Supported
            511 Network Authentication Required
            |} )
    ; prop "v and code round trip" (Gen.int_range 100 599) (fun n ->
          equal int n (Status.code (Status.v n)) )
    ; prop "kind follows the first digit" (Gen.int_range 100 599) (fun n ->
          let expected =
            match n / 100 with
            | 1 ->
                `Informational
            | 2 ->
                `Success
            | 3 ->
                `Redirection
            | 4 ->
                `Client_error
            | _ ->
                `Server_error
          in
          is_true (Status.kind (Status.v n) = expected) )
    ; test "constants used by the server have the right codes" (fun () ->
          List.iter
            (fun (s, n) ->
              equal status (Status.v n) s ;
              is_true ~msg:"has a reason" (Status.reason s <> "") )
            Status.
              [ (continue, 100)
              ; (switching_protocols, 101)
              ; (ok, 200)
              ; (no_content, 204)
              ; (not_modified, 304)
              ; (bad_request, 400)
              ; (request_timeout, 408)
              ; (content_too_large, 413)
              ; (uri_too_long, 414)
              ; (expectation_failed, 417)
              ; (request_header_fields_too_large, 431)
              ; (internal_server_error, 500)
              ; (not_implemented, 501)
              ; (http_version_not_supported, 505) ] ) ]

(* Headers *)

let fields = list (pair string string)

let h = Headers.of_list

let headers_tests =
  group "Headers"
    [ test "find is case-insensitive and returns the first field" (fun () ->
          let h = h [("Content-Type", "text/plain"); ("content-type", "x")] in
          equal (option string) (Some "text/plain")
            (Headers.find "CONTENT-TYPE" h) ;
          equal (list string) ["text/plain"; "x"]
            (Headers.find_all "content-type" h) ;
          equal (option string) None (Headers.find "Host" h) )
    ; test "to_list keeps order, case and duplicates" (fun () ->
          let l =
            [("Set-Cookie", "a=1"); ("Host", "x"); ("set-cookie", "b=2")]
          in
          equal fields l (Headers.to_list (h l)) )
    ; test "set replaces the first match in place and drops the others"
        (fun () ->
          let h' =
            Headers.set "accept" "*/*"
              (h [("Accept", "a"); ("Host", "x"); ("ACCEPT", "b")])
          in
          equal fields [("accept", "*/*"); ("Host", "x")] (Headers.to_list h') ;
          equal fields
            [("Host", "x"); ("Accept", "a")]
            (Headers.to_list (Headers.set "Accept" "a" (h [("Host", "x")]))) )
    ; test "tokens span every field of the name" (fun () ->
          let h =
            h
              [ ("Connection", "keep-alive, Upgrade")
              ; ("Connection", ",close ,,") ]
          in
          equal (list string)
            ["keep-alive"; "Upgrade"; "close"]
            (Headers.tokens "connection" h) ;
          is_true (Headers.has_token "Connection" "UPGRADE" h) ;
          is_true (not (Headers.has_token "connection" "keep" h)) )
    ; test "remove_hop_by_hop drops fields named by Connection" (fun () ->
          let h =
            h
              [ ("Host", "x")
              ; ("Connection", "X-Trace, close")
              ; ("X-Trace", "1")
              ; ("Keep-Alive", "timeout=5")
              ; ("Transfer-Encoding", "chunked")
              ; ("TE", "trailers")
              ; ("Upgrade", "websocket")
              ; ("Accept", "*/*") ]
          in
          equal fields
            [("Host", "x"); ("Accept", "*/*")]
            (Headers.to_list (Headers.remove_hop_by_hop h)) )
    ; test "rejects invalid names and values" (fun () ->
          List.iter
            (fun (n, v) ->
              raises_match is_invalid_arg (fun () ->
                  Headers.add n v Headers.empty ) )
            [ ("", "x")
            ; ("Bad Name", "x")
            ; ("Name:", "x")
            ; ("X", "a\r\nSet-Cookie: s=1")
            ; ("X", "a\nb")
            ; ("X", "a\000b")
            ; ("X", " leading")
            ; ("X", "trailing\t")
            ; ("X", "del\127") ] ;
          equal fields
            [("X", ""); ("Y", "a\tb c")]
            (Headers.to_list (h [("X", ""); ("Y", "a\tb c")])) )
    ; test "equal ignores name case but not order or values" (fun () ->
          let a = h [("Host", "x"); ("Accept", "*/*")] in
          is_true (Headers.equal a (h [("host", "x"); ("ACCEPT", "*/*")])) ;
          is_true (not (Headers.equal a (h [("Accept", "*/*"); ("Host", "x")]))) ;
          is_true (not (Headers.equal a (h [("Host", "X"); ("Accept", "*/*")]))) ;
          is_true (not (Headers.equal a (h [("Host", "x")]))) )
    ; test "fold and iter visit fields in order" (fun () ->
          let h = h [("A", "1"); ("b", "2"); ("A", "3")] in
          equal (list string) ["A=1"; "b=2"; "A=3"]
            (List.rev (Headers.fold (fun n v acc -> (n ^ "=" ^ v) :: acc) h [])) ;
          let seen = ref [] in
          Headers.iter (fun n _ -> seen := n :: !seen) h ;
          equal (list string) ["A"; "b"; "A"] (List.rev !seen) )
    ; test "pp" (fun () ->
          expect
            (Format.asprintf "%a" Headers.pp
               (h [("Host", "x"); ("Accept", "*/*")]) )
          @@ __POS_OF__ {|Host: x
Accept: */*|} ) ]

(* Generators that hit name collisions in different cases. *)
let name_gen =
  Gen.of_list ~pp:Format.pp_print_string
    ["Host"; "host"; "HOST"; "Accept"; "accept"; "X-Id"; "x-id"]

let value_gen =
  Gen.of_list
    ~pp:(fun ppf -> Format.fprintf ppf "%S")
    [""; "a"; "b"; "c d"; "*/*"]

let fields_gen =
  Gen.list ~size:(Gen.int_range 0 8) (Gen.pair name_gen value_gen)

let headers_props =
  group "Headers laws"
    [ prop "to_list (of_list l) = l" fields_gen (fun l ->
          equal fields l (Headers.to_list (h l)) )
    ; prop "find n (set n v h) = Some v"
        (Gen.triple name_gen value_gen fields_gen) (fun (n, v, l) ->
          equal (option string) (Some v)
            (Headers.find n (Headers.set n v (h l))) )
    ; prop "remove n h has no field n" (Gen.pair name_gen fields_gen)
        (fun (n, l) -> is_true (not (Headers.mem n (Headers.remove n (h l)))) )
    ; prop "add keeps the existing fields as a prefix"
        (Gen.triple name_gen value_gen fields_gen) (fun (n, v, l) ->
          equal fields (l @ [(n, v)]) (Headers.to_list (Headers.add n v (h l))) )
    ]

(* Stateful: Headers against an association list written as a specification. *)
module Model = struct
  let same a b = String.lowercase_ascii a = String.lowercase_ascii b

  let empty () = []

  let add n v l = l @ [(n, v)]

  let remove n l = List.filter (fun (n', _) -> not (same n n')) l

  let find_all n l = List.map snd (List.filter (fun (n', _) -> same n n') l)

  let find n l = match find_all n l with [] -> None | v :: _ -> Some v

  let set n v l =
    match List.find_index (fun (n', _) -> same n n') l with
    | None ->
        add n v l
    | Some i ->
        List.filteri (fun j (n', _) -> j = i || not (same n n')) l
        |> List.mapi (fun j f -> if j = i then (n, v) else f)
end

let hs = abstract "h" ~pp:(fun ppf l -> Testable.pp fields ppf l)

let headers_commands =
  [ command "empty" (Gen.unit @-> makes hs) Model.empty (fun () -> Headers.empty)
  ; command "add"
      (name_gen @-> value_gen @-> hs ^-> makes hs)
      Model.add Headers.add
  ; command "set"
      (name_gen @-> value_gen @-> hs ^-> makes hs)
      Model.set Headers.set
  ; command "remove" (name_gen @-> hs ^-> makes hs) Model.remove Headers.remove
  ; command "find"
      (name_gen @-> hs ^-> returns (option string))
      Model.find Headers.find
  ; command "find_all"
      (name_gen @-> hs ^-> returns (list string))
      Model.find_all Headers.find_all
  ; command "to_list" (hs ^-> returns fields) Fun.id Headers.to_list ]

let headers_stateful =
  group "Headers model"
    [stateful "behaves like an association list" headers_commands]

(* Date *)

module Date = Private.Date

let http_date = Testable.structural ~pp:Http_date.pp

let max_time = 253402300799 (* 9999-12-31T23:59:59Z *)

let date_tests =
  group "Date"
    [ test "known dates" (fun () ->
          let enc t = Http_date.encode (Date.of_unix_time t) in
          expect (enc 0.) @@ __POS_OF__ {|Thu, 01 Jan 1970 00:00:00 GMT|} ;
          expect (enc 784111777.)
          @@ __POS_OF__ {|Sun, 06 Nov 1994 08:49:37 GMT|} ;
          expect (enc 951782400.)
          @@ __POS_OF__ {|Tue, 29 Feb 2000 00:00:00 GMT|} ;
          expect (enc (Float.of_int max_time))
          @@ __POS_OF__ {|Fri, 31 Dec 9999 23:59:59 GMT|} )
    ; test "fractions of a second are dropped" (fun () ->
          equal http_date
            (Date.of_unix_time 784111777.)
            (Date.of_unix_time 784111777.9) )
    ; prop "encoding decodes to the same date" (Gen.int_range 0 max_time)
        (fun t ->
          let d = Date.of_unix_time (Float.of_int t) in
          equal http_date d (Http_date.decode (Http_date.encode d)) )
    ; test "of_posix caches one string per second" (fun () ->
          let enc t = Http_date.encode (Date.of_unix_time t) in
          let posix t = Option.get (Affect_unix.Ptime.of_float_s t) in
          let a = Date.of_posix (posix 1000.) in
          let b = Date.of_posix (posix 1000.5) in
          equal string (enc 1000.) a ;
          is_true ~msg:"same second reuses the string" (a == b) ;
          equal string (enc 1001.) (Date.of_posix (posix 1001.)) ;
          equal string (enc 1000.) (Date.of_posix (posix 1000.)) )
    ; test "of_posix drops the fraction of a time before 1970" (fun () ->
          let posix t = Option.get (Affect_unix.Ptime.of_float_s t) in
          equal string "Thu, 01 Jan 1970 00:00:00 GMT"
            (Date.of_posix (posix 0.5)) ;
          equal string "Wed, 31 Dec 1969 23:59:59 GMT"
            (Date.of_posix (posix (-0.5))) ;
          equal string "Wed, 31 Dec 1969 23:59:59 GMT"
            (Date.of_posix (posix (-1.))) )
    ; test "of_posix drops a fraction near 1 s" (fun () ->
          (* 100 ns before midnight: a float rounds it up to the next second. *)
          let p =
            Affect_unix.Ptime.(
              Option.get (of_span (Span.v (20500, 86_399_999_999_900_000L))) )
          in
          equal string "Mon, 16 Feb 2026 23:59:59 GMT" (Date.of_posix p) ) ]

(* Flow *)

(* Reads [flow] to the end, [size] bytes at a time. *)
let read_all ?(size = 4096) flow =
  let buf = Bytes.create size and out = Buffer.create 64 in
  let rec loop () =
    match Flow.read flow buf 0 size with
    | 0 ->
        Buffer.contents out
    | n ->
        Buffer.add_subbytes out buf 0 n ;
        loop ()
  in
  loop ()

let in_main f = Affect_unix.Unix.main ~domain_count:1 f

let readable_within_20ms flow =
  in_main
  @@ fun () ->
  Affect.Action.invoke
    (Affect.Action.choose
       [ Flow.readable' flow true
       ; Affect_unix.Mtime.wait_for' Affect_unix.Mtime.Span.(20 * ms) false ] )

let flow_tests =
  group "Flow"
    [ test "of_string reads the input, then 0 for good" (fun () ->
          let flow = Flow.of_string "hello" (Buffer.create 0) in
          equal string "hello" (read_all flow) ;
          equal int 0 (Flow.read flow (Bytes.create 4) 0 4) )
    ; test "chunk caps each read" (fun () ->
          let flow = Flow.of_string ~chunk:2 "hello" (Buffer.create 0) in
          let buf = Bytes.create 8 in
          equal (list int) [2; 2; 1; 0]
            (List.init 4 (fun _ -> Flow.read flow buf 0 8)) )
    ; test "writes append to the output" (fun () ->
          let out = Buffer.create 16 in
          let flow = Flow.of_string "" out in
          Flow.write flow (Bytes.of_string "xxabcxx") 2 3 ;
          Flow.write_string flow "def" ;
          Flow.write flow (Bytes.of_string "z") 0 0 ;
          equal string "abcdef" (Buffer.contents out) )
    ; test "invalid ranges are rejected" (fun () ->
          let flow = Flow.of_string "abc" (Buffer.create 0) in
          let buf = Bytes.create 4 in
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () -> Flow.read flow buf off len) )
            [(0, 0); (-1, 2); (0, 5); (3, 2)] ;
          raises_match is_invalid_arg (fun () -> Flow.write flow buf 2 3) ;
          raises_match is_invalid_arg (fun () ->
              Flow.of_string ~chunk:0 "" (Buffer.create 0) ) )
    ; test "read_now reads what is there without blocking" (fun () ->
          let buf = Bytes.create 4 in
          let f = Flow.of_string ~chunk:2 "abc" (Buffer.create 0) in
          equal
            (list (option int))
            [Some 2; Some 1; Some 0]
            (List.init 3 (fun _ -> Flow.read_now f buf 0 4)) ;
          let nothing =
            Flow.make ~time:Time.mono
              ~read:(fun _ _ _ -> failwith "blocking read")
              ~read_now:(fun _ _ _ -> None)
              ~write:(fun _ _ _ -> ())
              ~readable:Affect.Action.never ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          let p = Flow.prepend "x" nothing in
          equal
            (list (option int))
            [Some 1; None]
            (List.init 2 (fun _ -> Flow.read_now p buf 0 4)) ;
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () ->
                  Flow.read_now f buf off len ) )
            [(0, 0); (-1, 2); (3, 2)] ;
          Flow.close f ;
          raises Flow.Closed (fun () -> Flow.read_now f buf 0 1) )
    ; test "reset closes once, abortively, and close falls back to it"
        (fun () ->
          let log = ref [] in
          let flow ?reset () =
            Flow.make ~time:Time.mono
              ~read:(fun _ _ _ -> 0)
              ~read_now:(fun _ _ _ -> Some 0)
              ~write:(fun _ _ _ -> ())
              ~readable:(Affect.Action.always ()) ~shutdown_send:ignore
              ~close:(fun () -> log := "close" :: !log)
              ?reset ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          let f = flow ~reset:(fun () -> log := "reset" :: !log) () in
          Flow.reset f ;
          Flow.reset f ;
          Flow.close f ;
          equal (list string) ["reset"] !log ;
          raises Flow.Closed (fun () -> Flow.read f (Bytes.create 1) 0 1) ;
          log := [] ;
          Flow.reset (flow ()) ;
          equal (list string) ~msg:"without reset, close" ["close"] !log )
    ; test "linger sends the end of the stream, then discards at most max bytes"
        (fun () ->
          let log = ref [] and input = ref "abcdef" in
          let flow ?read_error () =
            Flow.make ~time:Time.mono
              ~read:(fun buf off len ->
                Option.iter raise read_error ;
                let n = Int.min (Int.min len 2) (String.length !input) in
                Bytes.blit_string !input 0 buf off n ;
                input := String.sub !input n (String.length !input - n) ;
                log := Printf.sprintf "read %d" n :: !log ;
                n )
              ~read_now:(fun _ _ _ -> None)
              ~write:(fun _ _ _ -> ())
              ~readable:(Affect.Action.always ())
              ~shutdown_send:(fun () -> log := "shutdown" :: !log)
              ~close:ignore ~set_read_deadline:ignore ~set_write_deadline:ignore
              ()
          in
          Flow.linger (flow ()) ~max:3 ;
          equal (list string) ["shutdown"; "read 2"; "read 1"] (List.rev !log) ;
          equal string "def" !input ;
          log := [] ;
          Flow.linger (flow ()) ~max:100 ;
          equal (list string) ~msg:"until the end of the stream"
            ["shutdown"; "read 2"; "read 1"; "read 0"]
            (List.rev !log) ;
          List.iter
            (fun e -> Flow.linger (flow ~read_error:e ()) ~max:100)
            [Flow.Timeout; Flow.Closed] ;
          raises Affect.Fun.Async.Cancelled (fun () ->
              Flow.linger
                (flow ~read_error:Affect.Fun.Async.Cancelled ())
                ~max:100 ) ;
          let f = flow () in
          Flow.close f ;
          log := [] ;
          Flow.linger f ~max:100 ;
          equal (list string) ~msg:"a closed flow" [] !log )
    ; test "close is idempotent and later use raises Closed" (fun () ->
          let closes = ref 0 in
          let flow =
            Flow.make ~time:Time.mono
              ~read:(fun _ _ _ -> 0)
              ~read_now:(fun _ _ _ -> Some 0)
              ~write:(fun _ _ _ -> ())
              ~readable:(Affect.Action.always ()) ~shutdown_send:ignore
              ~close:(fun () -> incr closes)
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          Flow.close flow ;
          Flow.close flow ;
          equal int 1 !closes ;
          raises Flow.Closed (fun () -> Flow.read flow (Bytes.create 1) 0 1) ;
          raises Flow.Closed (fun () -> Flow.write_string flow "x") )
    ; test "make delegates to its functions" (fun () ->
          let log = ref [] in
          let note s = log := s :: !log in
          let flow =
            Flow.make ~time:Time.mono
              ~read:(fun buf off _ -> note "read" ; Bytes.set buf off 'a' ; 1)
              ~read_now:(fun _ _ _ -> note "read_now" ; None)
              ~write:(fun _ _ len -> note (Printf.sprintf "write %d" len))
              ~readable:(Affect.Action.always ())
              ~shutdown_send:(fun () -> note "shutdown")
              ~close:(fun () -> note "close")
              ~set_read_deadline:(fun d ->
                note (if d = None then "rd none" else "rd") )
              ~set_write_deadline:(fun d ->
                note (if d = None then "wd none" else "wd") )
              ()
          in
          equal int 1 (Flow.read flow (Bytes.create 1) 0 1) ;
          equal (option int) None (Flow.read_now flow (Bytes.create 1) 0 1) ;
          Flow.write_string flow "abc" ;
          Flow.write_string flow "" ;
          Flow.shutdown_send flow ;
          Flow.set_read_deadline flow None ;
          Flow.set_write_deadline flow None ;
          Flow.close flow ;
          equal (list string)
            [ "read"
            ; "read_now"
            ; "write 3"
            ; "shutdown"
            ; "rd none"
            ; "wd none"
            ; "close" ]
            (List.rev !log) )
    ; test "prepend reads its bytes first" (fun () ->
          let out = Buffer.create 8 in
          let flow =
            Flow.prepend "ab" (Flow.prepend "" (Flow.of_string "cd" out))
          in
          equal string "abcd" (read_all ~size:3 flow) ;
          Flow.write_string flow "w" ;
          equal string "w" (Buffer.contents out) ;
          equal string "xyz"
            (read_all
               (Flow.prepend "x" (Flow.prepend "y" (Flow.of_string "z" out))) ) )
    ; test "prepend passes shutdown and close to the flow" (fun () ->
          let log = ref [] in
          let flow =
            Flow.make ~time:Time.mono
              ~read:(fun _ _ _ -> 0)
              ~read_now:(fun _ _ _ -> Some 0)
              ~write:(fun _ _ _ -> ())
              ~readable:(Affect.Action.always ())
              ~shutdown_send:(fun () -> log := "shutdown" :: !log)
              ~close:(fun () -> log := "close" :: !log)
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          let p = Flow.prepend "x" flow in
          Flow.shutdown_send p ;
          Flow.close p ;
          equal (list string) ["shutdown"; "close"] (List.rev !log) ;
          let f = Flow.of_string "" (Buffer.create 0) in
          Flow.set_read_deadline f (Some (Affect_unix.Mtime.now ())) ;
          Flow.set_write_deadline f None ;
          Flow.shutdown_send f ;
          equal int 0 (Flow.read f (Bytes.create 1) 0 1) )
    ; test "prepend is readable while its bytes remain" (fun () ->
          let never =
            Flow.make ~time:Time.mono
              ~read:(fun _ _ _ -> 0)
              ~read_now:(fun _ _ _ -> Some 0)
              ~write:(fun _ _ _ -> ())
              ~readable:Affect.Action.never ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          let flow = Flow.prepend "a" never in
          is_true ~msg:"readable before the prefix is read"
            (readable_within_20ms flow) ;
          equal int 1 (Flow.read flow (Bytes.create 4) 0 4) ;
          is_true ~msg:"waits on the underlying flow after"
            (not (readable_within_20ms flow)) )
    ; prop "reads return the input however it is split"
        Gen.(
          pair
            (pair string (int_range 0 64))
            (pair (int_range 1 9) (int_range 1 9)) )
        (fun ((input, cut), (chunk, size)) ->
          let cut = Int.min cut (String.length input) in
          let flow =
            Flow.prepend (String.sub input 0 cut)
              (Flow.of_string ~chunk
                 (String.sub input cut (String.length input - cut))
                 (Buffer.create 0) )
          in
          equal string input (read_all ~size flow) ) ]

(* Body *)

(* A source that returns [s] in pieces of at most [chunk] bytes. *)
let chunked_reader ?(chunk = max_int) s =
  let pos = ref 0 in
  fun buf off len ->
    let n = Int.min (Int.min len chunk) (String.length s - !pos) in
    Bytes.blit_string s !pos buf off n ;
    pos := !pos + n ;
    n

(* A sink recording writes and flushes as events. *)
let recording_writer () =
  let events = ref [] in
  let w =
    Body.Writer.make
      ~write:(fun buf off len ->
        events := Bytes.sub_string buf off len :: !events )
      ~flush:(fun () -> events := "<flush>" :: !events)
  in
  (w, fun () -> List.rev !events)

let trailers_x = Headers.of_list [("X-Checksum", "abc")]

let body_tests =
  group "Body"
    [ test "empty" (fun () ->
          equal (option int) (Some 0) (Body.length Body.empty) ;
          equal int 0 (Body.read Body.empty (Bytes.create 4) 0 4) ;
          Body.close Body.empty ;
          equal string "" (Body.to_string Body.empty) )
    ; test "of_string reads in pieces; to_string returns the rest" (fun () ->
          let b = Body.of_string "hello world" in
          equal (option int) (Some 11) (Body.length b) ;
          let buf = Bytes.create 6 in
          equal int 6 (Body.read b buf 0 6) ;
          equal string "hello " (Bytes.to_string buf) ;
          equal string "world" (Body.to_string b) ;
          equal int 0 (Body.read b buf 0 6) ;
          is_true (Headers.is_empty (Body.trailers b)) )
    ; test "of_source: length, trailers at the end, close once" (fun () ->
          let closes = ref 0 in
          let b =
            Body.of_source
              ~trailers:(fun () -> trailers_x)
              ~close:(fun () -> incr closes)
              (chunked_reader ~chunk:2 "abcde")
          in
          equal (option int) None (Body.length b) ;
          let buf = Bytes.create 8 in
          equal int 2 (Body.read b buf 0 8) ;
          is_true ~msg:"no trailers before the end"
            (Headers.is_empty (Body.trailers b)) ;
          equal string "cde" (Body.to_string b) ;
          is_true (Headers.equal trailers_x (Body.trailers b)) ;
          Body.close b ;
          equal int 1 !closes ;
          Body.close b ;
          equal int 1 !closes ;
          raises_match is_invalid_arg (fun () -> Body.read b buf 0 8) )
    ; test "invalid lengths and ranges" (fun () ->
          raises_match is_invalid_arg (fun () ->
              Body.of_source ~length:(-1) (chunked_reader "") ) ;
          raises_match is_invalid_arg (fun () ->
              Body.of_writer ~length:(-1) ignore ) ;
          (* A source and a sink that never touch the buffer, so only the range
             checks can raise. *)
          let b = Body.of_source (fun _ _ _ -> 0) in
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () ->
                  Body.read b (Bytes.create 4) off len ) )
            [(0, 0); (-1, 1); (2, 3); (4, 1)] ;
          let w = Body.Writer.make ~write:(fun _ _ _ -> ()) ~flush:ignore in
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () ->
                  Body.Writer.write w (Bytes.create 2) off len ) )
            [(-1, 1); (0, -1); (1, 2); (3, 0)] )
    ; test "of_writer runs its producer into the sink" (fun () ->
          let b =
            Body.of_writer ~length:6 (fun w ->
                Body.Writer.write_string w "abc" ;
                Body.Writer.flush w ;
                Body.Writer.write w (Bytes.of_string "xdefx") 1 3 ;
                Body.Writer.write_string w "" ;
                Body.Writer.set_trailers w trailers_x )
          in
          equal (option int) (Some 6) (Body.length b) ;
          raises_match is_invalid_arg (fun () ->
              Body.read b (Bytes.create 4) 0 4 ) ;
          let w, events = recording_writer () in
          Body.write_to b w ;
          equal (list string) ["abc"; "<flush>"; "def"] (events ()) ;
          is_true (Headers.equal trailers_x (Body.Writer.trailers w)) ;
          is_true (Headers.equal trailers_x (Body.trailers b)) ;
          raises_match is_invalid_arg (fun () -> Body.write_to b w) )
    ; test "the writer of a producer stops when the producer returns" (fun () ->
          let kept = ref None in
          let keep w =
            kept := Some w ;
            Body.Writer.write_string w "abc"
          in
          let w, events = recording_writer () in
          Body.write_to (Body.of_writer keep) w ;
          let own = Option.get !kept in
          raises_match is_invalid_arg (fun () ->
              Body.Writer.write_string own "late" ) ;
          raises_match is_invalid_arg (fun () -> Body.Writer.flush own) ;
          raises_match is_invalid_arg (fun () ->
              Body.Writer.set_trailers own trailers_x ) ;
          equal (list string) ["abc"] (events ()) ;
          (* Also after the producer raises. *)
          let raising w =
            kept := Some w ;
            failwith "producer"
          in
          let w, events = recording_writer () in
          raises (Failure "producer") (fun () ->
              Body.write_to (Body.of_writer raising) w ) ;
          raises_match is_invalid_arg (fun () ->
              Body.Writer.write_string (Option.get !kept) "late" ) ;
          equal (list string) [] (events ()) ;
          (* The sink of the consumer stays usable. *)
          Body.Writer.write_string w "next" ;
          equal (list string) ["next"] (events ()) )
    ; test "a closed writer body never runs" (fun () ->
          let ran = ref false in
          let b = Body.of_writer (fun _ -> ran := true) in
          Body.close b ;
          let w, _ = recording_writer () in
          raises_match is_invalid_arg (fun () -> Body.write_to b w) ;
          is_true (not !ran) )
    ; test "write_to flushes a source after each chunk" (fun () ->
          let b =
            Body.of_source
              ~trailers:(fun () -> trailers_x)
              (chunked_reader ~chunk:2 "abcde")
          in
          let w, events = recording_writer () in
          Body.write_to b w ;
          equal (list string)
            ["ab"; "<flush>"; "cd"; "<flush>"; "e"; "<flush>"]
            (events ()) ;
          is_true (Headers.equal trailers_x (Body.Writer.trailers w)) )
    ; test "to_string ~max" (fun () ->
          equal string "abc" (Body.to_string ~max:3 (Body.of_string "abc")) ;
          raises Body.Too_large (fun () ->
              Body.to_string ~max:2 (Body.of_string "abc") ) ;
          raises Body.Too_large (fun () ->
              Body.to_string ~max:2
                (Body.of_writer (fun w -> Body.Writer.write_string w "abc")) ) )
    ; prop "a source returns its bytes however they are split"
        Gen.(pair string (pair (int_range 1 9) (int_range 1 9)))
        (fun (s, (chunk, size)) ->
          let b = Body.of_source (chunked_reader ~chunk s) in
          let buf = Bytes.create size and out = Buffer.create 16 in
          let rec loop () =
            match Body.read b buf 0 size with
            | 0 ->
                Buffer.contents out
            | n ->
                Buffer.add_subbytes out buf 0 n ;
                loop ()
          in
          equal string s (loop ()) )
    ; prop "write_to and to_string agree for every shape"
        Gen.(pair string (int_range 1 9))
        (fun (s, chunk) ->
          let shapes () =
            [ Body.of_string s
            ; Body.of_source (chunked_reader ~chunk s)
            ; Body.of_writer (fun w -> Body.Writer.write_string w s) ]
          in
          List.iter2
            (fun b b' ->
              let out = Buffer.create 16 in
              Body.write_to b
                (Body.Writer.make ~write:(Buffer.add_subbytes out) ~flush:ignore) ;
              equal string (Body.to_string b') (Buffer.contents out) )
            (shapes ()) (shapes ()) ) ]

(* Conn_info, Request, Response, Handler *)

let loopback port = Unix.ADDR_INET (Unix.inet_addr_loopback, port)

let conn_info_tests =
  group "Conn_info"
    [ test "accessors and pp" (fun () ->
          let c =
            Conn_info.make ~id:7 ~peer:(loopback 51000) ~local:(loopback 8080)
              ()
          in
          equal int 7 (Conn_info.id c) ;
          is_true (Conn_info.peer c = loopback 51000) ;
          is_true (Conn_info.local c = loopback 8080) ;
          is_true (Conn_info.scheme c = `Http) ;
          equal (option string) None (Conn_info.alpn c) ;
          expect (Format.asprintf "%a" Conn_info.pp c)
          @@ __POS_OF__ {|#7 127.0.0.1:51000 -> 127.0.0.1:8080|} ;
          let c6 =
            Conn_info.make ~id:1
              ~peer:(Unix.ADDR_INET (Unix.inet6_addr_loopback, 1))
              ~local:(Unix.ADDR_UNIX "/run/s.sock") ~scheme:`Https ~alpn:"h2" ()
          in
          is_true (Conn_info.scheme c6 = `Https) ;
          equal (option string) (Some "h2") (Conn_info.alpn c6) ;
          expect (Format.asprintf "%a" Conn_info.pp c6)
          @@ __POS_OF__ {|#1 [::1]:1 -> /run/s.sock|} )
    ; test "the stopping action of a connection is never enabled by default"
        (fun () ->
          let c =
            Conn_info.make ~id:1 ~peer:(loopback 1) ~local:(loopback 2) ()
          in
          let enabled =
            Affect_unix.Unix.main ~domain_count:1 (fun () ->
                Affect.Action.(
                  invoke
                    (choose
                       [ map (fun () -> true) (Conn_info.stopping c)
                       ; always false ] ) ) )
          in
          is_false enabled ) ]

let request_tests =
  group "Request"
    [ test "defaults" (fun () ->
          let r = Request.make Method.GET "/" in
          equal meth Method.GET (Request.meth r) ;
          equal version Version.Http_1_1 (Request.version r) ;
          equal string "http" (Request.scheme r) ;
          equal (option string) None (Request.authority r) ;
          is_true (Headers.is_empty (Request.headers r)) ;
          equal (option int) (Some 0) (Body.length (Request.body r)) ;
          is_true (Request.conn r = None) )
    ; test "path_only and query" (fun () ->
          let split p =
            let r = Request.make Method.GET p in
            (Request.path_only r, Request.query r)
          in
          let pq = pair string (option string) in
          equal pq ("/a", Some "b=1&c=?") (split "/a?b=1&c=?") ;
          equal pq ("/a", None) (split "/a") ;
          equal pq ("/a", Some "") (split "/a?") ;
          equal pq ("*", None) (split "*") )
    ; test "with_* derive new requests" (fun () ->
          let r = Request.make ~authority:"a.example" Method.POST "/x" in
          let h = Headers.of_list [("X-A", "1")] in
          let r' =
            r |> Request.with_path "/y"
            |> Request.with_authority (Some "b.example:8080")
            |> Request.with_headers h
            |> Request.with_body (Body.of_string "hi")
          in
          equal string "/x" (Request.path r) ;
          equal (option string) (Some "a.example") (Request.authority r) ;
          equal string "/y" (Request.path r') ;
          equal (option string) (Some "b.example:8080") (Request.authority r') ;
          is_true (Headers.equal h (Request.headers r')) ;
          equal string "hi" (Body.to_string (Request.body r')) ;
          equal (option string) None
            (Request.authority (Request.with_authority None r')) )
    ; test "rejects paths and authorities that would corrupt a request line"
        (fun () ->
          List.iter
            (fun bad ->
              raises_match is_invalid_arg (fun () ->
                  Request.make Method.GET bad ) ;
              raises_match is_invalid_arg (fun () ->
                  Request.make ~authority:bad Method.GET "/" ) ;
              let r = Request.make Method.GET "/" in
              raises_match is_invalid_arg (fun () -> Request.with_path bad r) ;
              raises_match is_invalid_arg (fun () ->
                  Request.with_authority (Some bad) r ) )
            ["/a b"; "/a\r\nX: y"; "/a\n"; "/\000"; "/\127"; "/\t"] )
    ; test "pp" (fun () ->
          let r =
            Request.make ~authority:"example.com"
              ~headers:(Headers.of_list [("Accept", "*/*")])
              Method.GET "/a?b"
          in
          expect (Format.asprintf "%a" Request.pp r)
          @@ __POS_OF__ {|GET http://example.com/a?b HTTP/1.1
Accept: */*|} ) ]

let content_type r = Headers.find "content-type" (Response.headers r)

let body_of r =
  match Response.kind r with
  | Response.Body b ->
      b
  | Response.Upgrade _ ->
      fail "expected a body"

let response_tests =
  group "Response"
    [ test "make defaults to 200" (fun () ->
          let r = Response.make Body.empty in
          equal status Status.ok (Response.status r) ;
          is_true (Headers.is_empty (Response.headers r)) )
    ; test "string sets a Content-Type" (fun () ->
          let r = Response.string "hello" in
          equal (option string) (Some "text/plain; charset=utf-8")
            (content_type r) ;
          equal (option int) (Some 5) (Body.length (body_of r)) ;
          equal string "hello" (Body.to_string (body_of r)) ;
          equal (option string) (Some "application/json")
            (content_type
               (Response.string ~content_type:"application/json" "{}") ) ;
          let h = Headers.of_list [("Content-Type", "text/html")] in
          equal (option string) (Some "text/html")
            (content_type (Response.string ~headers:h "<p>")) ;
          equal (option string) (Some "text/csv")
            (content_type
               (Response.string ~headers:h ~content_type:"text/csv" "a,b") ) )
    ; test "empty is 204 with no body" (fun () ->
          let r = Response.empty () in
          equal status Status.no_content (Response.status r) ;
          equal (option int) (Some 0) (Body.length (body_of r)) )
    ; test "stream keeps its length and producer" (fun () ->
          let r =
            Response.stream ~length:3 (fun w ->
                Body.Writer.write_string w "abc" )
          in
          equal (option int) (Some 3) (Body.length (body_of r)) ;
          equal string "abc" (Body.to_string (body_of r)) )
    ; test "upgrade accepts 101 and 2xx only" (fun () ->
          let r = Response.upgrade ignore in
          equal status Status.switching_protocols (Response.status r) ;
          is_true
            ( match Response.kind r with
            | Response.Upgrade _ ->
                true
            | _ ->
                false ) ;
          equal status Status.ok
            (Response.status (Response.upgrade ~status:Status.ok ignore)) ;
          List.iter
            (fun s ->
              raises_match is_invalid_arg (fun () ->
                  Response.upgrade ~status:s ignore ) )
            Status.[continue; not_found; moved_permanently] )
    ; test "statuses must suit the response" (fun () ->
          let early = Status.v 103 in
          List.iter
            (fun (label, f) -> raises_match ~msg:label is_invalid_arg f)
            [ ( "make 103"
              , fun () -> ignore (Response.make ~status:early Body.empty) )
            ; ( "string 101"
              , fun () ->
                  ignore
                    (Response.string ~status:Status.switching_protocols "x") )
            ; ( "empty 100"
              , fun () -> ignore (Response.empty ~status:Status.continue ()) )
            ; ( "stream 103"
              , fun () -> ignore (Response.stream ~status:early ignore) )
            ; ( "with_status 103 on a body"
              , fun () ->
                  ignore (Response.with_status early (Response.string "x")) )
            ; ( "with_status 404 on an upgrade"
              , fun () ->
                  ignore
                    (Response.with_status Status.not_found
                       (Response.upgrade ignore) ) ) ] ;
          let tunnel =
            Response.with_status Status.ok (Response.upgrade ignore)
          in
          equal status Status.ok (Response.status tunnel) )
    ; test "with_status and with_headers" (fun () ->
          let r = Response.string "x" in
          let r' =
            Response.with_status Status.created
              (Response.with_headers Headers.empty r)
          in
          equal status Status.ok (Response.status r) ;
          equal status Status.created (Response.status r') ;
          is_true (Headers.is_empty (Response.headers r')) )
    ; test "pp" (fun () ->
          expect (Format.asprintf "%a" Response.pp (Response.string "x"))
          @@ __POS_OF__ {|200 OK
Content-Type: text/plain; charset=utf-8|} ;
          expect (Format.asprintf "%a" Response.pp (Response.upgrade ignore))
          @@ __POS_OF__ {|101 Switching Protocols (upgrade)|} ) ]

let handler_tests =
  group "Handler"
    [ test "compose runs the first middleware outermost" (fun () ->
          let log = Buffer.create 32 in
          let note s = Buffer.add_string log s in
          let mw name : Handler.middleware =
           fun h req ->
            note ("<" ^ name) ;
            let r = h req in
            note (name ^ ">") ;
            r
          in
          let h =
            Handler.compose
              [mw "a"; mw "b"]
              (fun _ -> note "h" ; Response.empty ())
          in
          ignore (h (Request.make Method.GET "/")) ;
          expect (Buffer.contents log) @@ __POS_OF__ {|<a<bhb>a>|} ;
          let r =
            Handler.compose [] Handler.not_found (Request.make Method.GET "/")
          in
          equal status Status.not_found (Response.status r) ) ]

(* Time *)

(* 2^63 ns + 1 s. [Time.after] can represent the deadline, and the span from the
   invoke to the deadline stays 2^63 ns or more. affect ends a wait of 2^63 ns
   or more at once: it compares with a signed comparison. *)
let beyond_2_63_ns = Affect_unix.Mtime.Span.(add (of_uint64_ns Int64.min_int) s)

(* Whether [action] is enabled within 50 ms. *)
let enabled_within_50ms action =
  in_main (fun () -> Fake_time.within_50ms action)

let time_tests =
  group "Time"
    [ test "a deadline that has passed is enabled at once" (fun () ->
          (* 1 s ago: a timer that waits for the time that has passed waits 1
             s. *)
          let d = Affect_unix.Mtime.(sub_span (now ()) Span.s) in
          is_true ~msg:"representable" (Option.is_some d) ;
          is_true (enabled_within_50ms (Time.at Time.mono d ())) )
    ; test "a timer reads the time when it is invoked" (fun () ->
          let timer =
            Time.at Time.mono
              (Time.after Time.mono Affect_unix.Mtime.Span.(100 * ms))
              ()
          in
          in_main (fun () ->
              Affect_unix.Mtime.wait_for Affect_unix.Mtime.Span.(100 * ms) ) ;
          is_true (enabled_within_50ms timer) )
    ; test "a deadline 10 ms ahead is enabled" (fun () ->
          let d = Time.after Time.mono Affect_unix.Mtime.Span.(10 * ms) in
          (* A 2 s window: a slow machine can take more than 50 ms. *)
          is_true
            (in_main (fun () ->
                 Affect.Action.invoke
                   (Affect.Action.choose
                      [ Time.at Time.mono d true
                      ; Affect_unix.Mtime.wait_for'
                          Affect_unix.Mtime.Span.(2 * s)
                          false ] ) ) ) )
    ; test "no deadline is never enabled" (fun () ->
          is_false (enabled_within_50ms (Time.at Time.mono None ())) )
    ; test "a deadline 2^63 ns or more ahead is never enabled" (fun () ->
          let d = Time.after Time.mono beyond_2_63_ns in
          is_true ~msg:"representable" (Option.is_some d) ;
          is_false (enabled_within_50ms (Time.at Time.mono d ())) )
    ; test "the last time is never enabled" (fun () ->
          let d = Some Affect_unix.Mtime.max_stamp in
          is_false (enabled_within_50ms (Time.at Time.mono d ())) )
    ; test "the library reads the time and waits only through Time" (fun () ->
          (* dune runs the test in _build/default/test; dune exec in the
             root. *)
          let root = if Sys.file_exists "../src/time.ml" then ".." else "." in
          let read dir file =
            In_channel.with_open_bin
              (Filename.concat (Filename.concat root dir) file)
              In_channel.input_all
          in
          let contains s sub =
            let n = String.length sub in
            let rec at i =
              i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
            in
            at 0
          in
          let direct =
            [ "Mtime.now"
            ; "Mtime.wait_for"
            ; "Mtime.observe_wait_for"
            ; "Mtime.count"
            ; "Mtime.elapsed"
            ; "Mtime.("
            ; "Unix.time"
            ; "Unix.gettimeofday"
            ; "Unix.sleep"
            ; "Unix.select"
            ; "Ptime.now"
            ; "Ptime.wait_until"
            ; "Ptime.observe_wait_until"
            ; "Ptime.(" ]
          in
          (* The server, the client and the WebSocket use the time line of their
             config or of their flows, also to add spans. *)
          let injected =
            [ "Time.mono"
            ; "Time.posix"
            ; "Mtime.add_span"
            ; "Ptime.add_span"
            ; "Time.("
            ; "open Time"
            ; "open Caravan.Time" ]
          in
          let files dir ~except =
            Sys.readdir (Filename.concat root dir)
            |> Array.to_list
            |> List.filter (fun file ->
                Filename.check_suffix file ".ml" && not (List.mem file except) )
          in
          let src = files "src" ~except:["time.ml"]
          and client = files "client" ~except:[]
          and websocket = files "websocket" ~except:[] in
          is_true ~msg:"the files of the server are found"
            (List.mem "server.ml" src && List.mem "h1_server.ml" src) ;
          is_true ~msg:"the files of the client are found"
            (List.mem "pool.ml" client && List.mem "config.ml" client) ;
          is_true ~msg:"the files of the WebSocket are found"
            (List.mem "connection.ml" websocket) ;
          let check dir file calls =
            let text = read dir file in
            List.iter
              (fun call ->
                is_false
                  ~msg:(dir ^ "/" ^ file ^ " calls " ^ call)
                  (contains text call) )
              calls
          in
          List.iter
            (fun file ->
              check "src" file
                ( if List.mem file ["server.ml"; "h1_server.ml"] then
                    direct @ injected
                  else direct ) )
            src ;
          (* client/config.ml has the default time line. *)
          List.iter
            (fun file ->
              check "client" file
                (if file = "config.ml" then direct else direct @ injected) )
            client ;
          List.iter
            (fun file -> check "websocket" file (direct @ injected))
            websocket )
    ; test "the POSIX time line reads the system clock" (fun () ->
          let module Ptime = Affect_unix.Ptime in
          let d = Ptime.diff (Time.now Time.posix) (Ptime.now ()) in
          is_true ~msg:"within 2 s"
            Ptime.Span.(is_shorter (abs d) ~than:(of_int_s 2)) ;
          let t = Time.now Time.posix in
          let d = Option.get (Time.after Time.posix (Ptime.Span.of_int_s 1)) in
          is_false ~msg:"after adds the span"
            Ptime.(
              is_earlier d ~than:(Option.get (add_span t (Span.of_int_s 1))) ) ;
          is_true ~msg:"within 2 s of the span"
            Ptime.(
              is_earlier d ~than:(Option.get (add_span t (Span.of_int_s 3))) ) ;
          is_true ~msg:"the default of the config"
            (Config.default.posix_time == Time.posix) )
    ; test "a POSIX deadline that has passed is enabled at once" (fun () ->
          let now = Affect_unix.Ptime.now () in
          is_true (enabled_within_50ms (Time.at Time.posix (Some now) ())) )
    ; test "a POSIX timer is enabled at its deadline" (fun () ->
          let module Ptime = Affect_unix.Ptime in
          let module Mtime = Affect_unix.Mtime in
          in_main
          @@ fun () ->
          let t0 = Mtime.now () in
          let span = Ptime.Span.of_mtime_span Mtime.Span.(20 * ms) in
          let p = Option.get (Ptime.add_span (Ptime.now ()) span) in
          ( match
              Affect.Action.invoke
                (Affect.Action.choose
                   [ Time.at Time.posix (Some p) `Enabled
                   ; Mtime.wait_for' Mtime.Span.(2 * s) `Late ] )
            with
          | `Enabled ->
              ()
          | `Late ->
              fail "not enabled after 2 s" ) ;
          is_false ~msg:"the POSIX clock before the deadline"
            (Ptime.is_earlier (Ptime.now ()) ~than:p) ;
          is_false ~msg:"less than 20 ms"
            Mtime.Span.(
              is_shorter (Mtime.span t0 (Mtime.now ())) ~than:(20 * ms) ) )
    ; test "a sleep waits for its span" (fun () ->
          in_main
          @@ fun () ->
          let t0 = Affect_unix.Mtime.now () in
          Time.sleep Time.mono Affect_unix.Mtime.Span.(20 * ms) ;
          let slept = Affect_unix.Mtime.(span t0 (now ())) in
          is_false ~msg:"at least 20 ms"
            Affect_unix.Mtime.Span.(is_shorter slept ~than:(20 * ms)) ) ]

(* Fake time *)

module Fake = Fake_time

let ms n = Affect_unix.Mtime.Span.(n * ms)

let sec n = Affect_unix.Mtime.Span.(n * s)

let ( +: ) = Fake.( +: )

(* The calls of a test. A test that fails must not leave a call that waits for
   ever: [Unix.main] would wait for it. *)
let calls = ref []

(* One domain unless TEST_DOMAINS says otherwise. *)
let domain_count =
  Option.value ~default:1
    (Option.bind (Sys.getenv_opt "TEST_DOMAINS") int_of_string_opt)

(* [f] in [Unix.main]. At its end, the calls of the test are cancelled. *)
let fake_main ?(domain_count = domain_count) f =
  calls := [] ;
  Affect_unix.Unix.main ~domain_count
  @@ fun () ->
  Fun.protect ~finally:(fun () -> List.iter (fun cancel -> cancel ()) !calls) f

(* A call that waits for the fake timer at [d]. *)
let timer_call f d =
  let c =
    Affect.Fun.Async.call (fun () ->
        Affect.Action.invoke (Time.at (Fake.mono f) (Some d) ()) )
  in
  calls := (fun () -> Affect.Fun.Async.cancel c) :: !calls ;
  c

(* A call that waits for the fake timer at [d], made after a mark, and the proof
   that the call made its timer. *)
let waiting_call f d =
  let m = Fake.mark f in
  let c = timer_call f d in
  Fake.wait_mono_timer f ~since:m d ;
  c

let ends_within_50ms = Fake.ends_within_50ms

let get_2s = Server_support.get_2s

let fake_time_tests =
  group "Fake time"
    [ test "a timer is enabled when the time reaches its deadline" (fun () ->
          fake_main
          @@ fun () ->
          let f = Fake.make () in
          let d = Fake.mono_now f +: sec 10 in
          let c = waiting_call f d in
          Fake.advance f Affect_unix.Mtime.Span.(sec 10 - ns) ;
          is_false ~msg:"at the deadline - 1 ns" (ends_within_50ms c) ;
          Fake.advance f Affect_unix.Mtime.Span.ns ;
          (* At the deadline, the call ends. *)
          get_2s c ;
          (* The timer is first: it is enabled, so it wins. *)
          let d = Fake.mono_now f +: sec 1 in
          Fake.advance f (sec 1) ;
          let first =
            Affect.Action.invoke
              (Affect.Action.choose
                 [ Time.at (Fake.mono f) (Some d) `Timer
                 ; Affect.Action.always `Always ] )
          in
          is_true ~msg:"the timer wins" (first = `Timer) )
    ; test "a timer made after its deadline is enabled at once" (fun () ->
          fake_main
          @@ fun () ->
          let f = Fake.make () in
          let d = Fake.mono_now f +: ms 500 in
          Fake.advance f (sec 1) ;
          (* A timer that is due at once is in the log too. *)
          let m = Fake.mark f in
          let c = timer_call f d in
          Fake.wait_mono_timer f ~since:m d ;
          get_2s c )
    ; test "a wait for a timer sees only the timers after its mark" (fun () ->
          fake_main
          @@ fun () ->
          let f = Fake.make () in
          let d = Fake.mono_now f +: sec 10 in
          let c1 = waiting_call f d in
          let m = Fake.mark f in
          ( match Fake.wait_mono_timer ~guard:(ms 50) f ~since:m d with
          | () ->
              fail "a timer before the mark"
          | exception Fake.No_timer _ ->
              () ) ;
          let c2 = waiting_call f d in
          ( match
              Fake.wait_mono_timer ~guard:(ms 50) f ~since:m
                (d +: Affect_unix.Mtime.Span.ns)
            with
          | () ->
              fail "a timer at a different deadline"
          | exception Fake.No_timer _ ->
              () ) ;
          Fake.advance f (sec 10) ;
          get_2s c1 ;
          get_2s c2 )
    ; test "one advance wakes the calls on all domains" (fun () ->
          (* The test needs several domains. TEST_DOMAINS=1 asks for one, for
             example in mutation testing, which cannot fork after a domain
             starts. *)
          if Sys.getenv_opt "TEST_DOMAINS" = Some "1" then
            skip ~reason:"TEST_DOMAINS=1 asks for one domain" () ;
          fake_main ~domain_count:4
          @@ fun () ->
          let f = Fake.make () in
          let d = Fake.mono_now f +: sec 10 in
          let waiting = List.init 8 (fun _ -> waiting_call f d) in
          Fake.advance f (sec 10) ;
          List.iter get_2s waiting )
    ; test "with_call_at_timer waits for the timer, and cancels the call"
        (fun () ->
          fake_main
          @@ fun () ->
          let f = Fake.make () in
          let d = Fake.mono_now f +: sec 10 in
          let call = ref None in
          Fake.with_call_at_timer f d
            (fun () -> Affect.Action.invoke (Time.at (Fake.mono f) (Some d) ()))
            (fun c ->
              call := Some c ;
              is_false ~msg:"waits at the timer" (ends_within_50ms c) ) ;
          let c = Option.get !call in
          raises Affect.Fun.Async.Cancelled (fun () -> Affect.Fun.Async.get c) ;
          (* A call that makes no timer: No_timer after the guard. *)
          raises_match
            (function Fake.No_timer _ -> true | _ -> false)
            (fun () -> Fake.with_call_at_timer f d (fun () -> ()) (fun _ -> ())) )
    ; test "the fake time works outside Unix.main" (fun () ->
          let f = Fake.make () in
          let t0 = Time.now (Fake.mono f) in
          is_true ~msg:"one day after the real time"
            Affect_unix.Mtime.(is_later t0 ~than:(now () +: Span.hour)) ;
          Fake.advance f (sec 3) ;
          is_true ~msg:"3 s later"
            (Affect_unix.Mtime.equal (Time.now (Fake.mono f)) (t0 +: sec 3)) ;
          is_true ~msg:"the same value" (Fake.mono f == Fake.mono f) ) ]

(* The fake POSIX clock *)

module Ptime = Affect_unix.Ptime

let fake_posix_tests =
  group "Fake POSIX clock"
    [ test "advance moves the two time lines" (fun () ->
          let f = Fake.make () in
          let m0 = Fake.mono_now f and p0 = Time.now (Fake.posix f) in
          equal float_exact ~msg:"the POSIX start" 784111777.
            (Ptime.to_float_s p0) ;
          Fake.advance f (sec 3) ;
          is_true ~msg:"mono"
            (Affect_unix.Mtime.equal (Fake.mono_now f) (m0 +: sec 3)) ;
          is_true ~msg:"POSIX"
            (Ptime.equal
               (Time.now (Fake.posix f))
               (Option.get (Ptime.add_span p0 (Ptime.Span.of_int_s 3))) ) ;
          is_true ~msg:"the same value" (Fake.posix f == Fake.posix f) )
    ; test "an advance out of range changes nothing" (fun () ->
          fake_main
          @@ fun () ->
          let f = Fake.make () in
          let m0 = Fake.mono_now f and p0 = Time.now (Fake.posix f) in
          let c = waiting_call f (m0 +: sec 1) in
          raises_match
            (function Invalid_argument _ -> true | _ -> false)
            (fun () -> Fake.advance f Affect_unix.Mtime.Span.max_span) ;
          is_true ~msg:"mono" (Affect_unix.Mtime.equal (Fake.mono_now f) m0) ;
          is_true ~msg:"POSIX" (Ptime.equal (Time.now (Fake.posix f)) p0) ;
          is_false ~msg:"no timer enabled" (ends_within_50ms c) ) ]

(* The time line of flows *)

(* A connected pair of non-blocking sockets. *)
let socket_pair () =
  let a, b = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock a ; Unix.set_nonblock b ; (a, b)

(* A call that reads one byte of [flow]. *)
let read_call flow =
  let c =
    Affect.Fun.Async.call (fun () -> Flow.read flow (Bytes.create 1) 0 1)
  in
  calls := (fun () -> Affect.Fun.Async.cancel c) :: !calls ;
  c

(* The steps of F2 on [flow], which reads from the peer [peer]: a read with the
   deadline L = 11 s gets a byte 1 ns before L, and a second read with the same
   deadline times out at L. *)
let read_until_deadline f ~deadline flow peer =
  Flow.set_read_deadline flow (Some deadline) ;
  let m = Fake.mark f in
  let r = read_call flow in
  Fake.wait_mono_timer f ~since:m deadline ;
  Fake.advance f Affect_unix.Mtime.Span.(sec 11 - ns) ;
  ignore (Unix.write_substring peer "x" 0 1) ;
  equal int ~msg:"a byte 1 ns before the deadline" 1 (get_2s r) ;
  let m = Fake.mark f in
  let r = read_call flow in
  Fake.wait_mono_timer f ~since:m deadline ;
  Fake.advance f Affect_unix.Mtime.Span.ns ;
  raises ~msg:"a timeout at the deadline" Flow.Timeout (fun () -> get_2s r)

let flow_time_tests =
  group "Flow time"
    [ test "a flow keeps its time line" (fun () ->
          let f = Fake.make () and a, b = socket_pair () in
          Fun.protect ~finally:(fun () -> Unix.close a ; Unix.close b)
          @@ fun () ->
          let flow = Tcp.flow ~time:(Fake.mono f) a in
          is_true ~msg:"Tcp.flow ~time" (Flow.time flow == Fake.mono f) ;
          is_true ~msg:"prepend"
            (Flow.time (Flow.prepend "x" flow) == Fake.mono f) ;
          is_true ~msg:"the default of Tcp.flow"
            (Flow.time (Tcp.flow b) == Time.mono) ;
          is_true ~msg:"of_string"
            (Flow.time (Flow.of_string "" (Buffer.create 0)) == Time.mono) )
    ; test "a tcp flow waits for its deadlines on its time line" (fun () ->
          let a, b = socket_pair () in
          Fun.protect ~finally:(fun () -> Unix.close a ; Unix.close b)
          @@ fun () ->
          fake_main
          @@ fun () ->
          let f = Fake.make () in
          let flow = Tcp.flow ~time:(Fake.mono f) a in
          let deadline = Option.get (Time.after (Flow.time flow) (sec 11)) in
          read_until_deadline f ~deadline flow b )
    ; test "a wrapper flow with the time line of its inner flow" (fun () ->
          let a, b = socket_pair () in
          Fun.protect ~finally:(fun () -> Unix.close a ; Unix.close b)
          @@ fun () ->
          fake_main
          @@ fun () ->
          let f = Fake.make () in
          let inner = Tcp.flow ~time:(Fake.mono f) a in
          let wrapper =
            Flow.make ~time:(Flow.time inner) ~read:(Flow.read inner)
              ~read_now:(Flow.read_now inner) ~write:(Flow.write inner)
              ~readable:(Flow.readable' inner ())
              ~shutdown_send:(fun () -> Flow.shutdown_send inner)
              ~close:(fun () -> Flow.close inner)
              ~set_read_deadline:(Flow.set_read_deadline inner)
              ~set_write_deadline:(Flow.set_write_deadline inner)
              ()
          in
          let deadline = Option.get (Time.after (Flow.time wrapper) (sec 11)) in
          read_until_deadline f ~deadline wrapper b ) ]

let () =
  exit
    (run "core"
       [ method_tests
       ; version_tests
       ; status_tests
       ; headers_tests
       ; headers_props
       ; headers_stateful
       ; date_tests
       ; flow_tests
       ; body_tests
       ; conn_info_tests
       ; request_tests
       ; response_tests
       ; handler_tests
       ; time_tests
       ; fake_time_tests
       ; fake_posix_tests
       ; flow_time_tests ] )
