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
          (* With no reason, no space follows the code. *)
          equal string "299" (Format.asprintf "%a" Status.pp (Status.v 299)) )
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
          equal (option string) None (Headers.find "Host" h) ;
          (* The compare starts at the first byte of the name. *)
          equal (option string) None (Headers.find "Xontent-Type" h) )
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
              raises_match ~msg:"add" is_invalid_arg (fun () ->
                  Headers.add n v Headers.empty ) ;
              raises_match ~msg:"set" is_invalid_arg (fun () ->
                  Headers.set n v Headers.empty ) ;
              raises_match ~msg:"of_list" is_invalid_arg (fun () ->
                  h [("Host", "x"); (n, v)] ) )
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

(* The last whole second of [Clock.posix]: Fri, 11 Apr 2262 23:47:16 GMT. *)
let max_time = 9223372036

let posix t = Weft.Time.posix_of_unix_s t

(* The date of the POSIX time [t], in seconds. *)
let date_of t = Date.of_utc (Weft.Time.to_utc (posix t))

let enc t = Http_date.encode (date_of t)

let date_tests =
  group "Date"
    [ test "known dates" (fun () ->
          expect (enc 0.) @@ __POS_OF__ {|Thu, 01 Jan 1970 00:00:00 GMT|} ;
          expect (enc 784111777.)
          @@ __POS_OF__ {|Sun, 06 Nov 1994 08:49:37 GMT|} ;
          expect (enc 951782400.)
          @@ __POS_OF__ {|Tue, 29 Feb 2000 00:00:00 GMT|} ;
          expect (enc (Float.of_int max_time))
          @@ __POS_OF__ {|Fri, 11 Apr 2262 23:47:16 GMT|} ;
          (* In the first second of the range of [Clock.posix]. *)
          expect (enc (-9223372036.5))
          @@ __POS_OF__ {|Tue, 21 Sep 1677 00:12:43 GMT|} )
    ; test "fractions of a second are dropped" (fun () ->
          equal http_date (date_of 784111777.) (date_of 784111777.9) )
    ; prop "encoding decodes to the same date" (Gen.int_range 0 max_time)
        (fun t ->
          let d = date_of (Float.of_int t) in
          equal http_date d (Http_date.decode (Http_date.encode d)) )
    ; test "of_posix caches one string per second" (fun () ->
          let a = Date.of_posix (posix 1000.) in
          let b = Date.of_posix (posix 1000.5) in
          equal string (enc 1000.) a ;
          is_true ~msg:"same second reuses the string" (a == b) ;
          equal string (enc 1001.) (Date.of_posix (posix 1001.)) ;
          equal string (enc 1000.) (Date.of_posix (posix 1000.)) )
    ; test "to_imf has no cache" (fun () ->
          let a = Date.of_posix (posix 1000.) in
          List.iter
            (fun (t, text) -> equal string text (Date.to_imf (posix t)))
            [ (784111777.9, "Sun, 06 Nov 1994 08:49:37 GMT")
            ; (-1.5, "Wed, 31 Dec 1969 23:59:58 GMT")
            ; (951782400., "Tue, 29 Feb 2000 00:00:00 GMT")
            ; (-9214560000., "Sat, 01 Jan 1678 00:00:00 GMT")
            ; (9214646400., "Wed, 01 Jan 2262 00:00:00 GMT")
            ; (-9223372036., "Tue, 21 Sep 1677 00:12:44 GMT") ] ;
          is_true ~msg:"the cache keeps its second"
            (a == Date.of_posix (posix 1000.5)) )
    ; test "of_posix drops the fraction of a time before 1970" (fun () ->
          equal string "Thu, 01 Jan 1970 00:00:00 GMT"
            (Date.of_posix (posix 0.5)) ;
          equal string "Wed, 31 Dec 1969 23:59:59 GMT"
            (Date.of_posix (posix (-0.5))) ;
          equal string "Wed, 31 Dec 1969 23:59:59 GMT"
            (Date.of_posix (posix (-1.))) ;
          (* A whole second before 1970 is not in the second before it. *)
          equal string "Wed, 31 Dec 1969 23:59:58 GMT"
            (Date.of_posix (posix (-1.5))) )
    ; test "of_posix drops a fraction near 1 s" (fun () ->
          (* 100 ns before midnight: a float rounds it up to the next second. *)
          let second =
            Weft.Time.of_utc
              { year= 2026
              ; month= 2
              ; day= 16
              ; hour= 23
              ; minute= 59
              ; second= 59
              ; weekday= 0 }
          in
          let p =
            Weft.Time.add Posix (Option.get second) (Weft.Span.ns 999_999_900)
          in
          equal string "Mon, 16 Feb 2026 23:59:59 GMT"
            (Date.of_posix (Option.get p)) ) ]

(* Flow *)

let read_all = Server_support.read_all

(* A flow for the tests of [Flow]. Each function that the test does not give
   does nothing: a read gives 0 (the end of the stream), and [readable] is ready
   at once. *)
let fake_flow ?(read = fun _ _ _ -> 0) ?(read_now = fun _ _ _ -> Some 0)
    ?(write = fun _ _ _ -> ()) ?(readable = Weft.always ())
    ?(shutdown_send = ignore) ?(close = ignore) ?reset ?set_read_timeout
    ?set_write_timeout () =
  Flow.make ~read ~read_now ~write ~readable ~shutdown_send ~close ?reset
    ~set_read_deadline:ignore ~set_write_deadline:ignore ?set_read_timeout
    ?set_write_timeout ()

(* [timeout_text name d] is the log line of a set of the timeout [name] to
   [d]. *)
let timeout_text name = function
  | None ->
      name ^ " none"
  | Some d ->
      Format.asprintf "%s %a" name Weft.Span.pp d

(* A fake flow that logs its timeouts, its shutdown and its reads in [log]. If
   [read_error] has a value, a read raises it. If not, a read gives at most 2
   bytes of [input] and removes them from [input]. *)
let logged_flow ?(input = ref "") ?read_error log =
  let note s = log := s :: !log in
  fake_flow
    ~read:(fun buf off len ->
      Option.iter raise read_error ;
      let n = Int.min (Int.min len 2) (String.length !input) in
      Bytes.blit_string !input 0 buf off n ;
      input := String.sub !input n (String.length !input - n) ;
      note (Printf.sprintf "read %d" n) ;
      n )
    ~read_now:(fun _ _ _ -> None)
    ~shutdown_send:(fun () -> note "shutdown")
    ~set_read_timeout:(fun d -> note (timeout_text "rt" d))
    ~set_write_timeout:(fun d -> note (timeout_text "wt" d))
    ()

let flow_tests =
  group "Flow"
    [ test "linger sends the end of the stream, then discards at most max bytes"
        (fun () ->
          let log = ref [] and input = ref "abcdef" in
          let flow ?read_error () = logged_flow ~input ?read_error log in
          Flow.linger (flow ()) ~max:3 ;
          equal (list string)
            ["rt none"; "shutdown"; "read 2"; "read 1"]
            (List.rev !log) ;
          equal string "def" !input ;
          log := [] ;
          Flow.linger (flow ()) ~max:100 ;
          equal (list string) ~msg:"until the end of the stream"
            ["rt none"; "shutdown"; "read 2"; "read 1"; "read 0"]
            (List.rev !log) ;
          List.iter
            (fun e -> Flow.linger (flow ~read_error:e ()) ~max:100)
            [Flow.Timeout; Flow.Closed] ;
          raises (Weft.Fiber.Cancelled Exit) (fun () ->
              Flow.linger
                (flow ~read_error:(Weft.Fiber.Cancelled Exit) ())
                ~max:100 ) ;
          let f = flow () in
          Flow.close f ;
          log := [] ;
          Flow.linger f ~max:100 ;
          equal (list string) ~msg:"a closed flow: no shutdown and no read"
            ["rt none"] !log )
    ; test "close is idempotent and later use raises Closed" (fun () ->
          let closes = ref 0 in
          let flow = fake_flow ~close:(fun () -> incr closes) () in
          Flow.close flow ;
          Flow.close flow ;
          equal int 1 !closes ;
          raises Flow.Closed (fun () -> Flow.read flow (Bytes.create 1) 0 1) ;
          raises Flow.Closed (fun () -> Flow.write_string flow "x") )
    ; test "Caravan.Flow and Caravan.Tcp are those of weft.flow" (fun () ->
          (* The two coercions compile only if the types are equal. *)
          let flow : Weft_flow.Flow.t = Flow.of_string "x" (Buffer.create 0) in
          let _ : Tcp.Listener.t -> Weft_flow.Tcp.Listener.t = Fun.id in
          equal string "x" (read_all flow) ;
          raises Weft_flow.Flow.Closed (fun () -> raise Flow.Closed) ;
          raises Weft_flow.Flow.Timeout (fun () -> raise Flow.Timeout) ) ]

(* Deadline *)

module Deadline = Private.Deadline

(* [same a b] is [true] iff the two deadlines are the same time, or are both
   [None]. *)
let same a b =
  match (a, b) with
  | None, None ->
      true
  | Some a, Some b ->
      Weft.(Span.equal (Time.diff Mono a b) Span.zero)
  | _ ->
      false

(* The printer runs in the fiber of the test. It gives a time as its distance
   from the time now. *)
let deadline =
  Testable.make
    ~pp:(fun ppf -> function
      | None ->
          Format.pp_print_string ppf "None"
      | Some d ->
          Format.fprintf ppf "Some (now + %a)" Weft.Span.pp
            Weft.(Time.diff Mono d (Time.now Mono)) )
    ~equal:same

let deadline_tests =
  let open Weft in
  let open Sim_support in
  group "Deadline"
    [ test "from_now is the time now plus the span" (fun () ->
          manual
          @@ fun sim ->
          advance sim (Span.s 3) ;
          let t0 = Time.now Mono in
          equal deadline (Some (t0 +: Span.s 5)) (Deadline.from_now (Span.s 5)) ;
          equal deadline ~msg:"an overflow is no deadline" None
            (Deadline.from_now (Span.of_ns Int64.max_int)) )
    ; test "has_passed is true from the deadline on" (fun () ->
          manual
          @@ fun sim ->
          let t0 = Time.now Mono in
          let d = Some (t0 +: Span.s 1) in
          holds_from sim ~t0 (Span.s 1) ~what:"has_passed" (fun () ->
              Deadline.has_passed d ) ;
          advance sim (Span.s 1) ;
          is_true ~msg:"after the deadline" (Deadline.has_passed d) ;
          is_false ~msg:"no deadline" (Deadline.has_passed None) )
    ; test "at is ready at its deadline, and never for None" (fun () ->
          manual
          @@ fun sim ->
          let t0 = Time.now Mono in
          Fiber.scope
          @@ fun s ->
          let c =
            call s (fun () -> sync (Deadline.at (Some (t0 +: Span.s 2))))
          in
          ends_at sim ~t0 (Span.s 2) c ;
          let t0 = Time.now Mono in
          let c =
            call s (fun () ->
                sync
                  (choose
                     [ wrap (Deadline.at None) (fun () -> "at None")
                     ; wrap (Time.after Mono (Span.s 10)) (fun () -> "10 s") ] ) )
          in
          equal string "10 s" (ends_at sim ~t0 (Span.s 10) c) ) ]

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
          equal (list string) ["next"] (events ()) ;
          (* A stopped writer given to a nested producer stops it too. *)
          let w, events = recording_writer () in
          Body.write_to (Body.of_writer keep) w ;
          let own = Option.get !kept in
          raises_match is_invalid_arg (fun () ->
              Body.write_to
                (Body.of_writer (fun w2 -> Body.Writer.write_string w2 "LATE"))
                own ) ;
          equal (list string) ["abc"] (events ()) ;
          (* write_to refuses a stopped writer for each kind of body. *)
          List.iter
            (fun b ->
              raises_match is_invalid_arg (fun () -> Body.write_to b own) )
            [Body.empty; Body.of_string ""; Body.of_string "abc"] )
    ; test "a writer body keeps the trailers that the sink has" (fun () ->
          let w, _ = recording_writer () in
          Body.write_to
            (Body.of_writer (fun o -> Body.Writer.set_trailers o trailers_x))
            w ;
          Body.write_to (Body.of_writer (fun _ -> ())) w ;
          is_true (Headers.equal trailers_x (Body.Writer.trailers w)) )
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

let loopback port = Weft.Net.Tcp ("127.0.0.1", port)

(* Body.on_end *)

let show_outcome = Server_support.show_outcome

let recorder = Server_support.on_end_log

(* A sink that keeps the bytes, and raises [Exit] at the write [fail_at]. *)
let sink ?(fail_at = 0) () =
  let out = Buffer.create 16 and writes = ref 0 in
  ( Body.Writer.make
      ~write:(fun buf off len ->
        incr writes ;
        if !writes = fail_at then raise Exit ;
        Buffer.add_subbytes out buf off len )
      ~flush:ignore
  , out )

let chunks_source ?close pieces =
  let rest = ref pieces in
  Body.of_source ?close (fun buf off _ ->
      match !rest with
      | [] ->
          0
      | p :: tl ->
          rest := tl ;
          Bytes.blit_string p 0 buf off (String.length p) ;
          String.length p )

let calls = list string

let on_end_tests =
  group "Body.on_end"
    [ test "write_to of a source" (fun () ->
          let f, log = recorder () in
          let w, out = sink () in
          Body.write_to (Body.on_end f (Body.of_string "abc")) w ;
          equal string "abc" (Buffer.contents out) ;
          equal calls ["Ended 3"] (log ()) )
    ; test "a producer and a sink that fails" (fun () ->
          let f, log = recorder () in
          let b =
            Body.on_end f
              (Body.of_writer ~length:5 (fun w ->
                   Body.Writer.write_string w "ab" ;
                   Body.Writer.write_string w "cde" ) )
          in
          equal (option int) (Some 5) (Body.length b) ;
          raises Exit (fun () -> Body.write_to b (fst (sink ~fail_at:2 ()))) ;
          equal calls ["Failed Stdlib.Exit 2"] (log ()) ;
          let f, log = recorder () in
          let b =
            Body.on_end f
              (Body.of_writer (fun w ->
                   Body.Writer.write_string w "ab" ;
                   Body.Writer.write_string w "cde" ) )
          in
          Body.write_to b (fst (sink ())) ;
          equal calls ["Ended 5"] (log ()) )
    ; test "a producer that raises" (fun () ->
          let f, log = recorder () in
          let b =
            Body.on_end f
              (Body.of_writer (fun w ->
                   Body.Writer.write_string w "ab" ;
                   raise Exit ) )
          in
          raises Exit (fun () -> Body.write_to b (fst (sink ()))) ;
          equal calls ["Failed Stdlib.Exit 2"] (log ()) )
    ; test "a source and a sink that fails at the second write" (fun () ->
          let f, log = recorder () in
          let b = Body.on_end f (chunks_source ["ab"; "cde"; "f"]) in
          raises Exit (fun () -> Body.write_to b (fst (sink ~fail_at:2 ()))) ;
          equal calls ["Failed Stdlib.Exit 2"] (log ()) )
    ; test "read" (fun () ->
          let f, log = recorder () in
          let b = Body.on_end f (Body.of_string "abc") in
          let buf = Bytes.create 8 in
          let rec all n =
            match Body.read b buf 0 8 with 0 -> n | k -> all (n + k)
          in
          equal int 3 (all 0) ;
          equal calls ["Ended 3"] (log ()) ;
          let f, log = recorder () in
          let b = Body.on_end f (Body.of_string "abc") in
          equal int 1 (Body.read b buf 0 1) ;
          Body.close b ;
          equal calls ["Abandoned 1"] (log ()) )
    ; test "close with no byte" (fun () ->
          List.iter
            (fun b ->
              let f, log = recorder () in
              Body.close (Body.on_end f b) ;
              equal calls ["Abandoned 0"] (log ()) )
            [Body.of_string "abc"; Body.of_writer ignore; Body.empty] )
    ; test "one call" (fun () ->
          let f, log = recorder () in
          let b = Body.on_end f (Body.of_string "abc") in
          Body.write_to b (fst (sink ())) ;
          Body.close b ;
          Body.close b ;
          equal calls ["Ended 3"] (log ()) ;
          let f, log = recorder () in
          let b = Body.on_end f (Body.of_string "abc") in
          Body.close b ;
          Body.close b ;
          equal calls ["Abandoned 0"] (log ()) )
    ; test "the trailers stay" (fun () ->
          let t = Headers.of_list [("X-T", "1")] in
          let f, _ = recorder () in
          let b =
            Body.on_end f
              (Body.of_writer (fun w -> Body.Writer.set_trailers w t))
          in
          let w, _ = sink () in
          Body.write_to b w ;
          is_true (Headers.equal t (Body.trailers b)) ;
          is_true (Headers.equal t (Body.Writer.trailers w)) ;
          let b =
            Body.on_end f
              (Body.of_source ~trailers:(fun () -> t) (fun _ _ _ -> 0))
          in
          let w, _ = sink () in
          Body.write_to b w ;
          is_true (Headers.equal t (Body.trailers b)) ;
          is_true (Headers.equal t (Body.Writer.trailers w)) )
    ; test "Body.empty is shared" (fun () ->
          let f, log = recorder () in
          Body.write_to (Body.on_end f Body.empty) (fst (sink ())) ;
          Body.write_to Body.empty (fst (sink ())) ;
          Body.close Body.empty ;
          equal calls ["Ended 0"] (log ()) ;
          (* A hook on Body.empty that nobody consumes. *)
          let g, glog = recorder () in
          ignore (Body.on_end g Body.empty) ;
          Body.write_to Body.empty (fst (sink ())) ;
          equal calls [] (glog ()) )
    ; test "a source whose close raises" (fun () ->
          let f, log = recorder () in
          let closes = ref 0 in
          let b =
            Body.on_end f
              (chunks_source
                 ~close:(fun () -> incr closes ; failwith "close")
                 ["a"] )
          in
          raises (Failure "close") (fun () -> Body.close b) ;
          equal calls ["Abandoned 0"] (log ()) ;
          Body.close b ;
          equal int 1 !closes ;
          equal calls ["Abandoned 0"] (log ()) )
    ; test "two hooks that do not raise" (fun () ->
          let f, log = recorder () and g, glog = recorder () in
          Body.write_to
            (Body.of_string "ab" |> Body.on_end f |> Body.on_end g)
            (fst (sink ())) ;
          equal calls ["Ended 2"] (log ()) ;
          equal calls ["Ended 2"] (glog ()) )
    ; test "read of an empty body" (fun () ->
          let f, log = recorder () in
          equal int 0
            (Body.read (Body.on_end f Body.empty) (Bytes.create 1) 0 1) ;
          equal calls ["Ended 0"] (log ()) )
    ; test "a read of the source that raises" (fun () ->
          let f, log = recorder () in
          let b = Body.on_end f (Body.of_source (fun _ _ _ -> raise Exit)) in
          raises Exit (fun () -> ignore (Body.read b (Bytes.create 1) 0 1)) ;
          equal calls ["Failed Stdlib.Exit 0"] (log ()) )
    ; test "write_to of a source that was closed before on_end" (fun () ->
          let f, log = recorder () in
          let b = Body.of_string "ab" in
          Body.close b ;
          let b = Body.on_end f b in
          raises_match is_invalid_arg (fun () ->
              Body.write_to b (fst (sink ())) ) ;
          equal calls [] (log ()) )
    ; test "the close of the source and the hook raise" (fun () ->
          let b =
            Body.on_end
              (fun _ ~bytes:_ -> failwith "hook")
              (Body.of_source
                 ~close:(fun () -> failwith "close")
                 (fun _ _ _ -> 0) )
          in
          raises (Failure "close") (fun () -> Body.close b) )
    ; test "a close from another fiber during write_to" (fun () ->
          let f, log = recorder () in
          let b =
            Body.on_end f
              (Body.of_source
                 ~close:(fun () -> Weft.Fiber.yield ())
                 (fun buf off _ -> Bytes.set buf off 'x' ; 1) )
          in
          let w =
            Body.Writer.make ~write:(fun _ _ _ -> ()) ~flush:Weft.Fiber.yield
          in
          Weft_sim.run (fun _ ->
              Weft.Fiber.scope (fun s ->
                  let p =
                    Weft.Fiber.fork_promise s (fun () ->
                        try Body.write_to b w with Invalid_argument _ -> () )
                  in
                  Weft.Fiber.yield () ; Body.close b ; Weft.Promise.await p ) ) ;
          match log () with
          | [l] ->
              is_true ~msg:l (String.starts_with ~prefix:"Abandoned" l)
          | l ->
              fail (String.concat "; " l) )
    ; test "an Invalid_argument of the body is a failure" (fun () ->
          let f, log = recorder () in
          let b = Body.on_end f (Body.of_writer (fun _ -> invalid_arg "x")) in
          raises (Invalid_argument "x") (fun () ->
              Body.write_to b (fst (sink ())) ) ;
          equal calls [{|Failed Invalid_argument("x") 0|}] (log ()) ;
          let f, log = recorder () in
          let b =
            Body.on_end f (Body.of_source (fun _ _ _ -> invalid_arg "x"))
          in
          raises (Invalid_argument "x") (fun () ->
              Body.write_to b (fst (sink ())) ) ;
          equal calls [{|Failed Invalid_argument("x") 0|}] (log ()) )
    ; test "two hooks that raise" (fun () ->
          let raising name _ ~bytes:_ = failwith name in
          let b =
            Body.of_string "a"
            |> Body.on_end (raising "first")
            |> Body.on_end (raising "second")
          in
          raises (Failure "first") (fun () -> Body.write_to b (fst (sink ()))) )
    ; test "a hook that raises on Failed" (fun () ->
          let b =
            Body.on_end
              (fun _ ~bytes:_ -> failwith "hook")
              (Body.of_writer (fun _ -> raise Exit))
          in
          raises Exit (fun () -> Body.write_to b (fst (sink ()))) )
    ; test "to_string with a max" (fun () ->
          let f, log = recorder () in
          raises Body.Too_large (fun () ->
              ignore
                (Body.to_string ~max:2 (Body.on_end f (Body.of_string "abcde"))) ) ;
          equal calls ["Failed Caravan__Body.Too_large 0"] (log ()) )
    ; test "two hooks" (fun () ->
          let order = ref [] in
          let hook name how ~bytes =
            order :=
              Printf.sprintf "%s %s %d" name (show_outcome how) bytes :: !order ;
            if name = "first" then failwith "first"
          in
          let b =
            Body.of_string "abc"
            |> Body.on_end (hook "first")
            |> Body.on_end (hook "second")
          in
          raises (Failure "first") (fun () -> Body.write_to b (fst (sink ()))) ;
          equal calls ["first Ended 3"; "second Ended 3"] (List.rev !order) )
    ; test "an argument error does not run the hook" (fun () ->
          let f, log = recorder () in
          let b = Body.on_end f (Body.of_string "abc") in
          raises_match is_invalid_arg (fun () ->
              ignore (Body.read b (Bytes.create 1) 0 2) ) ;
          let w, _ = sink () in
          let stopped = ref None in
          Body.write_to (Body.of_writer (fun w -> stopped := Some w)) w ;
          raises_match is_invalid_arg (fun () ->
              Body.write_to b (Option.get !stopped) ) ;
          let p = Body.on_end f (Body.of_writer ignore) in
          raises_match is_invalid_arg (fun () ->
              ignore (Body.read p (Bytes.create 1) 0 1) ) ;
          equal calls [] (log ()) )
    ; test "a cancel of the producer" (fun () ->
          let f, log = recorder () in
          let b =
            Body.on_end f
              (Body.of_writer (fun _ -> Weft.Time.sleep Mono (Weft.Span.s 3600)))
          in
          Weft_sim.run (fun _ ->
              Weft.Fiber.scope (fun s ->
                  Weft.Fiber.fork_daemon s (fun () ->
                      try Body.write_to b (fst (sink ())) with _ -> () ) ;
                  Weft.Time.sleep Mono (Weft.Span.s 1) ) ) ;
          match log () with
          | [l] ->
              is_true ~msg:l (String.starts_with ~prefix:"Failed Weft" l)
          | l ->
              fail (String.concat "; " l) )
    ; test "a hook on a body that ended, closed or ran" (fun () ->
          let f, log = recorder () in
          let b = Body.of_string "ab" in
          ignore (Body.to_string b) ;
          let b = Body.on_end f b in
          equal int 0 (Body.read b (Bytes.create 1) 0 1) ;
          equal calls ["Ended 0"] (log ()) ;
          let f, log = recorder () in
          let b = Body.of_string "ab" in
          Body.close b ;
          Body.close (Body.on_end f b) ;
          equal calls ["Abandoned 0"] (log ()) ;
          let f, log = recorder () in
          let b = Body.of_writer ignore in
          Body.write_to b (fst (sink ())) ;
          Body.close (Body.on_end f b) ;
          equal calls ["Abandoned 0"] (log ()) ) ]

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
              ~peer:(Weft.Net.Tcp ("::1", 1))
              ~local:(Weft.Net.Unix "/run/s.sock") ~scheme:`Https ~alpn:"h2" ()
          in
          is_true (Conn_info.scheme c6 = `Https) ;
          equal (option string) (Some "h2") (Conn_info.alpn c6) ;
          expect (Format.asprintf "%a" Conn_info.pp c6)
          @@ __POS_OF__ {|#1 [::1]:1 -> /run/s.sock|} )
    ; test "the stopping event of a connection is never ready by default"
        (fun () ->
          let c =
            Conn_info.make ~id:1 ~peer:(loopback 1) ~local:(loopback 2) ()
          in
          let ready =
            Sim_support.run (fun _ ->
                Weft.(
                  sync
                    (choose
                       [ wrap (Conn_info.stopping c) (fun () -> true)
                       ; always false ] ) ) )
          in
          is_false ready ) ]

(* Attrs *)

let user : string Attrs.key = Attrs.key ~name:"user" ()

let route : string Attrs.key = Attrs.key ~name:"route" ()

let attrs_tests =
  group "Attrs"
    [ test "empty has no values" (fun () ->
          equal (option string) None (Attrs.find user Attrs.empty) ;
          is_false (Attrs.mem user Attrs.empty) ;
          is_true (Attrs.is_empty Attrs.empty) )
    ; test "two keys with the same name and type are different" (fun () ->
          let a : int Attrs.key = Attrs.key ~name:"n" () in
          let b : int Attrs.key = Attrs.key ~name:"n" () in
          let m = Attrs.(empty |> add a 1 |> add b 2) in
          equal (option int) (Some 1) (Attrs.find a m) ;
          equal (option int) (Some 2) (Attrs.find b m) ;
          let c : int Attrs.key = Attrs.key ~name:"n" () in
          equal (option int) None (Attrs.find c m) )
    ; test "add replaces the value of the key" (fun () ->
          let m = Attrs.(empty |> add user "a" |> add user "b") in
          equal (option string) (Some "b") (Attrs.find user m) ;
          is_true (Attrs.mem user m) ;
          is_false (Attrs.is_empty m) )
    ; test "remove removes only its key" (fun () ->
          let m = Attrs.(empty |> add user "a" |> add route "/r") in
          let m' = Attrs.remove user m in
          equal (option string) None (Attrs.find user m') ;
          is_false (Attrs.mem user m') ;
          equal (option string) (Some "/r") (Attrs.find route m') ;
          let m'' = Attrs.remove user m' in
          equal (option string) (Some "/r") (Attrs.find route m'') ;
          is_true (Attrs.is_empty (Attrs.remove route m'')) )
    ; test "key_name" (fun () ->
          equal (option string) (Some "user") (Attrs.key_name user) ;
          equal (option string) None (Attrs.key_name (Attrs.key ())) )
    ; test "keys made on 4 domains at the same time are different" (fun () ->
          let n = 10_000 in
          let make d =
            Domain.spawn (fun () ->
                List.init n (fun i ->
                    ((d * n) + i, (Attrs.key () : int Attrs.key)) ) )
          in
          let keys = List.concat_map Domain.join (List.init 4 make) in
          let m =
            List.fold_left (fun m (v, k) -> Attrs.add k v m) Attrs.empty keys
          in
          List.iter
            (fun (v, k) -> equal (option int) (Some v) (Attrs.find k m))
            keys )
    ; test "pp prints the sorted names" (fun () ->
          let m =
            Attrs.(empty |> add user "a" |> add (key ()) 1 |> add route "/r")
          in
          expect (Format.asprintf "%a" Attrs.pp m)
          @@ __POS_OF__ {|_, route, user|} ;
          expect (Format.asprintf "%a" Attrs.pp Attrs.empty) @@ __POS_OF__ {||} )
    ]

(* Three keys of different types with the same name. An operation is [(i, Some
   v)], an add of a value made from [v] on key [i], or [(i, None)], a remove. *)
let attrs_props =
  let ki : int Attrs.key = Attrs.key ~name:"k" () in
  let ks : string Attrs.key = Attrs.key ~name:"k" () in
  let kb : bool Attrs.key = Attrs.key ~name:"k" () in
  let op = Gen.pair (Gen.int_range 0 2) (Gen.option (Gen.int_range 0 99)) in
  let apply m = function
    | 0, Some v ->
        Attrs.add ki v m
    | 1, Some v ->
        Attrs.add ks (string_of_int v) m
    | _, Some v ->
        Attrs.add kb (v mod 2 = 0) m
    | 0, None ->
        Attrs.remove ki m
    | 1, None ->
        Attrs.remove ks m
    | _, None ->
        Attrs.remove kb m
  in
  let model ops i =
    List.fold_left (fun last (j, v) -> if j = i then v else last) None ops
  in
  group "Attrs laws"
    [ prop "find and mem agree with a model"
        (Gen.list ~size:(Gen.int_range 0 20) op)
        (fun ops ->
          let m = List.fold_left apply Attrs.empty ops in
          equal (option int) (model ops 0) (Attrs.find ki m) ;
          equal (option string)
            (Option.map string_of_int (model ops 1))
            (Attrs.find ks m) ;
          equal (option bool)
            (Option.map (fun v -> v mod 2 = 0) (model ops 2))
            (Attrs.find kb m) ;
          equal bool (model ops 0 <> None) (Attrs.mem ki m) ;
          equal bool (model ops 1 <> None) (Attrs.mem ks m) ;
          equal bool (model ops 2 <> None) (Attrs.mem kb m) ;
          equal bool
            (List.for_all (fun i -> model ops i = None) [0; 1; 2])
            (Attrs.is_empty m) ) ]

let pairs = list (pair string string)

(* [source ?length s] is a body that gives [s] in one chunk, with no declared
   length if [length] is not given. *)
let source ?length s = Body.of_source ?length (chunked_reader s)

(* A body that raises [e] at its first read. *)
let raising ?length e = Body.of_source ?length (fun _ _ _ -> raise e)

(* A POST with a [Content-Type] field for each value of [types]. *)
let form_request ?(types = [Form.media_type]) body =
  Request.make
    ~headers:(Headers.of_list (List.map (fun v -> ("Content-Type", v)) types))
    ~body Method.POST "/"

let form_error = Testable.make ~pp:Form.pp_error ~equal:( = )

let form_result = result pairs form_error

let form_tests =
  group "Form"
    [ test "parse splits on & and on the first =" (fun () ->
          List.iter
            (fun (s, l) -> equal ~msg:s pairs l (Form.parse s))
            [ ("a=1&a=2&&b", [("a", "1"); ("a", "2"); ("b", "")])
            ; ("a+b=c%20d", [("a b", "c d")])
            ; ("%2B=%zz", [("+", "%zz")])
            ; ("=x", [("", "x")])
            ; ("a=b=c", [("a", "b=c")])
            ; ("a&b=c", [("a", ""); ("b", "c")])
            ; ("%26=%3D", [("&", "=")])
            ; ("&&", [])
            ; ("=", [("", "")])
            ; ("", []) ] )
    ; test "parse keeps an incorrect %" (fun () ->
          List.iter
            (fun (v, expected) ->
              equal ~msg:v pairs [("a", expected)] (Form.parse ("a=" ^ v)) )
            [ ("%", "%")
            ; ("%4", "%4")
            ; ("%4g", "%4g")
            ; ("%g4", "%g4")
            ; ("%41", "A")
            ; ("%c3%A9", "\xc3\xa9")
            ; ("%00", "\x00")
            ; ("%%41", "%A")
            ; ("x%", "x%")
            ; ("x%4", "x%4") ] ;
          equal pairs [("A", "1")] (Form.parse "%41=1") ;
          equal pairs [("%4", "1")] (Form.parse "%4=1") )
    ; test "parse changes + before the decode" (fun () ->
          equal pairs [("a b", " c ")] (Form.parse "a+b=+c+") ;
          equal pairs [("+", "+")] (Form.parse "%2B=%2b") )
    ; test "encode" (fun () ->
          equal string "a+b=*-._%7E%21" (Form.encode [("a b", "*-._~!")]) ;
          equal string "a=1&b=" (Form.encode [("a", "1"); ("b", "")]) ;
          equal string "=" (Form.encode [("", "")]) ;
          equal string "%26%3D%2B%25=%C3%A9%00"
            (Form.encode [("&=+%", "\xc3\xa9\x00")]) ;
          equal string "" (Form.encode []) )
    ; test "find, find_all and mem" (fun () ->
          let l = [("a", "1"); ("b", ""); ("a", "2")] in
          equal (option string) (Some "1") (Form.find "a" l) ;
          equal (list string) ["1"; "2"] (Form.find_all "a" l) ;
          is_true (Form.mem "b" l) ;
          equal (option string) None (Form.find "A" l) ;
          equal (list string) [] (Form.find_all "A" l) ;
          is_false (Form.mem "A" l) ;
          let s = "a=1&&b&a+b=%2&a=2" in
          equal (option string) (Some "1") (Form.find_text "a" s) ;
          equal (option string) (Some "") (Form.find_text "b" s) ;
          equal (option string) (Some "%2") (Form.find_text "a b" s) ;
          equal (option string) None (Form.find_text "A" s) ;
          equal (option string) None (Form.find_text "a" "") ;
          equal (option string) (Some "x") (Form.find_text "ab" "a=1&ab=x") ;
          equal (option string) None (Form.find_text "ab" "a=1&abc=x") )
    ; test "error_status and pp_error" (fun () ->
          List.iter
            (fun (e, code, name) ->
              equal ~msg:name status (Status.v code) (Form.error_status e) ;
              equal string name (Format.asprintf "%a" Form.pp_error e) )
            [ (Form.Unsupported_media_type, 415, "Unsupported_media_type")
            ; (Form.Too_large, 413, "Too_large")
            ; (Form.Too_many_fields, 413, "Too_many_fields") ] ) ]

(* The generator puts the bytes with a role in the format in most strings. *)
let form_props =
  let byte =
    Gen.frequency
      [ (3, Gen.of_list ['&'; '='; '+'; '%'; ' '])
      ; (1, Gen.char_range '\x80' '\xff')
      ; (2, Gen.char) ]
  in
  let text = Gen.string_of ~size:(Gen.int_range 0 6) byte in
  group "Form laws"
    [ prop "parse inverts encode"
        (Gen.list ~size:(Gen.int_range 0 6) (Gen.pair text text))
        (fun l -> equal pairs l (Form.parse (Form.encode l)))
    ; prop "count is the length of parse"
        (Gen.string_of ~size:(Gen.int_range 0 12) byte)
        (fun s -> equal int (List.length (Form.parse s)) (Form.count s))
    ; prop "find_text is find of parse"
        (Gen.pair
           (Gen.of_list ~pp:Format.pp_print_string
              [""; "a"; "b"; "ab"; "a b"; " "; "aa"; "%"; "%2"; "+"; "a+b"] )
           (Gen.string_of ~size:(Gen.int_range 0 14)
              (Gen.of_list ['a'; 'b'; '='; '&'; '+'; '%'; '2'; '0'; '5']) ) )
        (fun (name, s) ->
          equal ~msg:s (option string)
            (Form.find name (Form.parse s))
            (Form.find_text name s) ) ]

let request_form_tests =
  group "Request forms"
    [ test "query_params" (fun () ->
          List.iter
            (fun (path, l) ->
              equal ~msg:path pairs l
                (Request.query_params (Request.make Method.GET path)) )
            [ ("/a", [])
            ; ("/a?", [])
            ; ("/a?x=1&x=2", [("x", "1"); ("x", "2")])
            ; ("/a?q=b%3Fc?d", [("q", "b?c?d")]) ] )
    ; test "form checks the Content-Type" (fun () ->
          List.iter
            (fun (types, expected) ->
              equal ~msg:(String.concat ", " types) form_result expected
                (Request.form (form_request ~types (Body.of_string "a=1"))) )
            [ ([Form.media_type], Ok [("a", "1")])
            ; ( ["Application/X-WWW-Form-Urlencoded; charset=utf-8"]
              , Ok [("a", "1")] )
            ; (["application/x-www-form-urlencoded ;"], Ok [("a", "1")])
            ; (["application/x-www-form-urlencoded\t;x=1"], Ok [("a", "1")])
            ; ( ["application/x-www-form-urlencoded; charset=\"a;b\""]
              , Ok [("a", "1")] )
            ; (["text/plain"], Error Form.Unsupported_media_type)
            ; ( ["application/x-www-form-urlencodedx"]
              , Error Form.Unsupported_media_type )
            ; ( ["x-application/x-www-form-urlencoded"]
              , Error Form.Unsupported_media_type )
            ; ([], Error Form.Unsupported_media_type)
            ; ( [Form.media_type; Form.media_type]
              , Error Form.Unsupported_media_type ) ] )
    ; test "the default limits of form" (fun () ->
          let form text = Request.form (form_request (Body.of_string text)) in
          let size n = "a=" ^ String.make (n - 2) 'x' in
          is_true ~msg:"1 MiB" (Result.is_ok (form (size 1_048_576))) ;
          equal form_result (Error Form.Too_large) (form (size 1_048_577)) ;
          let pairs n = String.concat "&" (List.init n (fun _ -> "a")) in
          is_true ~msg:"1000 pairs" (Result.is_ok (form (pairs 1000))) ;
          equal form_result (Error Form.Too_many_fields) (form (pairs 1001)) )
    ; test "form with no Content-Type does not read the body" (fun () ->
          equal form_result (Error Form.Unsupported_media_type)
            (Request.form (form_request ~types:[] (raising Exit))) ;
          List.iter
            (fun types ->
              equal ~msg:(String.concat ", " types) form_result
                (Error Form.Unsupported_media_type)
                (Request.form (form_request ~types (raising Exit))) )
            [[]; ["text/plain"]; [Form.media_type; Form.media_type]] ;
          equal form_result (Error Form.Unsupported_media_type)
            (Request.form (form_request ~types:[] Body.empty)) )
    ; test "max_bytes in a chunk" (fun () ->
          equal form_result
            (Ok [("a", "12345678")])
            (Request.form ~max_bytes:10 (form_request (source "a=12345678"))) ;
          equal form_result (Error Form.Too_large)
            (Request.form ~max_bytes:10 (form_request (source "a=123456789"))) )
    ; test "max_bytes and a declared length" (fun () ->
          equal form_result (Error Form.Too_large)
            (Request.form ~max_bytes:10
               (form_request (raising ~length:11 Exit)) ) ;
          equal form_result
            (Ok [("a", "12345678")])
            (Request.form ~max_bytes:10
               (form_request (source ~length:10 "a=12345678")) ) )
    ; test "max_bytes 0" (fun () ->
          equal form_result (Ok [])
            (Request.form ~max_bytes:0 (form_request Body.empty)) ;
          equal form_result (Error Form.Too_large)
            (Request.form ~max_bytes:0 (form_request (source "a"))) )
    ; test "max_fields counts the non-empty sequences" (fun () ->
          List.iter
            (fun (s, expected) ->
              equal ~msg:s form_result expected
                (Request.form ~max_fields:2 (form_request (Body.of_string s))) )
            [ ("a&b", Ok [("a", ""); ("b", "")])
            ; ("a&&b&", Ok [("a", ""); ("b", "")])
            ; ("a&b&c", Error Form.Too_many_fields) ] ;
          equal form_result (Ok [])
            (Request.form ~max_fields:0 (form_request (Body.of_string "&&"))) )
    ; test "form lets an exception of the body go out" (fun () ->
          raises Body.Truncated (fun () ->
              ignore (Request.form (form_request (raising Body.Truncated))) ) )
    ; test "form refuses a negative limit" (fun () ->
          let r = form_request Body.empty in
          raises_match is_invalid_arg (fun () ->
              ignore (Request.form ~max_bytes:(-1) r) ) ;
          raises_match is_invalid_arg (fun () ->
              ignore (Request.form ~max_fields:(-1) r) ) )
    ; test "with_form" (fun () ->
          let l = [("a b", "&"); ("c", "")] in
          let r =
            Request.with_form l
              (form_request ~types:["text/plain"; "text/html"] (raising Exit))
          in
          equal (list string) [Form.media_type]
            (Headers.find_all "content-type" (Request.headers r)) ;
          equal (option int)
            (Some (String.length (Form.encode l)))
            (Body.length (Request.body r)) ;
          equal form_result (Ok l) (Request.form r) ) ]

(* Cookie *)

(* [refuses f] makes sure that [f ()] raises [Invalid_argument]. *)
let refuses ?msg f = raises_match ?msg is_invalid_arg (fun () -> ignore (f ()))

(* RFC 6265 §4.1.1, as a list of ranges. *)
let is_cookie_octet c =
  let k = Char.code c in
  k = 0x21
  || (k >= 0x23 && k <= 0x2B)
  || (k >= 0x2D && k <= 0x3A)
  || (k >= 0x3C && k <= 0x5B)
  || (k >= 0x5D && k <= 0x7E)

let cookie_tests =
  let ok ?msg c =
    ignore (c ()) ;
    ignore msg
  in
  group "Cookie"
    [ test "parse" (fun () ->
          List.iter
            (fun (s, l) -> equal ~msg:s pairs l (Cookie.parse s))
            [ ("a=1; b=2", [("a", "1"); ("b", "2")])
            ; ("a=1;b=2", [("a", "1"); ("b", "2")])
            ; (" a = 1 ;; b=2 ;", [("a", "1"); ("b", "2")])
            ; ("a=1; a=2", [("a", "1"); ("a", "2")])
            ; ("a=1, b=2", [("a", "1, b=2")])
            ; ("a", [])
            ; ("=x", [])
            ; (" =x; b=2", [("b", "2")])
            ; ("a=b=c", [("a", "b=c")])
            ; ("a=\"q\"", [("a", "q")])
            ; ("a=\"", [("a", "\"")])
            ; ("a=\"\"", [("a", "")])
            ; ("a=\"x", [("a", "\"x")])
            ; ("a=x\"", [("a", "x\"")])
            ; ("a= \"q\" ", [("a", "q")])
            ; ("a=\" q \"", [("a", " q ")])
            ; ("\ta=1\t;\tb=2", [("a", "1"); ("b", "2")])
            ; ("a=\x0c1", [("a", "\x0c1")])
            ; ("a=", [("a", "")])
            ; ("", []) ] )
    ; test "parse_set_cookie" (fun () ->
          List.iter
            (fun (s, p) ->
              equal ~msg:s
                (option (pair string string))
                p
                (Cookie.parse_set_cookie s) )
            [ ("a=1; Path=/", Some ("a", "1"))
            ; ("a", None)
            ; ("=1; Path=/", None)
            ; (" a = \"1\" ;x", Some ("a", "1"))
            ; ("a=1", Some ("a", "1")) ] )
    ; test "the defaults" (fun () ->
          equal string "a=1; Path=/; HttpOnly; SameSite=Lax"
            (Cookie.to_string (Cookie.make "a" "1")) )
    ; test "all attributes, in order" (fun () ->
          equal string
            "a=1; Expires=Sun, 06 Nov 1994 08:49:37 GMT; Max-Age=60; \
             Domain=example.com; Path=/x; Secure; HttpOnly; SameSite=Strict; \
             Partitioned"
            (Cookie.to_string
               (Cookie.make ~expires:(posix 784111777.) ~max_age:60
                  ~domain:"example.com" ~path:"/x" ~secure:true
                  ~same_site:`Strict ~partitioned:true "a" "1" ) ) )
    ; test "attributes turned off" (fun () ->
          equal string "a=1; Path=/x; Secure; SameSite=None"
            (Cookie.to_string
               (Cookie.make ~http_only:false ~same_site:`None ~secure:true
                  ~path:"/x" "a" "1" ) ) ;
          equal string "a=1; Path=/"
            (Cookie.to_string
               (Cookie.make ~http_only:false ~same_site:`Unspecified "a" "1") ) )
    ; test "the value has only cookie-octets" (fun () ->
          for k = 0 to 255 do
            let v = String.make 1 (Char.chr k) in
            let msg = Printf.sprintf "byte 0x%02X" k in
            if is_cookie_octet (Char.chr k) then
              equal ~msg string v (Cookie.make "a" v).value
            else refuses ~msg (fun () -> Cookie.make "a" v)
          done ;
          equal string "" (Cookie.make "a" "").value )
    ; test "make refuses incorrect names and attributes" (fun () ->
          List.iter
            (fun (msg, f) -> refuses ~msg f)
            [ ("a name with a space", fun () -> Cookie.make "a b" "1")
            ; ("an empty name", fun () -> Cookie.make "" "1")
            ; ("a name with =", fun () -> Cookie.make "a=" "1")
            ; ("a domain with ;", fun () -> Cookie.make ~domain:"a;b" "a" "1")
            ; ( "a domain with CR LF"
              , fun () -> Cookie.make ~domain:"a\r\nb" "a" "1" )
            ; ( "a domain with DEL"
              , fun () -> Cookie.make ~domain:"a\x7f" "a" "1" )
            ; ( "a domain with a space"
              , fun () -> Cookie.make ~domain:"a b" "a" "1" )
            ; ("an empty domain", fun () -> Cookie.make ~domain:"" "a" "1")
            ; ("a domain .", fun () -> Cookie.make ~domain:"." "a" "1")
            ; ("a domain ..", fun () -> Cookie.make ~domain:".." "a" "1")
            ; ("a domain a..b", fun () -> Cookie.make ~domain:"a..b" "a" "1")
            ; ("a domain a.", fun () -> Cookie.make ~domain:"a." "a" "1")
            ; ("a path with a space", fun () -> Cookie.make ~path:"/a b" "a" "1")
            ; ( "a path that ends with a space"
              , fun () -> Cookie.make ~path:"/ " "a" "1" )
            ; ("a path with no /", fun () -> Cookie.make ~path:"x" "a" "1")
            ; ("an empty path", fun () -> Cookie.make ~path:"" "a" "1")
            ; ("a path with ;", fun () -> Cookie.make ~path:"/a;b" "a" "1")
            ; ("a path with DEL", fun () -> Cookie.make ~path:"/\x7f" "a" "1")
            ; ("a path with 0x1F", fun () -> Cookie.make ~path:"/\x1f" "a" "1")
            ; ("a path with 0x80", fun () -> Cookie.make ~path:"/\x80" "a" "1")
            ; ("a negative max_age", fun () -> Cookie.make ~max_age:(-1) "a" "1")
            ] ;
          ok (fun () -> Cookie.make ~max_age:0 "a" "1") ;
          ok (fun () -> Cookie.make ~domain:".a-b_c.example9" "a" "1") ;
          ok (fun () -> Cookie.make ~path:"/!~" "a" "1") )
    ; test "the limits of the sizes" (fun () ->
          let name = "a" in
          ok (fun () -> Cookie.make name (String.make 4095 'v')) ;
          refuses (fun () -> Cookie.make name (String.make 4096 'v')) ;
          ok (fun () -> Cookie.make ~domain:(String.make 1024 'd') "a" "1") ;
          refuses (fun () -> Cookie.make ~domain:(String.make 1025 'd') "a" "1") ;
          ok (fun () -> Cookie.make ~path:("/" ^ String.make 1023 'p') "a" "1") ;
          refuses (fun () ->
              Cookie.make ~path:("/" ^ String.make 1024 'p') "a" "1" ) )
    ; test "SameSite=None, Partitioned and the prefixes need Secure" (fun () ->
          List.iter
            (fun (msg, f) -> refuses ~msg f)
            [ ("SameSite=None", fun () -> Cookie.make ~same_site:`None "a" "1")
            ; ("Partitioned", fun () -> Cookie.make ~partitioned:true "a" "1")
            ; ("__Secure-", fun () -> Cookie.make "__Secure-a" "1")
            ; ("__SECURE-", fun () -> Cookie.make "__SECURE-a" "1")
            ; ( "__Host- with a domain"
              , fun () ->
                  Cookie.make ~secure:true ~domain:"example.com" "__Host-a" "1"
              )
            ; ( "__Host- with a path"
              , fun () -> Cookie.make ~secure:true ~path:"/x" "__Host-a" "1" )
            ; ("__Host- with no Secure", fun () -> Cookie.make "__Host-a" "1")
            ; ("__host-", fun () -> Cookie.make "__host-a" "1")
            ; ("only __Secure-", fun () -> Cookie.make "__Secure-" "1")
            ; ("only __Host-", fun () -> Cookie.make "__Host-" "1") ] ;
          equal string "__Host-a=1; Path=/; Secure; HttpOnly; SameSite=Lax"
            (Cookie.to_string (Cookie.make ~secure:true "__Host-a" "1")) ;
          ok (fun () -> Cookie.make ~secure:true "__Secure-a" "1") ;
          ok (fun () -> Cookie.make "__Securea" "1") ;
          ok (fun () -> Cookie.make "__Hos" "1") ;
          ok (fun () -> Cookie.make "x_Secure-a" "1") ;
          ok (fun () -> Cookie.make "x_Host-a" "1") )
    ; test "expired keeps the attributes that the user agent matches" (fun () ->
          let c =
            Cookie.make ~max_age:60 ~domain:"example.com" ~path:"/x"
              ~secure:true ~partitioned:true "a" "1"
          in
          equal string
            "a=; Expires=Thu, 01 Jan 1970 00:00:00 GMT; Max-Age=0; \
             Domain=example.com; Path=/x; Secure; HttpOnly; SameSite=Lax; \
             Partitioned"
            (Cookie.to_string (Cookie.expired c)) ;
          equal string
            "__Host-a=; Expires=Thu, 01 Jan 1970 00:00:00 GMT; Max-Age=0; \
             Path=/; Secure; HttpOnly; SameSite=Lax"
            (Cookie.to_string
               (Cookie.expired (Cookie.make ~secure:true "__Host-a" "1")) ) )
    ; test "expired changes Expires and keeps the other attributes" (fun () ->
          let c =
            Cookie.make ~expires:(posix 784111777.) ~http_only:false
              ~same_site:`None ~secure:true "a" "1"
          in
          equal string
            "a=; Expires=Thu, 01 Jan 1970 00:00:00 GMT; Max-Age=0; Path=/; \
             Secure; SameSite=None"
            (Cookie.to_string (Cookie.expired c)) )
    ; test "with_value" (fun () ->
          let c = Cookie.make ~path:"/x" ~max_age:5 "a" "1" in
          equal string "a=2; Max-Age=5; Path=/x; HttpOnly; SameSite=Lax"
            (Cookie.to_string (Cookie.with_value "2" c)) ;
          refuses (fun () -> Cookie.with_value "a b" c) ;
          refuses (fun () -> Cookie.with_value (String.make 4096 'v') c) )
    ; test "pp hides the value" (fun () ->
          equal string "a=<6 bytes>; Path=/; Secure; HttpOnly; SameSite=Lax"
            (Format.asprintf "%a" Cookie.pp
               (Cookie.make ~secure:true "a" "secret") ) )
    ; test "Request.cookies and Request.cookie" (fun () ->
          let r fields =
            Request.make
              ~headers:
                (Headers.of_list (List.map (fun v -> ("Cookie", v)) fields))
              Method.GET "/"
          in
          let two = r ["a=1; b=2"; "c=3; a=4"] in
          equal pairs
            [("a", "1"); ("b", "2"); ("c", "3"); ("a", "4")]
            (Request.cookies two) ;
          equal (option string) (Some "1") (Request.cookie "a" two) ;
          equal (option string) (Some "3") (Request.cookie "c" two) ;
          equal (option string) (Some "1") (Request.cookie "a" (r ["a=1; a=2"])) ;
          equal (option string) None (Request.cookie "A" two) ;
          List.iter
            (fun (name, s, v) ->
              equal ~msg:s (option string) v (Cookie.find name s) )
            [ ("a", " a = \"1\" ; a=2", Some "1")
            ; ("a", "ab=1; a=2", Some "2")
            ; ("ab", "a=1; ab=2", Some "2")
            ; ("a", "a; a=3", Some "3")
            ; ("", "=1; a=2", None)
            ; ("a", "b=1", None)
            ; ("a", "", None) ] ;
          equal pairs [] (Request.cookies (r [])) ;
          equal (option string) None (Request.cookie "a" (r [])) )
    ; test "Response.add_cookie adds a field for each cookie" (fun () ->
          let r =
            Response.string "x"
            |> Response.add_cookie (Cookie.make "a" "1")
            |> Response.add_cookie (Cookie.make "b" "2")
          in
          equal (list string)
            [ "a=1; Path=/; HttpOnly; SameSite=Lax"
            ; "b=2; Path=/; HttpOnly; SameSite=Lax" ]
            (Headers.find_all "set-cookie" (Response.headers r)) ) ]

let cookie_props =
  let tchar = Gen.of_list tchars in
  let octet = Gen.map Char.chr (Gen.int_range 0 255) in
  let octet = Gen.such_that is_cookie_octet octet in
  let name = Gen.string_of ~size:(Gen.int_range 1 8) tchar in
  let value = Gen.string_of ~size:(Gen.int_range 0 8) octet in
  let cookie =
    let open Gen in
    let label = string_of ~size:(int_range 1 4) (of_list ['a'; '-'; '_']) in
    let+ n = name
    and+ v = value
    and+ secure = bool
    and+ http_only = bool
    and+ same_site = of_list [`Strict; `Lax; `None; `Unspecified]
    and+ partitioned = bool
    and+ max_age = option (int_range 0 1000)
    and+ domain =
      option
        ( pair bool (list ~size:(int_range 1 3) label)
        |> map (fun (dot, l) -> (if dot then "." else "") ^ String.concat "." l)
        )
    and+ path = string_of ~size:(int_range 0 8) (char_range '\x21' '\x7e') in
    let path = "/" ^ String.map (function ';' -> '_' | c -> c) path in
    (* [`None] and [partitioned] need [secure]. *)
    let secure = secure || same_site = `None || partitioned in
    (n, v, secure, http_only, same_site, partitioned, max_age, domain, path)
  in
  group "Cookie laws"
    [ prop "to_string gives a valid field value that parse_set_cookie reads"
        cookie
        (fun
          ( n
          , v
          , secure
          , http_only
          , same_site
          , partitioned
          , max_age
          , domain
          , path )
        ->
          (* A name with a prefix needs more attributes. *)
          let n = "x" ^ n in
          let c =
            Cookie.make ?max_age ?domain ~path ~secure ~http_only ~same_site
              ~partitioned ~expires:(posix 0.) n v
          in
          is_true (Headers.is_valid_value (Cookie.to_string c)) ;
          equal
            (option (pair string string))
            (Some (n, v))
            (Cookie.parse_set_cookie (Cookie.to_string c)) )
    ; prop "parse inverts the join of valid pairs"
        (Gen.list ~size:(Gen.int_range 0 5) (Gen.pair name value))
        (fun l ->
          (* A cookie-octet is not a double quote, thus no value loses
             quotes. *)
          equal pairs l
            (Cookie.parse
               (String.concat "; " (List.map (fun (n, v) -> n ^ "=" ^ v) l)) ) )
    ]

(* Multipart *)

module Mp = Multipart

let mp_field =
  Testable.make
    ~pp:(fun ppf (f : Mp.field) ->
      Format.fprintf ppf "{name=%S; filename=%a; content_type=%a; data=%S}"
        f.name
        Format.(pp_print_option pp_print_string)
        f.filename
        Format.(pp_print_option pp_print_string)
        f.content_type f.data )
    ~equal:( = )

let mp_error = Testable.make ~pp:Mp.pp_error ~equal:( = )

let mp_result = result (list mp_field) mp_error

let field ?filename ?content_type name data : Mp.field =
  {name; filename; content_type; data}

let disp name = "Content-Disposition: form-data; name=\"" ^ name ^ "\"\r\n"

(* A body with the boundary [b]: each part is a head and a content. *)
let mp_body ?(b = "b") parts =
  String.concat ""
    (List.map (fun (h, d) -> "--" ^ b ^ "\r\n" ^ h ^ "\r\n" ^ d ^ "\r\n") parts)
  ^ "--" ^ b ^ "--\r\n"

let mp_read_all ?max_parts ?max_header_bytes ?max_bytes ?chunk s =
  Mp.read_all ?max_parts ?max_header_bytes ?max_bytes ~boundary:"b"
    (Body.of_source (chunked_reader ?chunk s))

let is_malformed = function Error (Mp.Malformed _) -> true | _ -> false

let malformed ?msg r = is_true ?msg (is_malformed r)

let multipart_tests =
  group "Multipart"
    [ test "boundary" (fun () ->
          let b70 = String.make 70 'a' in
          List.iter
            (fun (v, expected) ->
              equal ~msg:v (option string) expected (Mp.boundary v) )
            [ ("multipart/form-data; boundary=abc", Some "abc")
            ; ("multipart/form-data; BOUNDARY = \"a b\"", Some "a b")
            ; ("multipart/form-data; boundary=\"a\\\"b\"", None)
            ; ("multipart/form-data; boundary=\"a\\:b\"", Some "a:b")
            ; ("multipart/form-data; boundary=" ^ b70, Some b70)
            ; ("multipart/form-data; boundary=" ^ b70 ^ "a", None)
            ; ("multipart/form-data; boundary=\"a \"", None)
            ; ("multipart/form-data; boundary=\"\"", None)
            ; ("multipart/form-data; boundary=a@b", None)
            ; ("multipart/form-data; boundary=a; boundary=a", None)
            ; ("multipart/form-data; charset=x; boundary=a", Some "a")
            ; ("multipart/form-data", None)
            ; ("multipart/form-data; boundary", None)
            ; ("multipart/form-data; boundary=", None)
            ; ("multipart/form-data; boundary=\"a\\", None) ] )
    ; test "mp_read_all of two fields and a file" (fun () ->
          let s =
            mp_body
              [ (disp "user", "alice")
              ; (disp "note", "a\r\nb")
              ; ( "Content-Disposition: form-data; name=\"f\"; \
                   filename=\"a.txt\"\r\n\
                   Content-Type: text/plain\r\n"
                , "line 1\r\nline 2\r\n" ) ]
          in
          equal mp_result
            (Ok
               [ field "user" "alice"
               ; field "note" "a\r\nb"
               ; field ~filename:"a.txt" ~content_type:"text/plain" "f"
                   "line 1\r\nline 2\r\n" ] )
            (mp_read_all s) )
    ; test "preamble, padding and epilogue" (fun () ->
          let s =
            "the preamble\r\n--b \t\r\n" ^ disp "a"
            ^ "\r\n1\r\n--b--the epilogue"
          in
          equal mp_result (Ok [field "a" "1"]) (mp_read_all s) ;
          equal mp_result
            (Ok [field "a" "1"])
            (mp_read_all ("--b\r\n" ^ disp "a" ^ "\r\n1\r\n--b--")) ;
          equal mp_result (Ok []) (mp_read_all "--b--\r\n") )
    ; test "bytes that look like a delimiter stay in the content" (fun () ->
          List.iter
            (fun d ->
              equal ~msg:(String.escaped d) mp_result
                (Ok [field "a" d])
                (mp_read_all (mp_body [(disp "a", d)])) )
            [ "x\r\n--"
            ; "x\r\n--c"
            ; "x\r\n-"
            ; "x--b"
            ; "x-b"
            ; "\r\n--B"
            ; "\r"
            ; "\r\n"
            ; "" ] )
    ; test "an empty part can have no CRLF before its delimiter" (fun () ->
          (* RFC 2046: the CRLF of the empty line can start the delimiter. *)
          equal mp_result
            (Ok [field "a" ""; field "c" "X"])
            (mp_read_all
               ( "--b\r\n" ^ disp "a" ^ "\r\n--b\r\n" ^ disp "c"
               ^ "\r\nX\r\n--b--\r\n" ) ) ;
          equal mp_result
            (Ok [field "a" ""])
            (mp_read_all ("--b\r\n" ^ disp "a" ^ "\r\n--b--")) ;
          (* In chunks of 1 byte. *)
          equal mp_result
            (Ok [field "a" ""])
            (mp_read_all ~chunk:1 ("--b\r\n" ^ disp "a" ^ "\r\n--b--")) ;
          malformed (mp_read_all ("--b\r\n" ^ disp "a" ^ "\r\n--bX")) ;
          equal mp_result
            (Ok [field "a" "--c"])
            (mp_read_all ("--b\r\n" ^ disp "a" ^ "\r\n--c\r\n--b--")) )
    ; test "incorrect bytes after a boundary" (fun () ->
          malformed (mp_read_all ("--b\r\n" ^ disp "a" ^ "\r\n1\r\n--bX\r\n")) ;
          malformed (mp_read_all ("--b\r\n" ^ disp "a" ^ "\r\n1\r\n--b-x")) ;
          malformed (mp_read_all ("--b \tx\r\n" ^ disp "a" ^ "\r\n1\r\n--b--")) ;
          malformed (mp_read_all ("--b\rX" ^ disp "a" ^ "\r\n1\r\n--b--")) ;
          (* The read of the part raises. *)
          let r =
            Mp.fold ~boundary:"b"
              (Body.of_string ("--b\r\n" ^ disp "a" ^ "\r\n1\r\n--bX\r\n"))
              ~init:()
              (fun () p ->
                raises Body.Truncated (fun () -> Body.to_string (Mp.body p)) )
          in
          malformed r ;
          (* The callback lets the error go, and a close delimiter comes after
             the incorrect bytes. *)
          malformed
            (Mp.fold ~boundary:"b"
               (Body.of_string
                  ("--b\r\n" ^ disp "a" ^ "\r\n1\r\n--bX\r\n--b--") )
               ~init:()
               (fun () p ->
                 raises Body.Truncated (fun () -> Body.to_string (Mp.body p)) ) ) ;
          (* The bytes after the boundary make a correct head. *)
          malformed
            (mp_read_all
               ( "--b\r\n" ^ disp "a" ^ "\r\n1\r\n--b" ^ disp "c"
               ^ "\r\n2\r\n--b--" ) ) )
    ; test "parts with no filename or empty values" (fun () ->
          equal mp_result
            (Ok [field "a" ""; field ~filename:"" "f" "x"; field "" "y"])
            (mp_read_all
               (mp_body
                  [ (disp "a", "")
                  ; ( "Content-Disposition: form-data; name=\"f\"; \
                       filename=\"\"\r\n"
                    , "x" )
                  ; (disp "", "y") ] ) ) )
    ; test "Content-Disposition" (fun () ->
          let one h = mp_read_all (mp_body [(h, "x")]) in
          let named ?filename n = Ok [field ?filename n "x"] in
          List.iter
            (fun (h, expected) -> equal ~msg:h mp_result expected (one h))
            [ ("Content-Disposition: Form-Data; NAME=a\r\n", named "a")
            ; ("Content-Disposition: form-data ; name = \"a\" \r\n", named "a")
            ; ("Content-Disposition: form-data; name=\"a\\b\"\r\n", named "a\\b")
            ; ( "Content-Disposition: form-data; name=a; filename=\"foo\\\"\r\n"
              , named ~filename:"foo\\" "a" )
            ; ( "Content-Disposition: form-data; name=a; filename=\"x;y=z\"\r\n"
              , named ~filename:"x;y=z" "a" )
            ; ( "Content-Disposition: form-data; name=a; filename*=UTF-8''x\r\n"
              , named "a" )
            ; (disp "a" ^ "X-Empty:   \r\n", named "a") ] ;
          List.iter
            (fun h -> malformed ~msg:h (one h))
            [ ""
            ; "Content-Type: text/plain\r\n"
            ; disp "a" ^ disp "a"
            ; "Content-Disposition: attachment; name=a\r\n"
            ; "Content-Disposition: form-data\r\n"
            ; "Content-Disposition: form-data; name=a; name=b\r\n"
            ; "Content-Disposition: form-data; name=a; filename=x; filename=y\r\n"
            ; "Content-Disposition: form-data; name=\"a\r\n"
            ; "Content-Disposition: form-data; name=a b\r\n"
            ; "Content-Disposition: form-data; name=\r\n"
            ; "Content-Disposition:\r\n" ] )
    ; test "incorrect head lines" (fun () ->
          List.iter
            (fun h ->
              malformed ~msg:(String.escaped h)
                (mp_read_all (mp_body [(h, "x")])) )
            [ disp "a" ^ "no colon\r\n"
            ; disp "a" ^ " X: folded\r\n"
            ; disp "a" ^ "X: a\nY: b\r\n"
            ; disp "a" ^ "X: a\rb\r\n"
            ; "\n" ^ disp "a" ] ;
          equal mp_result (Error (Mp.Malformed "not one Content-Disposition"))
            (mp_read_all ("--b\r\n\r\n" ^ disp "a" ^ "\r\nx\r\n--b--")) )
    ; test "a head at the limit with no end is too large" (fun () ->
          equal mp_result (Error Mp.Header_too_large)
            (mp_read_all ~max_header_bytes:10 "--b\r\nContent-Di") ;
          malformed (mp_read_all ~max_header_bytes:10 "--b\r\nContent-D") )
    ; test "max_header_bytes" (fun () ->
          let h = disp "a" in
          (* The head is [h] and the empty line. *)
          let size = String.length h + 2 in
          let s = mp_body [(h, "x")] in
          equal mp_result
            (Ok [field "a" "x"])
            (mp_read_all ~max_header_bytes:size s) ;
          equal mp_result (Error Mp.Header_too_large)
            (mp_read_all ~max_header_bytes:(size - 1) s) ;
          let padded = "--b  \r\n" ^ h ^ "\r\nx\r\n--b--" in
          equal mp_result
            (Ok [field "a" "x"])
            (mp_read_all ~max_header_bytes:(size + 2) padded) ;
          equal mp_result (Error Mp.Header_too_large)
            (mp_read_all ~max_header_bytes:(size + 1) padded) ;
          let pre n = String.make n 'p' ^ "\r\n" ^ s in
          equal mp_result
            (Ok [field "a" "x"])
            (mp_read_all ~max_header_bytes:size (pre size)) ;
          malformed (mp_read_all ~max_header_bytes:size (pre (size + 1))) )
    ; test "the limits at their exact values" (fun () ->
          (* The epilogue: the body ends iff it has at most max_header_bytes. *)
          let ended ?chunk k =
            let e = ref false in
            let s =
              "--b\r\n" ^ disp "a" ^ "\r\nx\r\n--b--" ^ String.make k 'e'
            in
            let body =
              Body.on_end
                (fun o ~bytes:_ -> e := o = Body.Ended)
                (Body.of_source (chunked_reader ?chunk s))
            in
            equal mp_result
              (Ok [field "a" "x"])
              (Mp.read_all ~max_header_bytes:100 ~boundary:"b" body) ;
            !e
          in
          is_true ~msg:"100" (ended 100) ;
          is_false ~msg:"101" (ended 101) ;
          (* In reads of 10 bytes, the limit counts each byte one time. *)
          is_true ~msg:"100 in chunks" (ended ~chunk:10 100) ;
          (* A preamble across many reads of a small buffer. *)
          let pre chunk k =
            Mp.fold ~max_header_bytes:100 ~buffer_size:14 ~boundary:"b"
              (Body.of_source
                 (chunked_reader ~chunk
                    (String.make k 'p' ^ "\r\n" ^ mp_body [(disp "a", "x")]) ) )
              ~init:[]
              (fun acc p -> (Mp.name p, Body.to_string (Mp.body p)) :: acc)
          in
          List.iter
            (fun chunk ->
              let msg = string_of_int chunk in
              equal ~msg (result pairs mp_error)
                (Ok [("a", "x")])
                (pre chunk 100) ;
              malformed ~msg (pre chunk 101) )
            (List.init 13 (fun i -> i + 1)) ;
          (* Padding after a delimiter between two parts. *)
          let pad k =
            mp_read_all ~max_header_bytes:100
              ( "--b\r\n" ^ disp "a" ^ "\r\nx\r\n--b" ^ String.make k ' '
              ^ "\r\n" ^ disp "c" ^ "\r\ny\r\n--b--" )
          in
          equal mp_result (Error Mp.Header_too_large) (pad 101) ;
          (* The part before the padding is complete: its read ends with no
             error. *)
          let got = ref "" in
          let r =
            Mp.fold ~max_header_bytes:100 ~boundary:"b"
              (Body.of_string
                 ( "--b\r\n" ^ disp "a" ^ "\r\nx\r\n--b" ^ String.make 101 ' '
                 ^ "\r\n" ) )
              ~init:()
              (fun () p ->
                got := try Body.to_string (Mp.body p) with _ -> "raised" )
          in
          equal string "x" !got ;
          equal (result unit mp_error) (Error Mp.Header_too_large) r ;
          equal mp_result (Ok [field "a" "x"; field "c" "y"]) (pad 2) ;
          (* A limit of 0. *)
          let parts0 s =
            Mp.fold ~max_parts:0 ~boundary:"b" (Body.of_string s) ~init:()
              (fun () _ -> () )
          in
          equal (result unit mp_error) (Ok ()) (parts0 "--b--") ;
          equal (result unit mp_error) (Error Mp.Too_many_parts)
            (parts0 (mp_body [(disp "a", "")])) ;
          (* A head at max_bytes, with no data. *)
          let h = disp "a" in
          equal mp_result
            (Ok [field "a" ""])
            (mp_read_all ~max_bytes:(String.length h + 2) (mp_body [(h, "")])) )
    ; test "no read of the body after its end (Body guards it)" (fun () ->
          let s = "--b\r\n" ^ disp "a" ^ "\r\nx\r\n--b" in
          let src = chunked_reader s in
          let over = ref false in
          let read b o l =
            if !over then failwith "a read after the end"
            else
              match src b o l with
              | 0 ->
                  over := true ;
                  0
              | n ->
                  n
          in
          malformed (Mp.read_all ~boundary:"b" (Body.of_source read)) )
    ; test "the epilogue is not read to its end" (fun () ->
          let reads = ref 0 in
          let s = mp_body [(disp "a", "x")] in
          let rest = String.make (1 lsl 20) 'e' in
          let src = chunked_reader ~chunk:1024 (s ^ rest) in
          let body = Body.of_source (fun b o l -> incr reads ; src b o l) in
          equal mp_result (Ok [field "a" "x"]) (Mp.read_all ~boundary:"b" body) ;
          is_true ~msg:"few reads" (!reads < 100) )
    ; test "max_parts" (fun () ->
          let calls = ref 0 in
          let s n =
            mp_body (List.init n (fun i -> (disp (string_of_int i), "x")))
          in
          let count n =
            calls := 0 ;
            Mp.fold ~max_parts:2 ~boundary:"b"
              (Body.of_string (s n))
              ~init:()
              (fun () _ -> incr calls)
          in
          equal (result unit mp_error) (Ok ()) (count 2) ;
          equal (result unit mp_error) (Error Mp.Too_many_parts) (count 3) ;
          equal int 2 !calls )
    ; test "the default limits" (fun () ->
          (* max_parts is 1000. *)
          let parts n = mp_body (List.init n (fun _ -> (disp "a", ""))) in
          equal (result int mp_error) (Ok 1000)
            (Result.map List.length (mp_read_all (parts 1000))) ;
          equal (result int mp_error) (Error Mp.Too_many_parts)
            (Result.map List.length (mp_read_all (parts 1001))) ;
          (* max_header_bytes is 16384: the head and its empty line. *)
          let head k =
            let h = disp "a" ^ "X: \r\n" in
            disp "a" ^ "X: "
            ^ String.make (k - String.length h - 2) 'v'
            ^ "\r\n"
          in
          equal mp_result
            (Ok [field "a" "x"])
            (mp_read_all (mp_body [(head 16384, "x")])) ;
          equal mp_result (Error Mp.Header_too_large)
            (mp_read_all (mp_body [(head 16385, "x")])) ;
          (* buffer_size is 16384: no read of the source asks for more. *)
          let most = ref 0 in
          let src = chunked_reader (mp_body [(disp "a", "x")]) in
          let body =
            Body.of_source (fun b o l ->
                most := max !most l ;
                src b o l )
          in
          equal mp_result (Ok [field "a" "x"]) (Mp.read_all ~boundary:"b" body) ;
          equal int 16384 !most ;
          (* max_bytes of read_all is 8 MiB: the head and the data. *)
          let h = disp "a" in
          let data k = String.make (k - String.length h - 2) 'x' in
          let limit = 8 * 1024 * 1024 in
          is_true ~msg:"8 MiB"
            (Result.is_ok (mp_read_all (mp_body [(h, data limit)]))) ;
          equal mp_result (Error Mp.Too_large)
            (mp_read_all (mp_body [(h, data (limit + 1))])) )
    ; test "max_bytes counts the heads and the data of all parts" (fun () ->
          let h = disp "a" in
          let s = mp_body [(h, "12345"); (h, "67890")] in
          let total = 2 * (String.length h + 2 + 5) in
          equal mp_result
            (Ok [field "a" "12345"; field "a" "67890"])
            (mp_read_all ~max_bytes:total s) ;
          equal mp_result (Error Mp.Too_large)
            (mp_read_all ~max_bytes:(total - 1) s) ;
          let empty =
            mp_body (List.init 1000 (fun _ -> (disp (String.make 1000 'n'), "")))
          in
          equal mp_result (Error Mp.Too_large)
            (mp_read_all ~max_bytes:100_000 empty) )
    ; test "the body ends before the close delimiter" (fun () ->
          List.iter
            (fun s -> malformed ~msg:(String.escaped s) (mp_read_all s))
            [ "--b\r\n" ^ disp "a" ^ "\r\nx"
            ; "--b\r\n" ^ disp "a" ^ "\r\nx\r\n--b"
            ; "--b\r\n" ^ disp "a"
            ; "only a preamble"
            ; "" ] ;
          equal mp_result (Error (Mp.Malformed "no close delimiter"))
            (mp_read_all ("--b\r\n" ^ disp "a" ^ "\r\nx\r\n--b")) ;
          equal mp_result
            (Error (Mp.Malformed "the body ends in the head of a part"))
            (mp_read_all ("--b\r\n" ^ disp "a" ^ "\r\nx\r\n--b\r\n")) )
    ; test "a copy of a part with no close delimiter fails" (fun () ->
          let outcome = ref "" in
          let s = "--b\r\n" ^ disp "a" ^ "\r\nsome data" in
          let w = Body.Writer.make ~write:(fun _ _ _ -> ()) ~flush:ignore in
          let r =
            Mp.fold ~boundary:"b" (Body.of_string s) ~init:() (fun () p ->
                let b =
                  Body.on_end
                    (fun o ~bytes:_ ->
                      outcome :=
                        match o with
                        | Body.Failed _ ->
                            "failed"
                        | Body.Ended ->
                            "ended"
                        | _ ->
                            "other" )
                    (Mp.body p)
                in
                raises Body.Truncated (fun () -> Body.write_to b w) )
          in
          malformed r ;
          equal string "failed" !outcome )
    ; test "the callback can read nothing or a part of the body" (fun () ->
          let s = mp_body [(disp "a", "0123456789"); (disp "b", "x")] in
          let run take =
            Mp.fold ~boundary:"b"
              (Body.of_source (chunked_reader ~chunk:3 s))
              ~init:[]
              (fun acc p ->
                let buf = Bytes.create take in
                let rec go n =
                  if n = take then n
                  else
                    match Body.read (Mp.body p) buf n (take - n) with
                    | 0 ->
                        n
                    | k ->
                        go (n + k)
                in
                (Mp.name p, Bytes.sub_string buf 0 (go 0)) :: acc )
          in
          equal (result pairs mp_error) (Ok [("b", ""); ("a", "")]) (run 0) ;
          equal (result pairs mp_error) (Ok [("b", "x"); ("a", "012")]) (run 3) ;
          (* The discard of a part larger than its read buffer. *)
          equal
            (result (list string) mp_error)
            (Ok ["b"; "a"])
            (Mp.fold ~boundary:"b"
               (Body.of_string
                  (mp_body
                     [(disp "a", String.make 10_000 'x'); (disp "b", "y")] ) )
               ~init:[]
               (fun acc p -> Mp.name p :: acc) ) )
    ; test "a read gives the safe bytes before the next read of the source"
        (fun () ->
          (* With a delimiter of 5 bytes, only "h" of "hello" cannot start a
             delimiter that the next bytes complete. *)
          let head = "--b\r\n" ^ disp "a" ^ "\r\nhello" in
          let first = ref true in
          let src buf off _ =
            if !first then begin
              first := false ;
              Bytes.blit_string head 0 buf off (String.length head) ;
              String.length head
            end
            else raise Exit
          in
          let got = ref "" in
          raises Exit (fun () ->
              Mp.fold ~boundary:"b" (Body.of_source src) ~init:() (fun () p ->
                  let b = Bytes.create 16 in
                  let n = Body.read (Mp.body p) b 0 16 in
                  got := Bytes.sub_string b 0 n ) ) ;
          equal string "h" !got )
    ; test "the discard finds a syntax error" (fun () ->
          let skip s =
            Mp.fold ~boundary:"b"
              (Body.of_source (chunked_reader ~chunk:3 s))
              ~init:()
              (fun () _ -> ())
          in
          List.iter
            (fun s -> malformed ~msg:(String.escaped s) (skip s))
            [ "--b\r\n" ^ disp "a" ^ "\r\nxyz"
            ; "--b\r\n" ^ disp "a" ^ "\r\nx\r\n--bX\r\n"
            ; "--b\r\n" ^ disp "a" ^ "\r\nx\r\n--b" ] )
    ; test "a hook that raises at the close keeps the exception of f" (fun () ->
          raises Exit (fun () ->
              Mp.fold ~boundary:"b"
                (Body.of_string (mp_body [(disp "a", "x")]))
                ~init:()
                (fun () p ->
                  ignore
                    (Body.on_end
                       (fun _ ~bytes:_ -> failwith "hook")
                       (Mp.body p) ) ;
                  raise Exit ) ) )
    ; test "a part body is valid only in its callback" (fun () ->
          let kept = ref None in
          let s = mp_body [(disp "a", "x")] in
          ignore
            (Mp.fold ~boundary:"b" (Body.of_string s) ~init:() (fun () p ->
                 kept := Some (Mp.body p) ) ) ;
          raises_match is_invalid_arg (fun () ->
              Body.read (Option.get !kept) (Bytes.create 4) 0 4 ) )
    ; test "a read in progress at the end of the callback" (fun () ->
          let gate = Weft.Ivar.create () in
          let head = "--b\r\n" ^ disp "a" ^ "\r\nxyz" in
          let first = ref true in
          let src buf off len =
            if !first then begin
              first := false ;
              Bytes.blit_string head 0 buf off (String.length head) ;
              String.length head
            end
            else begin
              Weft.sync (Weft.Ivar.read gate) ;
              ignore len ;
              0
            end
          in
          Sim_support.run (fun _ ->
              Weft.Fiber.scope (fun s ->
                  raises_match is_invalid_arg (fun () ->
                      Mp.fold ~boundary:"b" (Body.of_source src) ~init:()
                        (fun () p ->
                          Weft.Fiber.fork_daemon s (fun () ->
                              ignore
                                (Body.read (Mp.body p) (Bytes.create 4) 0 4) ) ;
                          Weft.Fiber.yield () ) ) ;
                  Weft.Fiber.cancel s Exit ) ) )
    ; test "an exception of the callback goes out with no more reads" (fun () ->
          let reads = ref 0 in
          let s =
            mp_body [(disp "a", String.make 10_000 'x'); (disp "b", "y")]
          in
          let src = chunked_reader ~chunk:100 s in
          let body = Body.of_source (fun b o l -> incr reads ; src b o l) in
          let kept = ref None in
          raises Exit (fun () ->
              Mp.fold ~boundary:"b" body ~init:() (fun () p ->
                  kept := Some (Mp.body p) ;
                  raise Exit ) ) ;
          let n = !reads in
          raises_match is_invalid_arg (fun () ->
              Body.read (Option.get !kept) (Bytes.create 4) 0 4 ) ;
          equal int n !reads ;
          is_true ~msg:"no discard" (n < 10) )
    ; test "a failure of the body is kept" (fun () ->
          (* More content than the check of an empty part needs. *)
          let head = "--b\r\n" ^ disp "a" ^ "\r\nxyzw" in
          let reads = ref 0 in
          let src buf off _ =
            incr reads ;
            if !reads = 1 then begin
              Bytes.blit_string head 0 buf off (String.length head) ;
              String.length head
            end
            else raise Body.Truncated
          in
          raises Body.Truncated (fun () ->
              Mp.fold ~boundary:"b" (Body.of_source src) ~init:() (fun () p ->
                  try ignore (Body.to_string (Mp.body p))
                  with Body.Truncated -> () ) ) ;
          equal int 2 !reads ;
          reads := 0 ;
          raises Body.Truncated (fun () ->
              Mp.fold ~boundary:"b" (Body.of_source src) ~init:() (fun () _ ->
                  () ) ) ;
          equal int 2 !reads )
    ; test "the parse is linear" (fun () ->
          (* A full buffer of 1 MiB and reads of 1 byte of the part: a search
             from the start of the buffer at each read takes minutes. *)
          let data = String.concat "" (List.init 87_000 (fun _ -> "\r\n-")) in
          let s = mp_body [(disp "a", data)] in
          let t0 = Sys.time () in
          let one = Bytes.create 1 in
          let rec size b n =
            match Body.read b one 0 1 with 0 -> n | k -> size b (n + k)
          in
          equal (result int mp_error)
            (Ok (String.length data))
            (Mp.fold ~buffer_size:(1 lsl 20) ~boundary:"b" (Body.of_string s)
               ~init:0 (fun n p -> n + size (Mp.body p) 0 ) ) ;
          let long = disp "a" ^ "X-A: " ^ String.make 16000 'a' ^ "\r\n" in
          ( match
              mp_read_all ~chunk:1 ~max_header_bytes:20_000
                (mp_body [(long, "x")])
            with
          | Ok [_] ->
              ()
          | _ ->
              fail "one field" ) ;
          (* Heads of many fields: an append to a list for each field takes more
             than 10 s. *)
          let many =
            disp "a" ^ String.concat "" (List.init 16000 (fun _ -> "a:\r\n"))
          in
          ( match
              mp_read_all ~max_header_bytes:65536
                (mp_body (List.init 40 (fun _ -> (many, ""))))
            with
          | Ok l ->
              equal int 40 (List.length l)
          | Error e ->
              fail (Format.asprintf "%a" Mp.pp_error e) ) ;
          (* The delimiter is after the buffer: each search finds no delimiter.
             A search from the start of the buffer at each read of 1 byte takes
             many seconds. *)
          let t1 = Sys.time () in
          let rec timed b n =
            if Sys.time () -. t1 > 1. then fail "a search examines bytes again"
            else match Body.read b one 0 1 with 0 -> n | k -> timed b (n + k)
          in
          let beyond = String.make (1 lsl 18) 'a' in
          equal (result int mp_error)
            (Ok (String.length beyond))
            (Mp.fold ~buffer_size:(1 lsl 16) ~boundary:"b"
               (Body.of_string (mp_body [(disp "a", beyond)]))
               ~init:0
               (fun n p -> n + timed (Mp.body p) 0) ) ;
          (* The CPU time of the process: other processes do not change it. *)
          is_true ~msg:"less than 5 s of CPU" (Sys.time () -. t0 < 5.) )
    ; test "fold refuses incorrect arguments" (fun () ->
          let f () _ = () in
          let b () = Body.of_string "" in
          List.iter
            (fun (msg, g) -> raises_match ~msg is_invalid_arg g)
            [ ( "max_parts"
              , fun () ->
                  ignore
                    (Mp.fold ~max_parts:(-1) ~boundary:"b" (b ()) ~init:() f) )
            ; ( "max_header_bytes"
              , fun () ->
                  ignore
                    (Mp.fold ~max_header_bytes:(-1) ~boundary:"b" (b ())
                       ~init:() f ) )
            ; ( "boundary"
              , fun () -> ignore (Mp.fold ~boundary:"" (b ()) ~init:() f) )
            ; ( "buffer_size"
              , fun () ->
                  ignore
                    (Mp.fold ~buffer_size:13 ~boundary:"b" (b ()) ~init:() f) )
            ; ( "max_bytes"
              , fun () ->
                  ignore (Mp.read_all ~max_bytes:(-1) ~boundary:"b" (b ())) ) ] ;
          equal (result unit mp_error) (Ok ())
            (Mp.fold ~buffer_size:14 ~boundary:"b" (Body.of_string "--b--")
               ~init:() f ) )
    ; test "Request.multipart_form checks the Content-Type" (fun () ->
          let req types =
            Request.make
              ~headers:
                (Headers.of_list
                   (List.map (fun v -> ("Content-Type", v)) types) )
              ~body:(raising Exit) Method.POST "/"
          in
          List.iter
            (fun types ->
              equal ~msg:(String.concat ", " types) mp_result
                (Error Mp.Unsupported_media_type)
                (Request.multipart_form (req types)) ;
              equal ~msg:(String.concat ", " types) (result unit mp_error)
                (Error Mp.Unsupported_media_type)
                (Request.multipart (req types) ~init:() (fun () _ -> ())) )
            [ []
            ; ["text/plain; boundary=b"]
            ; ["multipart/form-data"]
            ; [ "multipart/form-data; boundary=b"
              ; "multipart/form-data; boundary=b" ]
            ; ["multipart/mixed; boundary=b"] ] ;
          (* A limit of 0 is permitted. *)
          equal mp_result (Error Mp.Unsupported_media_type)
            (Request.multipart_form ~max_parts:0 ~max_header_bytes:0
               ~max_bytes:0 (req []) ) ;
          (* The limits come before the check of the Content-Type. *)
          List.iter
            (fun (msg, g) -> raises_match ~msg is_invalid_arg g)
            [ ( "max_parts"
              , fun () ->
                  ignore (Request.multipart_form ~max_parts:(-1) (req [])) )
            ; ( "max_header_bytes"
              , fun () ->
                  ignore
                    (Request.multipart_form ~max_header_bytes:(-1) (req [])) )
            ; ( "max_bytes"
              , fun () ->
                  ignore (Request.multipart_form ~max_bytes:(-1) (req [])) )
            ; ( "multipart max_parts"
              , fun () ->
                  ignore
                    (Request.multipart ~max_parts:(-1) (req []) ~init:()
                       (fun () _ -> () ) ) )
            ; ( "multipart max_header_bytes"
              , fun () ->
                  ignore
                    (Request.multipart ~max_header_bytes:(-1) (req []) ~init:()
                       (fun () _ -> () ) ) ) ] ;
          let r () =
            Request.with_multipart
              [ field "a" "1"
              ; field ~filename:"x\"y" ~content_type:"image/png" "f" "\x89PNG"
              ]
              (Request.make Method.POST "/")
          in
          equal mp_result
            (Ok
               [ field "a" "1"
               ; field ~filename:"x%22y" ~content_type:"image/png" "f" "\x89PNG"
               ] )
            (Request.multipart_form (r ())) ;
          equal
            (result (list string) mp_error)
            (Ok ["a"; "f"])
            ( Request.multipart (r ()) ~init:[] (fun acc p -> Mp.name p :: acc)
            |> Result.map List.rev ) )
    ; test "encode" (fun () ->
          let b, text = Mp.encode [field "a" "x"] in
          equal string "caravan-" b ;
          equal string
            "--caravan-\r\n\
             Content-Disposition: form-data; name=\"a\"\r\n\
             \r\n\
             x\r\n\
             --caravan---\r\n"
            text ;
          List.iter
            (fun (data, b) ->
              equal ~msg:(String.escaped data) string b
                (fst (Mp.encode [field "a" data])) )
            [ ("x caravan-0", "caravan-")
            ; ("x\n--caravan-0", "caravan-")
            ; ("\r\n--caravan-", "caravan-0")
            ; ("--caravan-0", "caravan-1")
            ; ("--caravan-00\r\n--caravan-01", "caravan-10")
            ; ( "--caravan-000\r\n--caravan-001\r\n--caravan-010\r\n--caravan-"
              , "caravan-011" )
            ; ("\r\n--caravan-" ^ String.make 62 '0', "caravan-1") ] ;
          (* The boundary is free in the data of each field. *)
          equal string "caravan-11"
            (fst
               (Mp.encode
                  [ field "a" "--caravan-00"
                  ; field "b" "--caravan-01\r\n--caravan-10" ] ) ) ;
          let _, text = Mp.encode [field ~filename:"\r\n" "\"" ""] in
          is_true
            (String.starts_with
               ~prefix:
                 "--caravan-\r\n\
                  Content-Disposition: form-data; name=\"%22\"; \
                  filename=\"%0D%0A\"\r\n"
               text ) ;
          raises_match is_invalid_arg (fun () ->
              Mp.encode [field ~content_type:"a\nb" "a" ""] ) ;
          refuses (fun () -> Mp.encode [field "a\x01" ""]) ;
          raises_match is_invalid_arg (fun () ->
              Mp.encode [field ~filename:"\x7f" "a" ""] ) ;
          equal (option mp_field)
            (Some (field "a" "1"))
            (Mp.find "a" [field "b" "0"; field "a" "1"; field "a" "2"]) ;
          equal (option mp_field) None (Mp.find "c" [field "a" "1"]) )
    ; test "error_status and pp_error" (fun () ->
          List.iter
            (fun (e, code, text) ->
              equal ~msg:text status (Status.v code) (Mp.error_status e) ;
              equal string text (Format.asprintf "%a" Mp.pp_error e) )
            [ (Mp.Unsupported_media_type, 415, "Unsupported_media_type")
            ; (Mp.Malformed "x", 400, "Malformed (x)")
            ; (Mp.Too_many_parts, 413, "Too_many_parts")
            ; (Mp.Header_too_large, 413, "Header_too_large")
            ; (Mp.Too_large, 413, "Too_large") ] ) ]

let multipart_props =
  let byte =
    Gen.frequency
      [ (3, Gen.of_list ['\r'; '\n'; '-'; 'b'; '\\'; '%'; ';'; '0'])
      ; (1, Gen.char_range '\x80' '\xff')
      ; (2, Gen.char) ]
  in
  (* The pieces of a delimiter of encode, thus partial matches of it. *)
  let piece =
    Gen.frequency
      [ (6, Gen.map (String.make 1) byte)
      ; (1, Gen.of_list ["\r\n--caravan-"; "--caravan-"; "\r\n--caravan"; "1"])
      ]
  in
  let data =
    Gen.map (String.concat "") (Gen.list ~size:(Gen.int_range 0 20) piece)
  in
  let name_byte =
    (* A double quote, CR and LF come back as %22, %0D and %0A, and the other
       control characters make an incorrect field. *)
    Gen.such_that
      (fun c -> c <> '"' && ((c >= ' ' && c <> '\x7f') || c >= '\x80'))
      byte
  in
  let name = Gen.string_of ~size:(Gen.int_range 0 6) name_byte in
  let gen_field =
    Gen.map
      (fun (n, f, d) -> field ?filename:f n d)
      (Gen.triple name (Gen.option name) data)
  in
  let input =
    Gen.triple
      (Gen.list ~size:(Gen.int_range 0 5) gen_field)
      (Gen.int_range 1 64) Gen.bool
  in
  group "Multipart laws"
    [ prop "fold inverts encode, in chunks and buffers of each size" input
        (fun (fields, chunk, small) ->
          let boundary, text = Mp.encode fields in
          let buffer_size =
            if small then (2 * (4 + String.length boundary)) + 4 else 16384
          in
          let got =
            Mp.fold ~buffer_size ~boundary
              (Body.of_source (chunked_reader ~chunk text))
              ~init:[]
              (fun acc p ->
                field ?filename:(Mp.filename p)
                  ?content_type:(Mp.content_type p) (Mp.name p)
                  (Body.to_string (Mp.body p))
                :: acc )
            |> Result.map List.rev
          in
          equal mp_result (Ok fields) got ) ]

let request_tests =
  group "Request"
    [ test "with_scheme" (fun () ->
          let r = Request.make Method.GET "/" in
          equal string "https" (Request.scheme (Request.with_scheme `Https r)) ;
          equal string "http"
            (Request.scheme
               (Request.with_scheme `Http (Request.with_scheme `Https r)) ) )
    ; test "is_valid_authority" (fun () ->
          List.iter
            (fun (s, ok) -> equal ~msg:s bool ok (Request.is_valid_authority s))
            [ ("a.example:80", true)
            ; ("[::1]:8080", true)
            ; ("a.example", true)
            ; ("a:", true)
            ; ("[::1]:", true)
            ; ("a,b", true)
            ; ("evil.com/x", false)
            ; ("a@b", false)
            ; ("x?y", false)
            ; ("a b", false)
            ; ("\xc3\xa9", false)
            ; ("", false)
            ; ("a:b", false) ] )
    ; test "defaults" (fun () ->
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
Accept: */*|} )
    ; test "attrs" (fun () ->
          let r = Request.make Method.GET "/" in
          is_true (Attrs.is_empty (Request.attrs r)) ;
          equal (option string) None (Request.find_attr user r) ;
          let r = Request.with_attr user "u" r in
          equal (option string) (Some "u") (Request.find_attr user r) ;
          let m = Attrs.(empty |> add route "/r") in
          let r = Request.with_attrs m r in
          equal (option string) None (Request.find_attr user r) ;
          equal (option string) (Some "/r") (Request.find_attr route r) ;
          let r = Request.with_attr user "u" r in
          equal (option string) (Some "/r") (Request.find_attr route r) )
    ; test "each with_ keeps the attrs" (fun () ->
          let r = Request.with_attr user "u" (Request.make Method.GET "/") in
          List.iter
            (fun (name, f) ->
              equal ~msg:name (option string) (Some "u")
                (Request.find_attr user (f r)) )
            [ ( "with_headers"
              , Request.with_headers (Headers.of_list [("A", "b")]) )
            ; ("with_body", Request.with_body (Body.of_string "x"))
            ; ("with_path", Request.with_path "/p")
            ; ("with_authority", Request.with_authority (Some "a.example")) ] )
    ; test "pp prints the attrs" (fun () ->
          let r =
            Request.make Method.GET "/"
            |> Request.with_attr user "u"
            |> Request.with_attr (Attrs.key ()) 1
          in
          expect (Format.asprintf "%a" Request.pp r)
          @@ __POS_OF__ {|GET http:/// HTTP/1.1
attrs: _, user|} ;
          let r = Request.with_headers (Headers.of_list [("A", "b")]) r in
          expect (Format.asprintf "%a" Request.pp r)
          @@ __POS_OF__ {|GET http:/// HTTP/1.1
A: b
attrs: _, user|} ) ]

let content_type r = Headers.find "content-type" (Response.headers r)

let body_of r =
  match Response.kind r with
  | Response.Body b ->
      b
  | Response.Upgrade _ ->
      fail "expected a body"

let response_tests =
  group "Response"
    [ test "Response.with_body" (fun () ->
          let k : string Attrs.key = Attrs.key () in
          let r =
            Response.string ~status:Status.created
              ~headers:(Headers.of_list [("X-A", "1")])
              "old"
            |> Response.with_attr k "v"
          in
          let r' = Response.with_body (Body.of_string "new") r in
          equal status Status.created (Response.status r') ;
          equal (option string) (Some "1")
            (Headers.find "x-a" (Response.headers r')) ;
          equal (option string) (Some "v") (Response.find_attr k r') ;
          ( match Response.kind r' with
          | Response.Body b ->
              equal string "new" (Body.to_string b)
          | Upgrade _ ->
              fail "an upgrade" ) ;
          raises_match is_invalid_arg (fun () ->
              Response.with_body Body.empty (Response.upgrade ignore) ) )
    ; test "make defaults to 200" (fun () ->
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
          equal fields
            [("Content-Type", "text/html")]
            (Headers.to_list
               (Response.headers (Response.string ~headers:h "<p>")) ) ;
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
          @@ __POS_OF__ {|101 Switching Protocols (upgrade)|} )
    ; test "pp prints no line break at the end" (fun () ->
          (* [expect] ignores blank lines at the end, thus [equal]. *)
          equal string "101 Switching Protocols (upgrade)"
            (Format.asprintf "%a" Response.pp (Response.upgrade ignore)) ;
          equal string "GET http:/// HTTP/1.1"
            (Format.asprintf "%a" Request.pp (Request.make Method.GET "/")) )
    ; test "attrs" (fun () ->
          let r = Response.string "x" in
          is_true (Attrs.is_empty (Response.attrs r)) ;
          equal (option string) None (Response.find_attr user r) ;
          let r = Response.with_attr user "u" r in
          equal (option string) (Some "u") (Response.find_attr user r) ;
          let r = Response.with_attrs Attrs.(empty |> add route "/r") r in
          equal (option string) None (Response.find_attr user r) ;
          equal (option string) (Some "/r") (Response.find_attr route r) ;
          let r = Response.with_attr user "u" r in
          equal (option string) (Some "/r") (Response.find_attr route r) )
    ; test "each with_ keeps the attrs and the kind" (fun () ->
          let is_upgrade r =
            match Response.kind r with
            | Response.Upgrade _ ->
                true
            | Response.Body _ ->
                false
          in
          List.iter
            (fun (base, r) ->
              let r = Response.with_attr user "u" r in
              List.iter
                (fun (name, f) ->
                  let r' = f r in
                  let msg = base ^ " " ^ name in
                  equal ~msg (option string) (Some "u")
                    (Response.find_attr user r') ;
                  equal ~msg bool (is_upgrade r) (is_upgrade r') )
                [ ("with_status", Response.with_status Status.ok)
                ; ("with_headers", Response.with_headers Headers.empty)
                ; ("with_attr", Response.with_attr route "/r") ] )
            [("body", Response.string "x"); ("upgrade", Response.upgrade ignore)] )
    ; test "pp prints the attrs" (fun () ->
          let u = Response.with_attr user "u" (Response.upgrade ignore) in
          expect (Format.asprintf "%a" Response.pp u)
          @@ __POS_OF__ {|101 Switching Protocols (upgrade)
attrs: user|} ;
          let r = Response.with_attr route "/r" (Response.string "x") in
          expect (Format.asprintf "%a" Response.pp r)
          @@ __POS_OF__
               {|200 OK
Content-Type: text/plain; charset=utf-8
attrs: route|} )
    ]

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
          equal status Status.not_found (Response.status r) )
    ; test "attrs go in and out through middleware" (fun () ->
          let set_user : Handler.middleware =
           fun h req -> h (Request.with_attr user "ann" req)
          in
          let seen = ref None in
          let log : Handler.middleware =
           fun h req ->
            let r = h req in
            seen := Response.find_attr route r ;
            r
          in
          let h req =
            match Request.find_attr user req with
            | Some u ->
                Response.with_attr route "/hello" (Response.string ("hi " ^ u))
            | None ->
                Response.string ~status:Status.unauthorized ""
          in
          let r =
            Handler.compose [log; set_user] h (Request.make Method.GET "/")
          in
          equal status Status.ok (Response.status r) ;
          equal (option string) (Some "/hello") !seen ) ]

(* accept(2) errors *)

let accept_error_tests =
  let module A = Private.Accept_error in
  let show e =
    match A.classify e with
    | `Connection ->
        "retry"
    | `Resources ->
        "wait"
    | `Listener ->
        "stop"
  in
  group "Accept_error"
    [ test "what each accept error is about" (fun () ->
          expect
            (String.concat "\n"
               (List.map
                  (fun (name, e) -> Printf.sprintf "%-30s %s" name (show e))
                  Weft.Net.
                    [ ("EMFILE", No_resources "EMFILE")
                    ; ("ENFILE", No_resources "ENFILE")
                    ; ("ENOBUFS", No_resources "ENOBUFS")
                    ; ("ENOMEM", No_resources "ENOMEM")
                    ; ("EINTR", Other "EINTR")
                    ; ("ENETDOWN", Other "ENETDOWN")
                    ; ("ENOPROTOOPT", Other "ENOPROTOOPT")
                    ; ("EHOSTUNREACH", Other "EHOSTUNREACH")
                    ; ("EOPNOTSUPP", Other "EOPNOTSUPP")
                    ; ("ENETUNREACH", Other "ENETUNREACH")
                    ; ("EPERM", Other "EPERM")
                    ; ("EPROTO", Other "EPROTO")
                    ; ("ENONET", Other "ENONET")
                    ; ("EHOSTDOWN", Other "EHOSTDOWN")
                    ; ("Bad_fd", Bad_fd)
                    ; ("EINVAL", Other "EINVAL")
                    ; ("ENOTSOCK", Other "ENOTSOCK")
                    ; ("Reset", Reset)
                    ; ("other", Other "other") ] ) )
          @@ __POS_OF__
               {|
            EMFILE                         wait
            ENFILE                         wait
            ENOBUFS                        wait
            ENOMEM                         wait
            EINTR                          retry
            ENETDOWN                       retry
            ENOPROTOOPT                    retry
            EHOSTUNREACH                   retry
            EOPNOTSUPP                     retry
            ENETUNREACH                    retry
            EPERM                          retry
            EPROTO                         retry
            ENONET                         retry
            EHOSTDOWN                      retry
            Bad_fd                         stop
            EINVAL                         stop
            ENOTSOCK                       stop
            Reset                          stop
            other                          stop
            |} )
    ]

(* The source rules. The libraries caravan, caravan.client, caravan.websocket,
   caravan.router, caravan.middleware, caravan.static, caravan.tls,
   caravan.session, caravan.auth, caravan.compress, caravan.jobs and
   caravan.html must use only weft for the time, the I/O and the domains, so
   that they run in each world of weft. The scan reads the .ml files of src,
   client, websocket, router, middleware, static, tls, session, auth, compress,
   jobs and html as text. It also reads caravan.testing (testing), which can
   also use the simulator of weft (Weft_sim). The guard of test/nounix links
   each module of eleven of these libraries (all but caravan.jobs), and forbids
   unix, threads and the worlds. The scan also forbids the parts of the stdlib
   that the guard cannot find, for example Sys.time and Domain. *)

(* The texts that the libraries must not contain. The first text is the name of
   the old effects library, in two parts, so that a search for that name in src,
   client, websocket, examples and test finds nothing. The other texts are the
   worlds and the internals of weft, the OS, the time and the processes of the
   stdlib, the stdio of the stdlib, and the locks of the stdlib. The internals
   of weft also have the names that follow "open Weft" (Private.World).
   clock.mli tells that Clock.to_ns and Clock.of_ns are for the worlds and the
   scheduler only. Fiber.Mutex and Fiber.Semaphore are permitted. *)
let forbidden =
  [ "Aff" ^ "ect"
  ; "Weft_linux"
  ; "Weft_sim"
  ; "Weft_posix"
  ; "Weft_kqueue"
  ; "Weft_windows"
  ; "Weft_os"
  ; "Weft.Private"
  ; "Private.run_world"
  ; "Private.scope_ctx"
  ; "Private.ivar_state"
  ; "Private.promise_report"
  ; "Private.Commit"
  ; "Private.World"
  ; "Private.Op"
  ; "Private.Sched"
  ; "Private.Event"
  ; "Private.Net"
  ; "Private.Chan"
  ; "Private.Ring"
  ; "Private.Job"
  ; "Private.Process"
  ; "Clock.to_ns"
  ; "Clock.of_ns"
  ; "Unix."
  ; "UnixLabels."
  ; "Thread."
  ; "Domain."
  ; "Sys.time"
  ; "Sys.command"
  ; "Sys.signal"
  ; "Sys.set_signal"
  ; "Sys.catch_break"
  ; "Sys.getenv"
  ; "Sys.readdir"
  ; "Sys.file_exists"
  ; "Sys.remove"
  ; "Sys.rename"
  ; "Random.self_init"
  ; "Random.State.make_self_init"
  ; "print_"
  ; "prerr_"
  ; "Printf.printf"
  ; "Printf.eprintf"
  ; "Format.printf"
  ; "Format.eprintf"
  ; "Format.std_formatter"
  ; "Format.err_formatter"
  ; "Format.get_std_formatter"
  ; "Format.get_err_formatter"
  ; "Scanf."
  ; "stdout"
  ; "stderr"
  ; "stdin"
  ; "open_in"
  ; "open_out"
  ; "In_channel."
  ; "Out_channel."
  ; "Printexc.print"
  ; "Mutex."
  ; "Condition."
  ; "Semaphore." ]

(* The texts that must also end a word: the next character is not a character of
   a name and not a dot. The first texts are an alias and an open of a module of
   [forbidden] ("module M = Mutex", "open Unix"). An alias or an open of Private
   is for "open Weft". The last texts are the reads of the stdin of the stdlib.
   The libraries have their own read_line functions, and [hits] permits them
   (see [reader_call]). *)
let forbidden_words =
  let modules =
    [ "Unix"
    ; "UnixLabels"
    ; "Thread"
    ; "Domain"
    ; "Sys"
    ; "Random"
    ; "Printf"
    ; "Format"
    ; "Scanf"
    ; "In_channel"
    ; "Out_channel"
    ; "Printexc"
    ; "Mutex"
    ; "Condition"
    ; "Semaphore"
    ; "Private" ]
  in
  List.concat_map
    (fun m ->
      List.concat_map
        (fun use -> [use ^ m; use ^ "Stdlib." ^ m])
        ["= "; "open "; "open! "; "include "] )
    modules
  @ ["read_line"; "read_int"; "read_float"]

(* The permitted uses: a file and a text of [forbidden]. *)
let allowed =
  [ (* The pool keeps a stdlib Mutex. No fiber code holds it across a sync, a
       world effect, or a call into weft or into a Flow. *)
    ("client/pool.ml", "Mutex.")
  ; (* The queue of caravan.jobs keeps its state in a stdlib Mutex, with the
       same rule as the pool. *)
    ("jobs/caravan_jobs.ml", "Mutex.")
  ; (* The default on_error of a queue writes to the standard error. *)
    ("jobs/caravan_jobs.ml", "Printf.eprintf")
  ; (* The default on_error writes to the standard error (write_error). *)
    ("src/config.ml", "Printf.eprintf")
  ; (* caravan.testing runs the server and the client in the simulator. *)
    ("testing/caravan_testing.ml", "Weft_sim") ]

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' ->
      true
  | _ ->
      false

(* [hits text] gives the line number and the text of each forbidden text in
   [text]. It is a plain match, with no lexer: a comment or a string must not
   contain a forbidden text. A match must start a word, so "print_" does not
   match "pp_print_string". A text of [forbidden_words] must also end a word, so
   "read_int" does not match "read_into". A lock of the stdlib does not match
   after "Fiber.". The read_line functions of the libraries take the reader [r]
   as the first argument, thus "read_line r " does not match. Also
   "Buf_reader.read_line" does not match. *)
let hits text =
  let n = String.length text in
  let at i sub =
    let k = String.length sub in
    let rec same j = j = k || (text.[i + j] = sub.[j] && same (j + 1)) in
    i + k <= n && same 0
  in
  let ends_word i = i = n || not (is_ident_char text.[i] || text.[i] = '.') in
  let after i prefix =
    let k = String.length prefix in
    i >= k && at (i - k) prefix
  in
  let is_lock sub = List.mem sub ["Mutex."; "Condition."; "Semaphore."] in
  let reader_call i sub =
    sub = "read_line" && (at (i + 9) " r " || after i "Buf_reader.")
  in
  let line = ref 1 and found = ref [] in
  for i = 0 to n - 1 do
    if i > 0 && text.[i - 1] = '\n' then incr line ;
    if i = 0 || not (is_ident_char text.[i - 1]) then begin
      List.iter
        (fun sub ->
          if at i sub && not (is_lock sub && after i "Fiber.") then
            found := (!line, sub) :: !found )
        forbidden ;
      List.iter
        (fun sub ->
          if
            at i sub
            && ends_word (i + String.length sub)
            && not (reader_call i sub)
          then found := (!line, sub) :: !found )
        forbidden_words
    end
  done ;
  List.rev !found

let show_hits l =
  String.concat "\n"
    (List.map (fun (line, sub) -> Printf.sprintf "%d %s" line sub) l)

let source_tests =
  group "Source rules"
    [ test "the scan finds a forbidden text at the start of a word" (fun () ->
          expect
            (show_hits
               (hits
                  "Unix.read fd buf 0 1\n\
                   let () = print_string \"x\"\n\
                   let m = Fiber.Mutex.create ()\n\
                   let s = Mutex.create ()\n\
                   let p = Format.pp_print_string\n\
                   let a = Net.Unix path\n\
                   let l = Buf_reader.read_line r ~max\n\
                   module M = Mutex\n\
                   module U = Stdlib.Unix\n\
                   open In_channel\n\
                   let open! Domain in\n\
                   let f = read_line\n\
                   let i = read_int ()\n\
                   let k = Buf_reader.read_into r b 0 1\n\
                   let () = Printexc.print f x\n\
                   let () = Sys.catch_break true\n\
                   let x = Scanf.scanf \"%d\" Fun.id\n\
                   let ppf = Format.get_std_formatter ()\n\
                   let w = Private.World.x\n\
                   let j = Private.Job.x\n\
                   let p = Private.Process.x\n\
                   module P = Private\n\
                   let c = Private.Cancel.check\n\
                   module F = Fiber.Mutex\n\
                   include Unix" ) )
          @@ __POS_OF__
               {|
            1 Unix.
            2 print_
            4 Mutex.
            8 = Mutex
            9 = Stdlib.Unix
            10 open In_channel
            11 open! Domain
            12 read_line
            13 read_int
            15 Printexc.print
            16 Sys.catch_break
            17 Scanf.
            18 Format.get_std_formatter
            19 Private.World
            20 Private.Job
            21 Private.Process
            22 = Private
            25 include Unix
            |} )
    ; test "the libraries use only weft for the time, the I/O and the domains"
        (fun () ->
          (* dune runs the test in _build/default/test. dune exec runs it in the
             root. *)
          let root = if Sys.file_exists "../src/config.ml" then ".." else "." in
          let files dir =
            Sys.readdir (Filename.concat root dir)
            |> Array.to_list
            (* The build of the mutation check also has the preprocessed files
               (.pp.ml) in these directories. *)
            |> List.filter (fun file ->
                Filename.check_suffix file ".ml"
                && not (Filename.check_suffix file ".pp.ml") )
            |> List.sort compare
            |> List.map (fun file -> dir ^ "/" ^ file)
          in
          let all =
            files "src" @ files "client" @ files "websocket" @ files "router"
            @ files "middleware" @ files "static" @ files "tls"
            @ files "session" @ files "auth" @ files "compress" @ files "jobs"
            @ files "html" @ files "testing"
          in
          List.iter
            (fun file ->
              is_true ~msg:("the scan finds " ^ file) (List.mem file all) )
            [ "src/server.ml"
            ; "src/h1_server.ml"
            ; "src/config.ml"
            ; "client/pool.ml"
            ; "websocket/connection.ml"
            ; "router/caravan_router.ml"
            ; "middleware/caravan_middleware.ml"
            ; "static/caravan_static.ml"
            ; "tls/caravan_tls.ml"
            ; "session/caravan_session.ml"
            ; "auth/caravan_auth.ml"
            ; "auth/json.ml"
            ; "compress/caravan_compress.ml"
            ; "jobs/caravan_jobs.ml"
            ; "html/caravan_html.ml"
            ; "testing/caravan_testing.ml" ] ;
          let found =
            List.concat_map
              (fun file ->
                In_channel.with_open_bin
                  (Filename.concat root file)
                  In_channel.input_all
                |> hits
                |> List.map (fun (line, sub) -> (file, line, sub)) )
              all
          in
          let not_allowed =
            List.filter
              (fun (file, _, sub) -> not (List.mem (file, sub) allowed))
              found
          in
          expect
            (String.concat "\n"
               (List.map
                  (fun (file, line, sub) ->
                    Printf.sprintf "%s:%d %s" file line sub )
                  not_allowed ) )
          @@ __POS_OF__ {||} ;
          (* An entry that matches nothing is old: remove it. *)
          List.iter
            (fun (file, sub) ->
              is_true
                ~msg:(file ^ " uses " ^ sub)
                (List.exists (fun (f, _, s) -> f = file && s = sub) found) )
            allowed ) ]

(* Caravan uses [is_own] where a handler, a writer job or a websocket function
   raises. Each case of the rule needs a test: a plant of a branch can look
   correct at the other sites. *)
let cancel_tests =
  let module C = Private.Cancel in
  let open Weft in
  group "Cancel"
    [ test "is_own is true only for the Cancelled of a cancelled fiber"
        (fun () ->
          Sim_support.run (fun _ ->
              is_false ~msg:"Cancelled, no cancel"
                (C.is_own (Fiber.Cancelled Exit)) ;
              is_false ~msg:"Failure, no cancel" (C.is_own (Failure "x")) ;
              Fiber.scope (fun s ->
                  Fiber.cancel s Exit ;
                  is_true ~msg:"Cancelled, cancelled"
                    (C.is_own (Fiber.Cancelled Exit)) ;
                  is_false ~msg:"Failure, cancelled" (C.is_own (Failure "x")) ;
                  is_false ~msg:"Promise.Cancelled, cancelled"
                    (C.is_own Promise.Cancelled) ) ) ) ]

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
       ; deadline_tests
       ; body_tests
       ; on_end_tests
       ; conn_info_tests
       ; attrs_tests
       ; attrs_props
       ; form_tests
       ; form_props
       ; request_form_tests
       ; cookie_tests
       ; cookie_props
       ; multipart_tests
       ; multipart_props
       ; request_tests
       ; response_tests
       ; handler_tests
       ; accept_error_tests
       ; cancel_tests
       ; source_tests ] )
