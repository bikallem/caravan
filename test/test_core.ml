open Windtrap
open Caravan

let meth = Testable.make ~pp:Method.pp ~equal:Method.equal
let version = Testable.structural ~pp:Version.pp
let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

let standard_methods =
  Method.[ GET; HEAD; POST; PUT; DELETE; CONNECT; OPTIONS; TRACE; PATCH ]

let tchars =
  List.of_seq (String.to_seq "!#$%&'*+-.^_`|~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

let token = Gen.string_of ~size:(Gen.int_range 1 16) (Gen.of_list tchars)

let method_tests =
  group "Method"
    [ test "standard names parse to their constructors" (fun () ->
          List.iter
            (fun m -> equal meth m (Method.of_string (Method.to_string m)))
            standard_methods;
          is_true (Method.of_string "GET" = Method.GET));
      test "names are case-sensitive" (fun () ->
          is_true (Method.of_string "get" = Method.Other "get"));
      test "rejects non-tokens" (fun () ->
          List.iter
            (fun s -> raises_match is_invalid_arg (fun () -> Method.of_string s))
            [ ""; "GE T"; "GET\r"; "GET\n"; "G\000T"; "(GET)" ]);
      test "extension methods keep their name" (fun () ->
          equal string "PURGE" (Method.to_string (Method.of_string "PURGE")));
      test "equal compares names" (fun () ->
          is_true (Method.equal (Method.Other "GET") Method.GET);
          is_true (Method.compare (Method.Other "GET") Method.GET = 0));
      test "safe and idempotent" (fun () ->
          is_true (Method.is_safe Method.GET);
          is_true (not (Method.is_safe Method.POST));
          is_true (Method.is_idempotent Method.PUT);
          is_true (not (Method.is_idempotent Method.PATCH)));
      prop "to_string (of_string s) = s for any token" token (fun s ->
          equal string s (Method.to_string (Method.of_string s))) ]

let version_tests =
  group "Version"
    [ test "round trips" (fun () ->
          List.iter
            (fun v ->
               equal (option version) (Some v)
                 (Version.of_string (Version.to_string v)))
            Version.[ Http_1_0; Http_1_1; Http_2; Http_3 ]);
      test "unknown strings" (fun () ->
          List.iter
            (fun s -> is_none ~pp:Version.pp (Version.of_string s))
            [ ""; "HTTP/1.2"; "http/1.1"; "HTTP/2.0"; "HTTP/1.1 " ]);
      test "equal" (fun () ->
          is_true (Version.equal Version.Http_2 Version.Http_2);
          is_true (not (Version.equal Version.Http_1_0 Version.Http_1_1)));
      test "ordered oldest to newest" (fun () ->
          is_true (Version.compare Version.Http_1_0 Version.Http_1_1 < 0);
          is_true (Version.compare Version.Http_2 Version.Http_3 < 0)) ]

let status_tests =
  group "Status"
    [ test "rejects codes outside [100;599]" (fun () ->
          raises_match is_invalid_arg (fun () -> Status.v 99);
          raises_match is_invalid_arg (fun () -> Status.v 600));
      test "known reasons" (fun () ->
          equal string "Not Found" (Status.reason Status.not_found);
          equal string "Content Too Large" (Status.reason Status.content_too_large);
          equal string "" (Status.reason (Status.v 299)));
      test "pp" (fun () ->
          expect (Format.asprintf "%a" Status.pp Status.ok) @@ __POS_OF__ {|200 OK|};
          expect (Format.asprintf "%a" Status.pp (Status.v 299)) @@ __POS_OF__ {|299|});
      test "registered reason phrases" (fun () ->
          let registered =
            List.filter_map
              (fun n ->
                 let s = Status.v n in
                 if Status.reason s = "" then None else Some (Format.asprintf "%a" Status.pp s))
              (List.init 500 (fun i -> i + 100))
          in
          expect (String.concat "\n" registered) @@ __POS_OF__ {|
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
            |});
      prop "v and code round trip" (Gen.int_range 100 599) (fun n ->
          equal int n (Status.code (Status.v n)));
      prop "kind follows the first digit" (Gen.int_range 100 599) (fun n ->
          let expected =
            match n / 100 with
            | 1 -> `Informational | 2 -> `Success | 3 -> `Redirection
            | 4 -> `Client_error | _ -> `Server_error
          in
          is_true (Status.kind (Status.v n) = expected));
      test "constants used by the server have the right codes" (fun () ->
          List.iter
            (fun (s, n) ->
               equal status (Status.v n) s;
               is_true ~msg:"has a reason" (Status.reason s <> ""))
            Status.
              [ (continue, 100); (switching_protocols, 101); (ok, 200);
                (no_content, 204); (not_modified, 304); (bad_request, 400);
                (request_timeout, 408); (content_too_large, 413);
                (uri_too_long, 414); (expectation_failed, 417);
                (request_header_fields_too_large, 431);
                (internal_server_error, 500); (not_implemented, 501);
                (http_version_not_supported, 505) ]) ]

(* Headers *)

let fields = list (pair string string)
let h = Headers.of_list

let headers_tests =
  group "Headers"
    [ test "find is case-insensitive and returns the first field" (fun () ->
          let h = h [ ("Content-Type", "text/plain"); ("content-type", "x") ] in
          equal (option string) (Some "text/plain") (Headers.find "CONTENT-TYPE" h);
          equal (list string) [ "text/plain"; "x" ] (Headers.find_all "content-type" h);
          equal (option string) None (Headers.find "Host" h));
      test "to_list keeps order, case and duplicates" (fun () ->
          let l = [ ("Set-Cookie", "a=1"); ("Host", "x"); ("set-cookie", "b=2") ] in
          equal fields l (Headers.to_list (h l)));
      test "set replaces the first match in place and drops the others" (fun () ->
          let h' = Headers.set "accept" "*/*"
              (h [ ("Accept", "a"); ("Host", "x"); ("ACCEPT", "b") ]) in
          equal fields [ ("accept", "*/*"); ("Host", "x") ] (Headers.to_list h');
          equal fields [ ("Host", "x"); ("Accept", "a") ]
            (Headers.to_list (Headers.set "Accept" "a" (h [ ("Host", "x") ]))));
      test "tokens span every field of the name" (fun () ->
          let h = h [ ("Connection", "keep-alive, Upgrade"); ("Connection", ",close ,,") ] in
          equal (list string) [ "keep-alive"; "Upgrade"; "close" ] (Headers.tokens "connection" h);
          is_true (Headers.has_token "Connection" "UPGRADE" h);
          is_true (not (Headers.has_token "connection" "keep" h)));
      test "remove_hop_by_hop drops fields named by Connection" (fun () ->
          let h = h [ ("Host", "x"); ("Connection", "X-Trace, close");
                      ("X-Trace", "1"); ("Keep-Alive", "timeout=5");
                      ("Transfer-Encoding", "chunked"); ("TE", "trailers");
                      ("Upgrade", "websocket"); ("Accept", "*/*") ] in
          equal fields [ ("Host", "x"); ("Accept", "*/*") ]
            (Headers.to_list (Headers.remove_hop_by_hop h)));
      test "rejects invalid names and values" (fun () ->
          List.iter
            (fun (n, v) -> raises_match is_invalid_arg (fun () -> Headers.add n v Headers.empty))
            [ ("", "x"); ("Bad Name", "x"); ("Name:", "x"); ("X", "a\r\nSet-Cookie: s=1");
              ("X", "a\nb"); ("X", "a\000b"); ("X", " leading"); ("X", "trailing\t");
              ("X", "del\127") ];
          equal fields [ ("X", ""); ("Y", "a\tb c") ]
            (Headers.to_list (h [ ("X", ""); ("Y", "a\tb c") ])));
      test "equal ignores name case but not order or values" (fun () ->
          let a = h [ ("Host", "x"); ("Accept", "*/*") ] in
          is_true (Headers.equal a (h [ ("host", "x"); ("ACCEPT", "*/*") ]));
          is_true (not (Headers.equal a (h [ ("Accept", "*/*"); ("Host", "x") ])));
          is_true (not (Headers.equal a (h [ ("Host", "X"); ("Accept", "*/*") ])));
          is_true (not (Headers.equal a (h [ ("Host", "x") ]))));
      test "fold and iter visit fields in order" (fun () ->
          let h = h [ ("A", "1"); ("b", "2"); ("A", "3") ] in
          equal (list string) [ "A=1"; "b=2"; "A=3" ]
            (List.rev (Headers.fold (fun n v acc -> (n ^ "=" ^ v) :: acc) h []));
          let seen = ref [] in
          Headers.iter (fun n _ -> seen := n :: !seen) h;
          equal (list string) [ "A"; "b"; "A" ] (List.rev !seen));
      test "pp" (fun () ->
          expect (Format.asprintf "%a" Headers.pp (h [ ("Host", "x"); ("Accept", "*/*") ]))
          @@ __POS_OF__ {|Host: x
Accept: */*|}) ]

(* Generators that hit name collisions in different cases. *)
let name_gen =
  Gen.of_list ~pp:Format.pp_print_string
    [ "Host"; "host"; "HOST"; "Accept"; "accept"; "X-Id"; "x-id" ]

let value_gen =
  Gen.of_list ~pp:(fun ppf -> Format.fprintf ppf "%S") [ ""; "a"; "b"; "c d"; "*/*" ]

let fields_gen = Gen.list ~size:(Gen.int_range 0 8) (Gen.pair name_gen value_gen)

let headers_props =
  group "Headers laws"
    [ prop "to_list (of_list l) = l" fields_gen (fun l ->
          equal fields l (Headers.to_list (h l)));
      prop "find n (set n v h) = Some v" (Gen.triple name_gen value_gen fields_gen)
        (fun (n, v, l) -> equal (option string) (Some v) (Headers.find n (Headers.set n v (h l))));
      prop "remove n h has no field n" (Gen.pair name_gen fields_gen) (fun (n, l) ->
          is_true (not (Headers.mem n (Headers.remove n (h l)))));
      prop "add keeps the existing fields as a prefix"
        (Gen.triple name_gen value_gen fields_gen) (fun (n, v, l) ->
            equal fields (l @ [ (n, v) ]) (Headers.to_list (Headers.add n v (h l)))) ]

(* Stateful: Headers against an association list written as a specification. *)
module Model = struct
  let same a b = String.lowercase_ascii a = String.lowercase_ascii b
  let empty () = []
  let add n v l = l @ [ (n, v) ]
  let remove n l = List.filter (fun (n', _) -> not (same n n')) l
  let find_all n l = List.map snd (List.filter (fun (n', _) -> same n n') l)
  let find n l = match find_all n l with [] -> None | v :: _ -> Some v

  let set n v l =
    match List.find_index (fun (n', _) -> same n n') l with
    | None -> add n v l
    | Some i ->
        List.filteri (fun j (n', _) -> j = i || not (same n n')) l
        |> List.mapi (fun j f -> if j = i then (n, v) else f)
end

let hs = abstract "h" ~pp:(fun ppf l -> Testable.pp fields ppf l)

let headers_commands =
  [ command "empty" (Gen.unit @-> makes hs) Model.empty (fun () -> Headers.empty);
    command "add" (name_gen @-> value_gen @-> hs ^-> makes hs) Model.add Headers.add;
    command "set" (name_gen @-> value_gen @-> hs ^-> makes hs) Model.set Headers.set;
    command "remove" (name_gen @-> hs ^-> makes hs) Model.remove Headers.remove;
    command "find" (name_gen @-> hs ^-> returns (option string)) Model.find Headers.find;
    command "find_all" (name_gen @-> hs ^-> returns (list string)) Model.find_all
      Headers.find_all;
    command "to_list" (hs ^-> returns fields) Fun.id Headers.to_list ]

let headers_stateful =
  group "Headers model" [ stateful "behaves like an association list" headers_commands ]

(* Date *)

module Date = Private.Date

let http_date = Testable.structural ~pp:Http_date.pp
let max_time = 253402300799 (* 9999-12-31T23:59:59Z *)

let date_tests =
  group "Date"
    [ test "known dates" (fun () ->
          let enc t = Http_date.encode (Date.of_unix_time t) in
          expect (enc 0.) @@ __POS_OF__ {|Thu, 01 Jan 1970 00:00:00 GMT|};
          expect (enc 784111777.) @@ __POS_OF__ {|Sun, 06 Nov 1994 08:49:37 GMT|};
          expect (enc 951782400.) @@ __POS_OF__ {|Tue, 29 Feb 2000 00:00:00 GMT|};
          expect (enc (Float.of_int max_time)) @@ __POS_OF__ {|Fri, 31 Dec 9999 23:59:59 GMT|});
      test "fractions of a second are dropped" (fun () ->
          equal http_date (Date.of_unix_time 784111777.) (Date.of_unix_time 784111777.9));
      prop "encoding decodes to the same date" (Gen.int_range 0 max_time) (fun t ->
          let d = Date.of_unix_time (Float.of_int t) in
          equal http_date d (Http_date.decode (Http_date.encode d)));
      test "now_at caches one string per second" (fun () ->
          let enc t = Http_date.encode (Date.of_unix_time t) in
          let a = Date.now_at 1000. in
          let b = Date.now_at 1000.5 in
          equal string (enc 1000.) a;
          is_true ~msg:"same second reuses the string" (a == b);
          equal string (enc 1001.) (Date.now_at 1001.);
          equal string (enc 1000.) (Date.now_at 1000.));
      test "now is the current IMF date" (fun () ->
          let before = Unix.time () in
          let s = Date.now () in
          let after = Unix.time () in
          let candidates = List.map (fun t -> Http_date.encode (Date.of_unix_time t)) [ before; after ] in
          is_true ~msg:"matches the clock" (List.mem s candidates);
          is_true ~msg:"decodes as IMF"
            (match Http_date.decode s with `IMF _ -> true | _ -> false)) ]

(* Flow *)

(* Reads [flow] to the end, [size] bytes at a time. *)
let read_all ?(size = 4096) flow =
  let buf = Bytes.create size and out = Buffer.create 64 in
  let rec loop () =
    match Flow.read flow buf 0 size with
    | 0 -> Buffer.contents out
    | n -> Buffer.add_subbytes out buf 0 n; loop ()
  in
  loop ()

let in_main f = Affect_unix.Unix.main ~domain_count:1 f

let readable_within_20ms flow =
  in_main @@ fun () ->
  Affect.Action.invoke
    (Affect.Action.choose
       [ Flow.readable' flow true;
         Affect_unix.Mtime.wait_for' Affect_unix.Mtime.Span.(20 * ms) false ])

let flow_tests =
  group "Flow"
    [ test "of_string reads the input, then 0 for good" (fun () ->
          let flow = Flow.of_string "hello" (Buffer.create 0) in
          equal string "hello" (read_all flow);
          equal int 0 (Flow.read flow (Bytes.create 4) 0 4));
      test "chunk caps each read" (fun () ->
          let flow = Flow.of_string ~chunk:2 "hello" (Buffer.create 0) in
          let buf = Bytes.create 8 in
          equal (list int) [ 2; 2; 1; 0 ]
            (List.init 4 (fun _ -> Flow.read flow buf 0 8)));
      test "writes append to the output" (fun () ->
          let out = Buffer.create 16 in
          let flow = Flow.of_string "" out in
          Flow.write flow (Bytes.of_string "xxabcxx") 2 3;
          Flow.write_string flow "def";
          Flow.write flow (Bytes.of_string "z") 0 0;
          equal string "abcdef" (Buffer.contents out));
      test "invalid ranges are rejected" (fun () ->
          let flow = Flow.of_string "abc" (Buffer.create 0) in
          let buf = Bytes.create 4 in
          List.iter
            (fun (off, len) -> raises_match is_invalid_arg (fun () -> Flow.read flow buf off len))
            [ (0, 0); (-1, 2); (0, 5); (3, 2) ];
          raises_match is_invalid_arg (fun () -> Flow.write flow buf 2 3);
          raises_match is_invalid_arg (fun () -> Flow.of_string ~chunk:0 "" (Buffer.create 0)));
      test "read_now reads what is there without blocking" (fun () ->
          let buf = Bytes.create 4 in
          let f = Flow.of_string ~chunk:2 "abc" (Buffer.create 0) in
          equal (list (option int)) [ Some 2; Some 1; Some 0 ]
            (List.init 3 (fun _ -> Flow.read_now f buf 0 4));
          let nothing =
            Flow.make ~read:(fun _ _ _ -> failwith "blocking read")
              ~read_now:(fun _ _ _ -> None) ~write:(fun _ _ _ -> ())
              ~readable:Affect.Action.never ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          let p = Flow.prepend "x" nothing in
          equal (list (option int)) [ Some 1; None ]
            (List.init 2 (fun _ -> Flow.read_now p buf 0 4));
          List.iter
            (fun (off, len) -> raises_match is_invalid_arg (fun () -> Flow.read_now f buf off len))
            [ (0, 0); (-1, 2); (3, 2) ];
          Flow.close f;
          raises Flow.Closed (fun () -> Flow.read_now f buf 0 1));
      test "reset closes once, abortively, and close falls back to it" (fun () ->
          let log = ref [] in
          let flow ?reset () =
            Flow.make ~read:(fun _ _ _ -> 0) ~read_now:(fun _ _ _ -> Some 0)
              ~write:(fun _ _ _ -> ())
              ~readable:(Affect.Action.always ()) ~shutdown_send:ignore
              ~close:(fun () -> log := "close" :: !log)
              ?reset ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          let f = flow ~reset:(fun () -> log := "reset" :: !log) () in
          Flow.reset f;
          Flow.reset f;
          Flow.close f;
          equal (list string) [ "reset" ] !log;
          raises Flow.Closed (fun () -> Flow.read f (Bytes.create 1) 0 1);
          log := [];
          Flow.reset (flow ());
          equal (list string) ~msg:"without reset, close" [ "close" ] !log);
      test "close is idempotent and later use raises Closed" (fun () ->
          let closes = ref 0 in
          let flow =
            Flow.make ~read:(fun _ _ _ -> 0) ~read_now:(fun _ _ _ -> Some 0)
              ~write:(fun _ _ _ -> ())
              ~readable:(Affect.Action.always ()) ~shutdown_send:ignore
              ~close:(fun () -> incr closes)
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          Flow.close flow;
          Flow.close flow;
          equal int 1 !closes;
          raises Flow.Closed (fun () -> Flow.read flow (Bytes.create 1) 0 1);
          raises Flow.Closed (fun () -> Flow.write_string flow "x"));
      test "make delegates to its functions" (fun () ->
          let log = ref [] in
          let note s = log := s :: !log in
          let flow =
            Flow.make
              ~read:(fun buf off _ -> note "read"; Bytes.set buf off 'a'; 1)
              ~read_now:(fun _ _ _ -> note "read_now"; None)
              ~write:(fun _ _ len -> note (Printf.sprintf "write %d" len))
              ~readable:(Affect.Action.always ())
              ~shutdown_send:(fun () -> note "shutdown")
              ~close:(fun () -> note "close")
              ~set_read_deadline:(fun d -> note (if d = None then "rd none" else "rd"))
              ~set_write_deadline:(fun d -> note (if d = None then "wd none" else "wd")) ()
          in
          equal int 1 (Flow.read flow (Bytes.create 1) 0 1);
          equal (option int) None (Flow.read_now flow (Bytes.create 1) 0 1);
          Flow.write_string flow "abc";
          Flow.write_string flow "";
          Flow.shutdown_send flow;
          Flow.set_read_deadline flow None;
          Flow.set_write_deadline flow None;
          Flow.close flow;
          equal (list string)
            [ "read"; "read_now"; "write 3"; "shutdown"; "rd none"; "wd none"; "close" ]
            (List.rev !log));
      test "prepend reads its bytes first" (fun () ->
          let out = Buffer.create 8 in
          let flow = Flow.prepend "ab" (Flow.prepend "" (Flow.of_string "cd" out)) in
          equal string "abcd" (read_all ~size:3 flow);
          Flow.write_string flow "w";
          equal string "w" (Buffer.contents out);
          equal string "xyz"
            (read_all (Flow.prepend "x" (Flow.prepend "y" (Flow.of_string "z" out)))));
      test "prepend passes shutdown and close to the flow" (fun () ->
          let log = ref [] in
          let flow =
            Flow.make ~read:(fun _ _ _ -> 0) ~read_now:(fun _ _ _ -> Some 0)
              ~write:(fun _ _ _ -> ())
              ~readable:(Affect.Action.always ())
              ~shutdown_send:(fun () -> log := "shutdown" :: !log)
              ~close:(fun () -> log := "close" :: !log)
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          let p = Flow.prepend "x" flow in
          Flow.shutdown_send p;
          Flow.close p;
          equal (list string) [ "shutdown"; "close" ] (List.rev !log);
          let f = Flow.of_string "" (Buffer.create 0) in
          Flow.set_read_deadline f (Some (Affect_unix.Mtime.now ()));
          Flow.set_write_deadline f None;
          Flow.shutdown_send f;
          equal int 0 (Flow.read f (Bytes.create 1) 0 1));
      test "prepend is readable while its bytes remain" (fun () ->
          let never =
            Flow.make ~read:(fun _ _ _ -> 0) ~read_now:(fun _ _ _ -> Some 0)
              ~write:(fun _ _ _ -> ())
              ~readable:Affect.Action.never ~shutdown_send:ignore ~close:ignore
              ~set_read_deadline:ignore ~set_write_deadline:ignore ()
          in
          let flow = Flow.prepend "a" never in
          is_true ~msg:"readable before the prefix is read" (readable_within_20ms flow);
          equal int 1 (Flow.read flow (Bytes.create 4) 0 4);
          is_true ~msg:"waits on the underlying flow after"
            (not (readable_within_20ms flow)));
      prop "reads return the input however it is split"
        Gen.(pair (pair string (int_range 0 64)) (pair (int_range 1 9) (int_range 1 9)))
        (fun ((input, cut), (chunk, size)) ->
           let cut = Int.min cut (String.length input) in
           let flow =
             Flow.prepend (String.sub input 0 cut)
               (Flow.of_string ~chunk
                  (String.sub input cut (String.length input - cut))
                  (Buffer.create 0))
           in
           equal string input (read_all ~size flow)) ]

(* Body *)

(* A source that returns [s] in pieces of at most [chunk] bytes. *)
let chunked_reader ?(chunk = max_int) s =
  let pos = ref 0 in
  fun buf off len ->
    let n = Int.min (Int.min len chunk) (String.length s - !pos) in
    Bytes.blit_string s !pos buf off n;
    pos := !pos + n;
    n

(* A sink recording writes and flushes as events. *)
let recording_writer () =
  let events = ref [] in
  let w =
    Body.Writer.make
      ~write:(fun buf off len -> events := Bytes.sub_string buf off len :: !events)
      ~flush:(fun () -> events := "<flush>" :: !events)
  in
  (w, fun () -> List.rev !events)

let trailers_x = Headers.of_list [ ("X-Checksum", "abc") ]

let body_tests =
  group "Body"
    [ test "empty" (fun () ->
          equal (option int) (Some 0) (Body.length Body.empty);
          equal int 0 (Body.read Body.empty (Bytes.create 4) 0 4);
          Body.close Body.empty;
          equal string "" (Body.to_string Body.empty));
      test "of_string reads in pieces; to_string returns the rest" (fun () ->
          let b = Body.of_string "hello world" in
          equal (option int) (Some 11) (Body.length b);
          let buf = Bytes.create 6 in
          equal int 6 (Body.read b buf 0 6);
          equal string "hello " (Bytes.to_string buf);
          equal string "world" (Body.to_string b);
          equal int 0 (Body.read b buf 0 6);
          is_true (Headers.is_empty (Body.trailers b)));
      test "of_source: length, trailers at the end, close once" (fun () ->
          let closes = ref 0 in
          let b =
            Body.of_source ~trailers:(fun () -> trailers_x)
              ~close:(fun () -> incr closes) (chunked_reader ~chunk:2 "abcde")
          in
          equal (option int) None (Body.length b);
          let buf = Bytes.create 8 in
          equal int 2 (Body.read b buf 0 8);
          is_true ~msg:"no trailers before the end" (Headers.is_empty (Body.trailers b));
          equal string "cde" (Body.to_string b);
          is_true (Headers.equal trailers_x (Body.trailers b));
          Body.close b;
          equal int 1 !closes;
          Body.close b;
          equal int 1 !closes;
          raises_match is_invalid_arg (fun () -> Body.read b buf 0 8));
      test "invalid lengths and ranges" (fun () ->
          raises_match is_invalid_arg (fun () -> Body.of_source ~length:(-1) (chunked_reader ""));
          raises_match is_invalid_arg (fun () -> Body.of_writer ~length:(-1) ignore);
          (* A source and a sink that never touch the buffer, so only the
             range checks can raise. *)
          let b = Body.of_source (fun _ _ _ -> 0) in
          List.iter
            (fun (off, len) -> raises_match is_invalid_arg (fun () -> Body.read b (Bytes.create 4) off len))
            [ (0, 0); (-1, 1); (2, 3); (4, 1) ];
          let w = Body.Writer.make ~write:(fun _ _ _ -> ()) ~flush:ignore in
          List.iter
            (fun (off, len) -> raises_match is_invalid_arg (fun () -> Body.Writer.write w (Bytes.create 2) off len))
            [ (-1, 1); (0, -1); (1, 2); (3, 0) ]);
      test "of_writer runs its producer into the sink" (fun () ->
          let b =
            Body.of_writer ~length:6 (fun w ->
                Body.Writer.write_string w "abc";
                Body.Writer.flush w;
                Body.Writer.write w (Bytes.of_string "xdefx") 1 3;
                Body.Writer.write_string w "";
                Body.Writer.set_trailers w trailers_x)
          in
          equal (option int) (Some 6) (Body.length b);
          raises_match is_invalid_arg (fun () -> Body.read b (Bytes.create 4) 0 4);
          let w, events = recording_writer () in
          Body.write_to b w;
          equal (list string) [ "abc"; "<flush>"; "def" ] (events ());
          is_true (Headers.equal trailers_x (Body.Writer.trailers w));
          is_true (Headers.equal trailers_x (Body.trailers b));
          raises_match is_invalid_arg (fun () -> Body.write_to b w));
      test "a closed writer body never runs" (fun () ->
          let ran = ref false in
          let b = Body.of_writer (fun _ -> ran := true) in
          Body.close b;
          let w, _ = recording_writer () in
          raises_match is_invalid_arg (fun () -> Body.write_to b w);
          is_true (not !ran));
      test "write_to flushes a source after each chunk" (fun () ->
          let b = Body.of_source ~trailers:(fun () -> trailers_x) (chunked_reader ~chunk:2 "abcde") in
          let w, events = recording_writer () in
          Body.write_to b w;
          equal (list string) [ "ab"; "<flush>"; "cd"; "<flush>"; "e"; "<flush>" ] (events ());
          is_true (Headers.equal trailers_x (Body.Writer.trailers w)));
      test "to_string ~max" (fun () ->
          equal string "abc" (Body.to_string ~max:3 (Body.of_string "abc"));
          raises Body.Too_large (fun () -> Body.to_string ~max:2 (Body.of_string "abc"));
          raises Body.Too_large (fun () ->
              Body.to_string ~max:2 (Body.of_writer (fun w -> Body.Writer.write_string w "abc"))));
      prop "a source returns its bytes however they are split"
        Gen.(pair string (pair (int_range 1 9) (int_range 1 9))) (fun (s, (chunk, size)) ->
            let b = Body.of_source (chunked_reader ~chunk s) in
            let buf = Bytes.create size and out = Buffer.create 16 in
            let rec loop () =
              match Body.read b buf 0 size with
              | 0 -> Buffer.contents out
              | n -> Buffer.add_subbytes out buf 0 n; loop ()
            in
            equal string s (loop ()));
      prop "write_to and to_string agree for every shape"
        Gen.(pair string (int_range 1 9)) (fun (s, chunk) ->
            let shapes () =
              [ Body.of_string s; Body.of_source (chunked_reader ~chunk s);
                Body.of_writer (fun w -> Body.Writer.write_string w s) ]
            in
            List.iter2
              (fun b b' ->
                 let out = Buffer.create 16 in
                 Body.write_to b
                   (Body.Writer.make ~write:(Buffer.add_subbytes out) ~flush:ignore);
                 equal string (Body.to_string b') (Buffer.contents out))
              (shapes ()) (shapes ())) ]

(* Conn_info, Request, Response, Handler *)

let loopback port = Unix.ADDR_INET (Unix.inet_addr_loopback, port)

let conn_info_tests =
  group "Conn_info"
    [ test "accessors and pp" (fun () ->
          let c = Conn_info.make ~id:7 ~peer:(loopback 51000) ~local:(loopback 8080) () in
          equal int 7 (Conn_info.id c);
          is_true (Conn_info.peer c = loopback 51000);
          is_true (Conn_info.local c = loopback 8080);
          is_true (Conn_info.scheme c = `Http);
          equal (option string) None (Conn_info.alpn c);
          expect (Format.asprintf "%a" Conn_info.pp c)
          @@ __POS_OF__ {|#7 127.0.0.1:51000 -> 127.0.0.1:8080|};
          let c6 =
            Conn_info.make ~id:1 ~peer:(Unix.ADDR_INET (Unix.inet6_addr_loopback, 1))
              ~local:(Unix.ADDR_UNIX "/run/s.sock") ~scheme:`Https ~alpn:"h2" ()
          in
          is_true (Conn_info.scheme c6 = `Https);
          equal (option string) (Some "h2") (Conn_info.alpn c6);
          expect (Format.asprintf "%a" Conn_info.pp c6) @@ __POS_OF__ {|#1 [::1]:1 -> /run/s.sock|}) ]

let request_tests =
  group "Request"
    [ test "defaults" (fun () ->
          let r = Request.make Method.GET "/" in
          equal meth Method.GET (Request.meth r);
          equal version Version.Http_1_1 (Request.version r);
          equal string "http" (Request.scheme r);
          equal (option string) None (Request.authority r);
          is_true (Headers.is_empty (Request.headers r));
          equal (option int) (Some 0) (Body.length (Request.body r));
          is_true (Request.conn r = None));
      test "path_only and query" (fun () ->
          let split p =
            let r = Request.make Method.GET p in
            (Request.path_only r, Request.query r)
          in
          let pq = pair string (option string) in
          equal pq ("/a", Some "b=1&c=?") (split "/a?b=1&c=?");
          equal pq ("/a", None) (split "/a");
          equal pq ("/a", Some "") (split "/a?");
          equal pq ("*", None) (split "*"));
      test "with_* derive new requests" (fun () ->
          let r = Request.make ~authority:"a.example" Method.POST "/x" in
          let h = Headers.of_list [ ("X-A", "1") ] in
          let r' =
            r |> Request.with_path "/y" |> Request.with_authority (Some "b.example:8080")
            |> Request.with_headers h |> Request.with_body (Body.of_string "hi")
          in
          equal string "/x" (Request.path r);
          equal (option string) (Some "a.example") (Request.authority r);
          equal string "/y" (Request.path r');
          equal (option string) (Some "b.example:8080") (Request.authority r');
          is_true (Headers.equal h (Request.headers r'));
          equal string "hi" (Body.to_string (Request.body r'));
          equal (option string) None (Request.authority (Request.with_authority None r')));
      test "rejects paths and authorities that would corrupt a request line" (fun () ->
          List.iter
            (fun bad ->
               raises_match is_invalid_arg (fun () -> Request.make Method.GET bad);
               raises_match is_invalid_arg (fun () -> Request.make ~authority:bad Method.GET "/");
               let r = Request.make Method.GET "/" in
               raises_match is_invalid_arg (fun () -> Request.with_path bad r);
               raises_match is_invalid_arg (fun () -> Request.with_authority (Some bad) r))
            [ "/a b"; "/a\r\nX: y"; "/a\n"; "/\000"; "/\127"; "/\t" ]);
      test "pp" (fun () ->
          let r =
            Request.make ~authority:"example.com"
              ~headers:(Headers.of_list [ ("Accept", "*/*") ]) Method.GET "/a?b"
          in
          expect (Format.asprintf "%a" Request.pp r)
          @@ __POS_OF__ {|GET http://example.com/a?b HTTP/1.1
Accept: */*|}) ]

let content_type r = Headers.find "content-type" (Response.headers r)

let body_of r =
  match Response.kind r with
  | Response.Body b -> b
  | Response.Upgrade _ -> fail "expected a body"

let response_tests =
  group "Response"
    [ test "make defaults to 200" (fun () ->
          let r = Response.make Body.empty in
          equal status Status.ok (Response.status r);
          is_true (Headers.is_empty (Response.headers r)));
      test "string sets a Content-Type" (fun () ->
          let r = Response.string "hello" in
          equal (option string) (Some "text/plain; charset=utf-8") (content_type r);
          equal (option int) (Some 5) (Body.length (body_of r));
          equal string "hello" (Body.to_string (body_of r));
          equal (option string) (Some "application/json")
            (content_type (Response.string ~content_type:"application/json" "{}"));
          let h = Headers.of_list [ ("Content-Type", "text/html") ] in
          equal (option string) (Some "text/html") (content_type (Response.string ~headers:h "<p>"));
          equal (option string) (Some "text/csv")
            (content_type (Response.string ~headers:h ~content_type:"text/csv" "a,b")));
      test "empty is 204 with no body" (fun () ->
          let r = Response.empty () in
          equal status Status.no_content (Response.status r);
          equal (option int) (Some 0) (Body.length (body_of r)));
      test "stream keeps its length and producer" (fun () ->
          let r = Response.stream ~length:3 (fun w -> Body.Writer.write_string w "abc") in
          equal (option int) (Some 3) (Body.length (body_of r));
          equal string "abc" (Body.to_string (body_of r)));
      test "upgrade accepts 101 and 2xx only" (fun () ->
          let r = Response.upgrade ignore in
          equal status Status.switching_protocols (Response.status r);
          is_true (match Response.kind r with Response.Upgrade _ -> true | _ -> false);
          equal status Status.ok (Response.status (Response.upgrade ~status:Status.ok ignore));
          List.iter
            (fun s -> raises_match is_invalid_arg (fun () -> Response.upgrade ~status:s ignore))
            Status.[ continue; not_found; moved_permanently ]);
      test "statuses must suit the response" (fun () ->
          let early = Status.v 103 in
          List.iter
            (fun (label, f) -> raises_match ~msg:label is_invalid_arg f)
            [ ("make 103", fun () -> ignore (Response.make ~status:early Body.empty));
              ("string 101", fun () -> ignore (Response.string ~status:Status.switching_protocols "x"));
              ("empty 100", fun () -> ignore (Response.empty ~status:Status.continue ()));
              ("stream 103", fun () -> ignore (Response.stream ~status:early ignore));
              ("with_status 103 on a body",
               fun () -> ignore (Response.with_status early (Response.string "x")));
              ("with_status 404 on an upgrade",
               fun () -> ignore (Response.with_status Status.not_found (Response.upgrade ignore))) ];
          let tunnel = Response.with_status Status.ok (Response.upgrade ignore) in
          equal status Status.ok (Response.status tunnel));
      test "with_status and with_headers" (fun () ->
          let r = Response.string "x" in
          let r' = Response.with_status Status.created (Response.with_headers Headers.empty r) in
          equal status Status.ok (Response.status r);
          equal status Status.created (Response.status r');
          is_true (Headers.is_empty (Response.headers r')));
      test "pp" (fun () ->
          expect (Format.asprintf "%a" Response.pp (Response.string "x")) @@ __POS_OF__ {|200 OK
Content-Type: text/plain; charset=utf-8|};
          expect (Format.asprintf "%a" Response.pp (Response.upgrade ignore))
          @@ __POS_OF__ {|101 Switching Protocols (upgrade)|}) ]

let handler_tests =
  group "Handler"
    [ test "compose runs the first middleware outermost" (fun () ->
          let log = Buffer.create 32 in
          let note s = Buffer.add_string log s in
          let mw name : Handler.middleware =
            fun h req ->
            note ("<" ^ name);
            let r = h req in
            note (name ^ ">");
            r
          in
          let h = Handler.compose [ mw "a"; mw "b" ] (fun _ -> note "h"; Response.empty ()) in
          ignore (h (Request.make Method.GET "/"));
          expect (Buffer.contents log) @@ __POS_OF__ {|<a<bhb>a>|};
          let r = Handler.compose [] Handler.not_found (Request.make Method.GET "/") in
          equal status Status.not_found (Response.status r)) ]

let () =
  exit
    (run "core"
       [ method_tests; version_tests; status_tests; headers_tests; headers_props;
         headers_stateful; date_tests; flow_tests; body_tests;
         conn_info_tests; request_tests; response_tests; handler_tests ])
