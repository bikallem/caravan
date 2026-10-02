open Windtrap
open Affect_http

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

let () =
  exit
    (run "core"
       [ method_tests; version_tests; status_tests; headers_tests; headers_props;
         headers_stateful; date_tests ])
