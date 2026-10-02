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

let () = exit (run "core" [ method_tests; version_tests; status_tests ])
