open Windtrap
open Affect_http
module Buf_reader = Private.Buf_reader

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

(* Buf_reader *)

let lines ?size ?chunk ~max s =
  let r = Buf_reader.of_string ?size ?chunk s in
  let rec loop acc =
    match Buf_reader.read_line r ~max with
    | line -> loop (line :: acc)
    | exception Buf_reader.Eof -> List.rev acc
  in
  loop []

(* Reads up to [n] bytes, fewer only at the end of the input. *)
let read_n r n =
  let buf = Bytes.create n and got = ref 0 in
  let rec loop () =
    if !got < n
    then match Buf_reader.read_into r buf !got (n - !got) with
    | 0 -> ()
    | k -> got := !got + k; loop ()
  in
  loop ();
  Bytes.sub_string buf 0 !got

let buf_reader_tests =
  group "Buf_reader"
    [ test "lines without their CRLF" (fun () ->
          equal (list string) [ "GET / HTTP/1.1"; "Host: x"; "" ]
            (lines ~max:100 "GET / HTTP/1.1\r\nHost: x\r\n\r\n"));
      test "bare LF and CR are rejected" (fun () ->
          List.iter
            (fun s ->
               raises Buf_reader.Bad_line_ending (fun () ->
                   Buf_reader.read_line (Buf_reader.of_string s) ~max:100))
            [ "a\nb"; "\n"; "a\rb\r\n"; "a\r\r\n" ]);
      test "input ending before CRLF" (fun () ->
          List.iter
            (fun s ->
               raises Buf_reader.Eof (fun () ->
                   Buf_reader.read_line (Buf_reader.of_string s) ~max:100))
            [ ""; "abc"; "abc\r" ]);
      test "max is inclusive" (fun () ->
          equal string "abcd" (Buf_reader.read_line (Buf_reader.of_string "abcd\r\n") ~max:4);
          raises Buf_reader.Line_too_long (fun () ->
              Buf_reader.read_line (Buf_reader.of_string "abcde\r\n") ~max:4);
          raises Buf_reader.Line_too_long (fun () ->
              Buf_reader.read_line (Buf_reader.of_string "abcde") ~max:4);
          equal string "" (Buf_reader.read_line (Buf_reader.of_string "\r\nx") ~max:0);
          raises Buf_reader.Line_too_long (fun () ->
              Buf_reader.read_line (Buf_reader.of_string "x\r\n") ~max:0);
          raises_match is_invalid_arg (fun () ->
              Buf_reader.read_line (Buf_reader.of_string "") ~max:(-1)));
      test "lines may outgrow the initial buffer" (fun () ->
          let long = String.make 100 'a' in
          equal (list string) [ long; "b" ]
            (lines ~size:4 ~chunk:3 ~max:100 (long ^ "\r\nb\r\n")));
      test "a failed read_line consumes nothing" (fun () ->
          let r = Buf_reader.of_string ~chunk:2 "abcdef\r\n" in
          raises Buf_reader.Line_too_long (fun () -> Buf_reader.read_line r ~max:3);
          equal int 0 (Buf_reader.consumed r);
          raises Buf_reader.Line_too_long (fun () -> Buf_reader.read_line r ~max:3);
          equal string "abcdef" (Buf_reader.read_line r ~max:6);
          let r = Buf_reader.of_string "ab" in
          raises Buf_reader.Eof (fun () -> Buf_reader.read_line r ~max:9);
          equal string "ab" (read_n r 9));
      test "read_into drains the buffer, then reads directly" (fun () ->
          let r = Buf_reader.of_string ~chunk:16 "line\r\nbody-bytes" in
          equal string "line" (Buf_reader.read_line r ~max:10);
          equal int 10 (Buf_reader.buffered r);
          let buf = Bytes.create 4 in
          equal int 4 (Buf_reader.read_into r buf 0 4);
          equal string "body" (Bytes.to_string buf);
          equal string "-bytes" (read_n r 100);
          equal int 0 (Buf_reader.read_into r buf 0 4);
          let r = Buf_reader.of_string ~size:2 "abcdef" in
          equal int 6 (Buf_reader.read_into r (Bytes.create 8) 0 8);
          List.iter
            (fun (off, len) ->
               raises_match is_invalid_arg (fun () -> Buf_reader.read_into r buf off len))
            [ (0, 0); (-1, 1); (2, 3); (5, 1) ]);
      test "take_buffered, peek_eof and consumed" (fun () ->
          let r = Buf_reader.of_string "GET / HTTP/1.1\r\n\x16\x03\x01" in
          is_true (not (Buf_reader.peek_eof r));
          ignore (Buf_reader.read_line r ~max:100);
          equal int 16 (Buf_reader.consumed r);
          equal string "\x16\x03\x01" (Buf_reader.take_buffered r);
          equal int 19 (Buf_reader.consumed r);
          equal int 0 (Buf_reader.buffered r);
          is_true (Buf_reader.peek_eof r);
          is_true (Buf_reader.peek_eof (Buf_reader.of_string ""));
          raises_match is_invalid_arg (fun () -> Buf_reader.make ~size:0 (fun _ _ _ -> 0));
          raises_match is_invalid_arg (fun () -> Buf_reader.of_string ~chunk:0 ""));
      test "of_flow reads the flow" (fun () ->
          let r = Buf_reader.of_flow (Flow.of_string ~chunk:1 "a\r\nb" (Buffer.create 0)) in
          equal string "a" (Buf_reader.read_line r ~max:5);
          equal string "b" (read_n r 5));
      prop "lines read the same however the input is split"
        Gen.(
          pair
            (list ~size:(int_range 0 6) (string_of ~size:(int_range 0 12) (char_range 'a' 'z')))
            (triple (string_of (char_range ' ' '~')) (int_range 1 7) (int_range 1 9)))
        (fun (ls, (payload, chunk, size)) ->
           let input = String.concat "" (List.map (fun l -> l ^ "\r\n") ls) ^ payload in
           let r = Buf_reader.of_string ~size ~chunk input in
           List.iter (fun l -> equal string l (Buf_reader.read_line r ~max:12)) ls;
           equal string payload (read_n r (String.length payload + 1))) ]

(* Stateful: Buf_reader against a reader that scans the string directly. *)
module Model = struct
  type t = { s : string; mutable pos : int }

  let make (s, _chunk, _size) = { s; pos = 0 }

  let read_line m max =
    let rest = String.length m.s - m.pos in
    let rec find j =
      if j >= Int.min rest (max + 1)
      then None
      else match m.s.[m.pos + j] with '\r' | '\n' -> Some j | _ -> find (j + 1)
    in
    match find 0 with
    | Some j when m.s.[m.pos + j] = '\n' -> raise Buf_reader.Bad_line_ending
    | Some j when j + 1 >= rest -> raise Buf_reader.Eof
    | Some j when m.s.[m.pos + j + 1] <> '\n' -> raise Buf_reader.Bad_line_ending
    | Some j ->
        let line = String.sub m.s m.pos j in
        m.pos <- m.pos + j + 2;
        line
    | None -> if rest > max then raise Buf_reader.Line_too_long else raise Buf_reader.Eof

  let read_n m n =
    let k = Int.min n (String.length m.s - m.pos) in
    let s = String.sub m.s m.pos k in
    m.pos <- m.pos + k;
    s

  let consumed m = m.pos
  let peek_eof m = m.pos = String.length m.s
end

let reader = abstract "r" ~pp:(fun ppf m -> Format.fprintf ppf "%S@%d" m.Model.s m.Model.pos)

let input_gen =
  Gen.(
    triple
      (string_of ~size:(int_range 0 24) (of_list ~pp:(fun ppf -> Format.fprintf ppf "%C") [ 'a'; 'b'; '\r'; '\n' ]))
      (int_range 1 6) (int_range 1 5))

let buf_reader_commands =
  [ command "of_string" (input_gen @-> makes reader) Model.make (fun (s, chunk, size) ->
        Buf_reader.of_string ~size ~chunk s);
    command "read_line" (reader ^-> Gen.int_range 0 6 @-> returns string) Model.read_line
      (fun r max -> Buf_reader.read_line r ~max);
    command "read_n" (reader ^-> Gen.int_range 1 8 @-> returns string) Model.read_n read_n;
    command "consumed" (reader ^-> returns int) Model.consumed Buf_reader.consumed;
    command "peek_eof" (reader ^-> returns bool) Model.peek_eof Buf_reader.peek_eof ]

let buf_reader_model =
  group "Buf_reader model"
    [ stateful ~count:300 "behaves like scanning the string" buf_reader_commands ]

(* H1_parse.request_head *)

module H1_error = Private.H1_error
module H1_parse = Private.H1_parse

let pp_head ppf (h : H1_parse.head) =
  Format.fprintf ppf "%a %s %a" Method.pp h.meth h.target Version.pp h.version;
  Headers.iter (fun n v -> Format.fprintf ppf "@\n  %s: %S" n v) h.headers

let parse ?(limits = H1_parse.default_limits) ?size ?chunk s =
  H1_parse.request_head limits (Buf_reader.of_string ?size ?chunk s)

let show ?limits s =
  match parse ?limits s with
  | h -> Format.asprintf "%a" pp_head h
  | exception H1_error.E e -> Format.asprintf "%a" H1_error.pp e
  | exception Buf_reader.Eof -> "end of input"

let table ?limits cases =
  String.concat "\n" (List.map (fun s -> Printf.sprintf "%S\n=> %s" s (show ?limits s)) cases)

let small = { H1_parse.max_request_line = 16; max_header_bytes = 24; max_header_count = 2 }

let request_head_tests =
  group "H1_parse.request_head"
    [ test "valid heads" (fun () ->
          expect
            (table
               [ "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n";
                 "\r\n\r\nPOST /a?b=1 HTTP/1.0\r\n\r\n";
                 "get /!~ HTTP/1.9\r\n\r\n";
                 "CONNECT example.com:443 HTTP/1.1\r\nhost:example.com:443\r\n\r\n";
                 "GET http://a.example/x HTTP/1.1\r\nX-Empty:\r\nX-Ows: \t v a l \t\r\nSet-Cookie: a\r\nset-cookie: b\r\n\r\n" ])
          @@ __POS_OF__ {|
            "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"
            => GET / HTTP/1.1
              Host: "example.com"
            "\r\n\r\nPOST /a?b=1 HTTP/1.0\r\n\r\n"
            => POST /a?b=1 HTTP/1.0
            "get /!~ HTTP/1.9\r\n\r\n"
            => get /!~ HTTP/1.1
            "CONNECT example.com:443 HTTP/1.1\r\nhost:example.com:443\r\n\r\n"
            => CONNECT example.com:443 HTTP/1.1
              host: "example.com:443"
            "GET http://a.example/x HTTP/1.1\r\nX-Empty:\r\nX-Ows: \t v a l \t\r\nSet-Cookie: a\r\nset-cookie: b\r\n\r\n"
            => GET http://a.example/x HTTP/1.1
              X-Empty: ""
              X-Ows: "v a l"
              Set-Cookie: "a"
              set-cookie: "b"
            |});
      test "invalid request lines" (fun () ->
          expect
            (table
               [ "GET  / HTTP/1.1\r\n\r\n"; "GET / HTTP/1.1 \r\n\r\n"; "GET /\r\n\r\n";
                 " GET / HTTP/1.1\r\n\r\n"; "G(T / HTTP/1.1\r\n\r\n"; "GET /a\tb HTTP/1.1\r\n\r\n";
                 "GET /\x80 HTTP/1.1\r\n\r\n"; "GET / HTTP/1\r\n\r\n"; "GET / http/1.1\r\n\r\n";
                 "GET / HTTP/1.x\r\n\r\n"; "GET / HTTP/0.9\r\n\r\n"; "GET / HTTP/2.0\r\n\r\n";
                 "GET / HTTP/1.1\n\n"; "\r\n\r\n\r\n\r\n\r\nGET / HTTP/1.1\r\n\r\n" ])
          @@ __POS_OF__ {|
            "GET  / HTTP/1.1\r\n\r\n"
            => 400 Bad Request (malformed request line)
            "GET / HTTP/1.1 \r\n\r\n"
            => 400 Bad Request (malformed request line)
            "GET /\r\n\r\n"
            => 400 Bad Request (malformed request line)
            " GET / HTTP/1.1\r\n\r\n"
            => 400 Bad Request (malformed request line)
            "G(T / HTTP/1.1\r\n\r\n"
            => 400 Bad Request (invalid method "G(T")
            "GET /a\tb HTTP/1.1\r\n\r\n"
            => 400 Bad Request (invalid request target "/a\tb")
            "GET /\128 HTTP/1.1\r\n\r\n"
            => 400 Bad Request (invalid request target "/\128")
            "GET / HTTP/1\r\n\r\n"
            => 400 Bad Request (invalid HTTP version "HTTP/1")
            "GET / http/1.1\r\n\r\n"
            => 400 Bad Request (invalid HTTP version "http/1.1")
            "GET / HTTP/1.x\r\n\r\n"
            => 400 Bad Request (invalid HTTP version "HTTP/1.x")
            "GET / HTTP/0.9\r\n\r\n"
            => 400 Bad Request (unsupported HTTP version "HTTP/0.9")
            "GET / HTTP/2.0\r\n\r\n"
            => 505 HTTP Version Not Supported
            "GET / HTTP/1.1\n\n"
            => 400 Bad Request (bare CR or LF)
            "\r\n\r\n\r\n\r\n\r\nGET / HTTP/1.1\r\n\r\n"
            => 400 Bad Request (too many empty lines before the request line)
            |});
      test "invalid field lines" (fun () ->
          expect
            (table
               [ "GET / HTTP/1.1\r\nHost: a\r\n folded\r\n\r\n";
                 "GET / HTTP/1.1\r\nHost : a\r\n\r\n"; "GET / HTTP/1.1\r\n: a\r\n\r\n";
                 "GET / HTTP/1.1\r\nHost\r\n\r\n"; "GET / HTTP/1.1\r\nX: a\000b\r\n\r\n";
                 "GET / HTTP/1.1\r\nX: a\rb\r\n\r\n"; "GET / HTTP/1.1\r\nX\x80: a\r\n\r\n";
                 "GET / HTTP/1.1\r\nX: \t \r\n\r\n" ])
          @@ __POS_OF__ {|
            "GET / HTTP/1.1\r\nHost: a\r\n folded\r\n\r\n"
            => 400 Bad Request (obsolete line folding)
            "GET / HTTP/1.1\r\nHost : a\r\n\r\n"
            => 400 Bad Request (invalid field name "Host ")
            "GET / HTTP/1.1\r\n: a\r\n\r\n"
            => 400 Bad Request (invalid field name "")
            "GET / HTTP/1.1\r\nHost\r\n\r\n"
            => 400 Bad Request (field line without a colon)
            "GET / HTTP/1.1\r\nX: a\000b\r\n\r\n"
            => 400 Bad Request (invalid value for X)
            "GET / HTTP/1.1\r\nX: a\rb\r\n\r\n"
            => 400 Bad Request (bare CR or LF)
            "GET / HTTP/1.1\r\nX\128: a\r\n\r\n"
            => 400 Bad Request (invalid field name "X\128")
            "GET / HTTP/1.1\r\nX: \t \r\n\r\n"
            => GET / HTTP/1.1
              X: ""
            |});
      test "limits" (fun () ->
          expect
            (table ~limits:small
               [ "GET /01 HTTP/1.1\r\n\r\n"; "GET /012 HTTP/1.1\r\n\r\n";
                 "GET / HTTP/1.1\r\nA: 1\r\nB: 2\r\n\r\n"; "GET / HTTP/1.1\r\nA: 1\r\nB: 2\r\nC: 3\r\n\r\n";
                 "GET / HTTP/1.1\r\nA: 0123456789\r\nB: 1234\r\n\r\n";
                 "GET / HTTP/1.1\r\nA: 0123456789\r\nB: 12345\r\n\r\n";
                 "GET / HTTP/1.1\r\nA: 0123456789012345678901\r\n\r\n" ])
          @@ __POS_OF__ {|
            "GET /01 HTTP/1.1\r\n\r\n"
            => GET /01 HTTP/1.1
            "GET /012 HTTP/1.1\r\n\r\n"
            => 414 URI Too Long
            "GET / HTTP/1.1\r\nA: 1\r\nB: 2\r\n\r\n"
            => GET / HTTP/1.1
              A: "1"
              B: "2"
            "GET / HTTP/1.1\r\nA: 1\r\nB: 2\r\nC: 3\r\n\r\n"
            => 431 Request Header Fields Too Large
            "GET / HTTP/1.1\r\nA: 0123456789\r\nB: 1234\r\n\r\n"
            => GET / HTTP/1.1
              A: "0123456789"
              B: "1234"
            "GET / HTTP/1.1\r\nA: 0123456789\r\nB: 12345\r\n\r\n"
            => 431 Request Header Fields Too Large
            "GET / HTTP/1.1\r\nA: 0123456789012345678901\r\n\r\n"
            => 431 Request Header Fields Too Large
            |});
      test "input ending early" (fun () ->
          expect (table [ ""; "GET / HTTP/1.1"; "GET / HTTP/1.1\r\nHost: a\r\n"; "GET / HTTP/1.1\r\nHost: a\r\n\r" ])
          @@ __POS_OF__ {|
            ""
            => end of input
            "GET / HTTP/1.1"
            => end of input
            "GET / HTTP/1.1\r\nHost: a\r\n"
            => end of input
            "GET / HTTP/1.1\r\nHost: a\r\n\r"
            => end of input
            |}) ]

(* Generated valid heads, serialized with random optional whitespace. *)
let token_gen =
  Gen.string_of ~size:(Gen.int_range 1 8)
    (Gen.of_list (List.of_seq (String.to_seq "abcxyzABC019-_.!~")))

let head_gen =
  let open Gen in
  let meth = one_of [ of_list Method.[ GET; POST; HEAD; DELETE; OPTIONS ]; map Method.of_string token_gen ] in
  let target =
    map (fun s -> "/" ^ s) (string_of ~size:(int_range 0 12) (of_list (List.of_seq (String.to_seq "abz09/?=&%-._~"))))
  in
  let value =
    map (fun s -> String.trim s) (string_of ~size:(int_range 0 10) (of_list (List.of_seq (String.to_seq "ab z9,;=\t\""))))
  in
  let ows = of_list [ ""; " "; "\t"; "  " ] in
  triple (triple meth target (of_list Version.[ Http_1_0; Http_1_1 ]))
    (list ~size:(int_range 0 6) (pair token_gen (pair value (pair ows ows))))
    (pair (int_range 1 9) (int_range 1 32))

let serialize ((meth, target, v), fields, _) =
  Printf.sprintf "%s %s %s\r\n%s\r\n" (Method.to_string meth) target (Version.to_string v)
    (String.concat "" (List.map (fun (n, (v, (o, o'))) -> n ^ ":" ^ o ^ v ^ o' ^ "\r\n") fields))

let head_props =
  group "H1_parse.request_head properties"
    [ prop "parses what it is given, however it is split" head_gen (fun (((meth, target, v), fields, (chunk, size)) as g) ->
          let h = parse ~chunk ~size (serialize g) in
          equal string (Method.to_string meth) (Method.to_string h.meth);
          equal string target h.target;
          is_true (h.version = v);
          equal (list (pair string string))
            (List.map (fun (n, (v, _)) -> (n, v)) fields)
            (Headers.to_list h.headers)) ]

let () =
  exit (run "h1" [ buf_reader_tests; buf_reader_model; request_head_tests; head_props ])
