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

(* H1_parse.request_framing and request_target *)

(* [req ~v fields target] is a head with method [meth]. *)
let req ?(meth = "GET") ?(v = "HTTP/1.1") ?(target = "/") fields =
  parse
    (Printf.sprintf "%s %s %s\r\n%s\r\n" meth target v
       (String.concat "" (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") fields)))

let errors f = match f () with s -> s | exception H1_error.E e -> Format.asprintf "%a" H1_error.pp e

let show_framing (label, h) =
  Printf.sprintf "%s => %s" label
    (errors (fun () ->
         match H1_parse.request_framing (Lazy.force h) with
         | H1_parse.No_body -> "no body"
         | Fixed n -> Printf.sprintf "fixed %d" n
         | Chunked -> "chunked"))

let show_target (label, h) =
  Printf.sprintf "%s => %s" label
    (errors (fun () ->
         let t = H1_parse.request_target ~scheme:"http" (Lazy.force h) in
         Printf.sprintf "%s %s %S" t.scheme (Option.value ~default:"-" t.authority) t.path))

let cl v = ("Content-Length", v)
let te v = ("Transfer-Encoding", v)

let framing_tests =
  group "H1_parse.request_framing"
    [ test "framing table" (fun () ->
          expect
            (String.concat "\n"
               (List.map show_framing
                  [ ("none", lazy (req []));
                    ("CL 0", lazy (req [ cl "0" ]));
                    ("CL 42", lazy (req [ cl "42" ]));
                    ("CL 007", lazy (req [ cl "007" ]));
                    ("CL 5, 5 in one field", lazy (req [ cl "5, 5" ]));
                    ("CL 5 in two fields", lazy (req [ cl "5"; cl "5" ]));
                    ("CL 5 and 05", lazy (req [ cl "5"; cl "05" ]));
                    ("CL 5 and 6", lazy (req [ cl "5"; cl "6" ]));
                    ("CL 5, 6", lazy (req [ cl "5, 6" ]));
                    ("CL empty", lazy (req [ cl "" ]));
                    ("CL ,", lazy (req [ cl "," ]));
                    ("CL -1", lazy (req [ cl "-1" ]));
                    ("CL +1", lazy (req [ cl "+1" ]));
                    ("CL 1.0", lazy (req [ cl "1.0" ]));
                    ("CL 0x10", lazy (req [ cl "0x10" ]));
                    ("CL 18 digits", lazy (req [ cl "999999999999999999" ]));
                    ("CL 19 digits", lazy (req [ cl "9999999999999999999" ]));
                    ("TE chunked", lazy (req [ te "chunked" ]));
                    ("TE Chunked", lazy (req [ te "Chunked" ]));
                    ("TE in two fields", lazy (req [ te ""; te "chunked" ]));
                    ("TE gzip, chunked", lazy (req [ te "gzip, chunked" ]));
                    ("TE gzip", lazy (req [ te "gzip" ]));
                    ("TE chunked, gzip", lazy (req [ te "chunked, gzip" ]));
                    ("TE chunked, chunked", lazy (req [ te "chunked, chunked" ]));
                    ("TE chunked;x=1", lazy (req [ te "chunked;x=1" ]));
                    ("TE empty", lazy (req [ te "" ]));
                    ("TE and CL", lazy (req [ te "chunked"; cl "5" ]));
                    ("TE in HTTP/1.0", lazy (req ~v:"HTTP/1.0" [ te "chunked" ])) ]))
          @@ __POS_OF__ {|
            none => no body
            CL 0 => fixed 0
            CL 42 => fixed 42
            CL 007 => fixed 7
            CL 5, 5 in one field => fixed 5
            CL 5 in two fields => fixed 5
            CL 5 and 05 => fixed 5
            CL 5 and 6 => 400 Bad Request (conflicting Content-Length)
            CL 5, 6 => 400 Bad Request (conflicting Content-Length)
            CL empty => 400 Bad Request (empty Content-Length)
            CL , => 400 Bad Request (empty Content-Length)
            CL -1 => 400 Bad Request (invalid Content-Length "-1")
            CL +1 => 400 Bad Request (invalid Content-Length "+1")
            CL 1.0 => 400 Bad Request (invalid Content-Length "1.0")
            CL 0x10 => 400 Bad Request (invalid Content-Length "0x10")
            CL 18 digits => fixed 999999999999999999
            CL 19 digits => 400 Bad Request (invalid Content-Length "9999999999999999999")
            TE chunked => chunked
            TE Chunked => chunked
            TE in two fields => chunked
            TE gzip, chunked => 501 Not Implemented (transfer codings other than chunked)
            TE gzip => 400 Bad Request (chunked is not the final transfer coding)
            TE chunked, gzip => 400 Bad Request (chunked is not the final transfer coding)
            TE chunked, chunked => 400 Bad Request (chunked applied twice)
            TE chunked;x=1 => 400 Bad Request (chunked is not the final transfer coding)
            TE empty => 400 Bad Request (chunked is not the final transfer coding)
            TE and CL => 400 Bad Request (both Transfer-Encoding and Content-Length)
            TE in HTTP/1.0 => 400 Bad Request (Transfer-Encoding in HTTP/1.0)
            |}) ]

let host v = ("Host", v)

let target_tests =
  group "H1_parse.request_target"
    [ test "target and Host table" (fun () ->
          expect
            (String.concat "\n"
               (List.map show_target
                  [ ("origin", lazy (req ~target:"/a?b" [ host "a.example" ]));
                    ("origin, port", lazy (req [ host "a.example:8080" ]));
                    ("origin, IPv6", lazy (req [ host "[::1]:8080" ]));
                    ("origin, IPv4", lazy (req [ host "127.0.0.1" ]));
                    ("origin, empty Host", lazy (req [ host "" ]));
                    ("1.1 without Host", lazy (req []));
                    ("1.0 without Host", lazy (req ~v:"HTTP/1.0" []));
                    ("two Host", lazy (req [ host "a"; host "a" ]));
                    ("two Host in 1.0", lazy (req ~v:"HTTP/1.0" [ host "a"; host "b" ]));
                    ("Host with path", lazy (req [ host "a/b" ]));
                    ("Host with userinfo", lazy (req [ host "u@a" ]));
                    ("Host with bad port", lazy (req [ host "a:8x" ]));
                    ("Host empty IPv6", lazy (req [ host "[]" ]));
                    ("Host port only", lazy (req [ host ":80" ]));
                    ("absolute", lazy (req ~target:"http://b.example:81/p?q" [ host "a.example" ]));
                    ("absolute https", lazy (req ~target:"HTTPS://b.example" [ host "b.example" ]));
                    ("absolute query only", lazy (req ~target:"http://b.example?q" [ host "x" ]));
                    ("absolute userinfo", lazy (req ~target:"http://u:p@b.example/" [ host "b.example" ]));
                    ("absolute ftp", lazy (req ~target:"ftp://b.example/" [ host "b.example" ]));
                    ("absolute no //", lazy (req ~target:"http:b.example/" [ host "b.example" ]));
                    ("absolute http:", lazy (req ~target:"http:" [ host "b" ]));
                    ("absolute http:/", lazy (req ~target:"http:/" [ host "b" ]));
                    ("absolute one slash", lazy (req ~target:"http:/b.example/" [ host "b" ]));
                    ("absolute second slash only", lazy (req ~target:"http:x/b.example/" [ host "b" ]));
                    ("absolute no host", lazy (req ~target:"http:///p" [ host "b.example" ]));
                    ("absolute, still two Host", lazy (req ~target:"http://b/" [ host "a"; host "b" ]));
                    ("CONNECT", lazy (req ~meth:"CONNECT" ~target:"b.example:443" [ host "b.example:443" ]));
                    ("CONNECT no port", lazy (req ~meth:"CONNECT" ~target:"b.example" [ host "b.example" ]));
                    ("CONNECT empty port", lazy (req ~meth:"CONNECT" ~target:"b.example:" [ host "b" ]));
                    ("CONNECT origin-form", lazy (req ~meth:"CONNECT" ~target:"/" [ host "b" ]));
                    ("OPTIONS *", lazy (req ~meth:"OPTIONS" ~target:"*" [ host "a" ]));
                    ("GET *", lazy (req ~target:"*" [ host "a" ]));
                    ("OPTIONS origin", lazy (req ~meth:"OPTIONS" ~target:"/x" [ host "a" ]));
                    ("GET authority-form", lazy (req ~target:"b.example:443" [ host "b" ])) ]))
          @@ __POS_OF__ {|
            origin => http a.example "/a?b"
            origin, port => http a.example:8080 "/"
            origin, IPv6 => http [::1]:8080 "/"
            origin, IPv4 => http 127.0.0.1 "/"
            origin, empty Host => http - "/"
            1.1 without Host => 400 Bad Request (missing Host)
            1.0 without Host => http - "/"
            two Host => 400 Bad Request (more than one Host)
            two Host in 1.0 => 400 Bad Request (more than one Host)
            Host with path => 400 Bad Request (invalid Host "a/b")
            Host with userinfo => 400 Bad Request (invalid Host "u@a")
            Host with bad port => 400 Bad Request (invalid Host "a:8x")
            Host empty IPv6 => 400 Bad Request (invalid Host "[]")
            Host port only => 400 Bad Request (invalid Host ":80")
            absolute => http b.example:81 "/p?q"
            absolute https => https b.example "/"
            absolute query only => http b.example "/?q"
            absolute userinfo => 400 Bad Request (userinfo in request target)
            absolute ftp => 400 Bad Request (invalid request target "ftp://b.example/")
            absolute no // => 400 Bad Request (invalid request target "http:b.example/")
            absolute http: => 400 Bad Request (invalid request target "http:")
            absolute http:/ => 400 Bad Request (invalid request target "http:/")
            absolute one slash => 400 Bad Request (invalid request target "http:/b.example/")
            absolute second slash only => 400 Bad Request (invalid request target "http:x/b.example/")
            absolute no host => 400 Bad Request (invalid request target "http:///p")
            absolute, still two Host => 400 Bad Request (more than one Host)
            CONNECT => http b.example:443 ""
            CONNECT no port => 400 Bad Request (CONNECT needs a host:port target)
            CONNECT empty port => 400 Bad Request (CONNECT needs a host:port target)
            CONNECT origin-form => 400 Bad Request (CONNECT needs a host:port target)
            OPTIONS * => http a "*"
            GET * => 400 Bad Request (* is only for OPTIONS)
            OPTIONS origin => http a "/x"
            GET authority-form => 400 Bad Request (invalid request target "b.example:443")
            |}) ]

let host_gen =
  Gen.(
    map
      (fun (h, p) -> match p with None -> h | Some p -> h ^ ":" ^ string_of_int p)
      (pair (string_of ~size:(int_range 1 12) (of_list (List.of_seq (String.to_seq "abcz09-._~"))))
         (option (int_range 0 65535))))

let framing_props =
  group "H1_parse framing and target properties"
    [ prop "Content-Length round trips, however it is repeated"
        Gen.(pair (int_range 0 1_000_000_000_000) (pair (int_range 1 3) bool))
        (fun (n, (copies, one_field)) ->
           let v = string_of_int n in
           let fields =
             if one_field
             then [ cl (String.concat ", " (List.init copies (fun _ -> v))) ]
             else List.init copies (fun _ -> cl v)
           in
           is_true (H1_parse.request_framing (req fields) = H1_parse.Fixed n));
      prop "origin-form takes its authority from Host" (Gen.pair host_gen head_gen)
        (fun (h, (((_, target, _), _, _))) ->
           let t = H1_parse.request_target ~scheme:"http" (req ~target [ host h ]) in
           equal (option string) (Some h) t.authority;
           equal string target t.path) ]

let () =
  exit
    (run "h1"
       [ buf_reader_tests; buf_reader_model; request_head_tests; head_props; framing_tests;
         target_tests; framing_props ])
