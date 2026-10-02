open Windtrap
open Affect_http
module Buf_reader = Private.Buf_reader

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false
let ignore3 _ _ _ = ()

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
                 "GET / HTTP/1.x\r\n\r\n"; "GET / HTTP/1-1\r\n\r\n"; "GET  HTTP/1.1\r\n\r\n"; "GET / HTTP/0.9\r\n\r\n"; "GET / HTTP/2.0\r\n\r\n";
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
            "GET / HTTP/1-1\r\n\r\n"
            => 400 Bad Request (invalid HTTP version "HTTP/1-1")
            "GET  HTTP/1.1\r\n\r\n"
            => 400 Bad Request (invalid request target "")
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
                    ("Host bad IPv6 literal", lazy (req [ host "[::g]" ]));
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
                    ("GET authority-form", lazy (req ~target:"b.example:443" [ host "b" ]));
                    ("GET relative path", lazy (req ~target:"index.html" [ host "b" ])) ]))
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
            Host bad IPv6 literal => 400 Bad Request (invalid Host "[::g]")
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
            GET relative path => 400 Bad Request (invalid request target "index.html")
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

(* H1_body *)

module H1_body = Private.H1_body

(* Reads [b] to its end, [size] bytes at a time. *)
let read_body ?(size = 4096) b =
  let buf = Bytes.create size and out = Buffer.create 64 in
  let rec loop () =
    match Body.read b buf 0 size with
    | 0 -> Buffer.contents out
    | n -> Buffer.add_subbytes out buf 0 n; loop ()
  in
  loop ()

let h1_body_tests =
  group "H1_body.fixed"
    [ test "reads exactly n bytes and leaves the next request" (fun () ->
          let r = Buf_reader.of_string "hello worldGET / HTTP/1.1\r\n" in
          let b = H1_body.fixed r 11 in
          equal (option int) (Some 11) (Body.length b);
          let buf = Bytes.create 64 in
          equal int 11 (Body.read b buf 0 64);
          equal string "hello world" (Bytes.sub_string buf 0 11);
          equal int 0 (Body.read b buf 0 64);
          equal string "GET / HTTP/1.1" (Buf_reader.read_line r ~max:100));
      test "reads are capped by what remains" (fun () ->
          let r = Buf_reader.of_string "abcdefXYZ" in
          let b = H1_body.fixed r 6 in
          let buf = Bytes.create 4 in
          equal (list int) [ 4; 2; 0 ] (List.init 3 (fun _ -> Body.read b buf 0 4));
          equal string "XYZ" (read_n r 10));
      test "an empty body reads nothing" (fun () ->
          let reads = ref 0 in
          let r = Buf_reader.make (fun _ _ _ -> incr reads; 0) in
          equal string "" (Body.to_string (H1_body.fixed r 0));
          equal int 0 !reads);
      test "early end of input is Truncated" (fun () ->
          let b = H1_body.fixed (Buf_reader.of_string "abc") 5 in
          let buf = Bytes.create 8 in
          equal int 3 (Body.read b buf 0 8);
          raises Body.Truncated (fun () -> Body.read b buf 0 8);
          raises Body.Truncated (fun () ->
              Body.to_string (H1_body.fixed (Buf_reader.of_string "") 1)));
      prop "the body and what follows it, however the input is split"
        Gen.(
          pair (pair string string) (triple (int_range 1 9) (int_range 1 9) (int_range 1 9)))
        (fun ((body, next), (chunk, size, read_size)) ->
           let r = Buf_reader.of_string ~chunk ~size (body ^ next) in
           let b = H1_body.fixed r (String.length body) in
           equal string body (read_body ~size:read_size b);
           equal string next (read_n r (String.length next + 1))) ]

(* Chunked bodies *)

let limits = H1_parse.default_limits

(* The decoded body, its trailers and what follows it, or the error. *)
let show_chunked ?max_body input =
  let r = Buf_reader.of_string input in
  let b = H1_body.chunked ?max_body ~limits r in
  match Body.to_string b with
  | body ->
      let trailers =
        String.concat "" (List.map (fun (n, v) -> Printf.sprintf " [%s: %s]" n v) (Headers.to_list (Body.trailers b)))
      in
      Printf.sprintf "%S%s rest %S" body trailers (read_n r 100)
  | exception H1_error.E e -> Format.asprintf "%a" H1_error.pp e
  | exception Body.Truncated -> "truncated"

let chunked_table ?max_body cases =
  String.concat "\n" (List.map (fun s -> Printf.sprintf "%S\n=> %s" s (show_chunked ?max_body s)) cases)

let chunked_tests =
  group "H1_body.chunked"
    [ test "valid chunked bodies" (fun () ->
          expect
            (chunked_table
               [ "5\r\nhello\r\n6\r\n world\r\n0\r\n\r\nNEXT";
                 "0\r\n\r\n";
                 "A\r\n0123456789\r\na\r\nabcdefghij\r\n000\r\n\r\n";
                 "0005\r\nhello\r\n0\r\n\r\n";
                 "5;name\r\nhello\r\n0;last=1\r\n\r\n";
                 "5 ; a = b ;c\r\nhello\r\n0\r\n\r\n"; "5\t;\ta\t=\tb\r\nhello\r\n0\r\n\r\n";
                 "5;q=\"a \\\"b;\"\r\nhello\r\n0\r\n\r\n";
                 "5\r\nhello\r\n0\r\nX-Checksum: abc\r\nX-Other:  v \r\n\r\nNEXT";
                 "5;a=\"\\x\"\r\nhello\r\n0\r\n\r\n";
                 "5;a=\"\\\\\"\r\nhello\r\n0\r\n\r\n";
                 "000000000000005\r\nhello\r\n0\r\n\r\n" ])
          @@ __POS_OF__ {|
            "5\r\nhello\r\n6\r\n world\r\n0\r\n\r\nNEXT"
            => "hello world" rest "NEXT"
            "0\r\n\r\n"
            => "" rest ""
            "A\r\n0123456789\r\na\r\nabcdefghij\r\n000\r\n\r\n"
            => "0123456789abcdefghij" rest ""
            "0005\r\nhello\r\n0\r\n\r\n"
            => "hello" rest ""
            "5;name\r\nhello\r\n0;last=1\r\n\r\n"
            => "hello" rest ""
            "5 ; a = b ;c\r\nhello\r\n0\r\n\r\n"
            => "hello" rest ""
            "5\t;\ta\t=\tb\r\nhello\r\n0\r\n\r\n"
            => "hello" rest ""
            "5;q=\"a \\\"b;\"\r\nhello\r\n0\r\n\r\n"
            => "hello" rest ""
            "5\r\nhello\r\n0\r\nX-Checksum: abc\r\nX-Other:  v \r\n\r\nNEXT"
            => "hello" [X-Checksum: abc] [X-Other: v] rest "NEXT"
            "5;a=\"\\x\"\r\nhello\r\n0\r\n\r\n"
            => "hello" rest ""
            "5;a=\"\\\\\"\r\nhello\r\n0\r\n\r\n"
            => "hello" rest ""
            "000000000000005\r\nhello\r\n0\r\n\r\n"
            => "hello" rest ""
            |});
      test "malformed and truncated chunked bodies" (fun () ->
          expect
            (chunked_table
               [ "5\r\nhelloX\r\n0\r\n\r\n"; "g\r\n"; "\r\nhello\r\n"; "5 x\r\nhello\r\n";
                 "-5\r\nhello\r\n"; "5 \r\nhello\r\n"; "5;\r\nhello\r\n"; "5;a=\r\nhello\r\n";
                 "5;a=\"x\r\nhello\r\n"; "5;a=\"\\\001\"\r\nhello\r\n"; "5;a=\"\\\r\nhello\r\n";
                 "1000000000000000\r\n"; "5\nhello\r\n";
                 "5\r\nhello\r\n0\r\nBad Trailer: x\r\n\r\n"; "5\r\nhel"; "5\r\nhello\r\n";
                 "5\r\nhello\r\n0\r\nX: y\r\n"; "5\r\nhello" ])
          @@ __POS_OF__ {|
            "5\r\nhelloX\r\n0\r\n\r\n"
            => 400 Bad Request (missing CRLF after chunk data)
            "g\r\n"
            => 400 Bad Request (invalid chunk size)
            "\r\nhello\r\n"
            => 400 Bad Request (invalid chunk size)
            "5 x\r\nhello\r\n"
            => 400 Bad Request (invalid chunk size line)
            "-5\r\nhello\r\n"
            => 400 Bad Request (invalid chunk size)
            "5 \r\nhello\r\n"
            => 400 Bad Request (invalid chunk size line)
            "5;\r\nhello\r\n"
            => 400 Bad Request (invalid chunk extension)
            "5;a=\r\nhello\r\n"
            => 400 Bad Request (invalid chunk extension)
            "5;a=\"x\r\nhello\r\n"
            => 400 Bad Request (unterminated chunk extension value)
            "5;a=\"\\\001\"\r\nhello\r\n"
            => 400 Bad Request (invalid chunk extension value)
            "5;a=\"\\\r\nhello\r\n"
            => 400 Bad Request (invalid chunk extension value)
            "1000000000000000\r\n"
            => 400 Bad Request (chunk size too large)
            "5\nhello\r\n"
            => 400 Bad Request (invalid chunk size line)
            "5\r\nhello\r\n0\r\nBad Trailer: x\r\n\r\n"
            => 400 Bad Request (invalid field name "Bad Trailer")
            "5\r\nhel"
            => truncated
            "5\r\nhello\r\n"
            => truncated
            "5\r\nhello\r\n0\r\nX: y\r\n"
            => truncated
            "5\r\nhello"
            => truncated
            |});
      test "max_body" (fun () ->
          expect
            (chunked_table ~max_body:10
               [ "5\r\nhello\r\n5\r\nworld\r\n0\r\n\r\n"; "5\r\nhello\r\n6\r\nworld!\r\n0\r\n\r\n";
                 "b\r\n" ])
          @@ __POS_OF__ {|
            "5\r\nhello\r\n5\r\nworld\r\n0\r\n\r\n"
            => "helloworld" rest ""
            "5\r\nhello\r\n6\r\nworld!\r\n0\r\n\r\n"
            => 413 Content Too Large
            "b\r\n"
            => 413 Content Too Large
            |});
      test "make: framing and max_body" (fun () ->
          let reads = ref 0 in
          let r = Buf_reader.make (fun _ _ _ -> incr reads; 0) in
          equal (option int) (Some 0) (Body.length (H1_body.body (H1_body.make ~limits r No_body)));
          raises (H1_error.E H1_error.Content_too_large) (fun () ->
              H1_body.make ~max_body:10 ~limits r (Fixed 11));
          equal int 0 !reads;
          let r = Buf_reader.of_string "helloXchunk" in
          equal string "hello"
            (Body.to_string (H1_body.body (H1_body.make ~max_body:5 ~limits r (Fixed 5))));
          equal string "X" (read_n r 1);
          equal string "ab"
            (Body.to_string
               (H1_body.body
                  (H1_body.make ~limits (Buf_reader.of_string "2\r\nab\r\n0\r\n\r\n") Chunked)))) ]

(* Chunked encoding of a body in pieces, for the round-trip property. *)
let encode_chunked pieces ext trailers =
  let chunk p = Printf.sprintf "%x%s\r\n%s\r\n" (String.length p) ext p in
  String.concat "" (List.map chunk (List.filter (fun p -> p <> "") pieces))
  ^ "0" ^ ext ^ "\r\n"
  ^ String.concat "" (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") trailers)
  ^ "\r\n"

let chunked_props =
  group "H1_body.chunked properties"
    [ prop "decodes what was encoded, however the input is split"
        Gen.(
          triple
            (pair (list ~size:(int_range 0 5) string) (of_list [ ""; ";a"; ";a=b"; " ; a = \"q\"" ]))
            (pair (list ~size:(int_range 0 3) (pair token_gen (map String.trim (string_of (char_range 'a' 'z'))))) string)
            (triple (int_range 1 9) (int_range 1 9) (int_range 1 9)))
        (fun ((pieces, ext), (trailers, next), (chunk, size, read_size)) ->
           let r = Buf_reader.of_string ~chunk ~size (encode_chunked pieces ext trailers ^ next) in
           let b = H1_body.chunked ~limits r in
           equal string (String.concat "" pieces) (read_body ~size:read_size b);
           equal (list (pair string string)) trailers (Headers.to_list (Body.trailers b));
           equal string next (read_n r (String.length next + 1))) ]

(* Request body handles: 100-continue, completion, draining *)

let handle ?max_body ?expect_continue ?chunk input framing =
  let r = Buf_reader.of_string ?chunk input in
  (r, H1_body.make ?max_body ?expect_continue ~limits r framing)

let next = "GET /next HTTP/1.1\r\n"

let handle_tests =
  group "H1_body handle"
    [ test "expect_continue runs once, before the first read" (fun () ->
          let log = ref [] in
          let r, t =
            handle ~expect_continue:(fun () -> log := "100" :: !log) "hello" (Fixed 5)
          in
          equal (list string) [] !log;
          let b = H1_body.body t in
          let buf = Bytes.create 2 in
          equal int 0 (Buf_reader.consumed r);
          ignore (Body.read b buf 0 2);
          log := "read" :: !log;
          ignore (Body.read b buf 0 2);
          equal (list string) [ "100"; "read" ] (List.rev !log));
      test "expect_continue never runs without a body" (fun () ->
          let ran = ref false in
          let _, t = handle ~expect_continue:(fun () -> ran := true) "" No_body in
          equal string "" (Body.to_string (H1_body.body t));
          is_true (H1_body.drain t ~max:0);
          is_true (not !ran));
      test "is_complete" (fun () ->
          let _, t = handle "" No_body in
          is_true (H1_body.is_complete t);
          let _, t = handle ("hello" ^ next) (Fixed 5) in
          is_true (not (H1_body.is_complete t));
          ignore (Body.read (H1_body.body t) (Bytes.create 5) 0 5);
          is_true ~msg:"complete at its last byte" (H1_body.is_complete t);
          let _, t = handle "5\r\nhello\r\n0\r\n\r\n" Chunked in
          ignore (Body.read (H1_body.body t) (Bytes.create 5) 0 5);
          is_true ~msg:"chunked needs its last chunk" (not (H1_body.is_complete t));
          ignore (Body.to_string (H1_body.body t));
          is_true (H1_body.is_complete t));
      test "drain within max keeps the next request" (fun () ->
          let r, t = handle ~chunk:3 ("hello world" ^ next) (Fixed 11) in
          let read = Body.read (H1_body.body t) (Bytes.create 4) 0 4 in
          is_true (H1_body.drain t ~max:(11 - read));
          equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100);
          let r, t = handle ("5\r\nhello\r\n6\r\n world\r\n0\r\nX: y\r\n\r\n" ^ next) Chunked in
          is_true (H1_body.drain t ~max:11);
          equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100));
      test "drain gives up past max" (fun () ->
          let _, t = handle ~chunk:3 ("hello world" ^ next) (Fixed 11) in
          let read = Body.read (H1_body.body t) (Bytes.create 4) 0 4 in
          is_true (not (H1_body.drain t ~max:(11 - read - 1)));
          let _, t = handle "5\r\nhello\r\n6\r\n world\r\n0\r\n\r\n" Chunked in
          is_true (not (H1_body.drain t ~max:10));
          (* [max] at the size of drain's scratch buffer *)
          let _, t = handle (String.make 5000 'x') (Fixed 5000) in
          is_true (not (H1_body.drain t ~max:4096));
          let _, t = handle (String.make 4096 'x') (Fixed 4096) in
          is_true (H1_body.drain t ~max:4096));
      test "drain fails on bad or truncated bodies" (fun () ->
          let _, t = handle "5\r\nhelloX" Chunked in
          is_true (not (H1_body.drain t ~max:100));
          let _, t = handle "hel" (Fixed 5) in
          is_true (not (H1_body.drain t ~max:100));
          let _, t = handle ~max_body:3 "5\r\nhello\r\n0\r\n\r\n" Chunked in
          is_true (not (H1_body.drain t ~max:100)));
      test "drain refuses while 100 Continue is owed" (fun () ->
          let r, t = handle ~expect_continue:ignore ("hello" ^ next) (Fixed 5) in
          is_true (not (H1_body.drain t ~max:100));
          equal int 0 (Buf_reader.consumed r);
          let r, t = handle ~expect_continue:ignore ("hello" ^ next) (Fixed 5) in
          ignore (Body.read (H1_body.body t) (Bytes.create 1) 0 1);
          is_true (H1_body.drain t ~max:100);
          equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100));
      test "drain works after the handler closed the body" (fun () ->
          let r, t = handle ("hello" ^ next) (Fixed 5) in
          Body.close (H1_body.body t);
          is_true (H1_body.drain t ~max:5);
          equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100));
      prop "drain completes any partly read body and keeps what follows"
        Gen.(triple (pair string bool) (int_range 0 40) (pair (int_range 1 9) (list ~size:(int_range 0 4) string)))
        (fun ((body, chunked), read, (chunk, pieces)) ->
           let input, framing =
             if chunked
             then (encode_chunked (body :: pieces) "" [], H1_parse.Chunked)
             else (body, H1_parse.Fixed (String.length body))
           in
           let r, t = handle ~chunk (input ^ next) framing in
           let b = H1_body.body t in
           let buf = Bytes.create 64 in
           let rec read_some left =
             if left > 0
             then match Body.read b buf 0 (Int.min 64 left) with 0 -> () | n -> read_some (left - n)
           in
           read_some read;
           is_true (H1_body.drain t ~max:max_int);
           is_true (H1_body.is_complete t);
           equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100)) ]

(* Buf_writer *)

module Buf_writer = Private.Buf_writer

(* A writer whose output calls are recorded. *)
let recorded ?size () =
  let calls = ref [] in
  let w = Buf_writer.make ?size (fun buf off len -> calls := Bytes.sub_string buf off len :: !calls) in
  (w, fun () -> List.rev !calls)

let buf_writer_tests =
  group "Buf_writer"
    [ test "writes are buffered until flush" (fun () ->
          let w, calls = recorded () in
          Buf_writer.write_string w "HTTP/1.1 200 OK\r\n";
          Buf_writer.write_char w 'X';
          Buf_writer.write_bytes w (Bytes.of_string "--abc--") 2 3;
          equal (list string) [] (calls ());
          equal int 21 (Buf_writer.buffered w);
          Buf_writer.flush w;
          equal (list string) [ "HTTP/1.1 200 OK\r\nXabc" ] (calls ());
          equal int 0 (Buf_writer.buffered w);
          Buf_writer.flush w;
          equal int 1 (List.length (calls ()));
          equal int 21 (Buf_writer.written w));
      test "a write that does not fit flushes first" (fun () ->
          let w, calls = recorded ~size:4 () in
          Buf_writer.write_string w "abc";
          Buf_writer.write_string w "de";
          Buf_writer.write_string w "fg";
          Buf_writer.flush w;
          equal (list string) [ "abc"; "defg" ] (calls ()));
      test "write_char flushes a full buffer" (fun () ->
          let w, calls = recorded ~size:2 () in
          String.iter (Buf_writer.write_char w) "abcde";
          Buf_writer.flush w;
          equal (list string) [ "ab"; "cd"; "e" ] (calls ());
          equal int 5 (Buf_writer.written w));
      test "large writes go straight to the output" (fun () ->
          let w, calls = recorded ~size:4 () in
          Buf_writer.write_string w "ab";
          Buf_writer.write_string w "0123456789";
          Buf_writer.write_string w "abcd";
          Buf_writer.write_string w "";
          equal (list string) [ "ab"; "0123456789"; "abcd" ] (calls ());
          equal int 0 (Buf_writer.buffered w);
          equal int 16 (Buf_writer.written w));
      test "invalid arguments" (fun () ->
          (* Large writes skip the copy, so only the range check can raise. *)
          let w = Buf_writer.make ~size:2 ignore3 in
          List.iter
            (fun (off, len) ->
               raises_match is_invalid_arg (fun () -> Buf_writer.write_bytes w (Bytes.create 4) off len))
            [ (-1, 1); (0, -1); (2, 3); (5, 0) ];
          raises_match is_invalid_arg (fun () -> Buf_writer.make ~size:0 ignore3));
      test "of_flow writes to the flow" (fun () ->
          let out = Buffer.create 8 in
          let w = Buf_writer.of_flow (Flow.of_string "" out) in
          Buf_writer.write_string w "hi";
          equal string "" (Buffer.contents out);
          Buf_writer.flush w;
          equal string "hi" (Buffer.contents out));
      prop "output is the writes in order; calls are bounded"
        Gen.(pair (list ~size:(int_range 0 12) (string_of ~size:(int_range 0 12) (char_range 'a' 'z'))) (int_range 1 8))
        (fun (writes, size) ->
           let w, calls = recorded ~size () in
           List.iter (Buf_writer.write_string w) writes;
           Buf_writer.flush w;
           equal string (String.concat "" writes) (String.concat "" (calls ()));
           equal int (String.length (String.concat "" writes)) (Buf_writer.written w);
           List.iter
             (fun c ->
                is_true ~msg:"a call is at most size bytes, or one large write"
                  (String.length c <= size || List.mem c writes))
             (calls ())) ]

(* H1_write: framing and response heads *)

module H1_write = Private.H1_write

let show_framing_of ~meth ~version r =
  match H1_write.framing ~meth ~version r with
  | H1_write.No_body -> "no body"
  | Head_length n -> Printf.sprintf "head, length %d" n
  | Length n -> Printf.sprintf "length %d" n
  | Chunked -> "chunked"
  | Close_delimited -> "close-delimited"

let stream_body = Response.stream ignore
let sized s = Response.string s

let response_framing_tests =
  group "H1_write.framing"
    [ test "framing table" (fun () ->
          let row (label, meth, version, r) =
            Printf.sprintf "%-26s => %s" label (show_framing_of ~meth ~version r)
          in
          let st code = Response.with_status (Status.v code) in
          expect
            (String.concat "\n"
               (List.map row
                  Method.
                    [ ("GET 200 sized", GET, Version.Http_1_1, sized "hello");
                      ("GET 200 empty", GET, Http_1_1, Response.make Body.empty);
                      ("GET 200 stream 1.1", GET, Http_1_1, stream_body);
                      ("GET 200 stream 1.0", GET, Http_1_0, stream_body);
                      ("GET 200 sized 1.0", GET, Http_1_0, sized "hello");
                      ("GET 100", GET, Http_1_1, st 100 (sized "x"));
                      ("GET 101 upgrade", GET, Http_1_1, Response.upgrade ignore);
                      ("GET 204", GET, Http_1_1, Response.empty ());
                      ("GET 204 with a body", GET, Http_1_1, st 204 (sized "x"));
                      ("GET 304", GET, Http_1_1, st 304 (sized "x"));
                      ("GET 404 sized", GET, Http_1_1, st 404 (sized "nope"));
                      ("HEAD 200 sized", HEAD, Http_1_1, sized "hello");
                      ("HEAD 200 stream", HEAD, Http_1_1, stream_body);
                      ("HEAD 304", HEAD, Http_1_1, st 304 (sized "x"));
                      ("CONNECT 200", CONNECT, Http_1_1, sized "x");
                      ("CONNECT 200 upgrade", CONNECT, Http_1_1, Response.upgrade ~status:Status.ok ignore);
                      ("CONNECT 404", CONNECT, Http_1_1, st 404 (sized "no"));
                      ("CONNECT 299", CONNECT, Http_1_1, st 299 stream_body);
                      ("CONNECT 300 stream", CONNECT, Http_1_1, st 300 stream_body) ]))
          @@ __POS_OF__ {|
            GET 200 sized              => length 5
            GET 200 empty              => length 0
            GET 200 stream 1.1         => chunked
            GET 200 stream 1.0         => close-delimited
            GET 200 sized 1.0          => length 5
            GET 100                    => no body
            GET 101 upgrade            => no body
            GET 204                    => no body
            GET 204 with a body        => no body
            GET 304                    => no body
            GET 404 sized              => length 4
            HEAD 200 sized             => head, length 5
            HEAD 200 stream            => no body
            HEAD 304                   => no body
            CONNECT 200                => no body
            CONNECT 200 upgrade        => no body
            CONNECT 404                => length 2
            CONNECT 299                => no body
            CONNECT 300 stream         => chunked
            |});
      test "wants_close" (fun () ->
          let with_conn v = Response.string ~headers:(Headers.of_list [ ("Connection", v) ]) "" in
          is_true (H1_write.wants_close (with_conn "close"));
          is_true (H1_write.wants_close (with_conn "Close"));
          is_true (H1_write.wants_close (with_conn "keep-alive, close"));
          is_true (not (H1_write.wants_close (with_conn "keep-alive")));
          is_true (not (H1_write.wants_close (Response.string "")))) ]

let render ?date ?(version = Version.Http_1_1) ?(keep_alive = true) ?(meth = Method.GET) r =
  let out = Buffer.create 256 in
  let w = Buf_writer.make (fun b o l -> Buffer.add_subbytes out b o l) in
  H1_write.head w ?date ~version ~keep_alive (H1_write.framing ~meth ~version r) r;
  Buf_writer.flush w;
  (* Show CRLFs, so that the expected bytes are exact. *)
  String.concat "\\r\\n\n" (String.split_on_char '\n' (String.concat "" (String.split_on_char '\r' (Buffer.contents out))))

let date = "Sun, 06 Nov 1994 08:49:37 GMT"
let hs = Headers.of_list

let response_head_tests =
  group "H1_write.head"
    [ test "a sized response with a Date" (fun () ->
          expect (render ~date (sized "hello")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Date: Sun, 06 Nov 1994 08:49:37 GMT\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            |});
      test "the handler's Date wins; no Date when none is given" (fun () ->
          let r = Response.string ~headers:(hs [ ("date", "Mon, 07 Nov 1994 00:00:00 GMT") ]) "x" in
          expect (render ~date r ^ render (sized "x")) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            date: Mon, 07 Nov 1994 00:00:00 GMT\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            \r\n
            |});
      test "fields the connection owns are dropped, others keep their order" (fun () ->
          let r =
            Response.stream
              ~headers:
                (hs
                   [ ("X-First", "1"); ("Content-Length", "99"); ("transfer-encoding", "gzip");
                     ("CONNECTION", "keep-alive"); ("Keep-Alive", "timeout=5"); ("Upgrade", "h2c");
                     ("Proxy-Connection", "x"); ("TE", "trailers"); ("Set-Cookie", "a=1");
                     ("Set-Cookie", "b=2"); ("Trailer", "X-Sum") ])
              ignore
          in
          expect (render r) @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            X-First: 1\r\n
            Set-Cookie: a=1\r\n
            Set-Cookie: b=2\r\n
            Trailer: X-Sum\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            |});
      test "101 keeps Upgrade and says Connection: Upgrade" (fun () ->
          let r = Response.upgrade ~headers:(hs [ ("Upgrade", "websocket"); ("Connection", "x") ]) ignore in
          expect (render ~keep_alive:false r) @@ __POS_OF__ {|
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: websocket\r\n
            Connection: Upgrade\r\n
            \r\n
            |});
      test "an unregistered status has an empty reason" (fun () ->
          expect (render (Response.with_status (Status.v 299) (sized ""))) @@ __POS_OF__ {|
            HTTP/1.1 299 \r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 0\r\n
            \r\n
            |});
      test "Connection by version and keep_alive" (fun () ->
          let r = sized "x" in
          expect
            (String.concat "--\n"
               [ render ~keep_alive:true r; render ~keep_alive:false r;
                 render ~version:Http_1_0 ~keep_alive:true r;
                 render ~version:Http_1_0 ~keep_alive:false r;
                 render ~version:Http_1_0 ~keep_alive:false stream_body;
                 render ~version:Http_1_0 ~keep_alive:true stream_body ])
          @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            \r\n
            --
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            Connection: close\r\n
            \r\n
            --
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            Connection: keep-alive\r\n
            \r\n
            --
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            Connection: close\r\n
            \r\n
            --
            HTTP/1.1 200 OK\r\n
            Connection: close\r\n
            \r\n
            --
            HTTP/1.1 200 OK\r\n
            Connection: close\r\n
            \r\n
            |});
      test "HEAD and 204/304 heads" (fun () ->
          expect
            (String.concat "--\n"
               [ render ~meth:Method.HEAD (sized "hello"); render ~meth:Method.HEAD stream_body;
                 render (Response.empty ()); render (Response.with_status Status.not_modified (sized "x")) ])
          @@ __POS_OF__ {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            --
            HTTP/1.1 200 OK\r\n
            \r\n
            --
            HTTP/1.1 204 No Content\r\n
            \r\n
            --
            HTTP/1.1 304 Not Modified\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            \r\n
            |}) ]

(* H1_write.body *)

(* Shows each CRLF as a visible \\r\\n and a line break; other bytes as they are. *)
let show_crlf s =
  let b = Buffer.create (String.length s) and n = String.length s in
  let rec loop i =
    if i < n
    then
      if s.[i] = '\r' && i + 1 < n && s.[i + 1] = '\n'
      then (Buffer.add_string b "\\r\\n\n"; loop (i + 2))
      else (Buffer.add_char b s.[i]; loop (i + 1))
  in
  loop 0;
  Buffer.contents b

(* The bytes [H1_write.body] writes, and its output calls. *)
let write_body ?(size = 64) framing b =
  let w, calls = recorded ~size () in
  H1_write.body w framing b;
  (String.concat "" (calls ()), calls ())

let pieces_source ?close pieces =
  let rest = ref pieces in
  Body.of_source ?close (fun buf off _ ->
      match !rest with
      | [] -> 0
      | p :: tl -> rest := tl; Bytes.blit_string p 0 buf off (String.length p); String.length p)

let response_body_tests =
  group "H1_write.body"
    [ test "golden bodies" (fun () ->
          let producer =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "data: 1\n\n";
                Body.Writer.flush w;
                Body.Writer.write_string w "";
                Body.Writer.write_string w "0123456789abcdef!";
                Body.Writer.set_trailers w (Headers.of_list [ ("X-Sum", "42") ]))
          in
          let cases =
            [ ("length", H1_write.Length 5, Body.of_string "hello");
              ("length 0", Length 0, Body.empty);
              ("chunked string", Chunked, Body.of_string "hello");
              ("chunked source", Chunked, pieces_source [ "ab"; "cdefghijklmnopq" ]);
              ("chunked producer", Chunked, producer);
              ("chunked empty", Chunked, Body.empty);
              ("close-delimited", Close_delimited, pieces_source [ "ab"; "cd" ]);
              ("no body", No_body, Body.of_string "hidden");
              ("head", Head_length 6, Body.of_string "hidden") ]
          in
          expect
            (String.concat "\n"
               (List.map
                  (fun (label, framing, b) ->
                     Printf.sprintf "-- %s\n%s" label (show_crlf (fst (write_body framing b))))
                  cases))
          @@ __POS_OF__ {|
            -- length
            hello
            -- length 0

            -- chunked string
            5\r\n
            hello\r\n
            0\r\n
            \r\n

            -- chunked source
            2\r\n
            ab\r\n
            f\r\n
            cdefghijklmnopq\r\n
            0\r\n
            \r\n

            -- chunked producer
            9\r\n
            data: 1

            \r\n
            11\r\n
            0123456789abcdef!\r\n
            0\r\n
            X-Sum: 42\r\n
            \r\n

            -- chunked empty
            0\r\n
            \r\n

            -- close-delimited
            abcd
            -- no body

            -- head
            |});
      test "Content-Length mismatches" (fun () ->
          raises H1_write.Length_mismatch (fun () ->
              write_body (Length 6) (Body.of_string "hello"));
          let w, calls = recorded () in
          raises H1_write.Length_mismatch (fun () ->
              H1_write.body w (Length 4) (pieces_source [ "ab"; "cde" ]));
          Buf_writer.flush w;
          equal string "ab" (String.concat "" (calls ()));
          raises H1_write.Length_mismatch (fun () ->
              write_body (Length 4) (Body.of_writer (fun w -> Body.Writer.write_string w "hello"))));
      test "bodies that are not sent are closed, producers never run" (fun () ->
          let closes = ref 0 and ran = ref false in
          List.iter
            (fun framing ->
               ignore (write_body framing (pieces_source ~close:(fun () -> incr closes) [ "x" ]));
               equal string "" (fst (write_body framing (Body.of_writer (fun _ -> ran := true)))))
            H1_write.[ No_body; Head_length 1 ];
          equal int 2 !closes;
          is_true (not !ran));
      test "the body is closed once, also when writing fails" (fun () ->
          let closes = ref 0 in
          ignore (write_body (Length 2) (pieces_source ~close:(fun () -> incr closes) [ "ab" ]));
          equal int 1 !closes;
          raises H1_write.Length_mismatch (fun () ->
              write_body (Length 9) (pieces_source ~close:(fun () -> incr closes) [ "ab" ]));
          equal int 2 !closes;
          raises Exit (fun () ->
              write_body Chunked
                (Body.of_source ~close:(fun () -> incr closes) (fun _ _ _ -> raise Exit)));
          equal int 3 !closes);
      test "each source chunk and each producer flush reaches the output" (fun () ->
          equal (list string) [ "2\r\nab\r\n"; "2\r\ncd\r\n"; "0\r\n\r\n" ]
            (snd (write_body Chunked (pieces_source [ "ab"; "cd" ])));
          equal (list string) [ "a"; "b" ]
            (snd
               (write_body (Length 2)
                  (Body.of_writer (fun w ->
                       Body.Writer.write_string w "a";
                       Body.Writer.flush w;
                       Body.Writer.write_string w "b")))));
      prop "what is written decodes back"
        Gen.(
          triple (list ~size:(int_range 0 6) string)
            (list ~size:(int_range 0 3) (pair token_gen (map String.trim (string_of (char_range 'a' 'z')))))
            (pair (int_range 1 9) (int_range 1 64)))
        (fun (pieces, trailers, (chunk, size)) ->
           let produce w =
             List.iter (Body.Writer.write_string w) pieces;
             Body.Writer.set_trailers w (Headers.of_list trailers)
           in
           let wire, _ = write_body ~size Chunked (Body.of_writer produce) in
           let b = H1_body.chunked ~limits (Buf_reader.of_string ~chunk (wire ^ next)) in
           equal string (String.concat "" pieces) (Body.to_string b);
           equal (list (pair string string)) trailers (Headers.to_list (Body.trailers b));
           let all = String.concat "" pieces in
           let wire, _ = write_body ~size (Length (String.length all)) (Body.of_writer produce) in
           equal string all wire) ]

let () =
  exit
    (run "h1"
       [ buf_reader_tests; buf_reader_model; request_head_tests; head_props; framing_tests;
         target_tests; framing_props; h1_body_tests; chunked_tests;
         chunked_props; handle_tests; buf_writer_tests;
         response_framing_tests; response_head_tests;
         response_body_tests ])
