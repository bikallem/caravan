open Windtrap
open Caravan
module Buf_reader = Private.Buf_reader

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

let ignore3 _ _ _ = ()

(* Buf_reader *)

let lines ?size ?chunk ~max s =
  let r = Buf_reader.of_string ?size ?chunk s in
  let rec loop acc =
    match Buf_reader.read_line r ~max with
    | line ->
        loop (line :: acc)
    | exception Buf_reader.Eof ->
        List.rev acc
  in
  loop []

(* Reads up to [n] bytes, fewer only at the end of the input. *)
let read_n r n =
  let buf = Bytes.create n and got = ref 0 in
  let rec loop () =
    if !got < n then
      match Buf_reader.read_into r buf !got (n - !got) with
      | 0 ->
          ()
      | k ->
          got := !got + k ;
          loop ()
  in
  loop () ;
  Bytes.sub_string buf 0 !got

let buf_reader_tests =
  group "Buf_reader"
    [ test "the buffer is allocated by the first read that needs it" (fun () ->
          let r = Buf_reader.of_string ~size:8 "0123456789abcdefghij\r\nrest" in
          equal int 0 (Buf_reader.capacity r) ;
          ignore (Buf_reader.read_into r (Bytes.create 8) 0 8) ;
          equal int ~msg:"a large read_into reads past an empty buffer" 0
            (Buf_reader.capacity r) ;
          equal string "89abcdefghij" (Buf_reader.read_line r ~max:100) ;
          equal int ~msg:"grown for a long line" 16 (Buf_reader.capacity r) )
    ; test "a given buffer is used from the start, until the reader grows"
        (fun () ->
          let buffer = Bytes.make 16 '.' in
          let input = "abc\r\n0123456789abcdefghijklmnop\r\n" in
          let pos = ref 0 in
          let r =
            Buf_reader.make ~buffer (fun buf off len ->
                let n = Int.min len (String.length input - !pos) in
                Bytes.blit_string input !pos buf off n ;
                pos := !pos + n ;
                n )
          in
          equal int 16 (Buf_reader.capacity r) ;
          equal string "abc" (Buf_reader.read_line r ~max:100) ;
          equal string ~msg:"the first read went into the given bytes"
            "abc\r\n0123456789a" (Bytes.to_string buffer) ;
          equal string "0123456789abcdefghijklmnop"
            (Buf_reader.read_line r ~max:100) ;
          equal int ~msg:"grown for a long line" 32 (Buf_reader.capacity r) ;
          let read _ _ _ = 0 in
          raises_match is_invalid_arg (fun () ->
              Buf_reader.make ~size:8 ~buffer read ) ;
          raises (Invalid_argument "Buf_reader.make: buffer must not be empty")
            (fun () -> Buf_reader.make ~buffer:Bytes.empty read ) )
    ; test "lines without their CRLF" (fun () ->
          equal (list string)
            ["GET / HTTP/1.1"; "Host: x"; ""]
            (lines ~max:100 "GET / HTTP/1.1\r\nHost: x\r\n\r\n") )
    ; test "bare LF and CR are rejected" (fun () ->
          List.iter
            (fun s ->
              raises Buf_reader.Bad_line_ending (fun () ->
                  Buf_reader.read_line (Buf_reader.of_string s) ~max:100 ) )
            ["a\nb"; "\n"; "a\rb\r\n"; "a\r\r\n"] )
    ; test "input ending before CRLF" (fun () ->
          List.iter
            (fun s ->
              raises Buf_reader.Eof (fun () ->
                  Buf_reader.read_line (Buf_reader.of_string s) ~max:100 ) )
            [""; "abc"; "abc\r"] )
    ; test "max is inclusive" (fun () ->
          equal string "abcd"
            (Buf_reader.read_line (Buf_reader.of_string "abcd\r\n") ~max:4) ;
          raises Buf_reader.Line_too_long (fun () ->
              Buf_reader.read_line (Buf_reader.of_string "abcde\r\n") ~max:4 ) ;
          raises Buf_reader.Line_too_long (fun () ->
              Buf_reader.read_line (Buf_reader.of_string "abcde") ~max:4 ) ;
          equal string ""
            (Buf_reader.read_line (Buf_reader.of_string "\r\nx") ~max:0) ;
          raises Buf_reader.Line_too_long (fun () ->
              Buf_reader.read_line (Buf_reader.of_string "x\r\n") ~max:0 ) ;
          raises_match is_invalid_arg (fun () ->
              Buf_reader.read_line (Buf_reader.of_string "") ~max:(-1) ) )
    ; test "lines may outgrow the initial buffer" (fun () ->
          let long = String.make 100 'a' in
          equal (list string) [long; "b"]
            (lines ~size:4 ~chunk:3 ~max:100 (long ^ "\r\nb\r\n")) )
    ; test "a failed read_line consumes nothing" (fun () ->
          let r = Buf_reader.of_string ~chunk:2 "abcdef\r\n" in
          raises Buf_reader.Line_too_long (fun () ->
              Buf_reader.read_line r ~max:3 ) ;
          equal int 0 (Buf_reader.consumed r) ;
          raises Buf_reader.Line_too_long (fun () ->
              Buf_reader.read_line r ~max:3 ) ;
          equal string "abcdef" (Buf_reader.read_line r ~max:6) ;
          let r = Buf_reader.of_string "ab" in
          raises Buf_reader.Eof (fun () -> Buf_reader.read_line r ~max:9) ;
          equal string "ab" (read_n r 9) )
    ; test "read_into drains the buffer, then reads directly" (fun () ->
          let r = Buf_reader.of_string ~chunk:16 "line\r\nbody-bytes" in
          equal string "line" (Buf_reader.read_line r ~max:10) ;
          equal int 10 (Buf_reader.buffered r) ;
          let buf = Bytes.create 4 in
          equal int 4 (Buf_reader.read_into r buf 0 4) ;
          equal string "body" (Bytes.to_string buf) ;
          equal string "-bytes" (read_n r 100) ;
          equal int 0 (Buf_reader.read_into r buf 0 4) ;
          let r = Buf_reader.of_string ~size:2 "abcdef" in
          equal int 6 (Buf_reader.read_into r (Bytes.create 8) 0 8) ;
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () ->
                  Buf_reader.read_into r buf off len ) )
            [(0, 0); (-1, 1); (2, 3); (5, 1)] )
    ; test "small reads of read_into read the input once" (fun () ->
          (* Each read of the input records if it went into [buf]. *)
          let input = "abcdefghijklmnopqrst"
          and pos = ref 0
          and reads = ref [] in
          let buf = Bytes.create 8 in
          let r =
            Buf_reader.make ~size:8 (fun b off len ->
                let n = Int.min len (String.length input - !pos) in
                Bytes.blit_string input !pos b off n ;
                pos := !pos + n ;
                reads := (b == buf) :: !reads ;
                n )
          in
          equal int 2 (Buf_reader.read_into r buf 0 2) ;
          equal int 4 (Buf_reader.read_into r buf 2 4) ;
          equal string "abcdef" (Bytes.sub_string buf 0 6) ;
          equal (list bool) ~msg:"one read into the buffer of the reader"
            [false] !reads ;
          equal int 2 (Buf_reader.buffered r) ;
          equal int 2 (Buf_reader.read_into r buf 0 7) ;
          equal string "gh" (Bytes.sub_string buf 0 2) ;
          equal int 8 (Buf_reader.read_into r buf 0 8) ;
          equal string "ijklmnop" (Bytes.to_string buf) ;
          equal (list bool) ~msg:"the large read went directly" [true; false]
            !reads ;
          equal int 0 (Buf_reader.buffered r) )
    ; test "take_buffered, peek_eof and consumed" (fun () ->
          let r = Buf_reader.of_string "GET / HTTP/1.1\r\n\x16\x03\x01" in
          is_true (not (Buf_reader.peek_eof r)) ;
          ignore (Buf_reader.read_line r ~max:100) ;
          equal int 16 (Buf_reader.consumed r) ;
          equal string "\x16\x03\x01" (Buf_reader.take_buffered r) ;
          equal int 19 (Buf_reader.consumed r) ;
          equal int 0 (Buf_reader.buffered r) ;
          is_true (Buf_reader.peek_eof r) ;
          is_true (Buf_reader.peek_eof (Buf_reader.of_string "")) ;
          raises_match is_invalid_arg (fun () ->
              Buf_reader.make ~size:0 (fun _ _ _ -> 0) ) ;
          raises_match is_invalid_arg (fun () ->
              Buf_reader.of_string ~chunk:0 "" ) )
    ; test "fill_now reads what is there without blocking" (fun () ->
          (* Each read takes the next answer; one too many fails. *)
          let script = ref [None; Some "ab"; None; Some ""] in
          let read_now buf off _ =
            match !script with
            | [] ->
                failwith "read after the end"
            | x :: rest ->
                script := rest ;
                Option.map
                  (fun s ->
                    Bytes.blit_string s 0 buf off (String.length s) ;
                    String.length s )
                  x
          in
          let r =
            Buf_reader.make ~size:4 ~read_now (fun _ _ _ ->
                failwith "blocking read" )
          in
          let fill () =
            match Buf_reader.fill_now r with
            | `Bytes ->
                "bytes"
            | `Eof ->
                "eof"
            | `Would_block ->
                "would block"
          in
          let first = List.init 3 (fun _ -> fill ()) in
          let taken = Buf_reader.take_buffered r in
          equal (list string)
            ["would block"; "bytes"; "bytes"; "ab"; "would block"; "eof"; "eof"]
            (first @ [taken] @ List.init 3 (fun _ -> fill ())) ;
          is_true (Buf_reader.peek_eof r) )
    ; test "of_flow reads the flow" (fun () ->
          let r =
            Buf_reader.of_flow
              (Flow.of_string ~chunk:1 "a\r\nb" (Buffer.create 0))
          in
          equal string "a" (Buf_reader.read_line r ~max:5) ;
          equal string "b" (read_n r 5) )
    ; prop "lines read the same however the input is split"
        Gen.(
          pair
            (list ~size:(int_range 0 6)
               (string_of ~size:(int_range 0 12) (char_range 'a' 'z')) )
            (triple
               (string_of (char_range ' ' '~'))
               (int_range 1 7) (int_range 1 9) ) )
        (fun (ls, (payload, chunk, size)) ->
          let input =
            String.concat "" (List.map (fun l -> l ^ "\r\n") ls) ^ payload
          in
          let r = Buf_reader.of_string ~size ~chunk input in
          List.iter
            (fun l -> equal string l (Buf_reader.read_line r ~max:12))
            ls ;
          equal string payload (read_n r (String.length payload + 1)) ) ]

(* Stateful: Buf_reader against a reader that scans the string directly. *)
module Model = struct
  type t = {s: string; mutable pos: int}

  let make (s, _chunk, _size) = {s; pos= 0}

  let read_line m max =
    let rest = String.length m.s - m.pos in
    let rec find j =
      if j >= Int.min rest (max + 1) then None
      else match m.s.[m.pos + j] with '\r' | '\n' -> Some j | _ -> find (j + 1)
    in
    match find 0 with
    | Some j when m.s.[m.pos + j] = '\n' ->
        raise Buf_reader.Bad_line_ending
    | Some j when j + 1 >= rest ->
        raise Buf_reader.Eof
    | Some j when m.s.[m.pos + j + 1] <> '\n' ->
        raise Buf_reader.Bad_line_ending
    | Some j ->
        let line = String.sub m.s m.pos j in
        m.pos <- m.pos + j + 2 ;
        line
    | None ->
        if rest > max then raise Buf_reader.Line_too_long
        else raise Buf_reader.Eof

  let read_n m n =
    let k = Int.min n (String.length m.s - m.pos) in
    let s = String.sub m.s m.pos k in
    m.pos <- m.pos + k ;
    s

  let consumed m = m.pos

  let peek_eof m = m.pos = String.length m.s

  let fill_now m = if peek_eof m then "eof" else "bytes"
end

let reader =
  abstract "r" ~pp:(fun ppf m ->
      Format.fprintf ppf "%S@%d" m.Model.s m.Model.pos )

let input_gen =
  Gen.(
    triple
      (string_of ~size:(int_range 0 24)
         (of_list
            ~pp:(fun ppf -> Format.fprintf ppf "%C")
            ['a'; 'b'; '\r'; '\n'] ) )
      (int_range 1 6) (int_range 1 5) )

let buf_reader_commands =
  [ command "of_string"
      (input_gen @-> makes reader)
      Model.make
      (fun (s, chunk, size) -> Buf_reader.of_string ~size ~chunk s)
  ; command "read_line"
      (reader ^-> Gen.int_range 0 6 @-> returns string)
      Model.read_line
      (fun r max -> Buf_reader.read_line r ~max)
  ; command "read_n"
      (reader ^-> Gen.int_range 1 8 @-> returns string)
      Model.read_n read_n
  ; command "consumed"
      (reader ^-> returns int)
      Model.consumed Buf_reader.consumed
  ; command "peek_eof"
      (reader ^-> returns bool)
      Model.peek_eof Buf_reader.peek_eof
  ; command "fill_now"
      (reader ^-> returns string)
      Model.fill_now
      (fun r ->
        match Buf_reader.fill_now r with
        | `Bytes ->
            "bytes"
        | `Eof ->
            "eof"
        | `Would_block ->
            "would block" ) ]

let buf_reader_model =
  group "Buf_reader model"
    [stateful ~count:300 "behaves like scanning the string" buf_reader_commands]

(* H1_parse.request_head *)

module H1_error = Private.H1_error
module H1_parse = Private.H1_parse

let pp_head ppf (h : H1_parse.head) =
  Format.fprintf ppf "%a %s %a" Method.pp h.meth h.target Version.pp h.version ;
  Headers.iter (fun n v -> Format.fprintf ppf "@\n  %s: %S" n v) h.headers

let default_limits = Private.H1_server.limits Config.default

let parse ?(limits = default_limits) ?size ?chunk s =
  H1_parse.request_head limits (Buf_reader.of_string ?size ?chunk s)

let show ?limits s =
  match parse ?limits s with
  | h ->
      Format.asprintf "%a" pp_head h
  | exception H1_error.E e ->
      Format.asprintf "%a" H1_error.pp e
  | exception Buf_reader.Eof ->
      "end of input"

let table ?limits cases =
  String.concat "\n"
    (List.map (fun s -> Printf.sprintf "%S\n=> %s" s (show ?limits s)) cases)

let small =
  {H1_parse.max_request_line= 16; max_header_bytes= 24; max_header_count= 2}

let request_head_tests =
  group "H1_parse.request_head"
    [ test "valid heads" (fun () ->
          expect
            (table
               [ "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"
               ; "\r\n\r\nPOST /a?b=1 HTTP/1.0\r\n\r\n"
               ; "get /!~ HTTP/1.9\r\n\r\n"
               ; "CONNECT example.com:443 HTTP/1.1\r\n\
                  host:example.com:443\r\n\
                  \r\n"
               ; "GET http://a.example/x HTTP/1.1\r\n\
                  X-Empty:\r\n\
                  X-Ows: \t v a l \t\r\n\
                  Set-Cookie: a\r\n\
                  set-cookie: b\r\n\
                  \r\n" ] )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "invalid request lines" (fun () ->
          expect
            (table
               [ "GET  / HTTP/1.1\r\n\r\n"
               ; "GET / HTTP/1.1 \r\n\r\n"
               ; "GET /\r\n\r\n"
               ; " GET / HTTP/1.1\r\n\r\n"
               ; "G(T / HTTP/1.1\r\n\r\n"
               ; "GET /a\tb HTTP/1.1\r\n\r\n"
               ; "GET /\x80 HTTP/1.1\r\n\r\n"
               ; "GET / HTTP/1\r\n\r\n"
               ; "GET / http/1.1\r\n\r\n"
               ; "GET / HTTP/1.x\r\n\r\n"
               ; "GET / HTTP/1-1\r\n\r\n"
               ; "GET  HTTP/1.1\r\n\r\n"
               ; "GET / HTTP/0.9\r\n\r\n"
               ; "GET / HTTP/2.0\r\n\r\n"
               ; "GET / HTTP/1.1\n\n"
               ; "\r\n\r\n\r\n\r\n\r\nGET / HTTP/1.1\r\n\r\n" ] )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "invalid field lines" (fun () ->
          expect
            (table
               [ "GET / HTTP/1.1\r\nHost: a\r\n folded\r\n\r\n"
               ; "GET / HTTP/1.1\r\nHost : a\r\n\r\n"
               ; "GET / HTTP/1.1\r\n: a\r\n\r\n"
               ; "GET / HTTP/1.1\r\nHost\r\n\r\n"
               ; "GET / HTTP/1.1\r\nX: a\000b\r\n\r\n"
               ; "GET / HTTP/1.1\r\nX: a\rb\r\n\r\n"
               ; "GET / HTTP/1.1\r\nX\x80: a\r\n\r\n"
               ; "GET / HTTP/1.1\r\nX: \t \r\n\r\n" ] )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "limits" (fun () ->
          expect
            (table ~limits:small
               [ "GET /01 HTTP/1.1\r\n\r\n"
               ; "GET /012 HTTP/1.1\r\n\r\n"
               ; "GET / HTTP/1.1\r\nA: 1\r\nB: 2\r\n\r\n"
               ; "GET / HTTP/1.1\r\nA: 1\r\nB: 2\r\nC: 3\r\n\r\n"
               ; "GET / HTTP/1.1\r\nA: 0123456789\r\nB: 1234\r\n\r\n"
               ; "GET / HTTP/1.1\r\nA: 0123456789\r\nB: 12345\r\n\r\n"
               ; "GET / HTTP/1.1\r\nA: 0123456789012345678901\r\n\r\n" ] )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "input ending early" (fun () ->
          expect
            (table
               [ ""
               ; "GET / HTTP/1.1"
               ; "GET / HTTP/1.1\r\nHost: a\r\n"
               ; "GET / HTTP/1.1\r\nHost: a\r\n\r" ] )
          @@ __POS_OF__
               {|
            ""
            => end of input
            "GET / HTTP/1.1"
            => end of input
            "GET / HTTP/1.1\r\nHost: a\r\n"
            => end of input
            "GET / HTTP/1.1\r\nHost: a\r\n\r"
            => end of input
            |} )
    ]

(* Generated valid heads, serialized with random optional whitespace. *)
let token_gen =
  Gen.string_of ~size:(Gen.int_range 1 8)
    (Gen.of_list (List.of_seq (String.to_seq "abcxyzABC019-_.!~")))

let head_gen =
  let open Gen in
  let meth =
    one_of
      [ of_list Method.[GET; POST; HEAD; DELETE; OPTIONS]
      ; map Method.of_string token_gen ]
  in
  let target =
    map
      (fun s -> "/" ^ s)
      (string_of ~size:(int_range 0 12)
         (of_list (List.of_seq (String.to_seq "abz09/?=&%-._~"))) )
  in
  let value =
    map
      (fun s -> String.trim s)
      (string_of ~size:(int_range 0 10)
         (of_list (List.of_seq (String.to_seq "ab z9,;=\t\""))) )
  in
  let ows = of_list [""; " "; "\t"; "  "] in
  triple
    (triple meth target (of_list Version.[Http_1_0; Http_1_1]))
    (list ~size:(int_range 0 6) (pair token_gen (pair value (pair ows ows))))
    (pair (int_range 1 9) (int_range 1 32))

let serialize ((meth, target, v), fields, _) =
  Printf.sprintf "%s %s %s\r\n%s\r\n" (Method.to_string meth) target
    (Version.to_string v)
    (String.concat ""
       (List.map
          (fun (n, (v, (o, o'))) -> n ^ ":" ^ o ^ v ^ o' ^ "\r\n")
          fields ) )

let head_props =
  group "H1_parse.request_head properties"
    [ prop "parses what it is given, however it is split" head_gen
        (fun (((meth, target, v), fields, (chunk, size)) as g) ->
          let h = parse ~chunk ~size (serialize g) in
          equal string (Method.to_string meth) (Method.to_string h.meth) ;
          equal string target h.target ;
          is_true (h.version = v) ;
          equal
            (list (pair string string))
            (List.map (fun (n, (v, _)) -> (n, v)) fields)
            (Headers.to_list h.headers) ) ]

(* H1_parse.response_head, for a client *)

let parse_response ?(limits = default_limits) ?size ?chunk s =
  H1_parse.response_head limits (Buf_reader.of_string ?size ?chunk s)

let show_response ?limits s =
  match parse_response ?limits s with
  | h ->
      Format.asprintf "%a %d %S%t" Version.pp h.version (Status.code h.status)
        h.reason (fun ppf ->
          Headers.iter
            (fun n v -> Format.fprintf ppf "@\n  %s: %S" n v)
            h.headers )
  | exception H1_error.E e ->
      Format.asprintf "%a" H1_error.pp e
  | exception Buf_reader.Eof ->
      "end of input"

let parse_response_tests =
  group "H1_parse.response_head"
    [ test "status lines" (fun () ->
          expect
            (String.concat "\n"
               (List.map
                  (fun s -> Printf.sprintf "%S\n=> %s" s (show_response s))
                  [ "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n"
                  ; "HTTP/1.0 404 Not Found\r\n\r\n"
                  ; "HTTP/1.1 204\r\n\r\n"
                  ; "HTTP/1.1 204 \r\n\r\n"
                  ; "HTTP/1.2 200 OK\r\n\r\n"
                  ; "HTTP/1.1 599 \tx\x80\r\n\r\n"
                  ; "HTTP/1.1 100 Continue\r\n\r\n"
                  ; "HTTP/2 200 OK\r\n\r\n"
                  ; "HTTP/2.0 200 OK\r\n\r\n"
                  ; "HTTP/1.1 99 X\r\n\r\n"
                  ; "HTTP/1.1 600 X\r\n\r\n"
                  ; "HTTP/1.1 2000 X\r\n\r\n"
                  ; "HTTP/1.1 20a X\r\n\r\n"
                  ; "HTTP/1.1  200 X\r\n\r\n"
                  ; "http/1.1 200 X\r\n\r\n"
                  ; "HTTP/1.1 200 a\x01b\r\n\r\n"
                  ; "HTTP/1.1 200 OK\n\n"
                  ; "\r\nHTTP/1.1 200 OK\r\n\r\n"
                  ; "HTTP/1.1 200 OK\r\n folded\r\n\r\n"
                  ; "HTTP/1.1 200 OK\r\n" ] ) )
          @@ __POS_OF__
               {|
            "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n"
            => HTTP/1.1 200 "OK"
              Content-Length: "5"
            "HTTP/1.0 404 Not Found\r\n\r\n"
            => HTTP/1.0 404 "Not Found"
            "HTTP/1.1 204\r\n\r\n"
            => HTTP/1.1 204 ""
            "HTTP/1.1 204 \r\n\r\n"
            => HTTP/1.1 204 ""
            "HTTP/1.2 200 OK\r\n\r\n"
            => HTTP/1.1 200 "OK"
            "HTTP/1.1 599 \tx\128\r\n\r\n"
            => HTTP/1.1 599 "\tx\128"
            "HTTP/1.1 100 Continue\r\n\r\n"
            => HTTP/1.1 100 "Continue"
            "HTTP/2 200 OK\r\n\r\n"
            => 400 Bad Request (malformed status line)
            "HTTP/2.0 200 OK\r\n\r\n"
            => 400 Bad Request (unsupported HTTP version "HTTP/2.0")
            "HTTP/1.1 99 X\r\n\r\n"
            => 400 Bad Request (invalid status code "99 ")
            "HTTP/1.1 600 X\r\n\r\n"
            => 400 Bad Request (status code 600 out of range)
            "HTTP/1.1 2000 X\r\n\r\n"
            => 400 Bad Request (malformed status line)
            "HTTP/1.1 20a X\r\n\r\n"
            => 400 Bad Request (invalid status code "20a")
            "HTTP/1.1  200 X\r\n\r\n"
            => 400 Bad Request (invalid status code " 20")
            "http/1.1 200 X\r\n\r\n"
            => 400 Bad Request (invalid HTTP version "http/1.1")
            "HTTP/1.1 200 a\001b\r\n\r\n"
            => 400 Bad Request (invalid reason phrase)
            "HTTP/1.1 200 OK\n\n"
            => 400 Bad Request (bare CR or LF)
            "\r\nHTTP/1.1 200 OK\r\n\r\n"
            => 400 Bad Request (malformed status line)
            "HTTP/1.1 200 OK\r\n folded\r\n\r\n"
            => 400 Bad Request (obsolete line folding)
            "HTTP/1.1 200 OK\r\n"
            => end of input
            |} )
    ; test "the limits apply to response heads" (fun () ->
          let line =
            "HTTP/1.1 200 "
            ^ String.make (default_limits.max_request_line - 12) 'x'
          in
          is_true (String.length line = default_limits.max_request_line + 1) ;
          equal string "400 Bad Request (status line too long)"
            (show_response (line ^ "\r\n\r\n")) ;
          let ok =
            "HTTP/1.1 200 "
            ^ String.make (default_limits.max_request_line - 13) 'x'
          in
          is_true
            ( parse_response (ok ^ "\r\n\r\n")
            |> fun h -> Status.code h.status = 200 ) ;
          let fields n =
            String.concat ""
              (List.init n (fun i -> Printf.sprintf "X-%d: v\r\n" i))
          in
          let limits = {default_limits with max_header_count= 100} in
          equal string "431 Request Header Fields Too Large"
            (show_response ~limits
               ("HTTP/1.1 200 OK\r\n" ^ fields 101 ^ "\r\n") ) ;
          is_true
            ( parse_response ~limits
                ("HTTP/1.1 200 OK\r\n" ^ fields 100 ^ "\r\n")
            |> fun h -> Headers.length h.headers = 100 ) )
    ; test "the bytes after the head stay in the reader" (fun () ->
          let r =
            Buf_reader.of_string
              "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nbody"
          in
          ignore (H1_parse.response_head default_limits r) ;
          equal string "body" (Buf_reader.take_buffered r) ) ]

let response_gen =
  let open Gen in
  let reason =
    string_of ~size:(int_range 0 10)
      (of_list (List.of_seq (String.to_seq "Ok z9\t-")))
  in
  let value =
    map
      (fun s -> String.trim s)
      (string_of ~size:(int_range 0 10)
         (of_list (List.of_seq (String.to_seq "ab z9,;=\t\""))) )
  in
  triple
    (triple (int_range 100 599) reason (of_list Version.[Http_1_0; Http_1_1]))
    (list ~size:(int_range 0 5) (pair token_gen value))
    (pair (int_range 1 9) (int_range 1 32))

let parse_response_props =
  group "H1_parse.response_head properties"
    [ prop "a response head parses the same however the input is split"
        response_gen (fun ((code, reason, v), fields, (chunk, size)) ->
          let s =
            Printf.sprintf "%s %d %s\r\n%s\r\n" (Version.to_string v) code
              reason
              (String.concat ""
                 (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") fields) )
          in
          let h = parse_response ~chunk ~size s in
          is_true (h.version = v) ;
          equal int code (Status.code h.status) ;
          equal string reason h.reason ;
          equal (list (pair string string)) fields (Headers.to_list h.headers) )
    ]

(* H1_parse.response_framing and H1_body.until_close, for a client *)

let show_response_framing (label, meth, s) =
  Printf.sprintf "%s => %s" label
    ( match H1_parse.response_framing ~meth (parse_response s) with
    | H1_parse.No_body ->
        "no body"
    | Fixed n ->
        Printf.sprintf "fixed %d" n
    | Chunked ->
        "chunked"
    | Until_close ->
        "until close"
    | Tunnel ->
        "tunnel"
    | exception H1_error.E e ->
        Format.asprintf "%a" H1_error.pp e )

let resp ?(v = "HTTP/1.1") code fields =
  Printf.sprintf "%s %d X\r\n%s\r\n" v code
    (String.concat "" (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") fields))

let response_framing_parse_tests =
  let te v = ("Transfer-Encoding", v) and cl v = ("Content-Length", v) in
  group "H1_parse.response_framing"
    [ test "framing table" (fun () ->
          expect
            (String.concat "\n"
               (List.map show_response_framing
                  Method.
                    [ ("HEAD, Content-Length: 5", HEAD, resp 200 [cl "5"])
                    ; ("HEAD, TE and CL", HEAD, resp 200 [te "chunked"; cl "5"])
                    ; ("100", GET, resp 100 [])
                    ; ("204 with CL", GET, resp 204 [cl "5"])
                    ; ("304 with TE", GET, resp 304 [te "chunked"])
                    ; ("101", GET, resp 101 [cl "5"])
                    ; ("200 to CONNECT", CONNECT, resp 200 [cl "5"])
                    ; ("300 to CONNECT", CONNECT, resp 300 [cl "0"])
                    ; ("404 to CONNECT", CONNECT, resp 404 [cl "3"])
                    ; ("TE and CL", GET, resp 200 [te "chunked"; cl "5"])
                    ; ("TE in 1.0", GET, resp ~v:"HTTP/1.0" 200 [te "chunked"])
                    ; ("chunked", GET, resp 200 [te "chunked"])
                    ; ("Chunked", GET, resp 200 [te "Chunked"])
                    ; ("gzip, chunked", GET, resp 200 [te "gzip, chunked"])
                    ; ("gzip", GET, resp 200 [te "gzip"])
                    ; ("CL 5, 5", GET, resp 200 [cl "5, 5"])
                    ; ("CL 5 and 6", GET, resp 200 [cl "5"; cl "6"])
                    ; ("CL -1", GET, resp 200 [cl "-1"])
                    ; ("no field", GET, resp 200 [])
                    ; ("205 with CL 0", GET, resp 205 [cl "0"]) ] ) )
          @@ __POS_OF__
               {|
            HEAD, Content-Length: 5 => no body
            HEAD, TE and CL => no body
            100 => no body
            204 with CL => no body
            304 with TE => no body
            101 => tunnel
            200 to CONNECT => tunnel
            300 to CONNECT => fixed 0
            404 to CONNECT => fixed 3
            TE and CL => 400 Bad Request (both Transfer-Encoding and Content-Length)
            TE in 1.0 => 400 Bad Request (Transfer-Encoding in HTTP/1.0)
            chunked => chunked
            Chunked => chunked
            gzip, chunked => 501 Not Implemented (transfer codings other than chunked)
            gzip => 501 Not Implemented (transfer codings other than chunked)
            CL 5, 5 => fixed 5
            CL 5 and 6 => 400 Bad Request (conflicting Content-Length)
            CL -1 => 400 Bad Request (invalid Content-Length "-1")
            no field => until close
            205 with CL 0 => fixed 0
            |} )
    ]

(* A reader whose input gives [s], then raises [Flow.Closed]. *)
let reset_after s =
  let pos = ref 0 in
  Buf_reader.make (fun buf off len ->
      if !pos = String.length s then raise Flow.Closed
      else begin
        let n = Int.min len (String.length s - !pos) in
        Bytes.blit_string s !pos buf off n ;
        pos := !pos + n ;
        n
      end )

let until_close_tests =
  group "H1_body.until_close"
    [ test "until_close reads to the end of the input" (fun () ->
          let b =
            Private.H1_body.until_close
              (Buf_reader.of_string ~chunk:3 "abcdefg")
          in
          equal (option int) None (Body.length b) ;
          equal string "abcdefg" (Body.to_string b) ;
          equal int 0 (Body.read b (Bytes.create 4) 0 4) )
    ; test "a reset is not a correct end" (fun () ->
          let b = Private.H1_body.until_close (reset_after "abc") in
          let buf = Bytes.create 8 in
          equal int 3 (Body.read b buf 0 8) ;
          raises Body.Truncated (fun () -> Body.read b buf 0 8) ) ]

(* H1_parse.request_framing and request_target *)

(* [req ~v fields target] is a head with method [meth]. *)
let req ?(meth = "GET") ?(v = "HTTP/1.1") ?(target = "/") fields =
  parse
    (Printf.sprintf "%s %s %s\r\n%s\r\n" meth target v
       (String.concat ""
          (List.map (fun (n, v) -> n ^ ": " ^ v ^ "\r\n") fields) ) )

let errors f =
  match f () with
  | s ->
      s
  | exception H1_error.E e ->
      Format.asprintf "%a" H1_error.pp e

let show_framing (label, h) =
  Printf.sprintf "%s => %s" label
    (errors (fun () ->
         match H1_parse.request_framing (Lazy.force h) with
         | H1_parse.No_body ->
             "no body"
         | Fixed n ->
             Printf.sprintf "fixed %d" n
         | Chunked ->
             "chunked" ) )

let show_target (label, h) =
  Printf.sprintf "%s => %s" label
    (errors (fun () ->
         let t = H1_parse.request_target ~scheme:"http" (Lazy.force h) in
         Printf.sprintf "%s %s %S" t.scheme
           (Option.value ~default:"-" t.authority)
           t.path ) )

let cl v = ("Content-Length", v)

let te v = ("Transfer-Encoding", v)

let framing_tests =
  group "H1_parse.request_framing"
    [ test "framing table" (fun () ->
          expect
            (String.concat "\n"
               (List.map show_framing
                  [ ("none", lazy (req []))
                  ; ("CL 0", lazy (req [cl "0"]))
                  ; ("CL 42", lazy (req [cl "42"]))
                  ; ("CL 007", lazy (req [cl "007"]))
                  ; ("CL 5, 5 in one field", lazy (req [cl "5, 5"]))
                  ; ("CL 5 in two fields", lazy (req [cl "5"; cl "5"]))
                  ; ("CL 5 and 05", lazy (req [cl "5"; cl "05"]))
                  ; ("CL 5 and 6", lazy (req [cl "5"; cl "6"]))
                  ; ("CL 5, 6", lazy (req [cl "5, 6"]))
                  ; ("CL empty", lazy (req [cl ""]))
                  ; ("CL ,", lazy (req [cl ","]))
                  ; ("CL -1", lazy (req [cl "-1"]))
                  ; ("CL +1", lazy (req [cl "+1"]))
                  ; ("CL 1.0", lazy (req [cl "1.0"]))
                  ; ("CL 0x10", lazy (req [cl "0x10"]))
                  ; ("CL 18 digits", lazy (req [cl "999999999999999999"]))
                  ; ("CL 19 digits", lazy (req [cl "9999999999999999999"]))
                  ; ("TE chunked", lazy (req [te "chunked"]))
                  ; ("TE Chunked", lazy (req [te "Chunked"]))
                  ; ("TE in two fields", lazy (req [te ""; te "chunked"]))
                  ; ("TE gzip, chunked", lazy (req [te "gzip, chunked"]))
                  ; ("TE gzip", lazy (req [te "gzip"]))
                  ; ("TE chunked, gzip", lazy (req [te "chunked, gzip"]))
                  ; ("TE chunked, chunked", lazy (req [te "chunked, chunked"]))
                  ; ("TE chunked;x=1", lazy (req [te "chunked;x=1"]))
                  ; ("TE empty", lazy (req [te ""]))
                  ; ("TE and CL", lazy (req [te "chunked"; cl "5"]))
                  ; ("TE in HTTP/1.0", lazy (req ~v:"HTTP/1.0" [te "chunked"]))
                  ] ) )
          @@ __POS_OF__
               {|
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
            |} )
    ]

let host v = ("Host", v)

let target_tests =
  group "H1_parse.request_target"
    [ test "an absolute target's scheme must be the connection's" (fun () ->
          let target scheme t =
            errors (fun () ->
                (H1_parse.request_target ~scheme
                   (req ~target:t [host "b.example"]) )
                  .scheme )
          in
          equal (list string)
            [ "https"
            ; "http"
            ; "421 Misdirected Request"
            ; "421 Misdirected Request" ]
            [ target "https" "https://b.example/"
            ; target "http" "HTTP://b.example/"
            ; target "http" "https://b.example/"
            ; target "https" "http://b.example/" ] )
    ; test "target and Host table" (fun () ->
          expect
            (String.concat "\n"
               (List.map show_target
                  [ ("origin", lazy (req ~target:"/a?b" [host "a.example"]))
                  ; ("origin, port", lazy (req [host "a.example:8080"]))
                  ; ("origin, IPv6", lazy (req [host "[::1]:8080"]))
                  ; ("origin, IPv4", lazy (req [host "127.0.0.1"]))
                  ; ("origin, empty Host", lazy (req [host ""]))
                  ; ("1.1 without Host", lazy (req []))
                  ; ("1.0 without Host", lazy (req ~v:"HTTP/1.0" []))
                  ; ("two Host", lazy (req [host "a"; host "a"]))
                  ; ( "two Host in 1.0"
                    , lazy (req ~v:"HTTP/1.0" [host "a"; host "b"]) )
                  ; ("Host with path", lazy (req [host "a/b"]))
                  ; ("Host with userinfo", lazy (req [host "u@a"]))
                  ; ("Host with bad port", lazy (req [host "a:8x"]))
                  ; ("Host empty IPv6", lazy (req [host "[]"]))
                  ; ("Host port only", lazy (req [host ":80"]))
                  ; ("Host bad IPv6 literal", lazy (req [host "[::g]"]))
                  ; ( "absolute"
                    , lazy
                        (req ~target:"http://b.example:81/p?q"
                           [host "a.example"] ) )
                  ; ( "absolute https"
                    , lazy (req ~target:"HTTPS://b.example" [host "b.example"])
                    )
                  ; ( "absolute query only"
                    , lazy (req ~target:"http://b.example?q" [host "x"]) )
                  ; ( "absolute userinfo"
                    , lazy
                        (req ~target:"http://u:p@b.example/" [host "b.example"])
                    )
                  ; ( "absolute ftp"
                    , lazy (req ~target:"ftp://b.example/" [host "b.example"])
                    )
                  ; ( "absolute no //"
                    , lazy (req ~target:"http:b.example/" [host "b.example"]) )
                  ; ("absolute http:", lazy (req ~target:"http:" [host "b"]))
                  ; ("absolute http:/", lazy (req ~target:"http:/" [host "b"]))
                  ; ( "absolute one slash"
                    , lazy (req ~target:"http:/b.example/" [host "b"]) )
                  ; ( "absolute second slash only"
                    , lazy (req ~target:"http:x/b.example/" [host "b"]) )
                  ; ( "absolute no host"
                    , lazy (req ~target:"http:///p" [host "b.example"]) )
                  ; ( "absolute, still two Host"
                    , lazy (req ~target:"http://b/" [host "a"; host "b"]) )
                  ; ( "CONNECT"
                    , lazy
                        (req ~meth:"CONNECT" ~target:"b.example:443"
                           [host "b.example:443"] ) )
                  ; ( "CONNECT no port"
                    , lazy
                        (req ~meth:"CONNECT" ~target:"b.example"
                           [host "b.example"] ) )
                  ; ( "CONNECT empty port"
                    , lazy (req ~meth:"CONNECT" ~target:"b.example:" [host "b"])
                    )
                  ; ( "CONNECT origin-form"
                    , lazy (req ~meth:"CONNECT" ~target:"/" [host "b"]) )
                  ; ( "OPTIONS *"
                    , lazy (req ~meth:"OPTIONS" ~target:"*" [host "a"]) )
                  ; ("GET *", lazy (req ~target:"*" [host "a"]))
                  ; ( "OPTIONS origin"
                    , lazy (req ~meth:"OPTIONS" ~target:"/x" [host "a"]) )
                  ; ( "GET authority-form"
                    , lazy (req ~target:"b.example:443" [host "b"]) )
                  ; ( "GET relative path"
                    , lazy (req ~target:"index.html" [host "b"]) ) ] ) )
          @@ __POS_OF__
               {|
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
            absolute https => 421 Misdirected Request
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
            |} )
    ]

let host_gen =
  Gen.(
    map
      (fun (h, p) ->
        match p with None -> h | Some p -> h ^ ":" ^ string_of_int p )
      (pair
         (string_of ~size:(int_range 1 12)
            (of_list (List.of_seq (String.to_seq "abcz09-._~"))) )
         (option (int_range 0 65535)) ) )

let framing_props =
  group "H1_parse framing and target properties"
    [ prop "Content-Length round trips, however it is repeated"
        Gen.(pair (int_range 0 1_000_000_000_000) (pair (int_range 1 3) bool))
        (fun (n, (copies, one_field)) ->
          let v = string_of_int n in
          let fields =
            if one_field then
              [cl (String.concat ", " (List.init copies (fun _ -> v)))]
            else List.init copies (fun _ -> cl v)
          in
          is_true (H1_parse.request_framing (req fields) = H1_parse.Fixed n) )
    ; prop "origin-form takes its authority from Host"
        (Gen.pair host_gen head_gen) (fun (h, ((_, target, _), _, _)) ->
          let t =
            H1_parse.request_target ~scheme:"http" (req ~target [host h])
          in
          equal (option string) (Some h) t.authority ;
          equal string target t.path ) ]

(* H1_body *)

module H1_body = Private.H1_body

(* [in_sim f ()] runs [f] in a fiber of the simulator. [H1_body.make] and the
   reads of a body fill an Ivar, and a fill needs a fiber. *)
let in_sim f () = Sim_support.run (fun _ -> f ())

(* Reads [b] to its end, [size] bytes at a time. *)
let read_body ?(size = 4096) b =
  let buf = Bytes.create size and out = Buffer.create 64 in
  let rec loop () =
    match Body.read b buf 0 size with
    | 0 ->
        Buffer.contents out
    | n ->
        Buffer.add_subbytes out buf 0 n ;
        loop ()
  in
  loop ()

let h1_body_tests =
  group "H1_body.fixed"
    [ test "reads exactly n bytes and leaves the next request" (fun () ->
          let r = Buf_reader.of_string "hello worldGET / HTTP/1.1\r\n" in
          let b = H1_body.fixed r 11 in
          equal (option int) (Some 11) (Body.length b) ;
          let buf = Bytes.create 64 in
          equal int 11 (Body.read b buf 0 64) ;
          equal string "hello world" (Bytes.sub_string buf 0 11) ;
          equal int 0 (Body.read b buf 0 64) ;
          equal string "GET / HTTP/1.1" (Buf_reader.read_line r ~max:100) )
    ; test "reads are capped by what remains" (fun () ->
          let r = Buf_reader.of_string "abcdefXYZ" in
          let b = H1_body.fixed r 6 in
          let buf = Bytes.create 4 in
          equal (list int) [4; 2; 0] (List.init 3 (fun _ -> Body.read b buf 0 4)) ;
          equal string "XYZ" (read_n r 10) )
    ; test "an empty body reads nothing" (fun () ->
          let reads = ref 0 in
          let r = Buf_reader.make (fun _ _ _ -> incr reads ; 0) in
          equal string "" (Body.to_string (H1_body.fixed r 0)) ;
          equal int 0 !reads )
    ; test "early end of input is Truncated" (fun () ->
          let b = H1_body.fixed (Buf_reader.of_string "abc") 5 in
          let buf = Bytes.create 8 in
          equal int 3 (Body.read b buf 0 8) ;
          raises Body.Truncated (fun () -> Body.read b buf 0 8) ;
          raises Body.Truncated (fun () ->
              Body.to_string (H1_body.fixed (Buf_reader.of_string "") 1) ) )
    ; prop "the body and what follows it, however the input is split"
        Gen.(
          pair (pair string string)
            (triple (int_range 1 9) (int_range 1 9) (int_range 1 9)) )
        (fun ((body, next), (chunk, size, read_size)) ->
          let r = Buf_reader.of_string ~chunk ~size (body ^ next) in
          let b = H1_body.fixed r (String.length body) in
          equal string body (read_body ~size:read_size b) ;
          equal string next (read_n r (String.length next + 1)) ) ]

(* Chunked bodies *)

let limits = default_limits

(* The decoded body, its trailers and what follows it, or the error. *)
let show_chunked ?max_body input =
  let r = Buf_reader.of_string input in
  let b = H1_body.chunked ?max_body ~limits r in
  match Body.to_string b with
  | body ->
      let trailers =
        String.concat ""
          (List.map
             (fun (n, v) -> Printf.sprintf " [%s: %s]" n v)
             (Headers.to_list (Body.trailers b)) )
      in
      Printf.sprintf "%S%s rest %S" body trailers (read_n r 100)
  | exception H1_error.E e ->
      Format.asprintf "%a" H1_error.pp e
  | exception Body.Truncated ->
      "truncated"

let chunked_table ?max_body cases =
  String.concat "\n"
    (List.map
       (fun s -> Printf.sprintf "%S\n=> %s" s (show_chunked ?max_body s))
       cases )

let chunked_tests =
  group "H1_body.chunked"
    [ test "valid chunked bodies" (fun () ->
          expect
            (chunked_table
               [ "5\r\nhello\r\n6\r\n world\r\n0\r\n\r\nNEXT"
               ; "0\r\n\r\n"
               ; "A\r\n0123456789\r\na\r\nabcdefghij\r\n000\r\n\r\n"
               ; "0005\r\nhello\r\n0\r\n\r\n"
               ; "5;name\r\nhello\r\n0;last=1\r\n\r\n"
               ; "5 ; a = b ;c\r\nhello\r\n0\r\n\r\n"
               ; "5\t;\ta\t=\tb\r\nhello\r\n0\r\n\r\n"
               ; "5;q=\"a \\\"b;\"\r\nhello\r\n0\r\n\r\n"
               ; "5\r\n\
                  hello\r\n\
                  0\r\n\
                  X-Checksum: abc\r\n\
                  X-Other:  v \r\n\
                  \r\n\
                  NEXT"
               ; "5;a=\"\\x\"\r\nhello\r\n0\r\n\r\n"
               ; "5;a=\"\\\\\"\r\nhello\r\n0\r\n\r\n"
               ; "000000000000005\r\nhello\r\n0\r\n\r\n" ] )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "malformed and truncated chunked bodies" (fun () ->
          expect
            (chunked_table
               [ "5\r\nhelloX\r\n0\r\n\r\n"
               ; "g\r\n"
               ; "\r\nhello\r\n"
               ; "5 x\r\nhello\r\n"
               ; "-5\r\nhello\r\n"
               ; "5 \r\nhello\r\n"
               ; "5;\r\nhello\r\n"
               ; "5;a=\r\nhello\r\n"
               ; "5;a=\"x\r\nhello\r\n"
               ; "5;a=\"\\\001\"\r\nhello\r\n"
               ; "5;a=\"\\\r\nhello\r\n"
               ; "1000000000000000\r\n"
               ; "5\nhello\r\n"
               ; "5\r\nhello\r\n0\r\nBad Trailer: x\r\n\r\n"
               ; "5\r\nhel"
               ; "5\r\nhello\r\n"
               ; "5\r\nhello\r\n0\r\nX: y\r\n"
               ; "5\r\nhello" ] )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "max_body counts the body as sent, framing included" (fun () ->
          (* 18 bytes: two 5-byte chunks fit with their framing (3 + 7 + 3 + 5),
             a 6-byte one does not. *)
          expect
            (chunked_table ~max_body:18
               [ "5\r\nhello\r\n5\r\nworld\r\n0\r\n\r\n"
               ; "5\r\nhello\r\n6\r\nworld!\r\n0\r\n\r\n"
               ; "10\r\n"
               ; (* Two bytes of data, but a long chunk extension. *)
                 "1;aaaaaaaaaaaa\r\nx\r\n1\r\ny\r\n0\r\n\r\n" ] )
          @@ __POS_OF__
               {|
            "5\r\nhello\r\n5\r\nworld\r\n0\r\n\r\n"
            => "helloworld" rest ""
            "5\r\nhello\r\n6\r\nworld!\r\n0\r\n\r\n"
            => 413 Content Too Large
            "10\r\n"
            => 413 Content Too Large
            "1;aaaaaaaaaaaa\r\nx\r\n1\r\ny\r\n0\r\n\r\n"
            => 413 Content Too Large
            |} )
    ; test "max_body counts from the body's start, not the connection's"
        (fun () ->
          (* A request head comes first on a real connection. *)
          let r =
            Buf_reader.of_string
              ("GET / HTTP/1.1\r\n" ^ "5\r\nhello\r\n5\r\nworld\r\n0\r\n\r\n")
          in
          ignore (Buf_reader.read_line r ~max:100) ;
          equal string "helloworld"
            (Body.to_string (H1_body.chunked ~max_body:18 ~limits r)) )
    ; test "make: framing and max_body"
      @@ in_sim (fun () ->
          let reads = ref 0 in
          let r = Buf_reader.make (fun _ _ _ -> incr reads ; 0) in
          equal (option int) (Some 0)
            (Body.length (H1_body.body (H1_body.make ~limits r No_body))) ;
          raises (H1_error.E H1_error.Content_too_large) (fun () ->
              H1_body.make ~max_body:10 ~limits r (Fixed 11) ) ;
          equal int 0 !reads ;
          let r = Buf_reader.of_string "helloXchunk" in
          equal string "hello"
            (Body.to_string
               (H1_body.body (H1_body.make ~max_body:5 ~limits r (Fixed 5))) ) ;
          equal string "X" (read_n r 1) ;
          equal string "ab"
            (Body.to_string
               (H1_body.body
                  (H1_body.make ~limits
                     (Buf_reader.of_string "2\r\nab\r\n0\r\n\r\n")
                     Chunked ) ) ) ) ]

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
            (pair
               (list ~size:(int_range 0 5) string)
               (of_list [""; ";a"; ";a=b"; " ; a = \"q\""]) )
            (pair
               (list ~size:(int_range 0 3)
                  (pair token_gen
                     (map String.trim (string_of (char_range 'a' 'z'))) ) )
               string )
            (triple (int_range 1 9) (int_range 1 9) (int_range 1 9)) )
        (fun ((pieces, ext), (trailers, next), (chunk, size, read_size)) ->
          let r =
            Buf_reader.of_string ~chunk ~size
              (encode_chunked pieces ext trailers ^ next)
          in
          let b = H1_body.chunked ~limits r in
          equal string (String.concat "" pieces) (read_body ~size:read_size b) ;
          equal
            (list (pair string string))
            trailers
            (Headers.to_list (Body.trailers b)) ;
          equal string next (read_n r (String.length next + 1)) ) ]

(* Request body handles: 100-continue, completion, draining *)

let handle ?max_body ?expect_continue ?chunk input framing =
  let r = Buf_reader.of_string ?chunk input in
  (r, H1_body.make ?max_body ?expect_continue ~limits r framing)

let next = "GET /next HTTP/1.1\r\n"

let handle_tests =
  group "H1_body handle"
    [ test "expect_continue runs once, before the first read"
      @@ in_sim (fun () ->
          let log = ref [] in
          let r, t =
            handle
              ~expect_continue:(fun () -> log := "100" :: !log)
              "hello" (Fixed 5)
          in
          equal (list string) [] !log ;
          let b = H1_body.body t in
          let buf = Bytes.create 2 in
          equal int 0 (Buf_reader.consumed r) ;
          ignore (Body.read b buf 0 2) ;
          log := "read" :: !log ;
          ignore (Body.read b buf 0 2) ;
          equal (list string) ["100"; "read"] (List.rev !log) )
    ; test "expect_continue never runs without a body"
      @@ in_sim (fun () ->
          let ran = ref false in
          let _, t =
            handle ~expect_continue:(fun () -> ran := true) "" No_body
          in
          equal string "" (Body.to_string (H1_body.body t)) ;
          is_true (H1_body.drain t ~max:0) ;
          is_true (not !ran) )
    ; test "completed is ready after the handler read the body"
      @@ in_sim (fun () ->
          (* Ready now, or not before 5 ms. *)
          let completed t =
            Weft.(
              sync
                (choose
                   [ wrap (H1_body.completed t) (fun () -> true)
                   ; wrap (Time.after Mono (Span.ms 5)) (fun () -> false) ] ) )
          in
          let _, t = handle "" No_body in
          is_true ~msg:"no body" (completed t) ;
          let _, t = handle "" (Fixed 0) in
          is_true ~msg:"an empty body" (completed t) ;
          let _, t = handle ("hello" ^ next) (Fixed 5) in
          let b = H1_body.body t in
          ignore (Body.read b (Bytes.create 4) 0 4) ;
          is_true ~msg:"one byte left" (not (completed t)) ;
          ignore (Body.read b (Bytes.create 4) 0 4) ;
          is_true ~msg:"at its last byte" (completed t) ;
          let _, t = handle "5\r\nhello\r\n0\r\n\r\n" Chunked in
          let b = H1_body.body t in
          ignore (Body.read b (Bytes.create 5) 0 5) ;
          is_true ~msg:"before the last chunk" (not (completed t)) ;
          ignore (Body.to_string b) ;
          is_true ~msg:"after the last chunk" (completed t) )
    ; test "is_complete"
      @@ in_sim (fun () ->
          let _, t = handle "" No_body in
          is_true (H1_body.is_complete t) ;
          let _, t = handle ("hello" ^ next) (Fixed 5) in
          is_true (not (H1_body.is_complete t)) ;
          ignore (Body.read (H1_body.body t) (Bytes.create 5) 0 5) ;
          is_true ~msg:"complete at its last byte" (H1_body.is_complete t) ;
          let _, t = handle "5\r\nhello\r\n0\r\n\r\n" Chunked in
          ignore (Body.read (H1_body.body t) (Bytes.create 5) 0 5) ;
          is_true ~msg:"chunked needs its last chunk"
            (not (H1_body.is_complete t)) ;
          ignore (Body.to_string (H1_body.body t)) ;
          is_true (H1_body.is_complete t) )
    ; test "drain within max keeps the next request"
      @@ in_sim (fun () ->
          let r, t = handle ~chunk:3 ("hello world" ^ next) (Fixed 11) in
          let read = Body.read (H1_body.body t) (Bytes.create 4) 0 4 in
          is_true (H1_body.drain t ~max:(11 - read)) ;
          equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100) ;
          (* 32 bytes as sent, framing and trailer included. *)
          let r, t =
            handle
              ("5\r\nhello\r\n6\r\n world\r\n0\r\nX: y\r\n\r\n" ^ next)
              Chunked
          in
          is_true (H1_body.drain t ~max:32) ;
          equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100) )
    ; test "drain counts chunk extensions"
      @@ in_sim (fun () ->
          (* Ten bytes of data in 1-byte chunks, each with a 100-byte extension:
             about 1.1 KiB as sent. *)
          let ext = ";" ^ String.make 99 'e' in
          let body =
            String.concat "" (List.init 10 (fun _ -> "1" ^ ext ^ "\r\nx\r\n"))
            ^ "0\r\n\r\n"
          in
          let _, t = handle (body ^ next) Chunked in
          is_true ~msg:"500 bytes are not enough"
            (not (H1_body.drain t ~max:500)) ;
          let r, t = handle (body ^ next) Chunked in
          is_true ~msg:"2000 are" (H1_body.drain t ~max:2000) ;
          equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100) )
    ; test "drain gives up past max"
      @@ in_sim (fun () ->
          let _, t = handle ~chunk:3 ("hello world" ^ next) (Fixed 11) in
          let read = Body.read (H1_body.body t) (Bytes.create 4) 0 4 in
          is_true (not (H1_body.drain t ~max:(11 - read - 1))) ;
          let _, t = handle "5\r\nhello\r\n6\r\n world\r\n0\r\n\r\n" Chunked in
          is_true (not (H1_body.drain t ~max:10)) ;
          (* [max] at the size of drain's scratch buffer, with a body whose size
             is not known up front *)
          let _, t =
            handle (encode_chunked [String.make 5000 'x'] "" []) Chunked
          in
          is_true (not (H1_body.drain t ~max:4096)) ;
          let _, t = handle (String.make 4096 'x') (Fixed 4096) in
          is_true (H1_body.drain t ~max:4096) )
    ; test "a body whose read failed stays failed"
      @@ in_sim (fun () ->
          let r, t = handle ("zz\r\n0\r\n\r\n" ^ next) Chunked in
          let b = H1_body.body t and buf = Bytes.create 8 in
          let bad () =
            raises_match
              (function H1_error.E (Bad_request _) -> true | _ -> false)
              (fun () -> Body.read b buf 0 8)
          in
          bad () ;
          let consumed = Buf_reader.consumed r in
          bad () ;
          equal int ~msg:"nothing more read" consumed (Buf_reader.consumed r) ;
          is_true ~msg:"failed" (H1_body.failed t) ;
          is_true ~msg:"not complete" (not (H1_body.is_complete t)) ;
          is_true ~msg:"not drained" (not (H1_body.drain t ~max:100)) ;
          equal (option int) ~msg:"unread unknown" None (H1_body.unread t) ;
          let _, t = handle "hello" (Fixed 5) in
          is_true ~msg:"a sound body has not failed" (not (H1_body.failed t)) )
    ; test "drain fails on bad or truncated bodies"
      @@ in_sim (fun () ->
          let _, t = handle "5\r\nhelloX" Chunked in
          is_true (not (H1_body.drain t ~max:100)) ;
          let _, t = handle "hel" (Fixed 5) in
          is_true (not (H1_body.drain t ~max:100)) ;
          let _, t = handle ~max_body:3 "5\r\nhello\r\n0\r\n\r\n" Chunked in
          is_true (not (H1_body.drain t ~max:100)) )
    ; test "drain refuses while 100 Continue is owed"
      @@ in_sim (fun () ->
          let r, t =
            handle ~expect_continue:ignore ("hello" ^ next) (Fixed 5)
          in
          is_true ~msg:"owed before the first read" (H1_body.continue_owed t) ;
          is_true (not (H1_body.drain t ~max:100)) ;
          equal int 0 (Buf_reader.consumed r) ;
          let r, t =
            handle ~expect_continue:ignore ("hello" ^ next) (Fixed 5)
          in
          ignore (Body.read (H1_body.body t) (Bytes.create 1) 0 1) ;
          is_true ~msg:"sent on the first read" (not (H1_body.continue_owed t)) ;
          is_true (H1_body.drain t ~max:100) ;
          let _, t = handle ~expect_continue:ignore "" (Fixed 0) in
          is_true ~msg:"not owed for an empty body"
            (not (H1_body.continue_owed t)) ;
          equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100) )
    ; test "drain works after the handler closed the body"
      @@ in_sim (fun () ->
          let r, t = handle ("hello" ^ next) (Fixed 5) in
          Body.close (H1_body.body t) ;
          is_true (H1_body.drain t ~max:5) ;
          equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100) )
    ; prop "drain completes any partly read body and keeps what follows"
        Gen.(
          triple (pair string bool) (int_range 0 40)
            (pair (int_range 1 9) (list ~size:(int_range 0 4) string)) )
        (fun ((body, chunked), read, (chunk, pieces)) ->
          Sim_support.run
          @@ fun _ ->
          let input, framing =
            if chunked then
              ( encode_chunked (body :: pieces) "" []
              , (H1_parse.Chunked : H1_parse.framing) )
            else (body, H1_parse.Fixed (String.length body))
          in
          let r, t = handle ~chunk (input ^ next) framing in
          let b = H1_body.body t in
          let buf = Bytes.create 64 in
          let rec read_some left =
            if left > 0 then
              match Body.read b buf 0 (Int.min 64 left) with
              | 0 ->
                  ()
              | n ->
                  read_some (left - n)
          in
          read_some read ;
          is_true (H1_body.drain t ~max:max_int) ;
          is_true (H1_body.is_complete t) ;
          equal string "GET /next HTTP/1.1" (Buf_reader.read_line r ~max:100) )
    ; test "unread"
      @@ in_sim (fun () ->
          let _, t = handle "" No_body in
          equal (option int) (Some 0) (H1_body.unread t) ;
          let _, t = handle "hello" (Fixed 5) in
          equal (option int) (Some 5) (H1_body.unread t) ;
          ignore (Body.read (H1_body.body t) (Bytes.create 2) 0 2) ;
          equal (option int) (Some 3) (H1_body.unread t) ;
          let _, t = handle "5\r\nhello\r\n0\r\n\r\n" Chunked in
          equal (option int) None (H1_body.unread t) ;
          ignore (Body.to_string (H1_body.body t)) ;
          equal (option int) (Some 0) (H1_body.unread t) )
    ; test "drain reads nothing when more than max is known to remain"
      @@ in_sim (fun () ->
          let r, t = handle ("hello world" ^ next) (Fixed 11) in
          ignore (Body.read (H1_body.body t) (Bytes.create 4) 0 4) ;
          let consumed = Buf_reader.consumed r in
          is_true (not (H1_body.drain t ~max:6)) ;
          equal int consumed (Buf_reader.consumed r) ;
          is_true (H1_body.drain t ~max:7) ) ]

(* Buf_writer *)

module Buf_writer = Private.Buf_writer

(* A writer whose output calls are recorded. *)
let recorded ?size () =
  let calls = ref [] in
  let w =
    Buf_writer.make ?size (fun buf off len ->
        calls := Bytes.sub_string buf off len :: !calls )
  in
  (w, fun () -> List.rev !calls)

let buf_writer_tests =
  group "Buf_writer"
    [ test "the buffer is allocated by the first write that needs it" (fun () ->
          let w, calls = recorded ~size:8 () in
          equal int 0 (Buf_writer.capacity w) ;
          Buf_writer.write_string w "12345678" ;
          equal int ~msg:"large writes go through" 0 (Buf_writer.capacity w) ;
          Buf_writer.write_char w 'a' ;
          equal int 8 (Buf_writer.capacity w) ;
          let w, _ = recorded ~size:8 () in
          Buf_writer.write_string w "abc" ;
          equal int 8 (Buf_writer.capacity w) ;
          equal (list string) ["12345678"] (calls ()) )
    ; test "a given buffer is used from the start" (fun () ->
          let buffer = Bytes.make 8 '.' in
          let calls = ref [] in
          let w =
            Buf_writer.make ~buffer (fun buf off len ->
                calls := Bytes.sub_string buf off len :: !calls )
          in
          equal int 8 (Buf_writer.capacity w) ;
          Buf_writer.write_string w "abc" ;
          equal string ~msg:"the write went into the given bytes" "abc....."
            (Bytes.to_string buffer) ;
          Buf_writer.flush w ;
          equal (list string) ["abc"] (List.rev !calls) ;
          let write _ _ _ = () in
          raises_match is_invalid_arg (fun () ->
              Buf_writer.make ~size:8 ~buffer write ) ;
          raises (Invalid_argument "Buf_writer.make: buffer must not be empty")
            (fun () -> Buf_writer.make ~buffer:Bytes.empty write ) )
    ; test "writes are buffered until flush" (fun () ->
          let w, calls = recorded () in
          Buf_writer.write_string w "HTTP/1.1 200 OK\r\n" ;
          Buf_writer.write_char w 'X' ;
          Buf_writer.write_bytes w (Bytes.of_string "--abc--") 2 3 ;
          equal (list string) [] (calls ()) ;
          equal int 21 (Buf_writer.buffered w) ;
          Buf_writer.flush w ;
          equal (list string) ["HTTP/1.1 200 OK\r\nXabc"] (calls ()) ;
          equal int 0 (Buf_writer.buffered w) ;
          Buf_writer.flush w ;
          equal int 1 (List.length (calls ())) )
    ; test "a write that does not fit flushes first" (fun () ->
          let w, calls = recorded ~size:4 () in
          Buf_writer.write_string w "abc" ;
          Buf_writer.write_string w "de" ;
          Buf_writer.write_string w "fg" ;
          Buf_writer.flush w ;
          equal (list string) ["abc"; "defg"] (calls ()) )
    ; test "write_char flushes a full buffer" (fun () ->
          let w, calls = recorded ~size:2 () in
          String.iter (Buf_writer.write_char w) "abcde" ;
          Buf_writer.flush w ;
          equal (list string) ["ab"; "cd"; "e"] (calls ()) )
    ; test
        "a large write fills the buffer, then the rest goes straight to the \
         output" (fun () ->
          let w, calls = recorded ~size:4 () in
          Buf_writer.write_string w "ab" ;
          Buf_writer.write_string w "0123456789" ;
          Buf_writer.write_string w "abcd" ;
          Buf_writer.write_string w "" ;
          equal (list string) ["ab01"; "23456789"; "abcd"] (calls ()) ;
          equal int 0 (Buf_writer.buffered w) )
    ; test "invalid arguments" (fun () ->
          (* A large write to an empty buffer skips the copy. Thus only the
             range check can raise. *)
          let w = Buf_writer.make ~size:2 ignore3 in
          List.iter
            (fun (off, len) ->
              raises_match is_invalid_arg (fun () ->
                  Buf_writer.write_bytes w (Bytes.create 4) off len ) )
            [(-1, 1); (0, -1); (2, 3); (5, 0)] ;
          raises_match is_invalid_arg (fun () ->
              Buf_writer.make ~size:0 ignore3 ) )
    ; prop "output is the writes in order; calls are bounded"
        Gen.(
          pair
            (list ~size:(int_range 0 12)
               (string_of ~size:(int_range 0 12) (char_range 'a' 'z')) )
            (int_range 1 8) )
        (fun (writes, size) ->
          let w, calls = recorded ~size () in
          List.iter (Buf_writer.write_string w) writes ;
          Buf_writer.flush w ;
          equal string (String.concat "" writes) (String.concat "" (calls ())) ;
          List.iter
            (fun c ->
              is_true
                ~msg:
                  "a call is at most size bytes, or the end of one large write"
                ( String.length c <= size
                || List.exists (fun s -> String.ends_with ~suffix:c s) writes ) )
            (calls ()) ) ]

(* H1_write: framing and response heads *)

module H1_write = Private.H1_write

let show_framing_of ~meth ~version r =
  match H1_write.framing ~meth ~version r with
  | H1_write.No_body ->
      "no body"
  | Length_only n ->
      Printf.sprintf "length only %d" n
  | Length n ->
      Printf.sprintf "length %d" n
  | Chunked ->
      "chunked"
  | Close_delimited ->
      "close-delimited"

let stream_body = Response.stream ignore

let sized s = Response.string s

let response_framing_tests =
  group "H1_write.framing"
    [ test "framing table" (fun () ->
          let row (label, meth, version, r) =
            Printf.sprintf "%-26s => %s" label
              (show_framing_of ~meth ~version r)
          in
          let st code = Response.with_status (Status.v code) in
          expect
            (String.concat "\n"
               (List.map row
                  Method.
                    [ ("GET 200 sized", GET, Version.Http_1_1, sized "hello")
                    ; ("GET 200 empty", GET, Http_1_1, Response.make Body.empty)
                    ; ("GET 200 stream 1.1", GET, Http_1_1, stream_body)
                    ; ("GET 200 stream 1.0", GET, Http_1_0, stream_body)
                    ; ("GET 200 sized 1.0", GET, Http_1_0, sized "hello")
                    ; ("GET 101 upgrade", GET, Http_1_1, Response.upgrade ignore)
                    ; ("GET 204", GET, Http_1_1, Response.empty ())
                    ; ("GET 204 with a body", GET, Http_1_1, st 204 (sized "x"))
                    ; ("GET 304", GET, Http_1_1, st 304 (sized "x"))
                    ; ("GET 404 sized", GET, Http_1_1, st 404 (sized "nope"))
                    ; ("HEAD 200 sized", HEAD, Http_1_1, sized "hello")
                    ; ("HEAD 200 stream", HEAD, Http_1_1, stream_body)
                    ; ("HEAD 304", HEAD, Http_1_1, st 304 (sized "x"))
                    ; ("GET 205 sized", GET, Http_1_1, st 205 (sized "x"))
                    ; ("HEAD 205 sized", HEAD, Http_1_1, st 205 (sized "x"))
                    ; ("GET 205 stream 1.1", GET, Http_1_1, st 205 stream_body)
                    ; ("GET 205 stream 1.0", GET, Http_1_0, st 205 stream_body)
                    ; ("CONNECT 200", CONNECT, Http_1_1, sized "x")
                    ; ( "CONNECT 200 upgrade"
                      , CONNECT
                      , Http_1_1
                      , Response.upgrade ~status:Status.ok ignore )
                    ; ("CONNECT 404", CONNECT, Http_1_1, st 404 (sized "no"))
                    ; ("CONNECT 299", CONNECT, Http_1_1, st 299 stream_body)
                    ; ("CONNECT 205", CONNECT, Http_1_1, st 205 (sized "x"))
                    ; ( "CONNECT 300 stream"
                      , CONNECT
                      , Http_1_1
                      , st 300 stream_body ) ] ) )
          @@ __POS_OF__
               {|
            GET 200 sized              => length 5
            GET 200 empty              => length 0
            GET 200 stream 1.1         => chunked
            GET 200 stream 1.0         => close-delimited
            GET 200 sized 1.0          => length 5
            GET 101 upgrade            => no body
            GET 204                    => no body
            GET 204 with a body        => no body
            GET 304                    => no body
            GET 404 sized              => length 4
            HEAD 200 sized             => length only 5
            HEAD 200 stream            => no body
            HEAD 304                   => no body
            GET 205 sized              => length only 0
            HEAD 205 sized             => length only 0
            GET 205 stream 1.1         => length only 0
            GET 205 stream 1.0         => length only 0
            CONNECT 200                => no body
            CONNECT 200 upgrade        => no body
            CONNECT 404                => length 2
            CONNECT 299                => no body
            CONNECT 205                => no body
            CONNECT 300 stream         => chunked
            |} )
    ; test "wants_close" (fun () ->
          let with_conn v =
            Response.string ~headers:(Headers.of_list [("Connection", v)]) ""
          in
          is_true (H1_write.wants_close (with_conn "close")) ;
          is_true (H1_write.wants_close (with_conn "Close")) ;
          is_true (H1_write.wants_close (with_conn "keep-alive, close")) ;
          is_true (not (H1_write.wants_close (with_conn "keep-alive"))) ;
          is_true (not (H1_write.wants_close (Response.string ""))) ) ]

let render ?date ?(version = Version.Http_1_1) ?(keep_alive = true)
    ?(meth = Method.GET) r =
  let out = Buffer.create 256 in
  let w = Buf_writer.make (fun b o l -> Buffer.add_subbytes out b o l) in
  H1_write.head w ?date ~version ~keep_alive
    (H1_write.framing ~meth ~version r)
    r ;
  Buf_writer.flush w ;
  (* Show CRLFs, so that the expected bytes are exact. *)
  String.concat "\\r\\n\n"
    (String.split_on_char '\n'
       (String.concat "" (String.split_on_char '\r' (Buffer.contents out))) )

let date = "Sun, 06 Nov 1994 08:49:37 GMT"

let hs = Headers.of_list

let response_head_tests =
  group "H1_write.head"
    [ test "a sized response with a Date" (fun () ->
          expect (render ~date (sized "hello"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Date: Sun, 06 Nov 1994 08:49:37 GMT\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 5\r\n
            \r\n
            |} )
    ; test "the handler's Date wins; no Date when none is given" (fun () ->
          let r =
            Response.string
              ~headers:(hs [("date", "Mon, 07 Nov 1994 00:00:00 GMT")])
              "x"
          in
          expect (render ~date r ^ render (sized "x"))
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            date: Mon, 07 Nov 1994 00:00:00 GMT\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            \r\n
            HTTP/1.1 200 OK\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            \r\n
            |} )
    ; test "fields the connection owns are dropped, others keep their order"
        (fun () ->
          let r =
            Response.stream
              ~headers:
                (hs
                   [ ("X-First", "1")
                   ; ("Content-Length", "99")
                   ; ("transfer-encoding", "gzip")
                   ; ("CONNECTION", "keep-alive")
                   ; ("Keep-Alive", "timeout=5")
                   ; ("Upgrade", "h2c")
                   ; ("Proxy-Connection", "x")
                   ; ("TE", "trailers")
                   ; ("Set-Cookie", "a=1")
                   ; ("Set-Cookie", "b=2")
                   ; ("Trailer", "X-Sum") ] )
              ignore
          in
          expect (render r)
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            X-First: 1\r\n
            Set-Cookie: a=1\r\n
            Set-Cookie: b=2\r\n
            Trailer: X-Sum\r\n
            Transfer-Encoding: chunked\r\n
            \r\n
            |} )
    ; test "101 keeps Upgrade and says Connection: Upgrade" (fun () ->
          let r =
            Response.upgrade
              ~headers:(hs [("Upgrade", "websocket"); ("Connection", "x")])
              ignore
          in
          expect (render ~keep_alive:false r)
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols\r\n
            Upgrade: websocket\r\n
            Connection: Upgrade\r\n
            \r\n
            |} )
    ; test "426 keeps Upgrade and adds the upgrade option (RFC 9110 7.8)"
        (fun () ->
          let r =
            Response.string ~status:Status.upgrade_required
              ~headers:(hs [("Upgrade", "example/1"); ("Connection", "x")])
              "!"
          in
          expect
            (String.concat "\n"
               [ render r
               ; render ~keep_alive:false r
               ; render ~version:Version.Http_1_0 r ] )
          @@ __POS_OF__
               {|
            HTTP/1.1 426 Upgrade Required\r\n
            Upgrade: example/1\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            Connection: Upgrade\r\n
            \r\n

            HTTP/1.1 426 Upgrade Required\r\n
            Upgrade: example/1\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            Connection: Upgrade, close\r\n
            \r\n

            HTTP/1.1 426 Upgrade Required\r\n
            Upgrade: example/1\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            Connection: Upgrade, keep-alive\r\n
            \r\n
            |} )
    ; test "a 2xx tunnel gets no Connection field" (fun () ->
          (* After the head the connection is a tunnel: keep-alive has no
             meaning, and close (RFC 9112 9.6) would end it. The writer drops
             the fields of the handler. *)
          let r =
            Response.upgrade ~status:Status.ok
              ~headers:
                (hs [("Connection", "close"); ("Keep-Alive", "timeout=5")])
              ignore
          in
          expect
            (String.concat "\n"
               [ render ~meth:Method.CONNECT ~version:Version.Http_1_0 r
               ; render ~meth:Method.CONNECT ~keep_alive:false r ] )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            \r\n

            HTTP/1.1 200 OK\r\n
            \r\n
            |} )
    ; test "426 without Upgrade gets no upgrade option" (fun () ->
          expect (render (Response.string ~status:Status.upgrade_required "!"))
          @@ __POS_OF__
               {|
            HTTP/1.1 426 Upgrade Required\r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 1\r\n
            \r\n
            |} )
    ; test
        "a 304 does not send four fields of the handler (RFC 9110 15.4.5, 14.4)"
        (fun () ->
          let fields =
            [ ("Content-Type", "text/html")
            ; ("CONTENT-ENCODING", "gzip")
            ; ("Content-Language", "en")
            ; ("Content-Range", "bytes 0-1/2")
            ; ("Content-Location", "/a")
            ; ("ETag", "\"x\"")
            ; ("Last-Modified", "Sun, 06 Nov 1994 08:49:37 GMT")
            ; ("Vary", "Accept")
            ; ("Cache-Control", "max-age=60")
            ; ("Expires", "Sun, 06 Nov 1994 09:49:37 GMT")
            ; ("X-Other", "1") ]
          in
          let r ?(extra = []) status body =
            Response.with_status status
              (Response.make ~headers:(hs (extra @ fields)) body)
          in
          expect
            (String.concat "--\n"
               [ render ~date (r Status.not_modified (Body.of_string "x"))
               ; render ~date
                   (r
                      ~extra:[("Date", "Mon, 07 Nov 1994 08:49:37 GMT")]
                      Status.not_modified Body.empty ) ] )
          @@ __POS_OF__
               {|
            HTTP/1.1 304 Not Modified\r\n
            Date: Sun, 06 Nov 1994 08:49:37 GMT\r\n
            Content-Location: /a\r\n
            ETag: "x"\r\n
            Last-Modified: Sun, 06 Nov 1994 08:49:37 GMT\r\n
            Vary: Accept\r\n
            Cache-Control: max-age=60\r\n
            Expires: Sun, 06 Nov 1994 09:49:37 GMT\r\n
            X-Other: 1\r\n
            \r\n
            --
            HTTP/1.1 304 Not Modified\r\n
            Date: Mon, 07 Nov 1994 08:49:37 GMT\r\n
            Content-Location: /a\r\n
            ETag: "x"\r\n
            Last-Modified: Sun, 06 Nov 1994 08:49:37 GMT\r\n
            Vary: Accept\r\n
            Cache-Control: max-age=60\r\n
            Expires: Sun, 06 Nov 1994 09:49:37 GMT\r\n
            X-Other: 1\r\n
            \r\n
            |} )
    ; test "other responses send the four fields" (fun () ->
          let fields =
            hs
              [ ("Content-Type", "text/html")
              ; ("Content-Encoding", "gzip")
              ; ("Content-Language", "en")
              ; ("Content-Range", "bytes 0-1/2") ]
          in
          expect
            (String.concat "--\n"
               [ render (Response.make ~headers:fields (Body.of_string "x"))
               ; render
                   (Response.with_status Status.no_content
                      (Response.make ~headers:fields Body.empty) )
               ; render ~meth:Method.HEAD
                   (Response.stream ~headers:fields ignore) ] )
          @@ __POS_OF__
               {|
            HTTP/1.1 200 OK\r\n
            Content-Type: text/html\r\n
            Content-Encoding: gzip\r\n
            Content-Language: en\r\n
            Content-Range: bytes 0-1/2\r\n
            Content-Length: 1\r\n
            \r\n
            --
            HTTP/1.1 204 No Content\r\n
            Content-Type: text/html\r\n
            Content-Encoding: gzip\r\n
            Content-Language: en\r\n
            Content-Range: bytes 0-1/2\r\n
            \r\n
            --
            HTTP/1.1 200 OK\r\n
            Content-Type: text/html\r\n
            Content-Encoding: gzip\r\n
            Content-Language: en\r\n
            Content-Range: bytes 0-1/2\r\n
            \r\n
            |} )
    ; test "an unregistered status has an empty reason" (fun () ->
          expect (render (Response.with_status (Status.v 299) (sized "")))
          @@ __POS_OF__
               {|
            HTTP/1.1 299 \r\n
            Content-Type: text/plain; charset=utf-8\r\n
            Content-Length: 0\r\n
            \r\n
            |} )
    ; test "Connection by version and keep_alive" (fun () ->
          let r = sized "x" in
          expect
            (String.concat "--\n"
               [ render ~keep_alive:true r
               ; render ~keep_alive:false r
               ; render ~version:Http_1_0 ~keep_alive:true r
               ; render ~version:Http_1_0 ~keep_alive:false r
               ; render ~version:Http_1_0 ~keep_alive:false stream_body
               ; render ~version:Http_1_0 ~keep_alive:true stream_body ] )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "HEAD and 204/304 heads" (fun () ->
          expect
            (String.concat "--\n"
               [ render ~meth:Method.HEAD (sized "hello")
               ; render ~meth:Method.HEAD stream_body
               ; render (Response.empty ())
               ; render (Response.with_status Status.not_modified (sized "x"))
               ] )
          @@ __POS_OF__
               {|
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
            \r\n
            |} )
    ]

(* H1_write.body *)

(* Shows each CRLF as a visible \\r\\n and a line break; other bytes as they
   are. *)
let show_crlf s =
  let b = Buffer.create (String.length s) and n = String.length s in
  let rec loop i =
    if i < n then
      if s.[i] = '\r' && i + 1 < n && s.[i + 1] = '\n' then (
        Buffer.add_string b "\\r\\n\n" ;
        loop (i + 2) )
      else (
        Buffer.add_char b s.[i] ;
        loop (i + 1) )
  in
  loop 0 ; Buffer.contents b

(* The bytes [H1_write.body] writes, and its output calls. *)
let write_body ?(size = 64) framing b =
  let w, calls = recorded ~size () in
  H1_write.body w framing b ;
  (String.concat "" (calls ()), calls ())

let pieces_source ?close pieces =
  let rest = ref pieces in
  Body.of_source ?close (fun buf off _ ->
      match !rest with
      | [] ->
          0
      | p :: tl ->
          rest := tl ;
          Bytes.blit_string p 0 buf off (String.length p) ;
          String.length p )

let response_body_tests =
  group "H1_write.body"
    [ test "before runs once, ahead of the body's first output" (fun () ->
          let show framing b =
            let w, calls = recorded () in
            let log = ref [] in
            let before () =
              log := "[head]" :: !log ;
              Buf_writer.write_string w "HEAD;"
            in
            H1_write.body ~before w framing b ;
            String.escaped (String.concat "" (calls ()))
            ^ " " ^ String.concat "," !log
          in
          let quiet_then_data =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "" ;
                Body.Writer.write_string w "ab" ;
                Body.Writer.flush w ;
                Body.Writer.write_string w "c" )
          in
          expect
            (String.concat "\n"
               [ show Chunked quiet_then_data
               ; show (Length 3) (Body.of_string "abc")
               ; show Close_delimited (Body.of_writer ignore)
               ; show No_body (Body.of_string "ignored")
               ; show Chunked (Body.of_writer ignore) ] )
          @@ __POS_OF__
               {|
            HEAD;2\r\nab\r\n1\r\nc\r\n0\r\n\r\n [head]
            HEAD;abc [head]
            HEAD; [head]
            HEAD; [head]
            HEAD;0\r\n\r\n [head]
            |} )
    ; test "trailers that may not follow the content are dropped" (fun () ->
          let b =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "x" ;
                Body.Writer.set_trailers w
                  (Headers.of_list
                     [ ("X-Sum", "1")
                     ; ("Content-Length", "99")
                     ; ("Set-Cookie", "a=b")
                     ; ("transfer-encoding", "chunked")
                     ; ("Server-Timing", "db;dur=5") ] ) )
          in
          expect (String.escaped (fst (write_body Chunked b)))
          @@ __POS_OF__
               {| 1\r\nx\r\n0\r\nX-Sum: 1\r\nServer-Timing: db;dur=5\r\n\r\n |} )
    ; test "golden bodies" (fun () ->
          let producer =
            Body.of_writer (fun w ->
                Body.Writer.write_string w "data: 1\n\n" ;
                Body.Writer.flush w ;
                Body.Writer.write_string w "" ;
                Body.Writer.write_string w "0123456789abcdef!" ;
                Body.Writer.set_trailers w (Headers.of_list [("X-Sum", "42")]) )
          in
          let cases =
            [ ("length", H1_write.Length 5, Body.of_string "hello")
            ; ("length 0", Length 0, Body.empty)
            ; ("chunked string", Chunked, Body.of_string "hello")
            ; ( "chunked source"
              , Chunked
              , pieces_source ["ab"; "cdefghijklmnopq"] )
            ; ("chunked producer", Chunked, producer)
            ; ("chunked empty", Chunked, Body.empty)
            ; ("close-delimited", Close_delimited, pieces_source ["ab"; "cd"])
            ; ("no body", No_body, Body.of_string "hidden")
            ; ("head", Length_only 6, Body.of_string "hidden") ]
          in
          expect
            (String.concat "\n"
               (List.map
                  (fun (label, framing, b) ->
                    Printf.sprintf "-- %s\n%s" label
                      (show_crlf (fst (write_body framing b))) )
                  cases ) )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "an exception of the body wins over an exception of the close"
        (fun () ->
          let cancel = Weft.Fiber.Cancelled Exit in
          let b =
            Body.of_source
              ~close:(fun () -> failwith "close")
              (fun _ _ _ -> raise cancel)
          in
          raises cancel (fun () -> ignore (write_body Chunked b)) )
    ; test "Content-Length mismatches" (fun () ->
          raises H1_write.Length_mismatch (fun () ->
              write_body (Length 6) (Body.of_string "hello") ) ;
          let w, calls = recorded () in
          raises H1_write.Length_mismatch (fun () ->
              H1_write.body w (Length 4) (pieces_source ["ab"; "cde"]) ) ;
          Buf_writer.flush w ;
          equal string "ab" (String.concat "" (calls ())) ;
          raises H1_write.Length_mismatch (fun () ->
              write_body (Length 4)
                (Body.of_writer (fun w -> Body.Writer.write_string w "hello")) ) )
    ; test "bodies that are not sent are closed, producers never run" (fun () ->
          let closes = ref 0 and ran = ref false in
          List.iter
            (fun framing ->
              ignore
                (write_body framing
                   (pieces_source ~close:(fun () -> incr closes) ["x"]) ) ;
              equal string ""
                (fst
                   (write_body framing (Body.of_writer (fun _ -> ran := true))) ) )
            H1_write.[No_body; Length_only 1] ;
          equal int 2 !closes ;
          is_true (not !ran) )
    ; test "framings without a body still flush the head" (fun () ->
          List.iter
            (fun framing ->
              let w, calls = recorded () in
              Buf_writer.write_string w "HTTP/1.1 204 No Content\r\n\r\n" ;
              H1_write.body w framing Body.empty ;
              equal (list string) ["HTTP/1.1 204 No Content\r\n\r\n"] (calls ()) )
            H1_write.[No_body; Length_only 5] )
    ; test "the body is closed once, also when writing fails" (fun () ->
          let closes = ref 0 in
          ignore
            (write_body (Length 2)
               (pieces_source ~close:(fun () -> incr closes) ["ab"]) ) ;
          equal int 1 !closes ;
          raises H1_write.Length_mismatch (fun () ->
              write_body (Length 9)
                (pieces_source ~close:(fun () -> incr closes) ["ab"]) ) ;
          equal int 2 !closes ;
          raises Exit (fun () ->
              write_body Chunked
                (Body.of_source
                   ~close:(fun () -> incr closes)
                   (fun _ _ _ -> raise Exit) ) ) ;
          equal int 3 !closes )
    ; test "each source chunk and each producer flush reaches the output"
        (fun () ->
          equal (list string)
            ["2\r\nab\r\n"; "2\r\ncd\r\n"; "0\r\n\r\n"]
            (snd (write_body Chunked (pieces_source ["ab"; "cd"]))) ;
          equal (list string) ["a"; "b"]
            (snd
               (write_body (Length 2)
                  (Body.of_writer (fun w ->
                       Body.Writer.write_string w "a" ;
                       Body.Writer.flush w ;
                       Body.Writer.write_string w "b" ) ) ) ) )
    ; prop "what is written decodes back"
        Gen.(
          triple
            (list ~size:(int_range 0 6) string)
            (list ~size:(int_range 0 3)
               (pair token_gen
                  (map String.trim (string_of (char_range 'a' 'z'))) ) )
            (pair (int_range 1 9) (int_range 1 64)) )
        (fun (pieces, trailers, (chunk, size)) ->
          let produce w =
            List.iter (Body.Writer.write_string w) pieces ;
            Body.Writer.set_trailers w (Headers.of_list trailers)
          in
          let wire, _ = write_body ~size Chunked (Body.of_writer produce) in
          let b =
            H1_body.chunked ~limits (Buf_reader.of_string ~chunk (wire ^ next))
          in
          equal string (String.concat "" pieces) (Body.to_string b) ;
          equal
            (list (pair string string))
            trailers
            (Headers.to_list (Body.trailers b)) ;
          let all = String.concat "" pieces in
          let wire, _ =
            write_body ~size
              (Length (String.length all))
              (Body.of_writer produce)
          in
          equal string all wire ) ]

(* H1_write.request_head, for a client *)

(* The bytes of [req] on the wire: its head, then its body. *)
let request_bytes req =
  let w, calls = recorded () in
  let framing = H1_write.request_framing req in
  H1_write.request_head w framing req ;
  H1_write.body w framing (Request.body req) ;
  String.concat "" (calls ())

let show_wire s =
  String.concat "\n" (List.map String.escaped (String.split_on_char '\n' s))

let request_write_tests =
  let req ?(authority = "a.example") ?(headers = []) ?body meth path =
    Request.make ~authority ~headers:(Headers.of_list headers) ?body meth path
  in
  group "H1_write.request_head"
    [ test "request heads" (fun () ->
          expect
            (String.concat "\n--\n"
               (List.map
                  (fun r -> show_wire (request_bytes r))
                  Method.
                    [ req GET ""
                    ; req GET "/search?q=ocaml"
                    ; req ~body:(Body.of_string "abc") POST "/p"
                    ; req POST "/empty"
                    ; req
                        ~body:
                          (Body.of_writer (fun w ->
                               Body.Writer.write_string w "ab" ;
                               Body.Writer.write_string w "cd" ) )
                        PUT "/w"
                    ; req OPTIONS "*"
                    ; req ~authority:"a.example:443" CONNECT ""
                    ; req
                        ~headers:
                          [ ("Host", "evil.example")
                          ; ("Connection", "close, X-Hop")
                          ; ("X-Hop", "1")
                          ; ("Transfer-Encoding", "chunked")
                          ; ("X-Keep", "1")
                          ; ("Content-Length", "9")
                          ; ("TE", "trailers")
                          ; ("Upgrade", "h2c")
                          ; ("Keep-Alive", "5") ]
                        GET "/h" ] ) )
          @@ __POS_OF__
               {|
            GET / HTTP/1.1\r
            Host: a.example\r
            \r

            --
            GET /search?q=ocaml HTTP/1.1\r
            Host: a.example\r
            \r

            --
            POST /p HTTP/1.1\r
            Host: a.example\r
            Content-Length: 3\r
            \r
            abc
            --
            POST /empty HTTP/1.1\r
            Host: a.example\r
            Content-Length: 0\r
            \r

            --
            PUT /w HTTP/1.1\r
            Host: a.example\r
            Transfer-Encoding: chunked\r
            \r
            2\r
            ab\r
            2\r
            cd\r
            0\r
            \r

            --
            OPTIONS * HTTP/1.1\r
            Host: a.example\r
            \r

            --
            CONNECT a.example:443 HTTP/1.1\r
            Host: a.example:443\r
            \r

            --
            GET /h HTTP/1.1\r
            Host: a.example\r
            X-Keep: 1\r
            Connection: close\r
            \r
            |} )
    ; test "bad requests raise Invalid_argument" (fun () ->
          List.iter
            (fun r -> raises_match is_invalid_arg (fun () -> request_bytes r))
            [ Request.make Method.GET "/"
            ; req Method.GET "foo"
            ; req Method.GET "/a#b"
            ; req Method.OPTIONS "foo" ] ) ]

let request_round_trip =
  let open Gen in
  let meth = of_list Method.[GET; POST; PUT; DELETE; PATCH] in
  let path =
    map
      (fun s -> "/" ^ s)
      (string_of ~size:(int_range 0 12)
         (of_list (List.of_seq (String.to_seq "abz09/?=&%-._~"))) )
  in
  let value =
    map
      (fun s -> String.trim s)
      (string_of ~size:(int_range 0 10)
         (of_list (List.of_seq (String.to_seq "ab z9,;=\t\""))) )
  in
  let body =
    pair bool
      (list ~size:(int_range 0 4) (string_of ~size:(int_range 0 20) char))
  in
  triple (pair meth path)
    (list ~size:(int_range 0 5) (pair token_gen value))
    body

let request_write_props =
  group "H1_write.request_head properties"
    [ prop "the server parser reads what the client writes" request_round_trip
        (fun ((meth, path), fields, (known, pieces)) ->
          Sim_support.run
          @@ fun _ ->
          let all = String.concat "" pieces in
          let body =
            if known then Body.of_string all
            else
              Body.of_writer (fun w ->
                  List.iter (Body.Writer.write_string w) pieces )
          in
          let req =
            Request.make ~authority:"a.example:8080"
              ~headers:(Headers.of_list fields) ~body meth path
          in
          let r = Buf_reader.of_string (request_bytes req) in
          let h = H1_parse.request_head limits r in
          let t = H1_parse.request_target ~scheme:"http" h in
          let b =
            H1_body.body (H1_body.make ~limits r (H1_parse.request_framing h))
          in
          equal string (Method.to_string meth) (Method.to_string h.meth) ;
          equal string path t.path ;
          equal (option string) (Some "a.example:8080") t.authority ;
          let framing_field (n, _) =
            List.mem (String.lowercase_ascii n)
              ["host"; "content-length"; "transfer-encoding"]
          in
          equal
            (list (pair string string))
            fields
            (List.filter
               (fun f -> not (framing_field f))
               (Headers.to_list h.headers) ) ;
          equal string all (Body.to_string b) ;
          is_true (Buf_reader.peek_eof r) ) ]

let () =
  exit
    (run "h1"
       [ buf_reader_tests
       ; buf_reader_model
       ; request_head_tests
       ; head_props
       ; parse_response_tests
       ; parse_response_props
       ; response_framing_parse_tests
       ; until_close_tests
       ; framing_tests
       ; target_tests
       ; framing_props
       ; h1_body_tests
       ; chunked_tests
       ; chunked_props
       ; handle_tests
       ; buf_writer_tests
       ; response_framing_tests
       ; response_head_tests
       ; response_body_tests
       ; request_write_tests
       ; request_write_props ] )
