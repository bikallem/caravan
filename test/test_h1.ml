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

let () = exit (run "h1" [ buf_reader_tests; buf_reader_model ])
