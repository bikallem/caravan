open Windtrap
open Caravan.Private
open Caravan_websocket.Private

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

let hex s =
  String.concat ""
    (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq s)))

let sha1_tests =
  (* Cases are [(name, input, hex digest)]; a failure lists every name. *)
  let digests cases =
    equal (list (pair string string))
      (List.map (fun (name, _, digest) -> (name, digest)) cases)
      (List.map (fun (name, input, _) -> (name, hex (Sha1.digest input))) cases)
  in
  group "Sha1"
    [ test "FIPS 180 examples" (fun () ->
          digests
            [ ("empty", "", "da39a3ee5e6b4b0d3255bfef95601890afd80709");
              ("abc", "abc", "a9993e364706816aba3e25717850c26c9cd0d89d");
              ( "448 bits",
                "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
                "84983e441c3bd26ebaae4aa1f95129e5e54670f1" );
              ( "a million a",
                String.make 1_000_000 'a',
                "34aa973cd4c4daa4f61eeb2bdbad27316534016f" ) ]);
      (* Digests from sha1sum, of the bytes 0, 1, 2, ... *)
      test "lengths around a block boundary" (fun () ->
          let bytes n = String.init n (fun i -> Char.chr (i land 255)) in
          digests
            (List.map
               (fun (n, d) -> (string_of_int n, bytes n, d))
               [ (55, "8ae2d46729cfe68ff927af5eec9c7d1b66d65ac2");
                 (56, "636e2ec698dac903498e648bd2f3af641d3c88cb");
                 (63, "6d942da0c4392b123528f2905c713a3ce28364bd");
                 (64, "c6138d514ffa2135bfce0ed0b8fac65669917ec7");
                 (65, "69bd728ad6e13cd76ff19751fde427b00e395746");
                 (119, "41c89d06001bab4ab78736b44efe7ce18ce6ae08") ])) ]

(* Short strings over the alphabet, padding and a few strangers, so that
   some are valid encodings and most are not. *)
let base64ish =
  Gen.string_of ~size:(Gen.int_range 0 12)
    (Gen.of_list (List.of_seq (String.to_seq "AQgw+/Zz09==-_ \n")))

let base64_tests =
  let vectors =
    [ ("", ""); ("f", "Zg=="); ("fo", "Zm8="); ("foo", "Zm9v"); ("foob", "Zm9vYg==");
      ("fooba", "Zm9vYmE="); ("foobar", "Zm9vYmFy") ]
  in
  group "Base64"
    [ test "RFC 4648 examples encode" (fun () ->
          equal (list string) (List.map snd vectors) (List.map (fun (s, _) -> Base64.encode s) vectors));
      test "RFC 4648 examples decode" (fun () ->
          equal (list (option string))
            (List.map (fun (s, _) -> Some s) vectors)
            (List.map (fun (_, e) -> Base64.decode e) vectors));
      (* Expected from base64(1); it uses all 64 characters. *)
      test "the bytes 0 to 191" (fun () ->
          let s = String.init 192 (fun i -> Char.chr (i land 255)) in
          equal string
            "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/"
            (Base64.encode s);
          equal (option string) (Some s) (Base64.decode (Base64.encode s)));
      test "rejects what is not a canonical encoding" (fun () ->
          List.iter
            (fun e -> is_none ~pp:Format.pp_print_string (Base64.decode e))
            [ "Zg"; "Zg="; "Zm9vY"; "Zm9v!A=="; "Zm-_"; "Zm 9"; "Z==="; "===="; "Zg==Zg==";
              "Zm=v"; "Zh=="; "Zm9="; "Zm8=\n" ]);
      prop "decode inverts encode" Gen.string (fun s ->
          equal (option string) (Some s) (Base64.decode (Base64.encode s)));
      prop "encodings take 4 characters per 3 bytes, rounded up" Gen.string (fun s ->
          equal int (4 * ((String.length s + 2) / 3)) (String.length (Base64.encode s)));
      prop ~count:1000 "only canonical encodings decode" base64ish (fun e ->
          match Base64.decode e with
          | None -> ()
          | Some d -> equal string e (Base64.encode d)) ]

let of_hex h =
  let h = String.concat "" (String.split_on_char ' ' h) in
  String.init (String.length h / 2) (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let opcode_name : Frame.opcode -> string = function
| Continuation -> "continuation"
| Text -> "text"
| Binary -> "binary"
| Close -> "close"
| Ping -> "ping"
| Pong -> "pong"

let pp_frame ppf (f : Frame.t) =
  Format.fprintf ppf "%s%s %S" (opcode_name f.opcode) (if f.fin then "" else " (more)") f.payload

let frame = Testable.structural ~pp:pp_frame

let encode ?mask f =
  let out = Buffer.create 64 in
  let w = Buf_writer.make (fun b off len -> Buffer.add_subbytes out b off len) in
  Frame.write ?mask w f;
  Buf_writer.flush w;
  Buffer.contents out

(* What [Frame.read] makes of [input]: a frame, or the error. *)
let outcome ?(masked = true) ?(max = 1000) input =
  match Frame.read (Buf_reader.of_string input) ~masked ~max with
  | f -> Format.asprintf "%a" pp_frame f
  | exception Frame.Error (Protocol_error m) -> "protocol error: " ^ m
  | exception Frame.Error Too_big -> "too big"
  | exception End_of_file -> "end of input"

let frame_tests =
  let f ?(fin = true) opcode payload = { Frame.fin; opcode; payload } in
  let key = of_hex "37 fa 21 3d" in
  let b256 = String.init 256 (fun i -> Char.chr (i land 255)) in
  let b64k = String.init 65536 (fun i -> Char.chr (i land 255)) in
  (* RFC 6455 §5.7: [(frame, mask, bytes)]. *)
  let examples =
    [ (f Text "Hello", None, of_hex "81 05 48 65 6c 6c 6f");
      (f Text "Hello", Some key, of_hex "81 85 37 fa 21 3d 7f 9f 4d 51 58");
      (f ~fin:false Text "Hel", None, of_hex "01 03 48 65 6c");
      (f Continuation "lo", None, of_hex "80 02 6c 6f");
      (f Ping "Hello", None, of_hex "89 05 48 65 6c 6c 6f");
      (f Pong "Hello", Some key, of_hex "8a 85 37 fa 21 3d 7f 9f 4d 51 58");
      (f Binary b256, None, of_hex "82 7e 01 00" ^ b256);
      (f Binary b64k, None, of_hex "82 7f 00 00 00 00 00 01 00 00" ^ b64k) ]
  in
  group "Frame"
    [ test "RFC 6455 examples encode" (fun () ->
          equal (list string)
            (List.map (fun (_, _, e) -> hex e) examples)
            (List.map (fun (fr, mask, _) -> hex (encode ?mask fr)) examples));
      test "RFC 6455 examples decode, one after another" (fun () ->
          let unmasked = List.filter (fun (_, m, _) -> m = None) examples in
          let r = Buf_reader.of_string ~size:16 ~chunk:5 (String.concat "" (List.map (fun (_, _, e) -> e) unmasked)) in
          equal (list frame)
            (List.map (fun (fr, _, _) -> fr) unmasked)
            (List.map (fun _ -> Frame.read r ~masked:false ~max:65536) unmasked);
          is_true (Buf_reader.peek_eof r);
          List.iter
            (fun (fr, m, e) ->
               if m <> None
               then equal frame fr (Frame.read (Buf_reader.of_string e) ~masked:true ~max:5))
            examples);
      test "lengths take the shortest encoding" (fun () ->
          equal (list (pair int int))
            [ (125, 2); (126, 4); (65535, 4); (65536, 10) ]
            (List.map
               (fun n -> (n, String.length (encode (f Binary (String.make n 'x'))) - n))
               [ 125; 126; 65535; 65536 ]);
          equal (list (pair int int))
            [ (125, 6); (126, 8); (65536, 14) ]
            (List.map
               (fun n -> (n, String.length (encode ~mask:key (f Binary (String.make n 'x'))) - n))
               [ 125; 126; 65536 ]));
      test "the length limit" (fun () ->
          let input = encode (f Binary "0123456789") in
          expect (outcome ~masked:false ~max:10 input) @@ __POS_OF__ {|binary "0123456789"|};
          expect (outcome ~masked:false ~max:9 input) @@ __POS_OF__ {|too big|};
          (* The limit is only for data frames. *)
          expect (outcome ~masked:false ~max:0 (encode (f Ping "0123456789")))
          @@ __POS_OF__ {|ping "0123456789"|});
      (* Inputs end right after their length, so reading further would give
         "end of input". *)
      test "bad frames" (fun () ->
          let cases =
            [ ("rsv1", "c1 80"); ("rsv2", "a1 80"); ("rsv3", "91 80"); ("opcode 3", "83 80");
              ("opcode 7", "87 80"); ("opcode b", "8b 80"); ("opcode f", "8f 80");
              ("fragmented ping", "09 80"); ("fragmented close", "08 80");
              ("ping of 126", "89 fe 00 7e"); ("pong of 127", "8a ff 00 00 00 00 00 00 00 7f");
              ("unmasked", "81 00"); ("top bit", "82 ff 80 00 00 00 00 00 00 00");
              ("2^62 - 1", "82 ff 3f ff ff ff ff ff ff ff"); ("1001", "82 fe 03 e9");
              ("1000", "82 fe 03 e8") ]
          in
          expect
            (String.concat "\n" (List.map (fun (name, h) -> name ^ ": " ^ outcome (of_hex h)) cases))
          @@ __POS_OF__ {|rsv1: protocol error: reserved bits set
rsv2: protocol error: reserved bits set
rsv3: protocol error: reserved bits set
opcode 3: protocol error: reserved opcode 3
opcode 7: protocol error: reserved opcode 7
opcode b: protocol error: reserved opcode 11
opcode f: protocol error: reserved opcode 15
fragmented ping: protocol error: fragmented control frame
fragmented close: protocol error: fragmented control frame
ping of 126: protocol error: control frame longer than 125 bytes
pong of 127: protocol error: control frame longer than 125 bytes
unmasked: protocol error: unmasked frame
top bit: protocol error: length with its top bit set
2^62 - 1: too big
1001: too big
1000: end of input|};
          expect (outcome ~masked:false (of_hex "81 85 37 fa 21 3d 7f 9f 4d 51 58"))
          @@ __POS_OF__ {|protocol error: masked frame|};
          (* Long encodings of short lengths are accepted. *)
          expect (outcome ~masked:false (of_hex "81 7e 00 02 68 69")) @@ __POS_OF__ {|text "hi"|};
          expect (outcome ~masked:false (of_hex "81 7f 00 00 00 00 00 00 00 00"))
          @@ __POS_OF__ {|text ""|});
      test "a declared length does not allocate before the payload arrives" (fun () ->
          let header = of_hex "827f" ^ "\x00\x00\x00\x00\x00\x80\x00\x00" in
          let r = Buf_reader.of_string (header ^ "short") in
          let before = Gc.allocated_bytes () in
          raises End_of_file (fun () -> Frame.read r ~masked:false ~max:max_int);
          let used = Gc.allocated_bytes () -. before in
          is_true ~msg:(Printf.sprintf "%.0f bytes" used) (used < 1_000_000.));
      test "truncated input" (fun () ->
          let input = of_hex "81 fe 00 02 37 fa 21 3d 7f 9f" in
          equal (list string)
            (List.init (String.length input) (fun _ -> "end of input"))
            (List.init (String.length input) (fun n -> outcome (String.sub input 0 n)));
          expect (outcome input) @@ __POS_OF__ {|text "He"|});
      test "write rejects what it cannot send" (fun () ->
          List.iter
            (fun (mask, fr) -> raises_match is_invalid_arg (fun () -> encode ?mask fr))
            [ (Some "abc", f Text ""); (Some "abcde", f Text ""); (None, f ~fin:false Ping "");
              (None, f Close (String.make 126 'x')); (None, f Pong (String.make 126 'x')) ];
          is_true (String.length (encode ~mask:key (f Ping (String.make 125 'x'))) = 131));
      prop "a masked payload is the XOR of each byte with the key"
        Gen.(pair (string_of ~size:(constant 4) char) (string_of ~size:(int_range 0 300) char))
        (fun (mask, payload) ->
           let out = encode ~mask (f Binary payload) in
           let n = String.length payload in
           let masked = String.sub out (String.length out - n) n in
           equal string
             (String.mapi (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3])) payload)
             masked);
      prop "read inverts write, however the input is split"
        Gen.(
          quad
            (triple (of_list ~pp:(fun ppf o -> Format.pp_print_string ppf (opcode_name o))
                       Frame.[ Continuation; Text; Binary; Close; Ping; Pong ])
               bool (string_of ~size:(int_range 0 300) char))
            (option (string_of ~size:(constant 4) char))
            (int_range 1 9) (int_range 1 9))
        (fun ((opcode, fin, payload), mask, (chunk : int), size) ->
           let fr =
             if Frame.is_control opcode
             then f opcode (String.sub payload 0 (Int.min 125 (String.length payload)))
             else f ~fin opcode payload
           in
           let r = Buf_reader.of_string ~size ~chunk (encode ?mask fr) in
           equal frame fr (Frame.read r ~masked:(mask <> None) ~max:300);
           is_true (Buf_reader.peek_eof r)) ]

module Ws = Caravan_websocket

(* A frame from a client: clients mask their frames. *)
let client ?(fin = true) opcode payload = encode ~mask:"\x01\x02\x03\x04" { Frame.fin; opcode; payload }

let code_reason code reason =
  let b = Bytes.create 2 in
  Bytes.set_uint16_be b 0 code;
  Bytes.to_string b ^ reason

(* A Close frame shows its code and its reason. *)
let show_frame (f : Frame.t) =
  match f.opcode with
  | Close when f.payload = "" -> "close"
  | Close when String.length f.payload = 2 -> Printf.sprintf "close %d" (String.get_uint16_be f.payload 0)
  | Close ->
      Printf.sprintf "close %d %S" (String.get_uint16_be f.payload 0)
        (String.sub f.payload 2 (String.length f.payload - 2))
  | _ -> Format.asprintf "%a" pp_frame f

(* The frames in [out], one for each line. *)
let frames out =
  let r = Buf_reader.of_string out in
  let rec loop acc =
    match Frame.read r ~masked:false ~max:max_int with
    | f -> loop (show_frame f :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

(* Runs [app] on a server connection that reads [input]. The result is the
   lines that [app] logs, a "--" line, then the frames that the server sent. *)
let converse ?max_message input app =
  let out = Buffer.create 64 and log = ref [] in
  let note s = log := s :: !log in
  Affect_unix.Unix.main ~domain_count:1 (fun () ->
      app note (Ws.Private.of_flow ?max_message (Caravan.Flow.of_string input out)));
  String.concat "\n" (List.rev !log @ ("--" :: frames (Buffer.contents out)))

let note_closed note = function
| Ws.Closed { code; reason } -> note (Printf.sprintf "closed %d %S" code reason)
| e -> raise e

(* Sends back each message until the connection closes. *)
let echo note c =
  let rec loop () =
    match Ws.recv c with
    | Text s -> note (Printf.sprintf "text %S" s); Ws.send c (Text s); loop ()
    | Binary s -> note (Printf.sprintf "binary %S" s); Ws.send c (Binary s); loop ()
    | exception e -> note_closed note e
  in
  loop ()

let connection_tests =
  group "Connection"
    [ test "messages, then a Close from the peer" (fun () ->
          expect
            (converse
               (client Text "Hello" ^ client Binary "\x00\xff"
                ^ client Close (code_reason 1000 "bye"))
               echo)
          @@ __POS_OF__ {|
            text "Hello"
            binary "\000\255"
            closed 1000 "bye"
            --
            text "Hello"
            binary "\000\255"
            close 1000
            |});
      test "fragments with control frames between them" (fun () ->
          expect
            (converse
               (client ~fin:false Text "H\xe2\x82" ^ client Ping "p"
                ^ client ~fin:false Continuation "\xac" ^ client Pong "q"
                ^ client Continuation "lo")
               echo)
          @@ __POS_OF__ {|
            text "H\226\130\172lo"
            closed 1006 ""
            --
            pong "p"
            text "H\226\130\172lo"
            |});
      test "an empty Close" (fun () ->
          expect (converse (client Close "") echo) @@ __POS_OF__ {|
            closed 1005 ""
            --
            close
            |});
      test "the Close codes that are permitted" (fun () ->
          expect
            (String.concat "\n"
               (List.map
                  (fun code -> converse (client Close (code_reason code "")) echo)
                  [ 1000; 1003; 1007; 1014; 3000; 4999 ]))
          @@ __POS_OF__ {|
            closed 1000 ""
            --
            close 1000
            closed 1003 ""
            --
            close 1003
            closed 1007 ""
            --
            close 1007
            closed 1014 ""
            --
            close 1014
            closed 3000 ""
            --
            close 3000
            closed 4999 ""
            --
            close 4999
            |});
      test "a peer that does not obey the protocol" (fun () ->
          let x n = String.make n 'x' in
          let cases =
            [ ("reserved opcode", of_hex "83 80", None);
              ("unmasked frame", of_hex "81 00", None);
              ("frame too large", client Binary (x 200), Some 16);
              ("text too large", client Text (x 20), Some 16);
              ("fragments too large", client ~fin:false Binary (x 10) ^ client Continuation (x 10), Some 16);
              ( "fragments too large, before the data",
                client ~fin:false Binary (x 900) ^ String.sub (client Continuation (x 200)) 0 4,
                Some 1000 );
              ("continuation first", client Continuation "x", None);
              ("message in a message", client ~fin:false Text "a" ^ client Text "b", None);
              ("text not UTF-8", client Text "\xff", None);
              ("fragments not UTF-8", client ~fin:false Text "\xe2\x82" ^ client Continuation "x", None);
              ("close of 1 byte", client Close "\x03", None);
              ("close reason not UTF-8", client Close (code_reason 1000 "\xff"), None) ]
            @ List.map
              (fun code -> (Printf.sprintf "close code %d" code, client Close (code_reason code ""), None))
              [ 0; 999; 1004; 1005; 1006; 1015; 1016; 2999; 5000; 65535 ]
          in
          expect
            (String.concat "\n"
               (List.map
                  (fun (name, input, max_message) ->
                     let lines = String.split_on_char '\n' (converse ?max_message input echo) in
                     name ^ ": " ^ String.concat " | " lines)
                  cases))
          @@ __POS_OF__ {|
            reserved opcode: closed 1002 "reserved opcode 3" | -- | close 1002
            unmasked frame: closed 1002 "unmasked frame" | -- | close 1002
            frame too large: closed 1009 "message is too large" | -- | close 1009
            text too large: closed 1009 "message is too large" | -- | close 1009
            fragments too large: closed 1009 "message is too large" | -- | close 1009
            fragments too large, before the data: closed 1009 "message is too large" | -- | close 1009
            continuation first: closed 1002 "continuation frame without a message" | -- | close 1002
            message in a message: closed 1002 "new message inside a fragmented message" | -- | close 1002
            text not UTF-8: closed 1007 "text is not UTF-8" | -- | close 1007
            fragments not UTF-8: closed 1007 "text is not UTF-8" | -- | close 1007
            close of 1 byte: closed 1002 "close frame of 1 byte" | -- | close 1002
            close reason not UTF-8: closed 1007 "close reason is not UTF-8" | -- | close 1007
            close code 0: closed 1002 "close code 0" | -- | close 1002
            close code 999: closed 1002 "close code 999" | -- | close 1002
            close code 1004: closed 1002 "close code 1004" | -- | close 1002
            close code 1005: closed 1002 "close code 1005" | -- | close 1002
            close code 1006: closed 1002 "close code 1006" | -- | close 1002
            close code 1015: closed 1002 "close code 1015" | -- | close 1002
            close code 1016: closed 1002 "close code 1016" | -- | close 1002
            close code 2999: closed 1002 "close code 2999" | -- | close 1002
            close code 5000: closed 1002 "close code 5000" | -- | close 1002
            close code 65535: closed 1002 "close code 65535" | -- | close 1002
            |});
      test "control frames do not count in the size of a message" (fun () ->
          expect
            (converse ~max_message:16
               (client ~fin:false Binary (String.make 16 'x') ^ client Ping (String.make 100 'p')
                ^ client Continuation "")
               (fun note c -> match Ws.recv c with
                 | Binary s -> note (Printf.sprintf "binary of %d bytes" (String.length s))
                 | Text _ -> note "text"
                 | exception e -> note_closed note e))
          @@ __POS_OF__ {|
            binary of 16 bytes
            --
            pong "pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp"
            |});
      test "the size of a message counts all of its fragments" (fun () ->
          expect
            (converse ~max_message:10
               (client ~fin:false Binary "aaaa" ^ client ~fin:false Continuation "bbbb"
                ^ client Continuation "cccc")
               (fun note c -> match Ws.recv c with
                 | Binary s -> note (Printf.sprintf "binary of %d bytes" (String.length s))
                 | Text _ -> note "text"
                 | exception e -> note_closed note e))
          @@ __POS_OF__ {|
            closed 1009 "message is too large"
            --
            close 1009
            |});
      test "empty fragments do not make the heap grow" (fun () ->
          (* 300 000 empty Continuation frames, then the last fragment.
             The read function records the largest heap. *)
          let first = client ~fin:false Binary "x" and empty = client ~fin:false Continuation "" in
          let last = client Continuation "y" and count = 300_000 in
          let size = String.length empty in
          let body = count * size in
          let total = String.length first + body + String.length last in
          let pos = ref 0 and top = ref 0 in
          let byte i =
            if i < String.length first then first.[i]
            else if i < String.length first + body then empty.[(i - String.length first) mod size]
            else last.[i - String.length first - body]
          in
          let read buf off len =
            top := Int.max !top (Gc.quick_stat ()).heap_words;
            let n = Int.min len (total - !pos) in
            for i = 0 to n - 1 do Bytes.set buf (off + i) (byte (!pos + i)) done;
            pos := !pos + n;
            n
          in
          let ignore_deadline (_ : Affect_unix.Mtime.t option) = () in
          let flow =
            Caravan.Flow.make ~read ~read_now:(fun b o l -> Some (read b o l))
              ~write:(fun _ _ _ -> ()) ~readable:(Affect.Action.always ()) ~shutdown_send:ignore
              ~close:ignore ~set_read_deadline:ignore_deadline ~set_write_deadline:ignore_deadline ()
          in
          let start = (Gc.quick_stat ()).heap_words in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              is_true (Ws.recv (Ws.Private.of_flow flow) = Binary "xy"));
          let grew = (!top - start) * (Sys.word_size / 8) in
          is_true ~msg:(Printf.sprintf "the heap grew by %d bytes" grew) (grew < 4_000_000));
      test "the buffer of a fragmented message stays within max_message" (fun () ->
          (* What recv allocates for a message of [m] bytes in fragments of
             [sizes], more than for the same message in one frame. The
             expected message is made before the measure. *)
          let m = 1024 * 1024 in
          let expected = String.make m 'x' in
          let allocated frames =
            let flow = Caravan.Flow.of_string (String.concat "" frames) (Buffer.create 16) in
            let used = ref 0. in
            Affect_unix.Unix.main ~domain_count:1 (fun () ->
                let c = Ws.Private.of_flow ~max_message:m flow in
                let before = Gc.allocated_bytes () in
                let msg = Ws.recv c in
                used := Gc.allocated_bytes () -. before;
                is_true (msg = Binary expected));
            !used
          in
          let whole = allocated [ client Binary expected ] in
          let more sizes =
            let rec frames first = function
            | [] -> []
            | [ n ] -> [ client (if first then Binary else Continuation) (String.make n 'x') ]
            | n :: rest ->
                client ~fin:false (if first then Binary else Continuation) (String.make n 'x')
                :: frames false rest
            in
            (allocated (frames true sizes) -. whole) /. Float.of_int m
          in
          (* At most one buffer of [m] bytes, and the copy when it grows:
             1.50 m at this time. A buffer of 2 times the first fragment
             gave 2.00 m to 4.44 m. A full first fragment and an empty last
             fragment do not cause a copy. In the last three cases, the
             double size is more than [m]: without the limit, they give
             2.44 m to 3.00 m. *)
          List.iter
            (fun (sizes, limit) ->
               let r = more sizes in
               is_true
                 ~msg:(Printf.sprintf "%s: %.2f m" (String.concat " + " (List.map string_of_int sizes)) r)
                 (r < limit))
            [ ([ m; 0 ], 0.5); ([ (m / 2) - 1; (m / 2) + 1 ], 1.75); ([ 3; m - 3 ], 1.75);
              ([ m - 1; 1 ], 1.75); ([ (m / 2) + 1; (m / 2) - 1 ], 1.75);
              ([ 6 * m / 10; m / 10; m - (6 * m / 10) - (m / 10) ], 1.75) ]);
      test "a connection that stops inside a frame" (fun () ->
          expect (converse (String.sub (client Text "Hello") 0 4) echo) @@ __POS_OF__ {|
            closed 1006 ""
            --
            |});
      test "a closed connection raises Closed again" (fun () ->
          expect
            (converse (client Close (code_reason 1001 "away")) (fun note c ->
                 List.iter
                   (fun f -> try f () with e -> note_closed note e)
                   [ (fun () -> ignore (Ws.recv c)); (fun () -> ignore (Ws.recv c));
                     (fun () -> Ws.send c (Text "x")); (fun () -> Ws.ping c "") ]))
          @@ __POS_OF__ {|
            closed 1001 "away"
            closed 1001 "away"
            closed 1001 "away"
            closed 1001 "away"
            --
            close 1001
            |});
      test "send and ping examine their arguments" (fun () ->
          expect
            (converse "" (fun _ c ->
                 raises_match is_invalid_arg (fun () -> Ws.send c (Text "\xff"));
                 raises_match is_invalid_arg (fun () -> Ws.ping c (String.make 126 'p'));
                 Ws.ping c (String.make 125 'p');
                 Ws.send c (Binary "\xff")))
          @@ __POS_OF__ {|
            --
            ping "ppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp"
            binary "\255"
            |});
      prop "recv joins fragments and answers Pings"
        Gen.(
          list ~size:(int_range 0 4)
            (triple bool
               (list ~size:(int_range 0 6) (of_list [ "a"; "\xc3\xa9"; "\xe2\x82\xac"; "\xf0\x9d\x84\x9e"; "\x00" ]))
               (list ~size:(int_range 0 4) (int_range 0 5))))
        (fun messages ->
           (* Cut [s] in fragments of the given sizes and a last fragment. *)
           let rec cut s = function
           | [] -> [ s ]
           | n :: ns ->
               let n = Int.min n (String.length s) in
               String.sub s 0 n :: cut (String.sub s n (String.length s - n)) ns
           in
           let input = Buffer.create 64 and expected = ref [] and log = ref [] in
           List.iteri
             (fun i (text, pieces, sizes) ->
                let payload = String.concat "" pieces in
                let fragments = cut payload sizes in
                let last = List.length fragments - 1 in
                List.iteri
                  (fun j f ->
                     let op : Frame.opcode = if j > 0 then Continuation else if text then Text else Binary in
                     Buffer.add_string input (client ~fin:(j = last) op f);
                     if j < last && j mod 2 = 0
                     then begin
                       let p = Printf.sprintf "%d.%d" i j in
                       Buffer.add_string input (client Ping p);
                       expected := Printf.sprintf "pong %S" p :: !expected
                     end)
                  fragments;
                let m = if text then Printf.sprintf "text %S" payload else Printf.sprintf "binary %S" payload in
                log := m :: !log;
                expected := m :: !expected)
             messages;
           equal string
             (String.concat "\n" (List.rev !log @ [ "closed 1006 \"\""; "--" ] @ List.rev !expected))
             (converse (Buffer.contents input) echo)) ]

let close_tests =
  let try_note note f = try f () with e -> note_closed note e in
  group "Close"
    [ test "close sends a Close frame and reads until the reply" (fun () ->
          expect
            (converse
               (client Text "discarded" ^ client Ping "no pong"
                ^ client Close (code_reason 1000 "ok"))
               (fun note c ->
                  Ws.close ~code:1001 ~reason:"bye" c;
                  note "close ended";
                  try_note note (fun () -> ignore (Ws.recv c))))
          @@ __POS_OF__ {|
            close ended
            closed 1000 "ok"
            --
            close 1001 "bye"
            |});
      test "after close, close does nothing and send raises Closed" (fun () ->
          expect
            (converse (client Close "") (fun note c ->
                 Ws.close c;
                 Ws.close ~code:3000 c;
                 try_note note (fun () -> Ws.send c (Text "x"));
                 try_note note (fun () -> Ws.ping c "")))
          @@ __POS_OF__ {|
            closed 1005 ""
            closed 1005 ""
            --
            close 1000
            |});
      test "a peer that goes before its reply" (fun () ->
          expect
            (converse "" (fun note c ->
                 Ws.close c;
                 try_note note (fun () -> ignore (Ws.recv c))))
          @@ __POS_OF__ {|
            closed 1006 ""
            --
            close 1000
            |});
      test "close stops at the time limit if the peer sends without a stop" (fun () ->
          (* A flow that always has bytes: a read never waits for its
             deadline. The peer sends 100 000 000 frames, then ends. A read
             of all of them takes many seconds. *)
          let frame = client Binary "" and count = 100_000_000 in
          let size = String.length frame in
          let total = count * size and pos = ref 0 in
          let read buf off len =
            let n = Int.min len (total - !pos) in
            for i = 0 to n - 1 do Bytes.set buf (off + i) frame.[(!pos + i) mod size] done;
            pos := !pos + n;
            n
          in
          let ignore_deadline (_ : Affect_unix.Mtime.t option) = () in
          let flow =
            Caravan.Flow.make ~read ~read_now:(fun b o l -> Some (read b o l))
              ~write:(fun _ _ _ -> ()) ~readable:(Affect.Action.always ()) ~shutdown_send:ignore
              ~close:ignore ~set_read_deadline:ignore_deadline ~set_write_deadline:ignore_deadline ()
          in
          let t = Affect_unix.Mtime.counter () in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              Ws.close (Ws.Private.of_flow ~close_timeout:Affect_unix.Mtime.Span.(50 * ms) flow));
          let took = Affect_unix.Mtime.Span.to_float_ns (Affect_unix.Mtime.count t) /. 1e6 in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took < 2000.);
          is_true ~msg:(Printf.sprintf "read %d of %d bytes" !pos total) (!pos < total));
      test "close examines its code and its reason" (fun () ->
          (* The largest reason: 123 bytes of UTF-8. *)
          let reason = String.make 120 'x' ^ "\xe2\x82\xac" in
          equal string
            (Printf.sprintf "--\nclose 4999 %S" reason)
            (converse "" (fun _ c ->
                 List.iter
                   (fun (code, reason) ->
                      raises_match is_invalid_arg (fun () -> Ws.close ~code ~reason c))
                   [ (999, ""); (1004, ""); (1005, ""); (1006, ""); (1015, ""); (2999, "");
                     (5000, ""); (1000, reason ^ "x"); (1000, "\xff") ];
                 Ws.close ~code:4999 ~reason c))) ]

(* Runs [f] with a server connection and the flow of its peer, on a TCP
   connection. *)
let with_socket ?close_timeout ?write_timeout ?read_timeout ?idle_timeout f =
  let open Affect in
  Affect_unix.Unix.main ~domain_count:Server_support.domain_count @@ fun () ->
  let l = Caravan.Tcp.Listener.listen Server_support.loopback in
  Stdlib.Fun.protect ~finally:(fun () -> Caravan.Tcp.Listener.close l) @@ fun () ->
  let peer = Fun.Async.call (fun () -> Caravan.Tcp.connect (Caravan.Tcp.Listener.addr l)) in
  let fd, _ = Caravan.Tcp.Listener.accept l in
  let server = Caravan.Tcp.flow fd and peer = Fun.Async.get peer in
  Stdlib.Fun.protect ~finally:(fun () -> Caravan.Flow.close peer; Caravan.Flow.close server)
  @@ fun () ->
  f (Ws.Private.of_flow ?close_timeout ?write_timeout ?read_timeout ?idle_timeout server) peer

let ms n = Affect_unix.Mtime.Span.(n * ms)
let elapsed_ms c = Affect_unix.Mtime.Span.to_float_ns (Affect_unix.Mtime.count c) /. 1e6

(* The code of the [Closed] exception that [f] raises. *)
let closed_code f =
  match f () with
  | _ -> fail "no Closed exception"
  | exception Ws.Closed { code; _ } -> code

let socket_tests =
  group "Connection on a socket"
    [ test ~timeout:30. "fibers that send at the same time do not mix their frames" (fun () ->
          let open Affect in
          with_socket @@ fun c peer ->
          (* Some messages are larger than a socket buffer: they need many
             writes. *)
          let body name i = String.make (if i mod 50 = 0 then 100_000 else i) name in
          let sender name =
            Fun.Async.call (fun () ->
                for i = 1 to 200 do
                  Ws.send c (Binary (Printf.sprintf "%c %d %s" name i (body name i)))
                done)
          in
          let a = sender 'a' and b = sender 'b' in
          let r = Buf_reader.of_flow peer and last = Hashtbl.create 2 in
          for _ = 1 to 400 do
            let f = Frame.read r ~masked:false ~max:max_int in
            match String.split_on_char ' ' f.payload with
            | [ name; i; data ] ->
                let i = int_of_string i and prev = Option.value ~default:0 (Hashtbl.find_opt last name) in
                equal int ~msg:name (prev + 1) i;
                is_true ~msg:"intact" (data = body name.[0] i);
                Hashtbl.replace last name i
            | _ -> fail "a message with incorrect content"
          done;
          Fun.Async.get a;
          Fun.Async.get b);
      test "close stops at the time limit if the peer does not reply" (fun () ->
          with_socket ~close_timeout:(ms 50) @@ fun c peer ->
          let t = Affect_unix.Mtime.counter () in
          Ws.close c;
          let took = elapsed_ms t in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took >= 40. && took < 2000.);
          equal string "close 1000" (show_frame (Frame.read (Buf_reader.of_flow peer) ~masked:false ~max:125));
          equal int 1006 (closed_code (fun () -> Ws.recv c)));
      test "a fiber in recv reads the reply to close" (fun () ->
          let open Affect in
          with_socket @@ fun c peer ->
          let reader = Fun.Async.call (fun () -> closed_code (fun () -> Ws.recv c)) in
          Affect_unix.Mtime.wait_for (ms 20);
          let closer = Fun.Async.call (fun () -> Ws.close ~code:1001 c) in
          equal string "close 1001" (show_frame (Frame.read (Buf_reader.of_flow peer) ~masked:false ~max:125));
          Caravan.Flow.write_string peer (client Close (code_reason 1001 ""));
          Fun.Async.get closer;
          equal int 1001 (Fun.Async.get reader));
      test "a fiber in recv stops at the time limit of close" (fun () ->
          let open Affect in
          with_socket ~close_timeout:(ms 50) @@ fun c _peer ->
          let reader = Fun.Async.call (fun () -> closed_code (fun () -> Ws.recv c)) in
          Affect_unix.Mtime.wait_for (ms 20);
          let t = Affect_unix.Mtime.counter () in
          Ws.close c;
          let took = elapsed_ms t in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took >= 40. && took < 2000.);
          equal int 1006 (Fun.Async.get reader));
      test "a fiber blocked in send stops at the time limit of close" (fun () ->
          let open Affect in
          with_socket ~close_timeout:(ms 50) @@ fun c _peer ->
          (* The peer reads nothing: the sends fill the socket buffers. *)
          let sender =
            Fun.Async.call (fun () ->
                closed_code (fun () ->
                    while true do Ws.send c (Binary (String.make 1_000_000 'x')) done))
          in
          Affect_unix.Mtime.wait_for (ms 50);
          let t = Affect_unix.Mtime.counter () in
          Ws.close c;
          let took = elapsed_ms t in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took >= 40. && took < 2000.);
          equal int 1006 (Fun.Async.get sender));
      test "a send that waits for a different send raises Closed after close" (fun () ->
          let open Affect in
          with_socket ~close_timeout:(ms 2000) @@ fun c peer ->
          (* The peer reads nothing: [big] fills the socket buffers. *)
          let big = Fun.Async.call (fun () -> Ws.send c (Binary (String.make 4_000_000 'x'))) in
          Affect_unix.Mtime.wait_for (ms 50);
          let late = Fun.Async.call (fun () -> closed_code (fun () -> Ws.send c (Text "late"))) in
          Affect_unix.Mtime.wait_for (ms 20);
          let closer = Fun.Async.call (fun () -> Ws.close c) in
          Affect_unix.Mtime.wait_for (ms 20);
          let r = Buf_reader.of_flow peer in
          let next () = Frame.read r ~masked:false ~max:max_int in
          equal int 4_000_000 (String.length (next ()).payload);
          equal string "close 1000" (show_frame (next ()));
          Caravan.Flow.write_string peer (client Close (code_reason 1000 ""));
          Fun.Async.get big;
          equal int 1000 (Fun.Async.get late);
          Fun.Async.get closer);
      test "a send to a peer that reads nothing stops at the write timeout" (fun () ->
          let open Affect in
          with_socket ~write_timeout:(ms 50) @@ fun c _peer ->
          let t = Affect_unix.Mtime.counter () in
          let send () = Ws.send c (Binary (String.make 10_000_000 'x')) in
          let sender = Fun.Async.call (fun () -> closed_code send) in
          let ended = Action.map (fun _ -> `Ended) (Fun.Async.get' sender) in
          (match Action.invoke (Action.choose [ ended; Affect_unix.Mtime.wait_for' (ms 2000) `Late ]) with
          | `Ended -> equal int 1006 (Fun.Async.get sender)
          | `Late -> Fun.Async.cancel sender; fail "the send did not stop");
          let took = elapsed_ms t in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took >= 40. && took < 2000.));
      test "a peer that sends no frame in the idle timeout gets Close 1001" (fun () ->
          with_socket ~idle_timeout:(ms 50) @@ fun c peer ->
          (* The connection does not wait for the reply of the peer. *)
          let t = Affect_unix.Mtime.counter () in
          (match Ws.recv c with
          | _ -> fail "a message"
          | exception Ws.Closed { code; reason } ->
              equal int 1001 code;
              equal string "idle timeout" reason);
          let took = elapsed_ms t in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took >= 40. && took < 1000.);
          equal string "close 1001" (show_frame (Frame.read (Buf_reader.of_flow peer) ~masked:false ~max:125)));
      test "a frame that stops in the middle gets Close 1001 at the read timeout" (fun () ->
          let open Affect in
          with_socket ~read_timeout:(ms 50) ~close_timeout:(ms 50) @@ fun c peer ->
          Caravan.Flow.write_string peer (String.sub (client Text "hello") 0 3);
          let reader = Fun.Async.call (fun () -> Ws.recv c) in
          let ended = Action.map (fun _ -> `Ended) (Fun.Async.get' reader) in
          (match Action.invoke (Action.choose [ ended; Affect_unix.Mtime.wait_for' (ms 2000) `Late ]) with
          | `Late -> Fun.Async.cancel reader; fail "no timeout after 2 s"
          | `Ended -> fail "a message"
          | exception Ws.Closed { code; reason } ->
              equal int 1001 code;
              equal string "read timeout" reason);
          equal string "close 1001" (show_frame (Frame.read (Buf_reader.of_flow peer) ~masked:false ~max:125)));
      test "a slow frame with no long pause gets no timeout" (fun () ->
          let open Affect in
          with_socket ~read_timeout:(ms 200) ~idle_timeout:(ms 200) @@ fun c peer ->
          (* 2 006 bytes, 100 bytes each 20 ms: about 420 ms in all. *)
          let frame = client Binary (String.make 2000 'x') in
          let writer =
            Fun.Async.call (fun () ->
                let rec loop off =
                  if off < String.length frame
                  then begin
                    let n = Int.min 100 (String.length frame - off) in
                    Caravan.Flow.write_string peer (String.sub frame off n);
                    Affect_unix.Mtime.wait_for (ms 20);
                    loop (off + n)
                  end
                in
                loop 0)
          in
          is_true (Ws.recv c = Binary (String.make 2000 'x'));
          Fun.Async.get writer);
      test "the read timeout does not apply between frames" (fun () ->
          with_socket ~read_timeout:(ms 50) @@ fun c peer ->
          (* A read inside the first frame sets the read timeout. *)
          let frame = client Text "one" in
          Caravan.Flow.write_string peer (String.sub frame 0 2);
          let first = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          Affect_unix.Mtime.wait_for (ms 20);
          Caravan.Flow.write_string peer (String.sub frame 2 (String.length frame - 2));
          is_true (Affect.Fun.Async.get first = Text "one");
          (* recv waits for 150 ms, more than the read timeout. *)
          let second = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          Affect_unix.Mtime.wait_for (ms 150);
          Caravan.Flow.write_string peer (client Text "two");
          is_true (Affect.Fun.Async.get second = Text "two"));
      test "each frame gets a new idle timeout" (fun () ->
          let open Affect in
          with_socket ~idle_timeout:(ms 100) @@ fun c peer ->
          let writer =
            Fun.Async.call (fun () ->
                for _ = 1 to 5 do
                  Affect_unix.Mtime.wait_for (ms 40);
                  Caravan.Flow.write_string peer (client Ping "")
                done;
                Caravan.Flow.write_string peer (client Text "end"))
          in
          is_true (Ws.recv c = Text "end");
          Fun.Async.get writer);
      test "close stops at a shorter idle timeout" (fun () ->
          with_socket ~close_timeout:Affect_unix.Mtime.Span.(10 * s) ~idle_timeout:(ms 50)
          @@ fun c _peer ->
          let t = Affect_unix.Mtime.counter () in
          Ws.close c;
          let took = elapsed_ms t in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took >= 40. && took < 2000.));
      test "close keeps the shorter write timeout of a send that waits" (fun () ->
          let open Affect in
          with_socket ~close_timeout:Affect_unix.Mtime.Span.(10 * s) ~write_timeout:(ms 100)
          @@ fun c peer ->
          (* The peer reads only the first bytes: the send waits until its
             write timeout. When the first bytes come, the send is in its
             frame. *)
          let t = Affect_unix.Mtime.counter () in
          let sender =
            Fun.Async.call (fun () -> closed_code (fun () -> Ws.send c (Binary (String.make 10_000_000 'x'))))
          in
          is_false (Buf_reader.peek_eof (Buf_reader.of_flow peer));
          let closer = Fun.Async.call (fun () -> Ws.close c) in
          equal int 1006 (Fun.Async.get sender);
          let took = elapsed_ms t in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took >= 90. && took < 2000.);
          Fun.Async.get closer);
      test "close keeps its time limit with a long idle timeout" (fun () ->
          with_socket ~close_timeout:(ms 50) ~idle_timeout:Affect_unix.Mtime.Span.(10 * s)
          @@ fun c _peer ->
          let t = Affect_unix.Mtime.counter () in
          Ws.close c;
          let took = elapsed_ms t in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took >= 40. && took < 2000.));
      test "after a protocol error, the peer gets the Close frame and the end of the stream"
        (fun () ->
           let open Affect in
           with_socket @@ fun c peer ->
           let reader = Fun.Async.call (fun () -> closed_code (fun () -> Ws.recv c)) in
           (* A frame of 20 MiB, over the limit of 8 MiB: the server reads
              only its header, and 1 MiB stays unread. *)
           let header = of_hex "82ff" ^ "\x00\x00\x00\x00\x01\x40\x00\x00" ^ "\x01\x02\x03\x04" in
           Caravan.Flow.write_string peer (header ^ String.make 1_000_000 'x');
           let r = Buf_reader.of_flow peer in
           equal string "close 1009" (show_frame (Frame.read r ~masked:false ~max:125));
           raises End_of_file (fun () -> Frame.read r ~masked:false ~max:125);
           Caravan.Flow.close peer;
           equal int 1009 (Fun.Async.get reader));
      test "a cancel in the linger after a protocol error closes the connection" (fun () ->
          let open Affect in
          with_socket ~close_timeout:Affect_unix.Mtime.Span.(10 * s) @@ fun c peer ->
          (* The peer does not close: the linger waits. *)
          let reader = Fun.Async.call (fun () -> Ws.recv c) in
          Caravan.Flow.write_string peer (client Continuation "x");
          Affect_unix.Mtime.wait_for (ms 50);
          Fun.Async.cancel reader;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get reader);
          let t = Affect_unix.Mtime.counter () in
          equal int 1002 (closed_code (fun () -> Ws.recv c));
          let took = elapsed_ms t in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took < 2000.));
      test "the linger after a protocol error discards at most 1 MiB" (fun () ->
          let open Affect in
          with_socket @@ fun c peer ->
          let t = Affect_unix.Mtime.counter () in
          let reader = Fun.Async.call (fun () -> closed_code (fun () -> Ws.recv c)) in
          (* The peer sends 3 MiB after a frame that is too large, and does
             not close. The server stops before its close timeout of 5 s. *)
          let header = of_hex "82ff" ^ "\x00\x00\x00\x00\x01\x40\x00\x00" ^ "\x01\x02\x03\x04" in
          let writer =
            Fun.Async.call (fun () ->
                try Caravan.Flow.write_string peer (header ^ String.make (3 * 1024 * 1024) 'x')
                with Caravan.Flow.Closed -> ())
          in
          equal int 1009 (Fun.Async.get reader);
          let took = elapsed_ms t in
          is_true ~msg:(Printf.sprintf "took %.0fms" took) (took < 2000.);
          Caravan.Flow.close peer;
          Fun.Async.get writer);
      (* A cancel in the middle of a frame closes the connection, because
         the stream is out of step. A cancel between messages does not. *)
      test "a cancel in the middle of a send closes with 1006" (fun () ->
          let open Affect in
          with_socket @@ fun c peer ->
          let sender = Fun.Async.call (fun () -> Ws.send c (Binary (String.make 10_000_000 'x'))) in
          (* When the first bytes come, the send is in its frame. *)
          let r = Buf_reader.of_flow peer in
          is_false (Buf_reader.peek_eof r);
          Fun.Async.cancel sender;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get sender);
          equal int 1006 (closed_code (fun () -> Ws.send c (Text "hello")));
          raises End_of_file (fun () -> Frame.read r ~masked:false ~max:max_int));
      test "a cancel between messages does not close" (fun () ->
          let open Affect in
          with_socket @@ fun c peer ->
          let reader = Fun.Async.call (fun () -> Ws.recv c) in
          Affect_unix.Mtime.wait_for (ms 20);
          Fun.Async.cancel reader;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get reader);
          Caravan.Flow.write_string peer (client Text "hello");
          is_true (Ws.recv c = Text "hello"));
      test "a cancel in the middle of a received frame closes with 1006" (fun () ->
          let open Affect in
          with_socket @@ fun c peer ->
          let reader = Fun.Async.call (fun () -> Ws.recv c) in
          let frame = client Text "hello" in
          Caravan.Flow.write_string peer (String.sub frame 0 3);
          Affect_unix.Mtime.wait_for (ms 20);
          Fun.Async.cancel reader;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get reader);
          Caravan.Flow.write_string peer (String.sub frame 3 (String.length frame - 3) ^ client Text "next");
          equal int 1006 (closed_code (fun () -> Ws.recv c)));
      test "a cancel in the wait to send a Pong inside a fragmented message closes with 1006"
        (fun () ->
           let open Affect in
           (* Without the fix, the next recv waits for the write timeout of
              [big]: 1 s. *)
           with_socket ~write_timeout:Affect_unix.Mtime.Span.(1 * s) @@ fun c peer ->
           (* The peer reads nothing: [big] keeps the writer semaphore. *)
           let big =
             Fun.Async.call (fun () ->
                 try Ws.send c (Binary (String.make 10_000_000 'x')) with Ws.Closed _ -> ())
           in
           Affect_unix.Mtime.wait_for (ms 50);
           let reader = Fun.Async.call (fun () -> Ws.recv c) in
           Caravan.Flow.write_string peer (client ~fin:false Text "Hel" ^ client Ping "p");
           Affect_unix.Mtime.wait_for (ms 20);
           Fun.Async.cancel reader;
           raises Fun.Async.Cancelled (fun () -> Fun.Async.get reader);
           Caravan.Flow.write_string peer (client Continuation "lo");
           let t = Affect_unix.Mtime.counter () in
           equal int 1006 (closed_code (fun () -> Ws.recv c));
           let took = elapsed_ms t in
           is_true ~msg:(Printf.sprintf "took %.0fms" took) (took < 500.);
           Fun.Async.get big);
      test "a cancel inside a fragmented message closes with 1006" (fun () ->
          let open Affect in
          with_socket @@ fun c peer ->
          let reader = Fun.Async.call (fun () -> Ws.recv c) in
          Caravan.Flow.write_string peer (client ~fin:false Text "Hel");
          Affect_unix.Mtime.wait_for (ms 20);
          Fun.Async.cancel reader;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get reader);
          Caravan.Flow.write_string peer (client Continuation "lo");
          equal int 1006 (closed_code (fun () -> Ws.recv c))) ]

(* The fields of the RFC 6455 §1.3 example, without [Origin]. *)
let request_fields =
  [ ("Host", "server.example.com"); ("Upgrade", "websocket"); ("Connection", "Upgrade");
    ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ=="); ("Sec-WebSocket-Version", "13") ]

(* A request with [request_fields], where [fields] replace the fields of the
   same names and add the others. [""] removes a field. *)
let request ?(meth = Caravan.Method.GET) ?(version = Caravan.Version.Http_1_1)
    ?(authority = "server.example.com") fields =
  let set h (name, value) =
    if value = "" then Caravan.Headers.remove name h else Caravan.Headers.set name value h
  in
  let headers = List.fold_left set (Caravan.Headers.of_list request_fields) fields in
  Caravan.Request.make ~version ~authority ~headers meth "/chat"

(* The status and the fields of the response to [req], and its body. *)
let answer ?protocols ?origin req =
  let r = Ws.accept ?protocols ?origin req (fun _ -> ()) in
  let body =
    match Caravan.Response.kind r with
    | Body b -> Caravan.Body.to_string b
    | Upgrade _ -> ""
  in
  String.trim (Format.asprintf "%a@.%s" Caravan.Response.pp r body)

let handshake_tests =
  group "Handshake"
    [ test "accept_key answers the RFC 6455 example" (fun () ->
          equal string "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
            (Caravan_websocket.accept_key "dGhlIHNhbXBsZSBub25jZQ=="));
      test "accept answers a correct request with 101" (fun () ->
          expect (answer (request [])) @@ __POS_OF__ {|
            101 Switching Protocols (upgrade)
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            |};
          (* Tokens are case-insensitive, and a list can hold them. *)
          expect
            (answer
               (request [ ("Upgrade", "WebSocket"); ("Connection", "keep-alive, upgrade") ]))
          @@ __POS_OF__ {|
            101 Switching Protocols (upgrade)
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            |});
      test "accept refuses an incorrect request" (fun () ->
          let first_line s = List.hd (String.split_on_char '\n' s) in
          let cases =
            [ ("POST", request ~meth:Caravan.Method.POST []);
              ("HTTP/1.0", request ~version:Caravan.Version.Http_1_0 []);
              ("no Upgrade", request [ ("Upgrade", "") ]);
              ("Upgrade: h2c", request [ ("Upgrade", "h2c") ]);
              ("no Connection token", request [ ("Connection", "keep-alive") ]);
              ("no key", request [ ("Sec-WebSocket-Key", "") ]);
              ("key of 5 bytes", request [ ("Sec-WebSocket-Key", "aGVsbG8=") ]);
              ("key not base64", request [ ("Sec-WebSocket-Key", "not base64 at all!!!") ]) ]
          in
          expect
            (String.concat "\n"
               (List.map
                  (fun (name, req) ->
                     let a = answer req in
                     name ^ ": " ^ first_line a ^ " | "
                     ^ List.nth (String.split_on_char '\n' a) 2)
                  cases))
          @@ __POS_OF__ {|
            POST: 400 Bad Request | WebSocket handshake: the method is not GET
            HTTP/1.0: 400 Bad Request | WebSocket handshake: the version is not HTTP/1.1
            no Upgrade: 400 Bad Request | WebSocket handshake: no Upgrade: websocket
            Upgrade: h2c: 400 Bad Request | WebSocket handshake: no Upgrade: websocket
            no Connection token: 400 Bad Request | WebSocket handshake: no Connection: upgrade
            no key: 400 Bad Request | WebSocket handshake: no Sec-WebSocket-Key
            key of 5 bytes: 400 Bad Request | WebSocket handshake: the key is not 16 bytes in base64
            key not base64: 400 Bad Request | WebSocket handshake: the key is not 16 bytes in base64
            |});
      test "a negative max_message is not permitted" (fun () ->
          raises_match is_invalid_arg (fun () -> Ws.accept ~max_message:(-1) (request []) ignore);
          raises_match is_invalid_arg (fun () ->
              Ws.Private.of_flow ~max_message:(-1) (Caravan.Flow.of_string "" (Buffer.create 16)));
          ignore (Ws.accept ~max_message:0 (request []) ignore);
          ignore (Ws.Private.of_flow ~max_message:0 (Caravan.Flow.of_string "" (Buffer.create 16))));
      test "accept asks for version 13" (fun () ->
          expect (answer (request [ ("Sec-WebSocket-Version", "8") ])) @@ __POS_OF__ {|
            426 Upgrade Required
            Sec-WebSocket-Version: 13
            Content-Type: text/plain; charset=utf-8
            WebSocket handshake: the version must be 13
            |};
          expect (answer (request [ ("Sec-WebSocket-Version", "") ])) @@ __POS_OF__ {|
            426 Upgrade Required
            Sec-WebSocket-Version: 13
            Content-Type: text/plain; charset=utf-8
            WebSocket handshake: the version must be 13
            |});
      test "accept permits only the origin of the request by default" (fun () ->
          let status ?origin ?authority ?scheme o =
            let req = request ?authority [ ("Origin", o) ] in
            let req =
              match scheme with
              | None -> req
              | Some scheme ->
                  Caravan.Request.make ~scheme ?authority:(Caravan.Request.authority req)
                    ~headers:(Caravan.Request.headers req) Caravan.Method.GET "/chat"
            in
            List.hd (String.split_on_char ' ' (answer ?origin req))
          in
          expect
            (String.concat " "
               [ status "http://server.example.com"; status "https://SERVER.example.com";
                 status "http://server.example.com:8080"; status "http://evil.example.com";
                 status "null"; status "server.example.com"; status "http:"; status "http:/";
                 status "http:/xserver.example.com";
                 status ~authority:"server.example.com:8080" "http://server.example.com:8080";
                 status ~origin:(fun o -> o = "null") "null";
                 status ~origin:(fun _ -> false) "http://server.example.com" ])
          @@ __POS_OF__ {|101 101 403 403 403 403 403 403 403 101 101 403|};
          (* Default ports, and the scheme of the request. *)
          expect
            (String.concat " "
               [ status ~authority:"server.example.com:443" "https://server.example.com";
                 status "https://server.example.com:443"; status "http://server.example.com:80";
                 status ~authority:"server.example.com:80" "http://server.example.com";
                 status ~authority:"server.example.com:443" "http://server.example.com";
                 status ~authority:"server.example.com:80" "https://server.example.com";
                 status "ftp://server.example.com";
                 status ~scheme:"https" "https://server.example.com";
                 status ~scheme:"https" "http://server.example.com" ])
          @@ __POS_OF__ {|101 101 101 101 403 403 403 101 403|});
      test "accept chooses the first subprotocol of the server that the client offers"
        (fun () ->
           let chosen ?protocols offered =
             let a = answer ?protocols (request [ ("Sec-WebSocket-Protocol", offered) ]) in
             match
               List.find_opt
                 (String.starts_with ~prefix:"Sec-WebSocket-Protocol")
                 (String.split_on_char '\n' a)
             with
             | Some line -> line
             | None -> "none"
           in
           expect
             (String.concat "\n"
                [ chosen ~protocols:[ "superchat"; "chat" ] "chat, superchat";
                  chosen ~protocols:[ "chat" ] "Chat, superchat"; chosen ~protocols:[ "mqtt" ] "chat";
                  chosen ~protocols:[ "chat" ] ""; chosen "chat" ])
           @@ __POS_OF__ {|
             Sec-WebSocket-Protocol: superchat
             none
             none
             none
             none
             |}) ]

(* What recv raised in the handler of "/record". *)
let recorded = Atomic.make None

let server_handler req =
  match Caravan.Request.path_only req with
  | "/echo" ->
      Ws.accept req (fun c ->
          let rec loop () =
            Ws.send c (Ws.recv c);
            loop ()
          in
          loop ())
  | "/protocol" ->
      Ws.accept ~protocols:[ "chat"; "echo" ] req (fun c ->
          Ws.send c (Text (Option.value ~default:"none" (Ws.protocol c))))
  | "/boom" -> Ws.accept req (fun _ -> failwith "boom")
  | "/record" ->
      Ws.accept req (fun c ->
          match Ws.recv c with
          | _ -> ()
          | exception e ->
              Atomic.set recorded (Some (Printexc.to_string e));
              raise e)
  | "/other" ->
      (* A Closed of a different connection is an exception of the
         handler. *)
      Ws.accept req (fun _ ->
          let input = client Close (code_reason 1000 "") in
          let other = Ws.Private.of_flow (Caravan.Flow.of_string input (Buffer.create 16)) in
          ignore (Ws.recv other))
  | "/child" ->
      (* The Cancelled of a child call is not a stop of the server. *)
      Ws.accept req (fun _ ->
          let open Affect in
          let child = Fun.Async.call (fun () -> Affect_unix.Mtime.wait_for (ms 10_000)) in
          Fun.Async.cancel child;
          Fun.Async.get child)
  | _ -> Caravan.Handler.not_found req

(* A WebSocket session with a server: the request for [path], with [first]
   in the same write; then [after], then [during ()]. The result is the
   response head, a "--" line, and the frames from the server until it
   closes the connection. If [reply], we answer a Close frame of the
   server. *)
let upgrade_request ?(first = "") path =
  String.concat "\r\n"
    [ "GET " ^ path ^ " HTTP/1.1"; "Host: x"; "Upgrade: websocket"; "Connection: Upgrade";
      "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ=="; "Sec-WebSocket-Version: 13";
      "Sec-WebSocket-Protocol: echo"; ""; first ]

let response_head r =
  let rec head acc =
    match Buf_reader.read_line r ~max:1000 with "" -> List.rev acc | l -> head (l :: acc)
  in
  head []

let session ?first ?(after = "") ?(during = ignore) ?(reply = true) server path =
  let flow = Caravan.Tcp.connect (Caravan.Server.local_addr server) in
  Stdlib.Fun.protect ~finally:(fun () -> Caravan.Flow.close flow) @@ fun () ->
  Caravan.Flow.write_string flow (upgrade_request ?first path);
  let r = Buf_reader.of_flow flow in
  let head = response_head r in
  Caravan.Flow.write_string flow after;
  during ();
  let rec frames acc =
    match Frame.read r ~masked:false ~max:max_int with
    | exception End_of_file -> List.rev acc
    | f ->
        if reply && f.opcode = Close then Caravan.Flow.write_string flow (client Close f.payload);
        frames (show_frame f :: acc)
  in
  String.concat "\n" (head @ ("--" :: frames []))

let server_tests =
  group "Server"
    [ test "an echo, with a first frame in the write of the request" (fun () ->
          expect
            (Server_support.with_server ~handler:server_handler (fun server ->
                 session server "/echo" ~first:(client Text "early") ~reply:false
                   ~after:
                     (client Text "hi" ^ client Binary "\x01"
                      ^ client Close (code_reason 1000 "bye"))))
          @@ __POS_OF__ {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            text "early"
            text "hi"
            binary "\001"
            close 1000
            |});
      test "a handler that ends closes with 1000" (fun () ->
          expect
            (Server_support.with_server ~handler:server_handler (fun server ->
                 session server "/protocol"))
          @@ __POS_OF__ {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Sec-WebSocket-Protocol: echo
            Connection: Upgrade
            --
            text "echo"
            close 1000
            |});
      test "a handler that raises closes with 1011, and the server reports it" (fun () ->
          let errors = ref [] in
          let config =
            { Server_support.config with
              on_error = (fun e _ -> errors := Printexc.to_string e :: !errors) }
          in
          expect
            (Server_support.with_server ~config ~handler:server_handler (fun server ->
                 session server "/boom"))
          @@ __POS_OF__ {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1011
            |};
          equal (list string) [ "Failure(\"boom\")" ] !errors);
      test "a Cancelled from a child call closes with 1011, and the server reports it" (fun () ->
          let errors = ref [] in
          let config =
            { Server_support.config with
              on_error = (fun e _ -> errors := Printexc.to_string e :: !errors) }
          in
          expect
            (Server_support.with_server ~config ~handler:server_handler (fun server ->
                 session server "/child"))
          @@ __POS_OF__ {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1011
            |};
          equal (list string)
            [ "Failure(\"Caravan_websocket: a call of the handler was cancelled\")" ]
            !errors);
      test "a Closed of a different connection closes with 1011, and the server reports it"
        (fun () ->
           let errors = ref [] in
           let config =
             { Server_support.config with
               on_error = (fun e _ -> errors := Printexc.to_string e :: !errors) }
           in
           expect
             (Server_support.with_server ~config ~handler:server_handler (fun server ->
                  session server "/other"))
           @@ __POS_OF__ {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1011
            |};
           equal int 1 (List.length !errors));
      test "a peer that leaves without a Close frame is not an error" (fun () ->
          let errors = ref [] in
          let config =
            { Server_support.config with
              on_error = (fun e _ -> errors := Printexc.to_string e :: !errors) }
          in
          Atomic.set recorded None;
          Server_support.with_server ~config ~handler:server_handler (fun server ->
              let flow = Caravan.Tcp.connect (Caravan.Server.local_addr server) in
              Caravan.Flow.write_string flow (upgrade_request "/record");
              equal string "HTTP/1.1 101 Switching Protocols"
                (List.hd (response_head (Buf_reader.of_flow flow)));
              Caravan.Flow.close flow;
              (* Waits at most 2 s for the handler to get Closed 1006. Then
                 the server stops: it waits for the end of the handler. *)
              let rec wait n =
                if Atomic.get recorded = None && n > 0
                then (Affect_unix.Mtime.wait_for (ms 10); wait (n - 1))
              in
              wait 200);
          equal (option string)
            (Some "Caravan_websocket__Connection.Closed(1006, \"\")")
            (Atomic.get recorded);
          equal (list string) [] !errors);
      test "a server that stops cancels the handler without a Close frame" (fun () ->
          let errors = ref [] in
          let config =
            { Server_support.config with
              shutdown_grace = ms 50;
              on_error = (fun e _ -> errors := Printexc.to_string e :: !errors) }
          in
          expect
            (Server_support.with_server ~config ~handler:server_handler (fun server ->
                 session server "/echo" ~during:(fun () -> Caravan.Server.shutdown server)))
          @@ __POS_OF__ {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            |};
          equal (list string) [] !errors) ]

let () = exit (run "websocket" [ sha1_tests; base64_tests; frame_tests; connection_tests; close_tests; socket_tests; handshake_tests; server_tests ])
