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
          expect (outcome ~masked:false ~max:9 input) @@ __POS_OF__ {|too big|});
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

(* The frames in [out], one for each line. A Close frame shows its code. *)
let frames out =
  let r = Buf_reader.of_string out in
  let show (f : Frame.t) =
    match f.opcode with
    | Close when f.payload = "" -> "close"
    | Close -> Printf.sprintf "close %d" (String.get_uint16_be f.payload 0)
    | _ -> Format.asprintf "%a" pp_frame f
  in
  let rec loop acc =
    match Frame.read r ~masked:false ~max:max_int with
    | f -> loop (show f :: acc)
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

let socket_tests =
  group "Connection on a socket"
    [ test ~timeout:30. "fibers that send at the same time do not mix their frames" (fun () ->
          let open Affect in
          Affect_unix.Unix.main ~domain_count:Server_support.domain_count @@ fun () ->
          let l = Caravan.Tcp.Listener.listen Server_support.loopback in
          Stdlib.Fun.protect ~finally:(fun () -> Caravan.Tcp.Listener.close l) @@ fun () ->
          let client = Fun.Async.call (fun () -> Caravan.Tcp.connect (Caravan.Tcp.Listener.addr l)) in
          let fd, _ = Caravan.Tcp.Listener.accept l in
          let server = Caravan.Tcp.flow fd and client = Fun.Async.get client in
          Stdlib.Fun.protect ~finally:(fun () -> Caravan.Flow.close client; Caravan.Flow.close server)
          @@ fun () ->
          let c = Ws.Private.of_flow server in
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
          let r = Buf_reader.of_flow client and last = Hashtbl.create 2 in
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
          Fun.Async.get b) ]

let handshake_tests =
  group "Handshake"
    [ test "accept_key answers the RFC 6455 example" (fun () ->
          equal string "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
            (Caravan_websocket.accept_key "dGhlIHNhbXBsZSBub25jZQ==")) ]

let () = exit (run "websocket" [ sha1_tests; base64_tests; frame_tests; connection_tests; socket_tests; handshake_tests ])
