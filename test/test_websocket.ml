open Windtrap
open Caravan.Private
open Caravan_websocket.Private

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

let hex s =
  String.concat ""
    (List.map
       (fun c -> Printf.sprintf "%02x" (Char.code c))
       (List.of_seq (String.to_seq s)) )

let sha1_tests =
  (* Cases are [(name, input, hex digest)]; a failure lists every name. *)
  let digests cases =
    equal
      (list (pair string string))
      (List.map (fun (name, _, digest) -> (name, digest)) cases)
      (List.map (fun (name, input, _) -> (name, hex (Sha1.digest input))) cases)
  in
  group "Sha1"
    [ test "FIPS 180 examples" (fun () ->
          digests
            [ ("empty", "", "da39a3ee5e6b4b0d3255bfef95601890afd80709")
            ; ("abc", "abc", "a9993e364706816aba3e25717850c26c9cd0d89d")
            ; ( "448 bits"
              , "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
              , "84983e441c3bd26ebaae4aa1f95129e5e54670f1" )
            ; ( "a million a"
              , String.make 1_000_000 'a'
              , "34aa973cd4c4daa4f61eeb2bdbad27316534016f" ) ] )
    ; (* Digests from sha1sum, of the bytes 0, 1, 2, ... *)
      test "lengths around a block boundary" (fun () ->
          let bytes n = String.init n (fun i -> Char.chr (i land 255)) in
          digests
            (List.map
               (fun (n, d) -> (string_of_int n, bytes n, d))
               [ (55, "8ae2d46729cfe68ff927af5eec9c7d1b66d65ac2")
               ; (56, "636e2ec698dac903498e648bd2f3af641d3c88cb")
               ; (63, "6d942da0c4392b123528f2905c713a3ce28364bd")
               ; (64, "c6138d514ffa2135bfce0ed0b8fac65669917ec7")
               ; (65, "69bd728ad6e13cd76ff19751fde427b00e395746")
               ; (119, "41c89d06001bab4ab78736b44efe7ce18ce6ae08") ] ) ) ]

(* Short strings over the alphabet, padding and a few strangers, so that some
   are valid encodings and most are not. *)
let base64ish =
  Gen.string_of ~size:(Gen.int_range 0 12)
    (Gen.of_list (List.of_seq (String.to_seq "AQgw+/Zz09==-_ \n")))

let base64_tests =
  let vectors =
    [ ("", "")
    ; ("f", "Zg==")
    ; ("fo", "Zm8=")
    ; ("foo", "Zm9v")
    ; ("foob", "Zm9vYg==")
    ; ("fooba", "Zm9vYmE=")
    ; ("foobar", "Zm9vYmFy") ]
  in
  group "Base64"
    [ test "RFC 4648 examples encode" (fun () ->
          equal (list string) (List.map snd vectors)
            (List.map (fun (s, _) -> Base64.encode s) vectors) )
    ; test "RFC 4648 examples decode" (fun () ->
          equal
            (list (option string))
            (List.map (fun (s, _) -> Some s) vectors)
            (List.map (fun (_, e) -> Base64.decode e) vectors) )
    ; (* Expected from base64(1); it uses all 64 characters. *)
      test "the bytes 0 to 191" (fun () ->
          let s = String.init 192 (fun i -> Char.chr (i land 255)) in
          equal string
            "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/"
            (Base64.encode s) ;
          equal (option string) (Some s) (Base64.decode (Base64.encode s)) )
    ; test "rejects what is not a canonical encoding" (fun () ->
          List.iter
            (fun e -> is_none ~pp:Format.pp_print_string (Base64.decode e))
            [ "Zg"
            ; "Zg="
            ; "Zm9vY"
            ; "Zm9v!A=="
            ; "Zm-_"
            ; "Zm 9"
            ; "Z==="
            ; "===="
            ; "Zg==Zg=="
            ; "Zm=v"
            ; "Zh=="
            ; "Zm9="
            ; "Zm8=\n" ] )
    ; prop "decode inverts encode" Gen.string (fun s ->
          equal (option string) (Some s) (Base64.decode (Base64.encode s)) )
    ; prop "encodings take 4 characters per 3 bytes, rounded up" Gen.string
        (fun s ->
          equal int
            (4 * ((String.length s + 2) / 3))
            (String.length (Base64.encode s)) )
    ; prop ~count:1000 "only canonical encodings decode" base64ish (fun e ->
          match Base64.decode e with
          | None ->
              ()
          | Some d ->
              equal string e (Base64.encode d) ) ]

let of_hex h =
  let h = String.concat "" (String.split_on_char ' ' h) in
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let opcode_name : Frame.opcode -> string = function
  | Continuation ->
      "continuation"
  | Text ->
      "text"
  | Binary ->
      "binary"
  | Close ->
      "close"
  | Ping ->
      "ping"
  | Pong ->
      "pong"

let pp_frame ppf (f : Frame.t) =
  Format.fprintf ppf "%s%s %S" (opcode_name f.opcode)
    (if f.fin then "" else " (more)")
    f.payload

let frame = Testable.structural ~pp:pp_frame

let encode ?mask f =
  let out = Buffer.create 64 in
  let w =
    Buf_writer.make (fun b off len -> Buffer.add_subbytes out b off len)
  in
  Frame.write ?mask w f ; Buf_writer.flush w ; Buffer.contents out

(* What [Frame.read] makes of [input]: a frame, or the error. *)
let outcome ?(masked = true) ?(max = 1000) input =
  match Frame.read (Buf_reader.of_string input) ~masked ~max with
  | f ->
      Format.asprintf "%a" pp_frame f
  | exception Frame.Error (Protocol_error m) ->
      "protocol error: " ^ m
  | exception Frame.Error Too_big ->
      "too big"
  | exception End_of_file ->
      "end of input"

let frame_tests =
  let f ?(fin = true) opcode payload = {Frame.fin; opcode; payload} in
  let key = of_hex "37 fa 21 3d" in
  let b256 = String.init 256 (fun i -> Char.chr (i land 255)) in
  let b64k = String.init 65536 (fun i -> Char.chr (i land 255)) in
  (* RFC 6455 §5.7: [(frame, mask, bytes)]. *)
  let examples =
    [ (f Text "Hello", None, of_hex "81 05 48 65 6c 6c 6f")
    ; (f Text "Hello", Some key, of_hex "81 85 37 fa 21 3d 7f 9f 4d 51 58")
    ; (f ~fin:false Text "Hel", None, of_hex "01 03 48 65 6c")
    ; (f Continuation "lo", None, of_hex "80 02 6c 6f")
    ; (f Ping "Hello", None, of_hex "89 05 48 65 6c 6c 6f")
    ; (f Pong "Hello", Some key, of_hex "8a 85 37 fa 21 3d 7f 9f 4d 51 58")
    ; (f Binary b256, None, of_hex "82 7e 01 00" ^ b256)
    ; (f Binary b64k, None, of_hex "82 7f 00 00 00 00 00 01 00 00" ^ b64k) ]
  in
  group "Frame"
    [ test "RFC 6455 examples encode" (fun () ->
          equal (list string)
            (List.map (fun (_, _, e) -> hex e) examples)
            (List.map (fun (fr, mask, _) -> hex (encode ?mask fr)) examples) )
    ; test "RFC 6455 examples decode, one after another" (fun () ->
          let unmasked = List.filter (fun (_, m, _) -> m = None) examples in
          let r =
            Buf_reader.of_string ~size:16 ~chunk:5
              (String.concat "" (List.map (fun (_, _, e) -> e) unmasked))
          in
          equal (list frame)
            (List.map (fun (fr, _, _) -> fr) unmasked)
            (List.map (fun _ -> Frame.read r ~masked:false ~max:65536) unmasked) ;
          is_true (Buf_reader.peek_eof r) ;
          List.iter
            (fun (fr, m, e) ->
              if m <> None then
                equal frame fr
                  (Frame.read (Buf_reader.of_string e) ~masked:true ~max:5) )
            examples )
    ; test "lengths take the shortest encoding" (fun () ->
          equal
            (list (pair int int))
            [(125, 2); (126, 4); (65535, 4); (65536, 10)]
            (List.map
               (fun n ->
                 (n, String.length (encode (f Binary (String.make n 'x'))) - n) )
               [125; 126; 65535; 65536] ) ;
          equal
            (list (pair int int))
            [(125, 6); (126, 8); (65536, 14)]
            (List.map
               (fun n ->
                 ( n
                 , String.length
                     (encode ~mask:key (f Binary (String.make n 'x')))
                   - n ) )
               [125; 126; 65536] ) )
    ; test "the length limit" (fun () ->
          let input = encode (f Binary "0123456789") in
          expect (outcome ~masked:false ~max:10 input)
          @@ __POS_OF__ {|binary "0123456789"|} ;
          expect (outcome ~masked:false ~max:9 input) @@ __POS_OF__ {|too big|} ;
          (* The limit is only for data frames. *)
          expect (outcome ~masked:false ~max:0 (encode (f Ping "0123456789")))
          @@ __POS_OF__ {|ping "0123456789"|} )
    ; (* Inputs end right after their length, so reading further would give "end
         of input". *)
      test "bad frames" (fun () ->
          let cases =
            [ ("rsv1", "c1 80")
            ; ("rsv2", "a1 80")
            ; ("rsv3", "91 80")
            ; ("opcode 3", "83 80")
            ; ("opcode 7", "87 80")
            ; ("opcode b", "8b 80")
            ; ("opcode f", "8f 80")
            ; ("fragmented ping", "09 80")
            ; ("fragmented close", "08 80")
            ; ("ping of 126", "89 fe 00 7e")
            ; ("pong of 127", "8a ff 00 00 00 00 00 00 00 7f")
            ; ("unmasked", "81 00")
            ; ("top bit", "82 ff 80 00 00 00 00 00 00 00")
            ; ("2^62 - 1", "82 ff 3f ff ff ff ff ff ff ff")
            ; ("1001", "82 fe 03 e9")
            ; ("1000", "82 fe 03 e8") ]
          in
          expect
            (String.concat "\n"
               (List.map
                  (fun (name, h) -> name ^ ": " ^ outcome (of_hex h))
                  cases ) )
          @@ __POS_OF__
               {|rsv1: protocol error: reserved bits set
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
1000: end of input|} ;
          expect
            (outcome ~masked:false (of_hex "81 85 37 fa 21 3d 7f 9f 4d 51 58"))
          @@ __POS_OF__ {|protocol error: masked frame|} ;
          (* Long encodings of short lengths are accepted. *)
          expect (outcome ~masked:false (of_hex "81 7e 00 02 68 69"))
          @@ __POS_OF__ {|text "hi"|} ;
          expect
            (outcome ~masked:false (of_hex "81 7f 00 00 00 00 00 00 00 00"))
          @@ __POS_OF__ {|text ""|} )
    ; test "a declared length does not allocate before the payload arrives"
        (fun () ->
          let header = of_hex "827f" ^ "\x00\x00\x00\x00\x00\x80\x00\x00" in
          let r = Buf_reader.of_string (header ^ "short") in
          let before = Gc.allocated_bytes () in
          raises End_of_file (fun () -> Frame.read r ~masked:false ~max:max_int) ;
          let used = Gc.allocated_bytes () -. before in
          is_true ~msg:(Printf.sprintf "%.0f bytes" used) (used < 1_000_000.) )
    ; test "truncated input" (fun () ->
          let input = of_hex "81 fe 00 02 37 fa 21 3d 7f 9f" in
          equal (list string)
            (List.init (String.length input) (fun _ -> "end of input"))
            (List.init (String.length input) (fun n ->
                 outcome (String.sub input 0 n) ) ) ;
          expect (outcome input) @@ __POS_OF__ {|text "He"|} )
    ; test "write rejects what it cannot send" (fun () ->
          List.iter
            (fun (mask, fr) ->
              raises_match is_invalid_arg (fun () -> encode ?mask fr) )
            [ (Some "abc", f Text "")
            ; (Some "abcde", f Text "")
            ; (None, f ~fin:false Ping "")
            ; (None, f Close (String.make 126 'x'))
            ; (None, f Pong (String.make 126 'x')) ] ;
          is_true
            ( String.length (encode ~mask:key (f Ping (String.make 125 'x')))
            = 131 ) )
    ; prop "a masked payload is the XOR of each byte with the key"
        Gen.(
          pair
            (string_of ~size:(constant 4) char)
            (string_of ~size:(int_range 0 300) char) )
        (fun (mask, payload) ->
          let out = encode ~mask (f Binary payload) in
          let n = String.length payload in
          let masked = String.sub out (String.length out - n) n in
          equal string
            (String.mapi
               (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3]))
               payload )
            masked )
    ; prop "read inverts write, however the input is split"
        Gen.(
          quad
            (triple
               (of_list
                  ~pp:(fun ppf o -> Format.pp_print_string ppf (opcode_name o))
                  Frame.[Continuation; Text; Binary; Close; Ping; Pong] )
               bool
               (string_of ~size:(int_range 0 300) char) )
            (option (string_of ~size:(constant 4) char))
            (int_range 1 9) (int_range 1 9) )
        (fun ((opcode, fin, payload), mask, (chunk : int), size) ->
          let fr =
            if Frame.is_control opcode then
              f opcode
                (String.sub payload 0 (Int.min 125 (String.length payload)))
            else f ~fin opcode payload
          in
          let r = Buf_reader.of_string ~size ~chunk (encode ?mask fr) in
          (* The parts that on_data gets: not empty, in order, and the payload,
             unmasked. None for a control frame. *)
          let parts = Buffer.create 64 and empty = ref false in
          let on_data op buf off len =
            equal string (opcode_name opcode) (opcode_name op) ;
            if len = 0 then empty := true ;
            Buffer.add_subbytes parts buf off len
          in
          equal frame fr (Frame.read ~on_data r ~masked:(mask <> None) ~max:300) ;
          is_true (Buf_reader.peek_eof r) ;
          is_true ~msg:"no empty part" (not !empty) ;
          equal string
            (if Frame.is_control opcode then "" else fr.payload)
            (Buffer.contents parts) ) ]

module Ws = Caravan_websocket

(* A frame from a client: clients mask their frames. *)
let client ?(fin = true) opcode payload =
  encode ~mask:"\x01\x02\x03\x04" {Frame.fin; opcode; payload}

let code_reason code reason =
  let b = Bytes.create 2 in
  Bytes.set_uint16_be b 0 code ;
  Bytes.to_string b ^ reason

(* A Close frame shows its code and its reason. *)
let show_frame (f : Frame.t) =
  match f.opcode with
  | Close when f.payload = "" ->
      "close"
  | Close when String.length f.payload = 2 ->
      Printf.sprintf "close %d" (String.get_uint16_be f.payload 0)
  | Close ->
      Printf.sprintf "close %d %S"
        (String.get_uint16_be f.payload 0)
        (String.sub f.payload 2 (String.length f.payload - 2))
  | _ ->
      Format.asprintf "%a" pp_frame f

(* The frames in [out], one for each line. *)
let frames out =
  let r = Buf_reader.of_string out in
  let rec loop acc =
    match Frame.read r ~masked:false ~max:max_int with
    | f ->
        loop (show_frame f :: acc)
    | exception End_of_file ->
        List.rev acc
  in
  loop []

(* Runs [app] on a server connection that reads [input]. The result is the lines
   that [app] logs, a "--" line, then the frames that the server sent. *)
let converse ?max_message input app =
  let out = Buffer.create 64 and log = ref [] in
  let note s = log := s :: !log in
  Affect_unix.Unix.main ~domain_count:1 (fun () ->
      app note
        (Ws.Private.of_flow ?max_message (Caravan.Flow.of_string input out)) ) ;
  String.concat "\n" (List.rev !log @ ("--" :: frames (Buffer.contents out)))

let note_closed note = function
  | Ws.Closed {code; reason} ->
      note (Printf.sprintf "closed %d %S" code reason)
  | e ->
      raise e

(* Sends back each message until the connection closes. *)
let echo note c =
  let rec loop () =
    match Ws.recv c with
    | Text s ->
        note (Printf.sprintf "text %S" s) ;
        Ws.send c (Text s) ;
        loop ()
    | Binary s ->
        note (Printf.sprintf "binary %S" s) ;
        Ws.send c (Binary s) ;
        loop ()
    | exception e ->
        note_closed note e
  in
  loop ()

(* The result of [Utf8.continue], or "invalid". *)
let continue_result pending s =
  match Utf8.continue pending s with
  | r ->
      Printf.sprintf "%S" r
  | exception Utf8.Invalid ->
      "invalid"

(* Parts of UTF-8 text: correct characters of 1 to 4 bytes, and bytes or
   sequences that are not correct, then cut at random points. *)
let utf8_parts =
  Gen.(
    pair
      (list ~size:(int_range 0 8)
         (of_list
            ~pp:(fun ppf s -> Format.fprintf ppf "%S" s)
            [ "a"
            ; "\xc2\xa9"
            ; "\xc3\xa9"
            ; "\xe2\x82\xac"
            ; "\xf0\x9d\x84\x9e"
            ; "\xed\x9f\xbf"
            ; "\xf4\x8f\xbf\xbf"
            ; "\x80"
            ; "\xc0"
            ; "\xff"
            ; "\xed\xa0"
            ; "\xe0"
            ; "\xf4"
            ; "\x90" ] ) )
      (list ~size:(int_range 0 5) (int_range 0 6)) )

let cut_parts (pieces, sizes) =
  let rec cut s = function
    | [] ->
        [s]
    | n :: ns ->
        let n = Int.min n (String.length s) in
        String.sub s 0 n :: cut (String.sub s n (String.length s - n)) ns
  in
  cut (String.concat "" pieces) sizes

(* The index of the part where [Utf8.continue] raises, if one, and the last
   result. *)
let fold_continue parts =
  let rec loop i pending = function
    | [] ->
        (None, pending)
    | p :: ps -> (
      match Utf8.continue pending p with
      | pending ->
          loop (i + 1) pending ps
      | exception Utf8.Invalid ->
          (Some i, pending) )
  in
  loop 0 "" parts

(* A model: [x] can start correct UTF-8 if 0 to 3 more bytes make it correct.
   The bytes 0x80, 0x90 and 0xA0 are in each range of a second byte (Unicode
   table 3-7), and 0x80 is correct after it. *)
let can_start x =
  let bytes = ["\x80"; "\x90"; "\xa0"] in
  let rec ends n =
    if n = 0 then [""]
    else List.concat_map (fun e -> List.map (( ^ ) e) bytes) (ends (n - 1))
  in
  List.exists
    (fun n -> List.exists (fun e -> String.is_valid_utf_8 (x ^ e)) (ends n))
    [0; 1; 2; 3]

let utf8_tests =
  group "UTF-8 in parts"
    [ test "boundary cases" (fun () ->
          equal
            (list (pair (pair string string) string))
            [ (("", "\xce\xba"), {|""|})
            ; (("", "\xf4"), {|"\244"|})
            ; (("\xf4", "\x90"), "invalid")
            ; (("", "\xf4\x90\x80\x80"), "invalid")
            ; (("\xf0\x9f", "\x98"), {|"\240\159\152"|})
            ; (("\xf0\x9f\x98", "\x80ab"), {|""|})
            ; (("\xe2", ""), {|"\226"|})
            ; (("", ""), {|""|})
            ; (("", "\xed"), {|"\237"|})
            ; (("\xe0", "\xa0"), {|"\224\160"|})
            ; (("", "\x80"), "invalid")
            ; (("", "a\x80"), "invalid")
            ; (("", "\xc0"), "invalid")
            ; (("", "\xc1"), "invalid")
            ; (("", "\xf5"), "invalid")
            ; (("", "\xff"), "invalid")
            ; (("\xed", "\xa0"), "invalid")
            ; (("\xe0", "\x80"), "invalid")
            ; (("\xc3", "\xa9\xe2\x82"), {|"\226\130"|})
            ; (("", "a\xc2"), {|"\194"|})
            ; (("\xc2", "\xa9b"), {|""|}) ]
            (List.map
               (fun (pending, s) -> ((pending, s), continue_result pending s))
               [ ("", "\xce\xba")
               ; ("", "\xf4")
               ; ("\xf4", "\x90")
               ; ("", "\xf4\x90\x80\x80")
               ; ("\xf0\x9f", "\x98")
               ; ("\xf0\x9f\x98", "\x80ab")
               ; ("\xe2", "")
               ; ("", "")
               ; ("", "\xed")
               ; ("\xe0", "\xa0")
               ; ("", "\x80")
               ; ("", "a\x80")
               ; ("", "\xc0")
               ; ("", "\xc1")
               ; ("", "\xf5")
               ; ("", "\xff")
               ; ("\xed", "\xa0")
               ; ("\xe0", "\x80")
               ; ("\xc3", "\xa9\xe2\x82")
               ; ("", "a\xc2")
               ; ("\xc2", "\xa9b") ] ) )
    ; prop ~count:2000 "the parts are correct exactly when the text is"
        utf8_parts (fun g ->
          let parts = cut_parts g in
          let correct =
            match fold_continue parts with None, "" -> true | _ -> false
          in
          equal bool (String.is_valid_utf_8 (String.concat "" parts)) correct )
    ; prop ~count:2000
        "continue fails at the first part that cannot start correct UTF-8"
        utf8_parts (fun g ->
          let parts = cut_parts g in
          let rec first i prefix = function
            | [] ->
                None
            | p :: ps ->
                let prefix = prefix ^ p in
                if can_start prefix then first (i + 1) prefix ps else Some i
          in
          equal (option int) (first 0 "" parts) (fst (fold_continue parts)) )
    ; test "continue agrees with the stdlib on each sequence of 1 to 4 bytes"
        (fun () ->
          (* Each lead byte, each second byte, and the limits of the ranges
             (Unicode table 3-7) for the third and the fourth byte. A sequence
             alone takes the careful path at the end of a range; with ASCII
             bytes after it, the fast path. *)
          let limits = [0x7f; 0x80; 0x8f; 0x90; 0x9f; 0xa0; 0xbf; 0xc0] in
          let ok s =
            match Utf8.continue "" s with
            | "" ->
                true
            | _ ->
                false
            | exception Utf8.Invalid ->
                false
          in
          (* [s] in two parts, cut after [k] bytes: the second part goes after
             the pending bytes of the first. *)
          let ok_split s k =
            match Utf8.continue "" (String.sub s 0 k) with
            | pending -> (
              match
                Utf8.continue pending (String.sub s k (String.length s - k))
              with
              | "" ->
                  true
              | _ ->
                  false
              | exception Utf8.Invalid ->
                  false )
            | exception Utf8.Invalid ->
                false
          in
          let bad = ref [] in
          let check s =
            List.iter
              (fun s ->
                let valid = String.is_valid_utf_8 s in
                if ok s <> valid then bad := s :: !bad ;
                for k = 1 to Int.min 3 (String.length s - 1) do
                  if ok_split s k <> valid then bad := s :: !bad
                done )
              [s; s ^ "aaaaaaa"]
          in
          for b0 = 0 to 255 do
            let c0 = String.make 1 (Char.chr b0) in
            check c0 ;
            for b1 = 0 to 255 do
              let c1 = c0 ^ String.make 1 (Char.chr b1) in
              check c1 ;
              List.iter
                (fun b2 ->
                  let c2 = c1 ^ String.make 1 (Char.chr b2) in
                  check c2 ;
                  List.iter
                    (fun b3 -> check (c2 ^ String.make 1 (Char.chr b3)))
                    limits )
                limits
            done
          done ;
          equal (list string) [] (List.rev !bad) )
    ; test "continue_bytes reads only its range" (fun () ->
          let r pending s off len =
            match Utf8.continue_bytes pending (Bytes.of_string s) off len with
            | p ->
                Printf.sprintf "%S" p
            | exception Utf8.Invalid ->
                "invalid"
          in
          equal (list string)
            [ {|"\226"|}
            ; {|"\226\130"|}
            ; {|""|}
            ; "invalid"
            ; {|"\240\159"|}
            ; {|""|} ]
            [ (* The bytes after the range would complete the sequence. *)
              r "" "a\xe2\x82\xac" 0 2
            ; r "" "a\xe2\x82\xac" 0 3
            ; r "" "a\xe2\x82\xac" 1 3
              (* The bytes before the range would start a sequence. *)
            ; r "" "\xe2\x82\xac" 1 2
            ; r "" "xx\xf0\x9f\x98\x80" 2 2
            ; r "\xf0\x9f" "xx\x98\x80yy" 2 2 ] ;
          raises_match
            (function Invalid_argument _ -> true | _ -> false)
            (fun () -> Utf8.continue_bytes "" (Bytes.create 2) 1 2) ;
          (* A pending value that this function cannot give. *)
          List.iter
            (fun pending ->
              raises_match
                (function Invalid_argument _ -> true | _ -> false)
                (fun () ->
                  Utf8.continue_bytes pending (Bytes.of_string "Aabc") 1 3 ) )
            ["\xff"; "a"; "\xc3\xa9"; "\xf0\x9f\x98\x80"] )
    ; prop ~count:2000
        "continue_bytes on a range gives the result of continue on its bytes"
        Gen.(triple utf8_parts (int_range 0 7) (int_range 0 7))
        (fun (g, before, after) ->
          (* Each part sits between bytes that are not in its range: correct and
             incorrect UTF-8 bytes. *)
          let junk n = String.init n (fun i -> "\x80\xe2\x82a\xf0".[i mod 5]) in
          let on_range pending p =
            let s = junk before ^ p ^ junk after in
            Utf8.continue_bytes pending (Bytes.of_string s) before
              (String.length p)
          in
          let result f =
            let rec loop pending = function
              | [] ->
                  Some pending
              | p :: ps -> (
                match f pending p with
                | pending ->
                    loop pending ps
                | exception Utf8.Invalid ->
                    None )
            in
            loop "" (cut_parts g)
          in
          equal (option string) (result Utf8.continue) (result on_range) )
    ; test "an incorrect fragment gives 1007 before the end of the message"
        (fun () ->
          (* Autobahn 6.4.1: the last fragment does not come. *)
          expect
            (converse
               ( client ~fin:false Text (of_hex "cebae1bdb9cf83cebcceb5")
               ^ client ~fin:false Continuation (of_hex "f4908080") )
               echo )
          @@ __POS_OF__
               {|
            closed 1007 "text is not UTF-8"
            --
            close 1007
            |} )
    ; test "a byte that ends a sequence too early gives 1007 at once" (fun () ->
          (* Autobahn 6.4.2. *)
          expect
            (converse
               ( client ~fin:false Text (of_hex "cebae1bdb9cf83cebcceb5f4")
               ^ client ~fin:false Continuation (of_hex "90") )
               echo )
          @@ __POS_OF__
               {|
            closed 1007 "text is not UTF-8"
            --
            close 1007
            |} )
    ; test "a character in three fragments is correct" (fun () ->
          expect
            (converse
               ( client ~fin:false Text "\xf0"
               ^ client ~fin:false Continuation "\x9d\x84"
               ^ client Continuation "\x9e" )
               echo )
          @@ __POS_OF__
               {|
            text "\240\157\132\158"
            closed 1006 ""
            --
            text "\240\157\132\158"
            |} )
    ; test "an incomplete character at the end of a message gives 1007"
        (fun () ->
          expect
            (converse
               (client ~fin:false Text "a\xe2" ^ client Continuation "\x82")
               echo )
          @@ __POS_OF__
               {|
            closed 1007 "text is not UTF-8"
            --
            close 1007
            |} )
    ; test "a binary message is not examined" (fun () ->
          expect
            (converse
               (client ~fin:false Binary "\xff" ^ client Continuation "\x80")
               echo )
          @@ __POS_OF__
               {|
            binary "\255\128"
            closed 1006 ""
            --
            binary "\255\128"
            |} )
    ]

let connection_tests =
  group "Connection"
    [ test "messages, then a Close from the peer" (fun () ->
          expect
            (converse
               ( client Text "Hello" ^ client Binary "\x00\xff"
               ^ client Close (code_reason 1000 "bye") )
               echo )
          @@ __POS_OF__
               {|
            text "Hello"
            binary "\000\255"
            closed 1000 "bye"
            --
            text "Hello"
            binary "\000\255"
            close 1000
            |} )
    ; test "fragments with control frames between them" (fun () ->
          expect
            (converse
               ( client ~fin:false Text "H\xe2\x82"
               ^ client Ping "p"
               ^ client ~fin:false Continuation "\xac"
               ^ client Pong "q" ^ client Continuation "lo" )
               echo )
          @@ __POS_OF__
               {|
            text "H\226\130\172lo"
            closed 1006 ""
            --
            pong "p"
            text "H\226\130\172lo"
            |} )
    ; test "an empty Close" (fun () ->
          expect (converse (client Close "") echo)
          @@ __POS_OF__
               {|
            closed 1005 ""
            --
            close
            |} )
    ; test "the Close codes that are permitted" (fun () ->
          expect
            (String.concat "\n"
               (List.map
                  (fun code ->
                    converse (client Close (code_reason code "")) echo )
                  [1000; 1003; 1007; 1014; 3000; 4999] ) )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "a peer that does not obey the protocol" (fun () ->
          let x n = String.make n 'x' in
          let cases =
            [ ("reserved opcode", of_hex "83 80", None)
            ; ("unmasked frame", of_hex "81 00", None)
            ; ("frame too large", client Binary (x 200), Some 16)
            ; ("text too large", client Text (x 20), Some 16)
            ; ( "fragments too large"
              , client ~fin:false Binary (x 10) ^ client Continuation (x 10)
              , Some 16 )
            ; ( "fragments too large, before the data"
              , client ~fin:false Binary (x 900)
                ^ String.sub (client Continuation (x 200)) 0 4
              , Some 1000 )
            ; ("continuation first", client Continuation "x", None)
            ; ( "message in a message"
              , client ~fin:false Text "a" ^ client Text "b"
              , None )
            ; ("text not UTF-8", client Text "\xff", None)
            ; ( "fragments not UTF-8"
              , client ~fin:false Text "\xe2\x82" ^ client Continuation "x"
              , None )
            ; ("close of 1 byte", client Close "\x03", None)
            ; ( "close reason not UTF-8"
              , client Close (code_reason 1000 "\xff")
              , None ) ]
            @ List.map
                (fun code ->
                  ( Printf.sprintf "close code %d" code
                  , client Close (code_reason code "")
                  , None ) )
                [0; 999; 1004; 1005; 1006; 1015; 1016; 2999; 5000; 65535]
          in
          expect
            (String.concat "\n"
               (List.map
                  (fun (name, input, max_message) ->
                    let lines =
                      String.split_on_char '\n'
                        (converse ?max_message input echo)
                    in
                    name ^ ": " ^ String.concat " | " lines )
                  cases ) )
          @@ __POS_OF__
               {|
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
            |} )
    ; test "control frames do not count in the size of a message" (fun () ->
          expect
            (converse ~max_message:16
               ( client ~fin:false Binary (String.make 16 'x')
               ^ client Ping (String.make 100 'p')
               ^ client Continuation "" )
               (fun note c ->
                 match Ws.recv c with
                 | Binary s ->
                     note
                       (Printf.sprintf "binary of %d bytes" (String.length s))
                 | Text _ ->
                     note "text"
                 | exception e ->
                     note_closed note e ) )
          @@ __POS_OF__
               {|
            binary of 16 bytes
            --
            pong "pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp"
            |} )
    ; test "the size of a message counts all of its fragments" (fun () ->
          expect
            (converse ~max_message:10
               ( client ~fin:false Binary "aaaa"
               ^ client ~fin:false Continuation "bbbb"
               ^ client Continuation "cccc" )
               (fun note c ->
                 match Ws.recv c with
                 | Binary s ->
                     note
                       (Printf.sprintf "binary of %d bytes" (String.length s))
                 | Text _ ->
                     note "text"
                 | exception e ->
                     note_closed note e ) )
          @@ __POS_OF__
               {|
            closed 1009 "message is too large"
            --
            close 1009
            |} )
    ; test "empty fragments do not make the heap grow" (fun () ->
          (* 300 000 empty Continuation frames, then the last fragment. The read
             function records the largest heap. *)
          let first = client ~fin:false Binary "x"
          and empty = client ~fin:false Continuation "" in
          let last = client Continuation "y" and count = 300_000 in
          let size = String.length empty in
          let body = count * size in
          let total = String.length first + body + String.length last in
          let pos = ref 0 and top = ref 0 in
          let byte i =
            if i < String.length first then first.[i]
            else if i < String.length first + body then
              empty.[(i - String.length first) mod size]
            else last.[i - String.length first - body]
          in
          let read buf off len =
            top := Int.max !top (Gc.quick_stat ()).heap_words ;
            let n = Int.min len (total - !pos) in
            for i = 0 to n - 1 do
              Bytes.set buf (off + i) (byte (!pos + i))
            done ;
            pos := !pos + n ;
            n
          in
          let ignore_deadline (_ : Affect_unix.Mtime.t option) = () in
          let flow =
            Caravan.Flow.make ~time:Caravan.Time.mono ~read
              ~read_now:(fun b o l -> Some (read b o l))
              ~write:(fun _ _ _ -> ())
              ~readable:(Affect.Action.always ()) ~shutdown_send:ignore
              ~close:ignore ~set_read_deadline:ignore_deadline
              ~set_write_deadline:ignore_deadline ()
          in
          let start = (Gc.quick_stat ()).heap_words in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              is_true (Ws.recv (Ws.Private.of_flow flow) = Binary "xy") ) ;
          let grew = (!top - start) * (Sys.word_size / 8) in
          is_true
            ~msg:(Printf.sprintf "the heap grew by %d bytes" grew)
            (grew < 4_000_000) )
    ; test "small frames take one read of the input" (fun () ->
          let input = client Binary "hello" ^ client Text "hi"
          and pos = ref 0
          and reads = ref 0 in
          let read buf off len =
            incr reads ;
            let n = Int.min len (String.length input - !pos) in
            Bytes.blit_string input !pos buf off n ;
            pos := !pos + n ;
            n
          in
          let ignore_deadline (_ : Affect_unix.Mtime.t option) = () in
          let flow =
            Caravan.Flow.make ~time:Caravan.Time.mono ~read
              ~read_now:(fun b o l -> Some (read b o l))
              ~write:(fun _ _ _ -> ())
              ~readable:(Affect.Action.always ()) ~shutdown_send:ignore
              ~close:ignore ~set_read_deadline:ignore_deadline
              ~set_write_deadline:ignore_deadline ()
          in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              let c = Ws.Private.of_flow flow in
              is_true (Ws.recv c = Binary "hello") ;
              is_true (Ws.recv c = Text "hi") ) ;
          equal int 1 !reads )
    ; test "the buffer of a fragmented message stays within max_message"
        (fun () ->
          (* What recv allocates for a message of [m] bytes in fragments of
             [sizes], more than for the same message in one frame. The expected
             message is made before the measure. *)
          let m = 1024 * 1024 in
          let expected = String.make m 'x' in
          let allocated frames =
            let flow =
              Caravan.Flow.of_string (String.concat "" frames)
                (Buffer.create 16)
            in
            let used = ref 0. in
            Affect_unix.Unix.main ~domain_count:1 (fun () ->
                let c = Ws.Private.of_flow ~max_message:m flow in
                let before = Gc.allocated_bytes () in
                let msg = Ws.recv c in
                used := Gc.allocated_bytes () -. before ;
                is_true (msg = Binary expected) ) ;
            !used
          in
          let whole = allocated [client Binary expected] in
          let more sizes =
            let rec frames first = function
              | [] ->
                  []
              | [n] ->
                  [ client
                      (if first then Binary else Continuation)
                      (String.make n 'x') ]
              | n :: rest ->
                  client ~fin:false
                    (if first then Binary else Continuation)
                    (String.make n 'x')
                  :: frames false rest
            in
            (allocated (frames true sizes) -. whole) /. Float.of_int m
          in
          (* At most one buffer of [m] bytes, and the copy when it grows: 1.50 m
             at this time. A buffer of 2 times the first fragment gave 2.00 m to
             4.44 m. A full first fragment and an empty last fragment do not
             cause a copy. In the last three cases, the double size is more than
             [m]: without the limit, they give 2.44 m to 3.00 m. *)
          List.iter
            (fun (sizes, limit) ->
              let r = more sizes in
              is_true
                ~msg:
                  (Printf.sprintf "%s: %.2f m"
                     (String.concat " + " (List.map string_of_int sizes))
                     r )
                (r < limit) )
            [ ([m; 0], 0.5)
            ; ([(m / 2) - 1; (m / 2) + 1], 1.75)
            ; ([3; m - 3], 1.75)
            ; ([m - 1; 1], 1.75)
            ; ([(m / 2) + 1; (m / 2) - 1], 1.75)
            ; ([6 * m / 10; m / 10; m - (6 * m / 10) - (m / 10)], 1.75) ] )
    ; test "a connection that stops inside a frame" (fun () ->
          expect (converse (String.sub (client Text "Hello") 0 4) echo)
          @@ __POS_OF__
               {|
            closed 1006 ""
            --
            |} )
    ; test "a closed connection raises Closed again" (fun () ->
          expect
            (converse
               (client Close (code_reason 1001 "away"))
               (fun note c ->
                 List.iter
                   (fun f -> try f () with e -> note_closed note e)
                   [ (fun () -> ignore (Ws.recv c))
                   ; (fun () -> ignore (Ws.recv c))
                   ; (fun () -> Ws.send c (Text "x"))
                   ; (fun () -> Ws.ping c "") ] ) )
          @@ __POS_OF__
               {|
            closed 1001 "away"
            closed 1001 "away"
            closed 1001 "away"
            closed 1001 "away"
            --
            close 1001
            |} )
    ; test "send and ping examine their arguments" (fun () ->
          expect
            (converse "" (fun _ c ->
                 raises_match is_invalid_arg (fun () -> Ws.send c (Text "\xff")) ;
                 raises_match is_invalid_arg (fun () ->
                     Ws.ping c (String.make 126 'p') ) ;
                 Ws.ping c (String.make 125 'p') ;
                 Ws.send c (Binary "\xff") ) )
          @@ __POS_OF__
               {|
            --
            ping "ppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp"
            binary "\255"
            |} )
    ; prop "recv joins fragments and answers Pings"
        Gen.(
          list ~size:(int_range 0 4)
            (triple bool
               (list ~size:(int_range 0 6)
                  (of_list
                     [ "a"
                     ; "\xc3\xa9"
                     ; "\xe2\x82\xac"
                     ; "\xf0\x9d\x84\x9e"
                     ; "\x00" ] ) )
               (list ~size:(int_range 0 4) (int_range 0 5)) ) )
        (fun messages ->
          (* Cut [s] in fragments of the given sizes and a last fragment. *)
          let rec cut s = function
            | [] ->
                [s]
            | n :: ns ->
                let n = Int.min n (String.length s) in
                String.sub s 0 n
                :: cut (String.sub s n (String.length s - n)) ns
          in
          let input = Buffer.create 64 and expected = ref [] and log = ref [] in
          List.iteri
            (fun i (text, pieces, sizes) ->
              let payload = String.concat "" pieces in
              let fragments = cut payload sizes in
              let last = List.length fragments - 1 in
              List.iteri
                (fun j f ->
                  let op : Frame.opcode =
                    if j > 0 then Continuation
                    else if text then Text
                    else Binary
                  in
                  Buffer.add_string input (client ~fin:(j = last) op f) ;
                  if j < last && j mod 2 = 0 then begin
                    let p = Printf.sprintf "%d.%d" i j in
                    Buffer.add_string input (client Ping p) ;
                    expected := Printf.sprintf "pong %S" p :: !expected
                  end )
                fragments ;
              let m =
                if text then Printf.sprintf "text %S" payload
                else Printf.sprintf "binary %S" payload
              in
              log := m :: !log ;
              expected := m :: !expected )
            messages ;
          equal string
            (String.concat "\n"
               (List.rev !log @ ["closed 1006 \"\""; "--"] @ List.rev !expected) )
            (converse (Buffer.contents input) echo) ) ]

let close_tests =
  let try_note note f = try f () with e -> note_closed note e in
  group "Close"
    [ test "close sends a Close frame and reads until the reply" (fun () ->
          expect
            (converse
               ( client Text "discarded" ^ client Ping "no pong"
               ^ client Close (code_reason 1000 "ok") )
               (fun note c ->
                 Ws.close ~code:1001 ~reason:"bye" c ;
                 note "close ended" ;
                 try_note note (fun () -> ignore (Ws.recv c)) ) )
          @@ __POS_OF__
               {|
            close ended
            closed 1000 "ok"
            --
            close 1001 "bye"
            |} )
    ; test "after close, close does nothing and send raises Closed" (fun () ->
          expect
            (converse (client Close "") (fun note c ->
                 Ws.close c ;
                 Ws.close ~code:3000 c ;
                 try_note note (fun () -> Ws.send c (Text "x")) ;
                 try_note note (fun () -> Ws.ping c "") ) )
          @@ __POS_OF__
               {|
            closed 1005 ""
            closed 1005 ""
            --
            close 1000
            |} )
    ; test "a peer that goes before its reply" (fun () ->
          expect
            (converse "" (fun note c ->
                 Ws.close c ;
                 try_note note (fun () -> ignore (Ws.recv c)) ) )
          @@ __POS_OF__
               {|
            closed 1006 ""
            --
            close 1000
            |} )
    ; test "close examines its code and its reason" (fun () ->
          (* The largest reason: 123 bytes of UTF-8. *)
          let reason = String.make 120 'x' ^ "\xe2\x82\xac" in
          equal string
            (Printf.sprintf "--\nclose 4999 %S" reason)
            (converse "" (fun _ c ->
                 List.iter
                   (fun (code, reason) ->
                     raises_match is_invalid_arg (fun () ->
                         Ws.close ~code ~reason c ) )
                   [ (999, "")
                   ; (1004, "")
                   ; (1005, "")
                   ; (1006, "")
                   ; (1015, "")
                   ; (2999, "")
                   ; (5000, "")
                   ; (1000, reason ^ "x")
                   ; (1000, "\xff") ] ;
                 Ws.close ~code:4999 ~reason c ) ) ) ]

(* Runs [f] with the flows of the server and of the peer of a TCP connection. *)
let with_flows = Server_support.with_flows

(* Runs [f] with a server connection and the flow of its peer, on a TCP
   connection. The flow of the server is on the time line [time]. *)
let with_socket ?time ?close_timeout ?write_timeout ?read_timeout ?idle_timeout
    f =
  with_flows ?time (fun server peer ->
      f
        (Ws.Private.of_flow ?close_timeout ?write_timeout ?read_timeout
           ?idle_timeout server )
        peer )

let ms n = Affect_unix.Mtime.Span.(n * ms)

(* [f ()], in a call that must end in 2 s. If not, the test cancels the call and
   fails, so that a regression does not stop the suite. *)
let within_2s f = Server_support.within_2s f

(* The code of the [Closed] exception that [f] raises. *)
let closed_code f =
  match f () with
  | _ ->
      fail "no Closed exception"
  | exception Ws.Closed {code; _} ->
      code

(* Waits at most 2 s until a close starts on [c]. *)
let wait_closing c =
  let rec wait n =
    if (not (is_closing c)) && n > 0 then (
      Affect_unix.Mtime.wait_for (ms 5) ;
      wait (n - 1) )
  in
  wait 400 ;
  is_true ~msg:"a close started" (is_closing c)

let socket_tests =
  group "Connection on a socket"
    [ test ~timeout:30.
        "fibers that send at the same time do not mix their frames" (fun () ->
          let open Affect in
          with_socket
          @@ fun c peer ->
          (* Some messages are larger than a socket buffer: they need many
             writes. *)
          let body name i =
            String.make (if i mod 50 = 0 then 100_000 else i) name
          in
          let sender name =
            Fun.Async.call (fun () ->
                for i = 1 to 200 do
                  Ws.send c
                    (Binary (Printf.sprintf "%c %d %s" name i (body name i)))
                done )
          in
          let a = sender 'a' and b = sender 'b' in
          let r = Buf_reader.of_flow peer and last = Hashtbl.create 2 in
          for _ = 1 to 400 do
            let f = Frame.read r ~masked:false ~max:max_int in
            match String.split_on_char ' ' f.payload with
            | [name; i; data] ->
                let i = int_of_string i
                and prev =
                  Option.value ~default:0 (Hashtbl.find_opt last name)
                in
                equal int ~msg:name (prev + 1) i ;
                is_true ~msg:"intact" (data = body name.[0] i) ;
                Hashtbl.replace last name i
            | _ ->
                fail "a message with incorrect content"
          done ;
          Fun.Async.get a ;
          Fun.Async.get b )
    ; test
        "after a protocol error, the peer gets the Close frame and the end of \
         the stream" (fun () ->
          let open Affect in
          with_socket
          @@ fun c peer ->
          let reader =
            Fun.Async.call (fun () -> closed_code (fun () -> Ws.recv c))
          in
          (* A frame of 20 MiB, over the limit of 8 MiB: the server reads only
             its header, and 1 MiB stays unread. *)
          let header =
            of_hex "82ff" ^ "\x00\x00\x00\x00\x01\x40\x00\x00"
            ^ "\x01\x02\x03\x04"
          in
          Caravan.Flow.write_string peer (header ^ String.make 1_000_000 'x') ;
          let r = Buf_reader.of_flow peer in
          equal string "close 1009"
            (show_frame (Frame.read r ~masked:false ~max:125)) ;
          raises End_of_file (fun () -> Frame.read r ~masked:false ~max:125) ;
          Caravan.Flow.close peer ;
          equal int 1009 (Fun.Async.get reader) )
    ; test "a cancel in the reply to a Close frame closes the connection"
        (fun () ->
          let open Affect in
          with_socket
          @@ fun c peer ->
          (* The peer reads only the first bytes: the send holds the writer
             until the connection closes. *)
          let sender =
            Fun.Async.call (fun () ->
                closed_code (fun () ->
                    Ws.send c (Binary (String.make 10_000_000 'x')) ) )
          in
          is_false (Buf_reader.peek_eof (Buf_reader.of_flow peer)) ;
          let reader = Fun.Async.call (fun () -> Ws.recv c) in
          Caravan.Flow.write_string peer (client Close (code_reason 1000 "")) ;
          (* The reply to the Close frame waits for the writer. *)
          wait_closing c ;
          Fun.Async.cancel reader ;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get reader) ;
          within_2s (fun () -> Ws.close c) ;
          equal int 1000 (closed_code (fun () -> Ws.recv c)) ;
          ignore (Fun.Async.get sender) )
    ; (* A cancel in the middle of a frame closes the connection, because the
         stream is out of step. The fake time tests have the cancels of a
         recv. *)
      test "a cancel in the middle of a send closes with 1006" (fun () ->
          let open Affect in
          with_socket
          @@ fun c peer ->
          let sender =
            Fun.Async.call (fun () ->
                Ws.send c (Binary (String.make 10_000_000 'x')) )
          in
          (* When the first bytes come, the send is in its frame. *)
          let r = Buf_reader.of_flow peer in
          is_false (Buf_reader.peek_eof r) ;
          Fun.Async.cancel sender ;
          raises Fun.Async.Cancelled (fun () -> Fun.Async.get sender) ;
          equal int 1006 (closed_code (fun () -> Ws.send c (Text "hello"))) ;
          raises End_of_file (fun () -> Frame.read r ~masked:false ~max:max_int) )
    ]

(* The fields of the RFC 6455 §1.3 example, without [Origin]. *)
let request_fields =
  [ ("Host", "server.example.com")
  ; ("Upgrade", "websocket")
  ; ("Connection", "Upgrade")
  ; ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")
  ; ("Sec-WebSocket-Version", "13") ]

(* A request with [request_fields], where [fields] replace the fields of the
   same names and add the others. [""] removes a field. *)
let request ?(meth = Caravan.Method.GET) ?(version = Caravan.Version.Http_1_1)
    ?(authority = "server.example.com") fields =
  let set h (name, value) =
    if value = "" then Caravan.Headers.remove name h
    else Caravan.Headers.set name value h
  in
  let headers =
    List.fold_left set (Caravan.Headers.of_list request_fields) fields
  in
  Caravan.Request.make ~version ~authority ~headers meth "/chat"

(* The status and the fields of the response to [req], and its body. *)
let answer ?protocols ?origin req =
  let r = Ws.accept ?protocols ?origin req (fun _ -> ()) in
  let body =
    match Caravan.Response.kind r with
    | Body b ->
        Caravan.Body.to_string b
    | Upgrade _ ->
        ""
  in
  String.trim (Format.asprintf "%a@.%s" Caravan.Response.pp r body)

let handshake_tests =
  group "Handshake"
    [ test "accept_key answers the RFC 6455 example" (fun () ->
          equal string "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
            (Caravan_websocket.accept_key "dGhlIHNhbXBsZSBub25jZQ==") )
    ; test "accept answers a correct request with 101" (fun () ->
          expect (answer (request []))
          @@ __POS_OF__
               {|
            101 Switching Protocols (upgrade)
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            |} ;
          (* Tokens are case-insensitive, and a list can hold them. *)
          expect
            (answer
               (request
                  [ ("Upgrade", "WebSocket")
                  ; ("Connection", "keep-alive, upgrade") ] ) )
          @@ __POS_OF__
               {|
            101 Switching Protocols (upgrade)
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            |} )
    ; test "accept refuses an incorrect request" (fun () ->
          let first_line s = List.hd (String.split_on_char '\n' s) in
          let cases =
            [ ("POST", request ~meth:Caravan.Method.POST [])
            ; ("HTTP/1.0", request ~version:Caravan.Version.Http_1_0 [])
            ; ("no Upgrade", request [("Upgrade", "")])
            ; ("Upgrade: h2c", request [("Upgrade", "h2c")])
            ; ("no Connection token", request [("Connection", "keep-alive")])
            ; ("no key", request [("Sec-WebSocket-Key", "")])
            ; ("key of 5 bytes", request [("Sec-WebSocket-Key", "aGVsbG8=")])
            ; ( "key not base64"
              , request [("Sec-WebSocket-Key", "not base64 at all!!!")] ) ]
          in
          expect
            (String.concat "\n"
               (List.map
                  (fun (name, req) ->
                    let a = answer req in
                    name ^ ": " ^ first_line a ^ " | "
                    ^ List.nth (String.split_on_char '\n' a) 2 )
                  cases ) )
          @@ __POS_OF__
               {|
            POST: 400 Bad Request | WebSocket handshake: the method is not GET
            HTTP/1.0: 400 Bad Request | WebSocket handshake: the version is not HTTP/1.1
            no Upgrade: 400 Bad Request | WebSocket handshake: no Upgrade: websocket
            Upgrade: h2c: 400 Bad Request | WebSocket handshake: no Upgrade: websocket
            no Connection token: 400 Bad Request | WebSocket handshake: no Connection: upgrade
            no key: 400 Bad Request | WebSocket handshake: no Sec-WebSocket-Key
            key of 5 bytes: 400 Bad Request | WebSocket handshake: the key is not 16 bytes in base64
            key not base64: 400 Bad Request | WebSocket handshake: the key is not 16 bytes in base64
            |} )
    ; test "a negative max_message is not permitted" (fun () ->
          raises_match is_invalid_arg (fun () ->
              Ws.accept ~max_message:(-1) (request []) ignore ) ;
          raises_match is_invalid_arg (fun () ->
              Ws.Private.of_flow ~max_message:(-1)
                (Caravan.Flow.of_string "" (Buffer.create 16)) ) ;
          ignore (Ws.accept ~max_message:0 (request []) ignore) ;
          ignore
            (Ws.Private.of_flow ~max_message:0
               (Caravan.Flow.of_string "" (Buffer.create 16)) ) )
    ; test "accept asks for version 13" (fun () ->
          expect (answer (request [("Sec-WebSocket-Version", "8")]))
          @@ __POS_OF__
               {|
            426 Upgrade Required
            Upgrade: websocket
            Sec-WebSocket-Version: 13
            Content-Type: text/plain; charset=utf-8
            WebSocket handshake: the version must be 13
            |} ;
          expect (answer (request [("Sec-WebSocket-Version", "")]))
          @@ __POS_OF__
               {|
            426 Upgrade Required
            Upgrade: websocket
            Sec-WebSocket-Version: 13
            Content-Type: text/plain; charset=utf-8
            WebSocket handshake: the version must be 13
            |} )
    ; test "accept permits only the origin of the request by default" (fun () ->
          let status ?origin ?authority ?scheme o =
            let req = request ?authority [("Origin", o)] in
            let req =
              match scheme with
              | None ->
                  req
              | Some scheme ->
                  Caravan.Request.make ~scheme
                    ?authority:(Caravan.Request.authority req)
                    ~headers:(Caravan.Request.headers req)
                    Caravan.Method.GET "/chat"
            in
            List.hd (String.split_on_char ' ' (answer ?origin req))
          in
          expect
            (String.concat " "
               [ status "http://server.example.com"
               ; status "https://SERVER.example.com"
               ; status "http://server.example.com:8080"
               ; status "http://evil.example.com"
               ; status "null"
               ; status "server.example.com"
               ; status "http:"
               ; status "http:/"
               ; status "http:/xserver.example.com"
               ; status ~authority:"server.example.com:8080"
                   "http://server.example.com:8080"
               ; status ~origin:(fun o -> o = "null") "null"
               ; status ~origin:(fun _ -> false) "http://server.example.com" ] )
          @@ __POS_OF__ {|101 101 403 403 403 403 403 403 403 101 101 403|} ;
          (* Default ports, and the scheme of the request. *)
          expect
            (String.concat " "
               [ status ~authority:"server.example.com:443"
                   "https://server.example.com"
               ; status "https://server.example.com:443"
               ; status "http://server.example.com:80"
               ; status ~authority:"server.example.com:80"
                   "http://server.example.com"
               ; status ~authority:"server.example.com:443"
                   "http://server.example.com"
               ; status ~authority:"server.example.com:80"
                   "https://server.example.com"
               ; status "ftp://server.example.com"
               ; status ~scheme:"https" "https://server.example.com"
               ; status ~scheme:"https" "http://server.example.com" ] )
          @@ __POS_OF__ {|101 101 101 101 403 403 403 101 403|} )
    ; test
        "accept chooses the first subprotocol of the server that the client \
         offers" (fun () ->
          let chosen ?protocols offered =
            let a =
              answer ?protocols (request [("Sec-WebSocket-Protocol", offered)])
            in
            match
              List.find_opt
                (String.starts_with ~prefix:"Sec-WebSocket-Protocol")
                (String.split_on_char '\n' a)
            with
            | Some line ->
                line
            | None ->
                "none"
          in
          expect
            (String.concat "\n"
               [ chosen ~protocols:["superchat"; "chat"] "chat, superchat"
               ; chosen ~protocols:["chat"] "Chat, superchat"
               ; chosen ~protocols:["mqtt"] "chat"
               ; chosen ~protocols:["chat"] ""
               ; chosen "chat" ] )
          @@ __POS_OF__
               {|
             Sec-WebSocket-Protocol: superchat
             none
             none
             none
             none
             |} )
    ]

(* What recv raised in the handler of "/record". *)
let recorded = Atomic.make None

(* "/ticker" sets it after its first message, and "/late" when it starts. *)
let started = ref (Affect.Cell.Once.make ())

let echo c =
  let rec loop () =
    Ws.send c (Ws.recv c) ;
    loop ()
  in
  loop ()

let server_handler req =
  match Caravan.Request.path_only req with
  | "/echo" ->
      Ws.accept req echo
  | "/echo-fast" ->
      Ws.accept ~close_timeout:(ms 100) req echo
  | "/wait" ->
      Ws.accept req (fun _ -> Affect_unix.Mtime.wait_for (ms 10_000))
  | "/wait-slow-close" ->
      Ws.accept ~close_timeout:(ms 2000) req (fun _ ->
          Affect_unix.Mtime.wait_for (ms 10_000) )
  | "/ticker" ->
      Ws.accept ~close_timeout:(ms 300) req (fun c ->
          let rec loop () =
            Ws.send c (Text "tick") ;
            Affect.Cell.Once.try_set !started () ;
            Affect_unix.Mtime.wait_for (ms 10) ;
            loop ()
          in
          loop () )
  | "/late" ->
      Affect.Cell.Once.try_set !started () ;
      Affect.Action.invoke
        (Caravan.Conn_info.stopping (Option.get (Caravan.Request.conn req))) ;
      Ws.accept req echo
  | "/protocol" ->
      Ws.accept ~protocols:["chat"; "echo"] req (fun c ->
          Ws.send c (Text (Option.value ~default:"none" (Ws.protocol c))) )
  | "/boom" ->
      Ws.accept req (fun _ -> failwith "boom")
  | "/record" ->
      Ws.accept req (fun c ->
          match Ws.recv c with
          | _ ->
              ()
          | exception e ->
              Atomic.set recorded (Some (Printexc.to_string e)) ;
              raise e )
  | "/other" ->
      (* A Closed of a different connection is an exception of the handler. *)
      Ws.accept req (fun _ ->
          let input = client Close (code_reason 1000 "") in
          let other =
            Ws.Private.of_flow (Caravan.Flow.of_string input (Buffer.create 16))
          in
          ignore (Ws.recv other) )
  | "/child" ->
      (* The Cancelled of a child call is not a stop of the server. *)
      Ws.accept req (fun _ ->
          let open Affect in
          let child =
            Fun.Async.call (fun () -> Affect_unix.Mtime.wait_for (ms 10_000))
          in
          Fun.Async.cancel child ; Fun.Async.get child )
  | _ ->
      Caravan.Handler.not_found req

(* A WebSocket session with a server: the request for [path], with [first] in
   the same write; then [after], then [during ()]. The result is the response
   head, a "--" line, and the frames from the server until it closes the
   connection. If [reply], we answer a Close frame of the server. *)
let upgrade_request ?(first = "") path =
  String.concat "\r\n"
    [ "GET " ^ path ^ " HTTP/1.1"
    ; "Host: x"
    ; "Upgrade: websocket"
    ; "Connection: Upgrade"
    ; "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ=="
    ; "Sec-WebSocket-Version: 13"
    ; "Sec-WebSocket-Protocol: echo"
    ; ""
    ; first ]

let response_head r =
  let rec head acc =
    match Buf_reader.read_line r ~max:1000 with
    | "" ->
        List.rev acc
    | l ->
        head (l :: acc)
  in
  head []

let session ?first ?(after = "") ?(during = ignore) ?(reply = true) server path
    =
  let flow = Caravan.Tcp.connect (Caravan.Server.local_addr server) in
  Stdlib.Fun.protect ~finally:(fun () -> Caravan.Flow.close flow)
  @@ fun () ->
  Caravan.Flow.write_string flow (upgrade_request ?first path) ;
  let r = Buf_reader.of_flow flow in
  let head = response_head r in
  Caravan.Flow.write_string flow after ;
  during () ;
  let rec frames acc =
    match Frame.read r ~masked:false ~max:max_int with
    | exception End_of_file ->
        List.rev acc
    | f ->
        if reply && f.opcode = Close then
          Caravan.Flow.write_string flow (client Close f.payload) ;
        frames (show_frame f :: acc)
  in
  String.concat "\n" (head @ ("--" :: frames []))

let server_tests =
  group "Server"
    [ test "an echo, with a first frame in the write of the request" (fun () ->
          expect
            (Server_support.with_server ~handler:server_handler (fun server ->
                 session server "/echo" ~first:(client Text "early")
                   ~reply:false
                   ~after:
                     ( client Text "hi" ^ client Binary "\x01"
                     ^ client Close (code_reason 1000 "bye") ) ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            text "early"
            text "hi"
            binary "\001"
            close 1000
            |} )
    ; test "a handler that ends closes with 1000" (fun () ->
          expect
            (Server_support.with_server ~handler:server_handler (fun server ->
                 session server "/protocol" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Sec-WebSocket-Protocol: echo
            Connection: Upgrade
            --
            text "echo"
            close 1000
            |} )
    ; test "a handler that raises closes with 1011, and the server reports it"
        (fun () ->
          let errors = ref [] in
          let config =
            { Server_support.config with
              on_error= (fun e _ -> errors := Printexc.to_string e :: !errors)
            }
          in
          expect
            (Server_support.with_server ~config ~handler:server_handler
               (fun server -> session server "/boom" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1011
            |} ;
          equal (list string) ["Failure(\"boom\")"] !errors )
    ; test
        "a Cancelled from a child call closes with 1011, and the server \
         reports it" (fun () ->
          let errors = ref [] in
          let config =
            { Server_support.config with
              on_error= (fun e _ -> errors := Printexc.to_string e :: !errors)
            }
          in
          expect
            (Server_support.with_server ~config ~handler:server_handler
               (fun server -> session server "/child" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1011
            |} ;
          equal (list string)
            [ "Failure(\"Caravan_websocket: a call of the handler was \
               cancelled\")" ]
            !errors )
    ; test
        "a Closed of a different connection closes with 1011, and the server \
         reports it" (fun () ->
          let errors = ref [] in
          let config =
            { Server_support.config with
              on_error= (fun e _ -> errors := Printexc.to_string e :: !errors)
            }
          in
          expect
            (Server_support.with_server ~config ~handler:server_handler
               (fun server -> session server "/other" ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1011
            |} ;
          equal int 1 (List.length !errors) )
    ; test "a peer that leaves without a Close frame is not an error" (fun () ->
          let errors = ref [] in
          let config =
            { Server_support.config with
              on_error= (fun e _ -> errors := Printexc.to_string e :: !errors)
            }
          in
          Atomic.set recorded None ;
          Server_support.with_server ~config ~handler:server_handler
            (fun server ->
              let flow =
                Caravan.Tcp.connect (Caravan.Server.local_addr server)
              in
              Caravan.Flow.write_string flow (upgrade_request "/record") ;
              equal string "HTTP/1.1 101 Switching Protocols"
                (List.hd (response_head (Buf_reader.of_flow flow))) ;
              Caravan.Flow.close flow ;
              (* Waits at most 2 s for the handler to get Closed 1006. Then the
                 server stops: it waits for the end of the handler. *)
              let rec wait n =
                if Atomic.get recorded = None && n > 0 then (
                  Affect_unix.Mtime.wait_for (ms 10) ;
                  wait (n - 1) )
              in
              wait 200 ) ;
          equal (option string)
            (Some "Caravan_websocket__Connection.Closed(1006, \"\")")
            (Atomic.get recorded) ;
          equal (list string) [] !errors )
    ; test "a server that stops sends Close 1001, and the handler ends"
        (fun () ->
          let errors = ref [] in
          let config =
            { Server_support.config with
              shutdown_grace= ms 30_000
            ; on_error= (fun e _ -> errors := Printexc.to_string e :: !errors)
            }
          in
          expect
            (Server_support.with_server ~config ~handler:server_handler
               (fun server ->
                 let s =
                   within_2s (fun () ->
                       session server "/echo" ~during:(fun () ->
                           Caravan.Server.shutdown server ) )
                 in
                 (* The handler does not wait for the grace of 30 s. *)
                 within_2s (fun () -> Caravan.Server.wait server) ;
                 s ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1001 "server shutdown"
            |} ;
          equal (list string) [] !errors )
    ; test "a handler that is in recv when the server stops gets Closed 1001"
        (fun () ->
          let config = {Server_support.config with shutdown_grace= ms 30_000} in
          Atomic.set recorded None ;
          Server_support.with_server ~config ~handler:server_handler
            (fun server ->
              ignore
                (within_2s (fun () ->
                     session server "/record" ~during:(fun () ->
                         Caravan.Server.shutdown server ) ) ) ) ;
          (* The server waits for the handler. *)
          equal (option string)
            (Some
               "Caravan_websocket__Connection.Closed(1001, \"server shutdown\")"
            ) (Atomic.get recorded) )
    ; test
        "a stop before the handler gives its response: 503, and no WebSocket \
         connection" (fun () ->
          let open Affect in
          started := Cell.Once.make () ;
          let config = {Server_support.config with shutdown_grace= ms 30_000} in
          expect
            (Server_support.with_server ~config ~handler:server_handler
               (fun server ->
                 (* The response comes only after the stop. *)
                 let s =
                   Fun.Async.call (fun () ->
                       let flow =
                         Caravan.Tcp.connect (Caravan.Server.local_addr server)
                       in
                       Stdlib.Fun.protect ~finally:(fun () ->
                           Caravan.Flow.close flow )
                       @@ fun () ->
                       Caravan.Flow.write_string flow (upgrade_request "/late") ;
                       let r = Buf_reader.of_flow flow in
                       let head = response_head r in
                       let buf = Bytes.create 256 and body = Buffer.create 64 in
                       let rec rest () =
                         match Buf_reader.read_into r buf 0 256 with
                         | 0 ->
                             ()
                         | n ->
                             Buffer.add_subbytes body buf 0 n ;
                             rest ()
                       in
                       rest () ;
                       String.concat "\n" (head @ ["--"; Buffer.contents body]) )
                 in
                 within_2s (fun () -> Cell.Once.get !started) ;
                 Caravan.Server.shutdown server ;
                 within_2s (fun () -> Fun.Async.get s) ) )
          @@ __POS_OF__
               {|
            HTTP/1.1 503 Service Unavailable
            Content-Type: text/plain; charset=utf-8
            Content-Length: 20
            Connection: close
            --
            Service Unavailable
            |} )
    ; test "a stop that is already set when the handler starts: Close 1001"
        (fun () ->
          (* A stop can start while the engine writes the 101, after its last
             check: the handler then starts with the stop set. *)
          let input = client Close (code_reason 1001 "") in
          let out = Buffer.create 16 in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              within_2s (fun () ->
                  run_handler ~stopping:(Affect.Action.always ())
                    (fun c -> try ignore (Ws.recv c) with Ws.Closed _ -> ())
                    (Caravan.Flow.of_string input out) ) ) ;
          equal (list string) ["close 1001"] (frames (Buffer.contents out)) )
    ; test "the end of the handler stops the wait for the stop" (fun () ->
          let input = client Close (code_reason 1000 "") in
          let out = Buffer.create 16 in
          Affect_unix.Unix.main ~domain_count:1 (fun () ->
              within_2s (fun () ->
                  run_handler (fun _ -> ()) (Caravan.Flow.of_string input out) ) ) ;
          equal (list string) ["close 1000"] (frames (Buffer.contents out)) )
    ; test "a cancel of the handler after the peer left gives Cancelled"
        (fun () ->
          let open Affect in
          with_flows
          @@ fun server peer ->
          let left = Cell.Once.make () in
          let handler =
            Fun.Async.call (fun () ->
                run_handler
                  (fun c ->
                    (* The recv sees the end of the stream, and the connection
                       is closed. Thus a close after the cancel ends at once,
                       without a check of the cancel. *)
                    (try ignore (Ws.recv c) with Ws.Closed _ -> ()) ;
                    Cell.Once.try_set left () ;
                    Cell.Once.get (Cell.Once.make ()) )
                  server )
          in
          Stdlib.Fun.protect ~finally:(fun () -> Fun.Async.cancel handler)
          @@ fun () ->
          Caravan.Flow.close peer ;
          within_2s (fun () -> Cell.Once.get left) ;
          (* The cancel is not an error of the handler. *)
          Fun.Async.cancel handler ;
          within_2s (fun () ->
              raises Fun.Async.Cancelled (fun () -> Fun.Async.get handler) ) )
    ]

(* The connection on a fake time line *)

module Fake = Fake_time
module Mtime = Affect_unix.Mtime

let ( +: ) = Fake.( +: )

(* The spans of the fake time tests: different from each other and from the
   spans of the server. A test advances only to deadlines that it proved with
   [Fake.wait_mono_timer]. *)
let ws_idle = Mtime.Span.(19 * s)

let ws_close = Mtime.Span.(23 * s)

let ws_write = Mtime.Span.(29 * s)

let ws_read = Mtime.Span.(31 * s)

(* A connection with the server flow on the fake time line [f], and its peer.
   The spans are [idle], [close], [write] and [ws_read]. With [~idle:None], no
   idle timeout: then [close] waits for its own deadline. The connection reads
   and writes through [Server_support.counted]: [test] gets the counts too. When
   [test] ends, also by a failure, the fake time advances one day: then each
   wait on it ends. *)
let with_counted_socket ?(idle = Some ws_idle) ?(close = ws_close)
    ?(write = ws_write) f test =
  with_flows ~time:(Fake.mono f)
  @@ fun server peer ->
  let counted = Server_support.counted server in
  let c =
    Ws.Private.of_flow ~close_timeout:close ~write_timeout:write
      ~read_timeout:ws_read ?idle_timeout:idle counted.flow
  in
  Stdlib.Fun.protect
    ~finally:(fun () -> Fake.advance f Mtime.Span.day)
    (fun () -> test c peer counted)

(* [with_counted_socket] without the counts. *)
let with_fake_socket ?idle ?close ?write f test =
  with_counted_socket ?idle ?close ?write f (fun c peer _ -> test c peer)

let eventually = Server_support.eventually

(* [f ()], with its [Closed] exception as text. *)
let outcome f =
  match f () with
  | () ->
      "ended"
  | exception Ws.Closed {code; reason} ->
      Printf.sprintf "closed %d %S" code reason

(* [f ()] in a child call that gives [outcome f]. [test] gets the call, and the
   call is cancelled at the end. *)
let with_call f test =
  let call = Affect.Fun.Async.call (fun () -> outcome f) in
  Stdlib.Fun.protect
    ~finally:(fun () -> Affect.Fun.Async.cancel call)
    (fun () -> test call)

let get_2s = Server_support.get_2s

let ends_within_50ms = Fake.ends_within_50ms

let peer_frame peer =
  within_2s (fun () ->
      show_frame (Frame.read (Buf_reader.of_flow peer) ~masked:false ~max:125) )

let fake_time_tests =
  group "Connection on a fake time line"
    [ test "a frame 1 ns before the idle timeout comes, and then Close 1001"
        (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let first = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          Stdlib.Fun.protect ~finally:(fun () -> Affect.Fun.Async.cancel first)
          @@ fun () ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_idle) ;
          Fake.advance f Mtime.Span.(ws_idle - ns) ;
          Caravan.Flow.write_string peer (client Text "one") ;
          is_true ~msg:"the frame"
            (within_2s (fun () -> Affect.Fun.Async.get first) = Text "one") ;
          let t1 = t0 +: Mtime.Span.(ws_idle - ns) and m = Fake.mark f in
          with_call (fun () -> ignore (Ws.recv c))
          @@ fun second ->
          Fake.wait_mono_timer f ~since:m (t1 +: ws_idle) ;
          Fake.advance f ws_idle ;
          equal string "closed 1001 \"idle timeout\"" (get_2s second) ;
          equal string "close 1001" (peer_frame peer) )
    ; test "a frame that stops in the middle gets the read timeout" (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          let frame = client Text "hello" and next = client Text "two" in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let first = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          Stdlib.Fun.protect ~finally:(fun () -> Affect.Fun.Async.cancel first)
          @@ fun () ->
          (* The first read gets 2 bytes. The next read in the frame waits
             [read_timeout]. *)
          Caravan.Flow.write_string peer (String.sub frame 0 2) ;
          Fake.wait_mono_timer f ~since:m (t0 +: ws_read) ;
          Fake.advance f Mtime.Span.(ws_read - ns) ;
          let m = Fake.mark f in
          Caravan.Flow.write_string peer (String.sub frame 2 1) ;
          (* The new timer proves that the code took the byte. *)
          let t1 = t0 +: Mtime.Span.(ws_read - ns) in
          Fake.wait_mono_timer f ~since:m (t1 +: ws_read) ;
          (* A slow frame with no long pause gets no timeout: the frame passed
             the idle deadline and the first read deadline. *)
          Fake.advance f Mtime.Span.(ws_read - ns) ;
          Caravan.Flow.write_string peer
            (String.sub frame 3 (String.length frame - 3)) ;
          is_true ~msg:"the slow frame"
            (within_2s (fun () -> Affect.Fun.Async.get first) = Text "hello") ;
          (* The next frame stops in the middle. *)
          let t2 = t1 +: Mtime.Span.(ws_read - ns) and m = Fake.mark f in
          with_call (fun () -> ignore (Ws.recv c))
          @@ fun reader ->
          Caravan.Flow.write_string peer (String.sub next 0 2) ;
          Fake.wait_mono_timer f ~since:m (t2 +: ws_read) ;
          Fake.advance f ws_read ;
          equal string "close 1001" (peer_frame peer) ;
          (* Then the connection lingers for [close_timeout]. *)
          let t3 = t2 +: ws_read in
          Fake.wait_mono_timer f ~since:m (t3 +: ws_close) ;
          Fake.advance f ws_close ;
          equal string "closed 1001 \"read timeout\"" (get_2s reader) )
    ; test "a send that waits ends at the write timeout and not 1 ns before"
        (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c _peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          (* The peer never reads: only an advance can wake the send. *)
          with_call (fun () -> Ws.send c (Binary (String.make 10_000_000 'x')))
          @@ fun sender ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_write) ;
          Fake.advance f Mtime.Span.(ws_write - ns) ;
          is_false ~msg:"no end 1 ns before the write timeout"
            (ends_within_50ms sender) ;
          Fake.advance f Mtime.Span.ns ;
          equal string "closed 1006 \"\"" (get_2s sender) )
    ; test "each frame gets a new write deadline" (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c _peer ->
          Ws.send c (Text "one") ;
          Fake.advance f Mtime.Span.(ws_write - ns) ;
          let t1 = Fake.mono_now f and m = Fake.mark f in
          (* The peer never reads: the second frame waits from t1. *)
          with_call (fun () -> Ws.send c (Binary (String.make 10_000_000 'x')))
          @@ fun sender ->
          Fake.wait_mono_timer f ~since:m (t1 +: ws_write) ;
          Fake.advance f ws_write ;
          equal string "closed 1006 \"\"" (get_2s sender) )
    ; test "close ends at the close timeout and not 1 ns before" (fun () ->
          let f = Fake.make () in
          with_fake_socket ~idle:None f
          @@ fun c peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          with_call (fun () -> Ws.close c)
          @@ fun closer ->
          equal string "close 1000" (peer_frame peer) ;
          (* The peer does not reply: only an advance can end the close. *)
          Fake.wait_mono_timer f ~since:m (t0 +: ws_close) ;
          Fake.advance f Mtime.Span.(ws_close - ns) ;
          is_false ~msg:"no end 1 ns before the close timeout"
            (ends_within_50ms closer) ;
          Fake.advance f Mtime.Span.ns ;
          equal string "ended" (get_2s closer) ;
          equal string "closed 1006 \"\""
            (outcome (fun () -> ignore (Ws.recv c))) )
    ; test "close stops at its deadline if the peer sends without a stop"
        (fun () ->
          (* A flow that always has a frame: a read never waits for a deadline,
             thus only the check of the close deadline stops [close]. The reads
             advance the fake time: to 1 ns before the close deadline at read
             10, and to the deadline at read 20. *)
          let f = Fake.make () in
          let frame = client Binary "" and count = 1000 in
          let size = String.length frame in
          let reads = Atomic.make 0
          and served = Atomic.make 0
          and at_deadline = Atomic.make (-1) in
          let read buf off len =
            if Atomic.get served >= count * size then 0
            else begin
              let k = Atomic.fetch_and_add reads 1 + 1 in
              if k = 10 then Fake.advance f Mtime.Span.(ws_close - ns) ;
              if k = 20 then Fake.advance f Mtime.Span.ns ;
              let pos = Atomic.get served in
              let n = Int.min len (size - (pos mod size)) in
              Bytes.blit_string frame (pos mod size) buf off n ;
              Atomic.set served (pos + n) ;
              if k = 20 then Atomic.set at_deadline (Atomic.get served) ;
              n
            end
          in
          let out = Buffer.create 16 and lock = Mutex.create () in
          let ignore_deadline (_ : Mtime.t option) = () in
          let flow =
            Caravan.Flow.make ~time:(Fake.mono f) ~read
              ~read_now:(fun b o l -> Some (read b o l))
              ~write:(fun b o l ->
                Mutex.protect lock (fun () -> Buffer.add_subbytes out b o l) )
              ~readable:(Affect.Action.always ()) ~shutdown_send:ignore
              ~close:ignore ~set_read_deadline:ignore_deadline
              ~set_write_deadline:ignore_deadline ()
          in
          Affect_unix.Unix.main ~domain_count:Server_support.domain_count
            (fun () ->
              within_2s (fun () ->
                  Ws.close (Ws.Private.of_flow ~close_timeout:ws_close flow) ) ) ;
          equal (list string) ["close 1000"] (frames (Buffer.contents out)) ;
          is_true ~msg:"reads 1 ns before the close deadline"
            (Atomic.get reads >= 20) ;
          equal int ~msg:"bytes after the close deadline"
            (Atomic.get at_deadline) (Atomic.get served) )
    ; test "a fiber in recv stops at the time limit of close" (fun () ->
          let f = Fake.make () in
          with_fake_socket ~idle:None f
          @@ fun c peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          with_call (fun () -> ignore (Ws.recv c))
          @@ fun reader ->
          (* The reader waits inside a frame until its read deadline. *)
          Caravan.Flow.write_string peer (String.sub (client Text "hello") 0 2) ;
          Fake.wait_mono_timer f ~since:m (t0 +: ws_read) ;
          let m = Fake.mark f in
          with_call (fun () -> Ws.close c)
          @@ fun closer ->
          (* The close deadline is earlier: the read waits for it. *)
          Fake.wait_mono_timer f ~since:m (t0 +: ws_close) ;
          Fake.advance f Mtime.Span.(ws_close - ns) ;
          is_false ~msg:"no end 1 ns before the close timeout"
            (ends_within_50ms closer) ;
          Fake.advance f Mtime.Span.ns ;
          equal string "closed 1006 \"\"" (get_2s reader) ;
          equal string "ended" (get_2s closer) )
    ; test "a fiber blocked in send stops at the time limit of close" (fun () ->
          let f = Fake.make () in
          with_fake_socket ~idle:None f
          @@ fun c _peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          (* The peer reads nothing: the send waits until its write deadline. *)
          with_call (fun () -> Ws.send c (Binary (String.make 10_000_000 'x')))
          @@ fun sender ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_write) ;
          let m = Fake.mark f in
          with_call (fun () -> Ws.close c)
          @@ fun closer ->
          (* The close deadline is earlier: the write waits for it. *)
          Fake.wait_mono_timer f ~since:m (t0 +: ws_close) ;
          Fake.advance f Mtime.Span.(ws_close - ns) ;
          is_false ~msg:"no end 1 ns before the close timeout"
            (ends_within_50ms closer) ;
          Fake.advance f Mtime.Span.ns ;
          equal string "closed 1006 \"\"" (get_2s sender) ;
          equal string "ended" (get_2s closer) )
    ; test "a frame that is in part in the buffer gets the read timeout"
        (fun () ->
          let f = Fake.make () in
          with_counted_socket ~idle:None f
          @@ fun c peer counted ->
          (* One read gets the first frame and 1 byte of the next frame. Without
             an idle timeout, only the read timeout stops the wait. *)
          let input = client Text "one" ^ String.sub (client Text "two") 0 1 in
          Caravan.Flow.write_string peer input ;
          is_true (within_2s (fun () -> Ws.recv c) = Text "one") ;
          equal int ~msg:"the bytes in the buffer" (String.length input)
            (Atomic.get counted.read_bytes) ;
          let t0 = Fake.mono_now f and m = Fake.mark f in
          with_call (fun () -> ignore (Ws.recv c))
          @@ fun second ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_read) ;
          Fake.advance f ws_read ;
          equal string "close 1001" (peer_frame peer) ;
          (* Then the connection lingers for [close_timeout]. *)
          let t1 = t0 +: ws_read in
          Fake.wait_mono_timer f ~since:m (t1 +: ws_close) ;
          Fake.advance f ws_close ;
          equal string "closed 1001 \"read timeout\"" (get_2s second) )
    ; test "the read timeout does not apply between frames" (fun () ->
          let f = Fake.make () in
          let idle = Mtime.Span.(37 * s) in
          with_fake_socket ~idle:(Some idle) f
          @@ fun c peer ->
          (* A read inside the first frame sets the read deadline. *)
          let frame = client Text "one" in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let first = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          Stdlib.Fun.protect ~finally:(fun () -> Affect.Fun.Async.cancel first)
          @@ fun () ->
          Caravan.Flow.write_string peer (String.sub frame 0 2) ;
          Fake.wait_mono_timer f ~since:m (t0 +: ws_read) ;
          Caravan.Flow.write_string peer
            (String.sub frame 2 (String.length frame - 2)) ;
          is_true (within_2s (fun () -> Affect.Fun.Async.get first) = Text "one") ;
          (* The second recv waits past the read deadline of the first frame.
             Its idle timer, later than the read deadline, proves the wait. *)
          let m = Fake.mark f in
          with_call (fun () -> ignore (Ws.recv c))
          @@ fun second ->
          Fake.wait_mono_timer f ~since:m (t0 +: idle) ;
          Fake.advance f ws_read ;
          is_false ~msg:"no end after the read deadline"
            (ends_within_50ms second) ;
          Caravan.Flow.write_string peer (client Text "two") ;
          equal string "ended" (get_2s second) )
    ; test "each frame gets a new idle timeout" (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let reader = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          Stdlib.Fun.protect ~finally:(fun () -> Affect.Fun.Async.cancel reader)
          @@ fun () ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_idle) ;
          (* A Ping 1 ns before the idle deadline. The new timer proves that the
             code took it. *)
          Fake.advance f Mtime.Span.(ws_idle - ns) ;
          let t1 = Fake.mono_now f and m = Fake.mark f in
          Caravan.Flow.write_string peer (client Ping "") ;
          Fake.wait_mono_timer f ~since:m (t1 +: ws_idle) ;
          equal string "pong \"\"" (peer_frame peer) ;
          Fake.advance f Mtime.Span.(ws_idle - ns) ;
          Caravan.Flow.write_string peer (client Text "end") ;
          is_true
            (within_2s (fun () -> Affect.Fun.Async.get reader) = Text "end") )
    ; test "close stops at a shorter idle timeout" (fun () ->
          let f = Fake.make () in
          (* The idle timeout is shorter than the close timeout. *)
          with_fake_socket f
          @@ fun c _peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          with_call (fun () -> Ws.close c)
          @@ fun closer ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_idle) ;
          Fake.advance f Mtime.Span.(ws_idle - ns) ;
          is_false ~msg:"no end 1 ns before the idle timeout"
            (ends_within_50ms closer) ;
          Fake.advance f Mtime.Span.ns ;
          equal string "ended" (get_2s closer) )
    ; test "close keeps the shorter write timeout of a send that waits"
        (fun () ->
          let f = Fake.make () in
          (* The write timeout is shorter than the close timeout. *)
          with_fake_socket ~close:Mtime.Span.(37 * s) f
          @@ fun c _peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          (* The peer reads nothing: the send waits until its write deadline. *)
          with_call (fun () -> Ws.send c (Binary (String.make 10_000_000 'x')))
          @@ fun sender ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_write) ;
          with_call (fun () -> Ws.close c)
          @@ fun closer ->
          (* The close sets its deadline before the close starts. *)
          wait_closing c ;
          Fake.advance f Mtime.Span.(ws_write - ns) ;
          is_false ~msg:"no end 1 ns before the write timeout"
            (ends_within_50ms sender) ;
          Fake.advance f Mtime.Span.ns ;
          equal string "closed 1006 \"\"" (get_2s sender) ;
          equal string "ended" (get_2s closer) )
    ; test "close keeps its time limit with a long idle timeout" (fun () ->
          let f = Fake.make () in
          (* The idle timeout is longer than the close timeout. *)
          with_fake_socket ~idle:(Some Mtime.Span.(37 * s)) f
          @@ fun c _peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          with_call (fun () -> Ws.close c)
          @@ fun closer ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_close) ;
          Fake.advance f Mtime.Span.(ws_close - ns) ;
          is_false ~msg:"no end 1 ns before the close timeout"
            (ends_within_50ms closer) ;
          Fake.advance f Mtime.Span.ns ;
          equal string "ended" (get_2s closer) )
    ; test
        "close keeps its time limit when the reply to the peer waits for a send"
        (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          (* The peer reads nothing: the send holds the writer until its write
             deadline. *)
          with_call (fun () -> Ws.send c (Binary (String.make 10_000_000 'x')))
          @@ fun sender ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_write) ;
          (* The reply to the Close frame of the peer waits for the writer. *)
          with_call (fun () -> ignore (Ws.recv c))
          @@ fun reader ->
          Caravan.Flow.write_string peer (client Close (code_reason 1000 "")) ;
          wait_closing c ;
          let m = Fake.mark f in
          with_call (fun () -> Ws.close c)
          @@ fun closer ->
          (* The close deadline is earlier: the write waits for it. *)
          Fake.wait_mono_timer f ~since:m (t0 +: ws_close) ;
          Fake.advance f Mtime.Span.(ws_close - ns) ;
          is_false ~msg:"no end 1 ns before the close timeout"
            (ends_within_50ms closer) ;
          Fake.advance f Mtime.Span.ns ;
          equal string "ended" (get_2s closer) ;
          equal string "closed 1006 \"\"" (get_2s sender) ;
          equal string "closed 1006 \"\"" (get_2s reader) )
    ; test "a second close does not make the close deadline later" (fun () ->
          let f = Fake.make () in
          with_counted_socket ~idle:None f
          @@ fun c peer counted ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          with_call (fun () -> Ws.close c)
          @@ fun first ->
          equal string "close 1000" (peer_frame peer) ;
          Fake.wait_mono_timer f ~since:m (t0 +: ws_close) ;
          (* The second close comes 1 s after the first. A close sets the write
             deadline: the count proves that the second close set its
             deadline. *)
          Fake.advance f Mtime.Span.s ;
          let m = Fake.mark f and w = Atomic.get counted.write_deadlines in
          with_call (fun () -> Ws.close c)
          @@ fun second ->
          eventually ~msg:"the second close set its deadline" (fun () ->
              Atomic.get counted.write_deadlines > w ) ;
          (* After a frame, the read waits again, until the first deadline. *)
          Caravan.Flow.write_string peer (client Text "x") ;
          Fake.wait_mono_timer f ~since:m (t0 +: ws_close) ;
          Fake.advance f Mtime.Span.(ws_close - s) ;
          equal string "ended" (get_2s first) ;
          equal string "ended" (get_2s second) )
    ; test "a fiber in recv reads the reply to close" (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          with_call (fun () -> ignore (Ws.recv c))
          @@ fun reader ->
          (* The idle timer proves that the reader waits. *)
          Fake.wait_mono_timer f ~since:m (t0 +: ws_idle) ;
          with_call (fun () -> Ws.close ~code:1001 c)
          @@ fun closer ->
          equal string "close 1001" (peer_frame peer) ;
          Caravan.Flow.write_string peer (client Close (code_reason 1001 "")) ;
          equal string "ended" (get_2s closer) ;
          equal string "closed 1001 \"\"" (get_2s reader) )
    ; test "a send that waits for a different send raises Closed after close"
        (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          (* The peer reads nothing yet: [big] fills the socket buffers. Its
             write timer proves that it waits. *)
          let size = 10_000_000 in
          with_call (fun () -> Ws.send c (Binary (String.make size 'x')))
          @@ fun big ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_write) ;
          (* No proof shows that [late] waits for the writer before the close
             starts. If it starts after, it raises at once: the result is the
             same. *)
          with_call (fun () -> Ws.send c (Text "late"))
          @@ fun late ->
          with_call (fun () -> Ws.close c)
          @@ fun closer ->
          wait_closing c ;
          let r = Buf_reader.of_flow peer in
          let next () =
            within_2s (fun () -> Frame.read r ~masked:false ~max:max_int)
          in
          equal int size (String.length (next ()).payload) ;
          equal string "close 1000" (show_frame (next ())) ;
          Caravan.Flow.write_string peer (client Close (code_reason 1000 "")) ;
          equal string "ended" (get_2s big) ;
          equal string "closed 1000 \"\"" (get_2s late) ;
          equal string "ended" (get_2s closer) )
    ; test "a second close waits for the end of the first closing handshake"
        (fun () ->
          let f = Fake.make () in
          with_counted_socket f
          @@ fun c peer counted ->
          with_call (fun () -> Ws.close ~code:1001 c)
          @@ fun first ->
          let r = Buf_reader.of_flow peer in
          equal string "close 1001"
            (within_2s (fun () ->
                 show_frame (Frame.read r ~masked:false ~max:125) ) ) ;
          (* The count proves that the second close set its deadline. No proof
             shows that it then waits: the check of 50 ms can miss a bug, but it
             cannot fail on correct code. *)
          let w = Atomic.get counted.write_deadlines in
          with_call (fun () -> Ws.close c)
          @@ fun second ->
          eventually ~msg:"the second close set its deadline" (fun () ->
              Atomic.get counted.write_deadlines > w ) ;
          is_false ~msg:"the second close waits" (ends_within_50ms second) ;
          Caravan.Flow.write_string peer (client Close (code_reason 1001 "")) ;
          equal string "ended" (get_2s first) ;
          equal string "ended" (get_2s second) ;
          raises End_of_file (fun () -> Frame.read r ~masked:false ~max:125) )
    ; test "a cancel in the linger after a protocol error closes the connection"
        (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          (* The peer does not close, and the time does not advance: only the
             cancel ends the linger. *)
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let reader = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          Caravan.Flow.write_string peer (client Continuation "x") ;
          let r = Buf_reader.of_flow peer in
          equal string "close 1002"
            (within_2s (fun () ->
                 show_frame (Frame.read r ~masked:false ~max:125) ) ) ;
          (* The end of the stream and the timer of the linger prove that the
             server is in the linger. *)
          is_true (within_2s (fun () -> Buf_reader.peek_eof r)) ;
          Fake.wait_mono_timer f ~since:m (t0 +: ws_close) ;
          Affect.Fun.Async.cancel reader ;
          raises Affect.Fun.Async.Cancelled (fun () ->
              Affect.Fun.Async.get reader ) ;
          equal int 1002
            (within_2s (fun () -> closed_code (fun () -> Ws.recv c))) )
    ; test "the linger after a protocol error discards at most 1 MiB" (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          (* The peer sends 3 MiB after a frame that is too large, and does not
             close. The time does not advance: only the limit of 1 MiB stops the
             linger. *)
          with_call (fun () -> ignore (Ws.recv c))
          @@ fun reader ->
          let header =
            of_hex "82ff" ^ "\x00\x00\x00\x00\x01\x40\x00\x00"
            ^ "\x01\x02\x03\x04"
          in
          let writer =
            Affect.Fun.Async.call (fun () ->
                try
                  Caravan.Flow.write_string peer
                    (header ^ String.make (3 * 1024 * 1024) 'x')
                with Caravan.Flow.Closed -> () )
          in
          equal string "closed 1009 \"message is too large\"" (get_2s reader) ;
          Caravan.Flow.close peer ;
          within_2s (fun () -> Affect.Fun.Async.get writer) )
    ; test "a cancel between messages does not close" (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let reader = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          (* The idle timer proves that the reader waits. *)
          Fake.wait_mono_timer f ~since:m (t0 +: ws_idle) ;
          Affect.Fun.Async.cancel reader ;
          raises Affect.Fun.Async.Cancelled (fun () ->
              Affect.Fun.Async.get reader ) ;
          Caravan.Flow.write_string peer (client Text "hello") ;
          is_true (within_2s (fun () -> Ws.recv c) = Text "hello") )
    ; test "a cancel in the middle of a received frame closes with 1006"
        (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let reader = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          let frame = client Text "hello" in
          Caravan.Flow.write_string peer (String.sub frame 0 3) ;
          (* The read timer proves that the reader took the bytes and waits
             inside the frame. *)
          Fake.wait_mono_timer f ~since:m (t0 +: ws_read) ;
          Affect.Fun.Async.cancel reader ;
          raises Affect.Fun.Async.Cancelled (fun () ->
              Affect.Fun.Async.get reader ) ;
          Caravan.Flow.write_string peer
            (String.sub frame 3 (String.length frame - 3) ^ client Text "next") ;
          equal int 1006
            (within_2s (fun () -> closed_code (fun () -> Ws.recv c))) )
    ; test
        "a cancel in the wait to send a Pong inside a fragmented message \
         closes with 1006" (fun () ->
          let f = Fake.make () in
          with_counted_socket f
          @@ fun c peer counted ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          (* The peer reads nothing: [big] keeps the writer. Its write timer
             proves that it waits. *)
          with_call (fun () -> Ws.send c (Binary (String.make 10_000_000 'x')))
          @@ fun big ->
          Fake.wait_mono_timer f ~since:m (t0 +: ws_write) ;
          let reader = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          let input = client ~fin:false Text "Hel" ^ client Ping "p" in
          Caravan.Flow.write_string peer input ;
          (* The count proves that the reader took the fragment and the Ping.
             Then it waits for the writer to send the Pong. *)
          eventually ~msg:"the reader took the input" (fun () ->
              Atomic.get counted.read_bytes >= String.length input ) ;
          Affect.Fun.Async.cancel reader ;
          raises Affect.Fun.Async.Cancelled (fun () ->
              Affect.Fun.Async.get reader ) ;
          Caravan.Flow.write_string peer (client Continuation "lo") ;
          (* Without the fix, the next recv waits for the write deadline of
             [big]. The time does not advance. *)
          equal int 1006
            (within_2s (fun () -> closed_code (fun () -> Ws.recv c))) ;
          equal string "closed 1006 \"\"" (get_2s big) )
    ; test "a cancel inside a fragmented message closes with 1006" (fun () ->
          let f = Fake.make () in
          with_fake_socket f
          @@ fun c peer ->
          let t0 = Fake.mono_now f and m = Fake.mark f in
          let reader = Affect.Fun.Async.call (fun () -> Ws.recv c) in
          Fake.wait_mono_timer f ~since:m (t0 +: ws_idle) ;
          (* The fragment comes 1 s later: then the idle deadline of the next
             frame is different. Its timer proves that the reader took the
             fragment. *)
          Fake.advance f Mtime.Span.s ;
          let t1 = Fake.mono_now f and m = Fake.mark f in
          Caravan.Flow.write_string peer (client ~fin:false Text "Hel") ;
          Fake.wait_mono_timer f ~since:m (t1 +: ws_idle) ;
          Affect.Fun.Async.cancel reader ;
          raises Affect.Fun.Async.Cancelled (fun () ->
              Affect.Fun.Async.get reader ) ;
          Caravan.Flow.write_string peer (client Continuation "lo") ;
          equal int 1006
            (within_2s (fun () -> closed_code (fun () -> Ws.recv c))) )
    ; test
        "the end of the handler does not stop the closing handshake of a stop"
        (fun () ->
          let open Affect in
          let f = Fake.make () in
          with_flows ~time:(Fake.mono f)
          @@ fun server peer ->
          Stdlib.Fun.protect ~finally:(fun () -> Fake.advance f Mtime.Span.day)
          @@ fun () ->
          let stop = Cell.Once.make () and raised = Cell.Once.make () in
          let close = Mtime.Span.(37 * s) in
          let handler =
            Fun.Async.call (fun () ->
                run_handler ~stopping:(Cell.Once.get' stop) ~close_timeout:close
                  (fun c ->
                    (* The peer does not read yet: the send holds the writer,
                       and the Close frame of the stop waits for it. *)
                    ignore
                      (Fun.Async.call (fun () ->
                           try Ws.send c (Binary (String.make 10_000_000 'x'))
                           with Ws.Closed _ -> () ) ) ;
                    Cell.Once.get stop ;
                    wait_closing c ;
                    Cell.Once.try_set raised () ;
                    raise (Ws.Closed {code= 1001; reason= "server shutdown"}) )
                  server )
          in
          Stdlib.Fun.protect ~finally:(fun () -> Fun.Async.cancel handler)
          @@ fun () ->
          let r = Buf_reader.of_flow peer in
          is_false (within_2s (fun () -> Buf_reader.peek_eof r)) ;
          let t0 = Fake.mono_now f and m = Fake.mark f in
          Cell.Once.try_set stop () ;
          within_2s (fun () -> Cell.Once.get raised) ;
          (* The read of the close is the only read: its timer proves that after
             the end of the handler, the connection waits for the end of the
             closing handshake. Then the peer reads. *)
          Fake.wait_mono_timer f ~since:m (t0 +: close) ;
          let first =
            within_2s (fun () -> Frame.read r ~masked:false ~max:max_int)
          in
          let second =
            within_2s (fun () ->
                match Frame.read r ~masked:false ~max:125 with
                | f ->
                    Caravan.Flow.write_string peer (client Close f.payload) ;
                    show_frame f
                | exception End_of_file ->
                    "end of the stream" )
          in
          within_2s (fun () -> Fun.Async.get handler) ;
          equal (list string)
            ["binary"; "close 1001 \"server shutdown\""]
            [opcode_name first.opcode; second] )
    ; test "an upgraded connection keeps the time line of the server" (fun () ->
          let f = Fake.make () in
          (* The handler reads the first frame. Then it waits for a frame until
             the idle timeout. *)
          let first = Affect.Cell.Once.make () in
          let handler req =
            Ws.accept ~close_timeout:ws_close ~write_timeout:ws_write
              ~read_timeout:ws_read ~idle_timeout:ws_idle req (fun c ->
                Affect.Cell.Once.try_set first (Ws.recv c) ;
                ignore (Ws.recv c) )
          in
          let t0 = Fake.mono_now f and m = Fake.mark f in
          Server_support.with_fake_time_server f ~handler
          @@ fun server ->
          let flow = Caravan.Tcp.connect (Caravan.Server.local_addr server) in
          Stdlib.Fun.protect ~finally:(fun () -> Caravan.Flow.close flow)
          @@ fun () ->
          Caravan.Flow.set_read_deadline flow
            (Some (Mtime.now () +: Mtime.Span.(2 * s))) ;
          (* The first frame goes in the write of the request: the connection
             gets it through Flow.prepend. *)
          Caravan.Flow.write_string flow
            (upgrade_request ~first:(client Text "early") "/") ;
          let r = Buf_reader.of_flow flow in
          equal string "HTTP/1.1 101 Switching Protocols"
            (List.hd (response_head r)) ;
          Fake.wait_mono_timer f ~since:m (t0 +: ws_idle) ;
          Fake.advance f ws_idle ;
          equal string "close 1001"
            (show_frame (Frame.read r ~masked:false ~max:125)) ;
          is_true ~msg:"the first frame"
            (within_2s (fun () -> Affect.Cell.Once.get first) = Text "early") )
    ]

(* The server on a fake time line *)

(* [Server_support.fake_config] with an idle timeout of 41 s: the default close
   timeout of a WebSocket connection is 5 s, the idle timeout of [fake_config].
   The errors go to [errors]. *)
let fake_server_config f errors =
  { (Server_support.fake_config f) with
    idle_timeout= Mtime.Span.(41 * s)
  ; on_error= (fun e _ -> errors := Printexc.to_string e :: !errors) }

(* [session server path] in a child call. [test] gets the call when the response
   head came. The call is cancelled at the end. *)
let with_session ?reply server path test =
  let head = Affect.Cell.Once.make () in
  let call =
    Affect.Fun.Async.call (fun () ->
        session ?reply server path ~during:(fun () ->
            Affect.Cell.Once.try_set head () ) )
  in
  Stdlib.Fun.protect ~finally:(fun () -> Affect.Fun.Async.cancel call)
  @@ fun () ->
  within_2s (fun () -> Affect.Cell.Once.get head) ;
  test call

(* The Close frame of the server in the result of [session]. *)
let server_close s = List.nth (String.split_on_char '\n' s) 5

let fake_server_tests =
  group "Server on a fake time line"
    [ test
        "a handler that does not use the connection gets Close 1001, then the \
         cancel" (fun () ->
          let f = Fake.make () and errors = ref [] in
          Server_support.with_fake_time_server f
            ~config:(fake_server_config f errors) ~handler:server_handler
            (fun server ->
              with_session server "/wait"
              @@ fun exchange ->
              let t0 = Fake.mono_now f and m = Fake.mark f in
              Caravan.Server.shutdown server ;
              (* The peer replies to the Close frame, and the session ends. The
                 handler waits until the grace. *)
              let s = within_2s (fun () -> Affect.Fun.Async.get exchange) in
              equal string "close 1001 \"server shutdown\"" (server_close s) ;
              let grace = Server_support.grace_g in
              with_call (fun () -> Caravan.Server.wait server)
              @@ fun wait ->
              Fake.wait_mono_timer f ~since:m (t0 +: grace) ;
              Fake.advance f Mtime.Span.(grace - ns) ;
              is_false ~msg:"no end 1 ns before the grace"
                (ends_within_50ms wait) ;
              Fake.advance f Mtime.Span.ns ;
              equal string "ended" (get_2s wait) ) ;
          equal (list string) [] !errors )
    ; test
        "a cancel at the grace while the stop waits for the reply is not an \
         error" (fun () ->
          let f = Fake.make () and errors = ref [] in
          (* The grace of 1 s ends before the close timeout of 2 s of the
             handler. *)
          let config =
            {(fake_server_config f errors) with shutdown_grace= Mtime.Span.s}
          in
          Server_support.with_fake_time_server f ~config ~handler:server_handler
            (fun server ->
              with_session ~reply:false server "/wait-slow-close"
              @@ fun exchange ->
              let t0 = Fake.mono_now f and m = Fake.mark f in
              Caravan.Server.shutdown server ;
              (* The close timer proves that the stop waits for the reply. *)
              Fake.wait_mono_timer f ~since:m (t0 +: Mtime.Span.(2 * s)) ;
              Fake.wait_mono_timer f ~since:m (t0 +: Mtime.Span.s) ;
              Fake.advance f Mtime.Span.s ;
              let s = within_2s (fun () -> Affect.Fun.Async.get exchange) in
              equal string "close 1001 \"server shutdown\"" (server_close s) ) ;
          equal (list string) [] !errors )
    ; test
        "a peer that does not reply to Close 1001 gets the end of the stream \
         at the close timeout" (fun () ->
          let f = Fake.make () and errors = ref [] in
          Server_support.with_fake_time_server f
            ~config:(fake_server_config f errors) ~handler:server_handler
            (fun server ->
              with_session ~reply:false server "/echo-fast"
              @@ fun exchange ->
              let t0 = Fake.mono_now f and m = Fake.mark f in
              Caravan.Server.shutdown server ;
              let close = Mtime.Span.(100 * ms) in
              Fake.wait_mono_timer f ~since:m (t0 +: close) ;
              Fake.advance f Mtime.Span.(close - ns) ;
              is_false ~msg:"no end 1 ns before the close timeout"
                (ends_within_50ms exchange) ;
              Fake.advance f Mtime.Span.ns ;
              let s = within_2s (fun () -> Affect.Fun.Async.get exchange) in
              equal string "close 1001 \"server shutdown\"" (server_close s) ) ;
          equal (list string) [] !errors )
    ; test "a handler that only sends gets the full closing handshake"
        (fun () ->
          let f = Fake.make () and errors = ref [] in
          started := Affect.Cell.Once.make () ;
          Server_support.with_fake_time_server f
            ~config:(fake_server_config f errors) ~handler:server_handler
            (fun server ->
              with_session ~reply:false server "/ticker"
              @@ fun exchange ->
              within_2s (fun () -> Affect.Cell.Once.get !started) ;
              let t0 = Fake.mono_now f and m = Fake.mark f in
              Caravan.Server.shutdown server ;
              (* The close waits for the reply until the close timeout. *)
              let close = Mtime.Span.(300 * ms) in
              Fake.wait_mono_timer f ~since:m (t0 +: close) ;
              Fake.advance f Mtime.Span.(close - ns) ;
              is_false ~msg:"no end 1 ns before the close timeout"
                (ends_within_50ms exchange) ;
              Fake.advance f Mtime.Span.ns ;
              let s = within_2s (fun () -> Affect.Fun.Async.get exchange) in
              let frames =
                List.filteri (fun i _ -> i > 4) (String.split_on_char '\n' s)
              in
              is_true ~msg:"one or more ticks" (List.hd frames = "text \"tick\"") ;
              equal (list string)
                ["close 1001 \"server shutdown\""]
                (List.filter (fun f -> f <> "text \"tick\"") frames) ;
              equal string "close 1001 \"server shutdown\""
                (List.nth frames (List.length frames - 1)) ) ;
          equal (list string) [] !errors ) ]

let () =
  exit
    (run "websocket"
       [ sha1_tests
       ; base64_tests
       ; frame_tests
       ; utf8_tests
       ; connection_tests
       ; close_tests
       ; socket_tests
       ; handshake_tests
       ; server_tests
       ; fake_time_tests
       ; fake_server_tests ] )
