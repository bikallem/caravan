(* The tests of caravan.websocket. The tests of a connection or a server run in
   the simulator of weft (Sim_support): 1 seed and 1 domain, except a test whose
   name starts with "race:" (100 seeds on 4 domains). A test that checks a time
   runs in manual mode. *)

open Windtrap
open Weft
open Caravan.Private
open Caravan_websocket.Private
module Reader = Weft_flow.Reader
module Writer = Weft_flow.Writer
module Flow = Caravan.Flow

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
  let w = Writer.create (Flow.of_string "" out) in
  Frame.write ?mask w f ; Writer.flush w ; Buffer.contents out

(* [reader ~size ~chunk s] is a reader of [s] with a buffer of [size] bytes. A
   read of the flow gives at most [chunk] bytes. *)
let reader ?size ?chunk s =
  Reader.create ?size (Flow.of_string ?chunk s (Buffer.create 0))

(* [at_end r] is [true] iff [r] holds no byte and its stream ended. It waits for
   a byte or the end. *)
let at_end r =
  Reader.buffered r = 0
  && match Reader.ensure r 1 with () -> false | exception End_of_file -> true

(* What [Frame.read] makes of [input]: a frame, or the error. *)
let outcome ?(masked = true) ?(max = 1000) input =
  match Frame.read (reader input) ~masked ~max with
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
            reader ~size:16 ~chunk:5
              (String.concat "" (List.map (fun (_, _, e) -> e) unmasked))
          in
          equal (list frame)
            (List.map (fun (fr, _, _) -> fr) unmasked)
            (List.map (fun _ -> Frame.read r ~masked:false ~max:65536) unmasked) ;
          is_true (at_end r) ;
          List.iter
            (fun (fr, m, e) ->
              if m <> None then
                equal frame fr (Frame.read (reader e) ~masked:true ~max:5) )
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
            ; ("2^62", "82 ff 40 00 00 00 00 00 00 00")
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
2^62: too big
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
          let r = reader (header ^ "short") in
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
          let r = reader ~size ~chunk (encode ?mask fr) in
          (* The parts that on_data gets: not empty, in order, and the payload,
             unmasked. None for a control frame. *)
          let parts = Buffer.create 64 and empty = ref false in
          let on_data op buf off len =
            equal string (opcode_name opcode) (opcode_name op) ;
            if len = 0 then empty := true ;
            Buffer.add_subbytes parts buf off len
          in
          equal frame fr (Frame.read ~on_data r ~masked:(mask <> None) ~max:300) ;
          is_true (at_end r) ;
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
  let r = reader out in
  let rec loop acc =
    match Frame.read r ~masked:false ~max:max_int with
    | f ->
        loop (show_frame f :: acc)
    | exception End_of_file ->
        List.rev acc
  in
  loop []

(* Runs [app] on a server connection that reads [input], in the simulator. The
   result is the lines that [app] logs, a "--" line, then the frames that the
   server sent. *)
let converse ?max_message ?chunk input app =
  let out = Buffer.create 64 and log = ref [] in
  let note s = log := s :: !log in
  Sim_support.run (fun _ ->
      app note
        (Ws.Private.of_flow ?max_message
           (Caravan.Flow.of_string ?chunk input out) ) ) ;
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
    ; test "an incorrect byte gives 1007 before the end of its frame" (fun () ->
          (* Autobahn 6.4.3: the rest of the frame does not come. *)
          let frame =
            client Text (of_hex "cebae1bdb9cf83cebcceb5f4908080656469746564")
          in
          expect (converse (String.sub frame 0 (6 + 15)) echo)
          @@ __POS_OF__
               {|
            closed 1007 "text is not UTF-8"
            --
            close 1007
            |} )
    ; test "an incorrect byte in a continuation gives 1007 before its end"
        (fun () ->
          (* Autobahn 6.4.4, in a continuation frame. *)
          let frame =
            client Continuation
              (of_hex "cebae1bdb9cf83cebcceb5f4908080656469746564")
          in
          expect
            (converse
               (client ~fin:false Text "ok" ^ String.sub frame 0 (6 + 13))
               echo )
          @@ __POS_OF__
               {|
            closed 1007 "text is not UTF-8"
            --
            close 1007
            |} )
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

(* A flow in memory that reads with [read] and writes with [write]. The default
   [write] drops the bytes. The flow ignores the deadlines and the timeouts. *)
let memory_flow ?(write = fun _ _ _ -> ()) read =
  Caravan.Flow.make ~read
    ~read_now:(fun b o l -> Some (read b o l))
    ~write ~readable:(always ()) ~shutdown_send:ignore ~close:ignore
    ~set_read_deadline:ignore ~set_write_deadline:ignore ()

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
            ; ("text that ends inside a sequence", client Text "\xe2\x82", None)
            ; ( "fragments not UTF-8"
              , client ~fin:false Text "\xe2\x82" ^ client Continuation "x"
              , None )
            ; ( "fragments that end inside a sequence"
              , client ~fin:false Text "\xe2\x82" ^ client Continuation ""
              , None )
              (* A protocol error comes before the UTF-8 of its frame. *)
            ; ( "message in a message, not UTF-8"
              , client ~fin:false Text "a" ^ client Text "\xff"
              , None )
            ; ("continuation first, not UTF-8", client Continuation "\xff", None)
            ; ( "text in a binary message, not UTF-8"
              , client ~fin:false Binary "a" ^ client Text "\xff"
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
            text that ends inside a sequence: closed 1007 "text is not UTF-8" | -- | close 1007
            fragments not UTF-8: closed 1007 "text is not UTF-8" | -- | close 1007
            fragments that end inside a sequence: closed 1007 "text is not UTF-8" | -- | close 1007
            message in a message, not UTF-8: closed 1002 "new message inside a fragmented message" | -- | close 1002
            continuation first, not UTF-8: closed 1002 "continuation frame without a message" | -- | close 1002
            text in a binary message, not UTF-8: closed 1002 "new message inside a fragmented message" | -- | close 1002
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
    ; test "the default max_message is 8 MiB" (fun () ->
          (* Only the header of each frame: an 8 MiB frame passes the size
             check, then the input ends. *)
          let header n =
            let b = Bytes.create 8 in
            Bytes.set_int64_be b 0 (Int64.of_int n) ;
            of_hex "82 ff" ^ Bytes.to_string b ^ "\x01\x02\x03\x04"
          in
          let size = 8 * 1024 * 1024 in
          let recv note c =
            match Ws.recv c with
            | _ ->
                note "a message"
            | exception e ->
                note_closed note e
          in
          expect
            ( converse (header size) recv
            ^ "\n"
            ^ converse (header (size + 1)) recv )
          @@ __POS_OF__
               {|
            closed 1006 ""
            --
            closed 1009 "message is too large"
            --
            close 1009
            |} )
    ; test "after a protocol error, the linger reads at most 1 MiB" (fun () ->
          (* The first read gives a frame with a reserved bit. The next reads
             never end the input. *)
          let first = ref true and after = ref 0 in
          let read buf off len =
            if !first then begin
              first := false ;
              Bytes.blit_string (of_hex "c1 80") 0 buf off 2 ;
              2
            end
            else begin
              Bytes.fill buf off len 'x' ;
              after := !after + len ;
              len
            end
          in
          let out = Buffer.create 16 in
          let flow =
            memory_flow ~write:(fun b o l -> Buffer.add_subbytes out b o l) read
          in
          let code =
            Sim_support.run (fun _ ->
                match Ws.recv (Ws.Private.of_flow flow) with
                | _ ->
                    0
                | exception Ws.Closed {code; _} ->
                    code )
          in
          equal int 1002 code ;
          equal (list string) ["close 1002"] (frames (Buffer.contents out)) ;
          equal int ~msg:"the bytes after the frame" (1024 * 1024) !after )
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
          let pos = ref 0 and top = ref 0 and reads = ref 0 in
          let byte i =
            if i < String.length first then first.[i]
            else if i < String.length first + body then
              empty.[(i - String.length first) mod size]
            else last.[i - String.length first - body]
          in
          let read buf off len =
            incr reads ;
            top := Int.max !top (Gc.quick_stat ()).heap_words ;
            let n = Int.min len (total - !pos) in
            for i = 0 to n - 1 do
              Bytes.set buf (off + i) (byte (!pos + i))
            done ;
            pos := !pos + n ;
            n
          in
          let flow = memory_flow read in
          let start = (Gc.quick_stat ()).heap_words in
          Sim_support.run (fun _ ->
              is_true (Ws.recv (Ws.Private.of_flow flow) = Binary "xy") ) ;
          let grew = (!top - start) * (Sys.word_size / 8) in
          is_true
            ~msg:
              (Printf.sprintf
                 "the heap grew by %d bytes in %d reads of the flow" grew !reads )
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
          let flow = memory_flow read in
          Sim_support.run (fun _ ->
              let c = Ws.Private.of_flow flow in
              is_true (Ws.recv c = Binary "hello") ;
              is_true (Ws.recv c = Text "hi") ) ;
          equal int ~msg:"the reads of the flow" 1 !reads )
    ; test "the buffer of a fragmented message stays within max_message"
        (fun () ->
          (* What recv allocates for a message of [m] bytes in fragments of
             [sizes], more than for the same message in one frame. The expected
             message is made before the measure. *)
          let m = 1024 * 1024 in
          let expected = String.make m 'x' and reads = ref 0 in
          let allocated frames =
            let input = String.concat "" frames and pos = ref 0 in
            let read buf off len =
              incr reads ;
              let n = Int.min len (String.length input - !pos) in
              Bytes.blit_string input !pos buf off n ;
              pos := !pos + n ;
              n
            in
            let used = ref 0. in
            Sim_support.run (fun _ ->
                let c = Ws.Private.of_flow ~max_message:m (memory_flow read) in
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
              reads := 0 ;
              let r = more sizes in
              is_true
                ~msg:
                  (Printf.sprintf "%s: %.2f m in %d reads of the flow"
                     (String.concat " + " (List.map string_of_int sizes))
                     r !reads )
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
    ; prop "recv joins fragments and answers Pings, however the input is split"
        Gen.(
          pair
            (list ~size:(int_range 0 4)
               (triple bool
                  (list ~size:(int_range 0 6)
                     (of_list
                        [ "a"
                        ; "\xc3\xa9"
                        ; "\xe2\x82\xac"
                        ; "\xf0\x9d\x84\x9e"
                        ; "\x00" ] ) )
                  (list ~size:(int_range 0 4) (int_range 0 5)) ) )
            (int_range 1 9) )
        (fun (messages, chunk) ->
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
            (converse ~chunk (Buffer.contents input) echo) ) ]

let close_tests =
  let try_note note f = try f () with e -> note_closed note e in
  group "Close"
    [ test "close sends a Close frame and reads until the reply" (fun () ->
          expect
            (converse
               ( client Text "discarded" ^ client Text "\xff"
               ^ client Ping "no pong"
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

(* [with_socket sim f] runs [f c peer] with a server connection [c] on a TCP
   connection of the simulator, and the flow [peer] of the other side. *)
let with_socket ?close_timeout ?write_timeout ?read_timeout ?idle_timeout sim f
    =
  Server_support.with_flows sim (fun ~server ~client ->
      f
        (Ws.Private.of_flow ?close_timeout ?write_timeout ?read_timeout
           ?idle_timeout server )
        client )

(* [in_scope f] runs [f s] in a new scope [s]. When [f] ends, also by an
   exception, the scope cancels the calls that did not end. Thus a call that
   waits for ever does not stop the end of the test. *)
let in_scope f =
  Fiber.scope (fun s ->
      Fun.protect ~finally:(fun () -> Fiber.cancel s Exit) (fun () -> f s) )

let call = Sim_support.call

let await = Sim_support.await

(* The code of the [Closed] exception that [f] raises. *)
let closed_code f =
  match f () with
  | _ ->
      fail "no Closed exception"
  | exception Ws.Closed {code; _} ->
      code

(* [f ()], with its [Closed] exception as text. *)
let outcome f =
  match f () with
  | () ->
      "ended"
  | exception Ws.Closed {code; reason} ->
      Printf.sprintf "closed %d %S" code reason

(* [was_cancelled c] makes sure that the call [c] ended with a cancel. *)
let was_cancelled c = raises Promise.Cancelled (fun () -> ignore (await c))

(* Settles, then makes sure that a close started on [c]. *)
let is_closing_now sim c =
  Weft_sim.settle sim ;
  is_true ~msg:"a close started" (is_closing c)

(* A payload larger than the buffers of a socket of the simulator: a send of it
   waits until the peer reads. *)
let large = String.make (1024 * 1024) 'x'

let socket_tests =
  group "Connection on a socket"
    [ test
        "after a protocol error, the peer gets the Close frame and the end of \
         the stream" (fun () ->
          (* The linger ends at the close timeout of 5 s. *)
          Sim_support.run ~guard:(Span.s 10)
          @@ fun sim ->
          with_socket sim
          @@ fun c peer ->
          in_scope
          @@ fun s ->
          let reader = call s (fun () -> closed_code (fun () -> Ws.recv c)) in
          (* A frame of 20 MiB, over the limit of 8 MiB: the server reads only
             its header, and 1 MB stays unread. *)
          let header =
            of_hex "82ff" ^ "\x00\x00\x00\x00\x01\x40\x00\x00"
            ^ "\x01\x02\x03\x04"
          in
          Flow.write_string peer (header ^ String.make 1_000_000 'x') ;
          let r = Reader.create peer in
          equal string "close 1009"
            (show_frame (Frame.read r ~masked:false ~max:125)) ;
          raises End_of_file (fun () -> Frame.read r ~masked:false ~max:125) ;
          Flow.close peer ;
          equal int 1009 (await reader) )
    ; test "a cancel in the reply to a Close frame closes the connection"
        (fun () ->
          Sim_support.run
          @@ fun sim ->
          with_socket sim
          @@ fun c peer ->
          in_scope
          @@ fun s ->
          (* The peer reads only the first bytes: the send holds the writer
             until the connection closes. *)
          let sender =
            call s (fun () -> closed_code (fun () -> Ws.send c (Binary large)))
          in
          is_false (at_end (Reader.create peer)) ;
          let reader = call s (fun () -> Ws.recv c) in
          Flow.write_string peer (client Close (code_reason 1000 "")) ;
          (* The reply to the Close frame waits for the writer. *)
          is_closing_now sim c ;
          Sim_support.cancel reader ;
          was_cancelled reader ;
          Ws.close c ;
          equal int 1000 (closed_code (fun () -> Ws.recv c)) ;
          ignore (await sender) )
    ; (* A cancel in the middle of a frame closes the connection, because the
         stream is out of step. The tests on exact time have the cancels of a
         recv. *)
      test "a cancel in the middle of a send closes with 1006" (fun () ->
          Sim_support.run
          @@ fun sim ->
          with_socket sim
          @@ fun c peer ->
          in_scope
          @@ fun s ->
          let sender = call s (fun () -> Ws.send c (Binary large)) in
          (* When the first bytes come, the send is in its frame. *)
          let r = Reader.create peer in
          is_false (at_end r) ;
          Sim_support.cancel sender ;
          was_cancelled sender ;
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

(* What recv raised in the handler of "/record". A test makes a new Ivar before
   it starts. *)
let recorded : string Ivar.t ref = ref (Ivar.create ())

(* "/ticker" fills it after its first message, and "/late" when it starts. *)
let started : unit Ivar.t ref = ref (Ivar.create ())

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
      Ws.accept ~close_timeout:(Span.ms 100) req echo
  | "/wait" ->
      Ws.accept req (fun _ -> Time.sleep Mono (Span.s 10))
  | "/wait-slow-close" ->
      Ws.accept ~close_timeout:(Span.s 2) req (fun _ ->
          Time.sleep Mono (Span.s 10) )
  | "/ticker" ->
      Ws.accept ~close_timeout:(Span.ms 300) req (fun c ->
          let rec loop () =
            Ws.send c (Text "tick") ;
            ignore (Ivar.try_fill !started ()) ;
            Time.sleep Mono (Span.ms 10) ;
            loop ()
          in
          loop () )
  | "/late" ->
      ignore (Ivar.try_fill !started ()) ;
      sync (Caravan.Conn_info.stopping (Option.get (Caravan.Request.conn req))) ;
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
              ignore (Ivar.try_fill !recorded (Printexc.to_string e)) ;
              raise e )
  | "/other" ->
      (* A Closed of a different connection is an exception of the handler. *)
      Ws.accept req (fun _ ->
          let input = client Close (code_reason 1000 "") in
          let other =
            Ws.Private.of_flow (Flow.of_string input (Buffer.create 16))
          in
          ignore (Ws.recv other) )
  | "/other_late" ->
      (* The peer closes, then the handler raises a Closed of a different
         connection: our connection closes at that time, but the Closed is not
         ours, also with the same code and reason. *)
      Ws.accept req (fun c ->
          (try ignore (Ws.recv c) with Ws.Closed _ -> ()) ;
          let input = client Close (code_reason 1000 "bye") in
          let other =
            Ws.Private.of_flow (Flow.of_string input (Buffer.create 16))
          in
          ignore (Ws.recv other) )
  | "/send_loop" ->
      (* Sends until the stop: then send raises the Closed of our connection,
         and the handler raises it again. *)
      Ws.accept req (fun c ->
          while true do
            Ws.send c (Text "x") ;
            Time.sleep Mono (Span.ms 10)
          done )
  | "/child" ->
      (* The await of a cancelled child is not a stop of the server. *)
      Ws.accept req (fun _ ->
          Fiber.scope (fun s ->
              let child =
                Fiber.fork_promise s (fun () -> Time.sleep Mono (Span.s 10))
              in
              Promise.cancel child ; Promise.await child ) )
  | "/inner" ->
      (* The Cancelled of a scope inside the handler is not a stop of the
         server. *)
      Ws.accept req (fun _ ->
          Fiber.scope (fun s -> Fiber.cancel s Exit ; sync never) )
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
  Fun.protect ~finally:(fun () -> Flow.close flow)
  @@ fun () ->
  Flow.write_string flow (upgrade_request ?first path) ;
  let r = Buf_reader.of_flow flow in
  let head = response_head r in
  (* The frames that came with the head are in [r]. *)
  let r = Reader.create (Flow.prepend (Buf_reader.take_buffered r) flow) in
  Flow.write_string flow after ;
  during () ;
  let rec frames acc =
    match Frame.read r ~masked:false ~max:max_int with
    | exception End_of_file ->
        List.rev acc
    | f ->
        if reply && f.opcode = Close then
          Flow.write_string flow (client Close f.payload) ;
        frames (show_frame f :: acc)
  in
  String.concat "\n" (head @ ("--" :: frames []))

let with_server = Server_support.with_server

(* The configuration of the server, with the errors in [errors]. *)
let config_with errors =
  { Server_support.config with
    on_error= (fun e _ -> errors := Printexc.to_string e :: !errors) }

(* [session server path] in the simulator, with the errors of the server. *)
let errors_of_session path =
  let errors = ref [] in
  let s =
    Sim_support.run (fun _ ->
        with_server ~config:(config_with errors) ~handler:server_handler
          (fun server -> session server path ) )
  in
  (s, List.rev !errors)

let server_tests =
  group "Server"
    [ test "an echo, with a first frame in the write of the request" (fun () ->
          expect
            (Sim_support.run (fun _ ->
                 with_server ~handler:server_handler (fun server ->
                     session server "/echo" ~first:(client Text "early")
                       ~reply:false
                       ~after:
                         ( client Text "hi" ^ client Binary "\x01"
                         ^ client Close (code_reason 1000 "bye") ) ) ) )
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
            (Sim_support.run (fun _ ->
                 with_server ~handler:server_handler (fun server ->
                     session server "/protocol" ) ) )
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
          let s, errors = errors_of_session "/boom" in
          expect s
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1011
            |} ;
          equal (list string) ["Failure(\"boom\")"] errors )
    ; test
        "a Cancelled from a child call closes with 1011, and the server \
         reports it" (fun () ->
          let s, errors = errors_of_session "/child" in
          expect s
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1011
            |} ;
          equal (list string) [Printexc.to_string Promise.Cancelled] errors )
    ; test "a Fiber.Cancelled from a scope inside the handler closes with 1011"
        (fun () ->
          let s, errors = errors_of_session "/inner" in
          expect s
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
            [ Printexc.to_string
                (Failure
                   "Caravan_websocket: the handler raised the Cancelled of a \
                    different scope" ) ]
            errors )
    ; test
        "a Closed of a different connection closes with 1011, and the server \
         reports it" (fun () ->
          let s, errors = errors_of_session "/other" in
          expect s
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1011
            |} ;
          equal int 1 (List.length errors) )
    ; test
        "a Closed of a different connection, while our connection closes: the \
         server reports it" (fun () ->
          let errors = ref [] in
          let s =
            Sim_support.run (fun _ ->
                with_server ~config:(config_with errors) ~handler:server_handler
                  (fun server ->
                    session server "/other_late" ~reply:false
                      ~after:(client Close (code_reason 1000 "bye")) ) )
          in
          expect s
          @@ __POS_OF__
               {|
            HTTP/1.1 101 Switching Protocols
            Upgrade: websocket
            Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
            Connection: Upgrade
            --
            close 1000
            |} ;
          equal (list string)
            ["Caravan_websocket__Connection.Closed(1000, \"bye\")"]
            (List.rev !errors) )
    ; test
        "a handler that raises the Closed of its connection at a stop: no error"
        (fun () ->
          let errors = ref [] in
          let config = {(config_with errors) with shutdown_grace= Span.s 30} in
          let s =
            Sim_support.run (fun _ ->
                with_server ~config ~handler:server_handler (fun server ->
                    session server "/send_loop" ~during:(fun () ->
                        Time.sleep Mono (Span.ms 25) ;
                        Caravan.Server.shutdown server ) ) )
          in
          is_true ~msg:s
            (String.ends_with ~suffix:"close 1001 \"server shutdown\"" s) ;
          equal (list string) [] !errors )
    ; test "a peer that leaves without a Close frame is not an error" (fun () ->
          let errors = ref [] in
          recorded := Ivar.create () ;
          let recorded =
            Sim_support.run (fun _ ->
                with_server ~config:(config_with errors) ~handler:server_handler
                  (fun server ->
                    let flow =
                      Caravan.Tcp.connect (Caravan.Server.local_addr server)
                    in
                    Flow.write_string flow (upgrade_request "/record") ;
                    equal string "HTTP/1.1 101 Switching Protocols"
                      (List.hd (response_head (Buf_reader.of_flow flow))) ;
                    Flow.close flow ;
                    sync (Ivar.read !recorded) ) )
          in
          equal string "Caravan_websocket__Connection.Closed(1006, \"\")"
            recorded ;
          equal (list string) [] !errors )
    ; test "a server that stops sends Close 1001, and the handler ends"
        (fun () ->
          let errors = ref [] in
          let config = {(config_with errors) with shutdown_grace= Span.s 30} in
          (* Manual mode: the session and the stop end with no time. *)
          expect
            (Sim_support.manual (fun _ ->
                 let t0 = Time.now Mono in
                 with_server ~config ~handler:server_handler (fun server ->
                     let s =
                       session server "/echo" ~during:(fun () ->
                           Caravan.Server.shutdown server )
                     in
                     (* The handler does not wait for the grace of 30 s. *)
                     Caravan.Server.wait server ;
                     equal Server_support.span Span.zero
                       (Time.diff Mono (Time.now Mono) t0) ;
                     s ) ) )
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
          let config = {Server_support.config with shutdown_grace= Span.s 30} in
          recorded := Ivar.create () ;
          Sim_support.run (fun _ ->
              with_server ~config ~handler:server_handler (fun server ->
                  ignore
                    (session server "/record" ~during:(fun () ->
                         Caravan.Server.shutdown server ) ) ) ) ;
          (* The server waits for the handler. *)
          equal (option string)
            (Some
               "Caravan_websocket__Connection.Closed(1001, \"server shutdown\")"
            ) (Ivar.peek !recorded) )
    ; test
        "a stop before the handler gives its response: 503, and no WebSocket \
         connection" (fun () ->
          started := Ivar.create () ;
          let config = {Server_support.config with shutdown_grace= Span.s 30} in
          expect
            (Sim_support.run (fun _ ->
                 with_server ~config ~handler:server_handler (fun server ->
                     in_scope
                     @@ fun s ->
                     (* The response comes only after the stop. *)
                     let session =
                       call s (fun () ->
                           let flow =
                             Caravan.Tcp.connect
                               (Caravan.Server.local_addr server)
                           in
                           Fun.protect ~finally:(fun () -> Flow.close flow)
                           @@ fun () ->
                           Flow.write_string flow (upgrade_request "/late") ;
                           let r = Buf_reader.of_flow flow in
                           let head = response_head r in
                           let buf = Bytes.create 256
                           and body = Buffer.create 64 in
                           let rec rest () =
                             match Buf_reader.read_into r buf 0 256 with
                             | 0 ->
                                 ()
                             | n ->
                                 Buffer.add_subbytes body buf 0 n ;
                                 rest ()
                           in
                           rest () ;
                           String.concat "\n"
                             (head @ ["--"; Buffer.contents body]) )
                     in
                     sync (Ivar.read !started) ;
                     Caravan.Server.shutdown server ;
                     await session ) ) )
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
          Sim_support.run (fun _ ->
              run_handler ~stopping:(always ())
                (fun c -> try ignore (Ws.recv c) with Ws.Closed _ -> ())
                (Flow.of_string input out) ) ;
          equal (list string) ["close 1001"] (frames (Buffer.contents out)) )
    ; test "the end of the handler stops the wait for the stop" (fun () ->
          let input = client Close (code_reason 1000 "") in
          let out = Buffer.create 16 in
          Sim_support.run (fun _ ->
              run_handler (fun _ -> ()) (Flow.of_string input out) ) ;
          equal (list string) ["close 1000"] (frames (Buffer.contents out)) )
    ; test "a cancel of the handler after the peer left gives Cancelled"
        (fun () ->
          Sim_support.run
          @@ fun sim ->
          Server_support.with_flows sim
          @@ fun ~server ~client:peer ->
          in_scope
          @@ fun s ->
          let left = Ivar.create () in
          let handler =
            call s (fun () ->
                run_handler
                  (fun c ->
                    (* The recv sees the end of the stream, and the connection
                       is closed. Thus a close after the cancel ends at once,
                       without a check of the cancel. *)
                    (try ignore (Ws.recv c) with Ws.Closed _ -> ()) ;
                    Ivar.fill left () ; sync never )
                  server )
          in
          Flow.close peer ;
          sync (Ivar.read left) ;
          (* The cancel is not an error of the handler. *)
          Sim_support.cancel handler ;
          was_cancelled handler )
    ; test "a handler that catches Cancelled and returns sends no Close frame"
        (fun () ->
          Sim_support.run
          @@ fun sim ->
          Server_support.with_flows sim
          @@ fun ~server ~client:peer ->
          in_scope
          @@ fun s ->
          let waits = Ivar.create () in
          let handler =
            call s (fun () ->
                run_handler
                  (fun _ ->
                    Ivar.fill waits () ;
                    try sync never with Fiber.Cancelled _ -> () )
                  server )
          in
          sync (Ivar.read waits) ;
          Sim_support.cancel handler ;
          was_cancelled handler ;
          (* The engine closes the flow after the handler. *)
          Flow.close server ;
          equal (list string) [] (frames (Server_support.read_all peer)) )
    ; test "the context is cancelled before run starts" (fun () ->
          let out = Buffer.create 16 and ran = ref false in
          let result =
            Sim_support.run (fun _ ->
                Fiber.scope (fun inner ->
                    Fiber.cancel inner Exit ;
                    (* No sync comes before the call. *)
                    match
                      run_handler
                        (fun c ->
                          ran := true ;
                          ignore (Ws.recv c) )
                        (Flow.of_string "" out)
                    with
                    | () ->
                        "returned"
                    | exception Fiber.Cancelled Exit ->
                        "the cancel of the caller"
                    | exception e ->
                        Printexc.to_string e ) )
          in
          equal string "the cancel of the caller" result ;
          is_false ~msg:"the handler ran" !ran ;
          equal (list string) [] (frames (Buffer.contents out)) ) ]

(* The connection and exact time. The tests run in manual mode: only an advance
   moves the time. *)

let ( +: ) = Sim_support.( +: )

(* [before t] is 1 ns before [t]. *)
let before t = t +: Span.ns (-1)

(* The spans of the tests of exact time: different from each other and from the
   spans of the server. *)
let ws_idle = Span.s 19

let ws_close = Span.s 23

let ws_write = Span.s 29

let ws_read = Span.s 31

(* A connection on the server flow of a TCP connection of the simulator, and its
   peer. The spans are [idle], [close], [write] and [ws_read]. With
   [~idle:None], no idle timeout: then [close] waits for its own deadline. The
   connection reads and writes through [Server_support.counted]: [test] gets the
   counts too. [test] runs in [in_scope]. *)
let with_counted_socket ?(idle = Some ws_idle) ?(close = ws_close)
    ?(write = ws_write) sim test =
  Server_support.with_flows sim
  @@ fun ~server ~client:peer ->
  let counted = Server_support.counted server in
  let c =
    Ws.Private.of_flow ~close_timeout:close ~write_timeout:write
      ~read_timeout:ws_read ?idle_timeout:idle counted.flow
  in
  in_scope (fun s -> test s c peer counted)

(* [with_counted_socket] without the counts. *)
let with_timed_socket ?idle ?close ?write sim test =
  with_counted_socket ?idle ?close ?write sim (fun s c peer _ -> test s c peer)

let ended = Sim_support.ended

let ends_at = Sim_support.ends_at

(* [all_end_at sim ~t0 d calls] makes sure that no call of [calls] ended at [t0
   + d - 1 ns], and that each one ended at [t0 + d]. *)
let all_end_at sim ~t0 d calls =
  Sim_support.holds_from sim ~t0 d ~what:"the end of a call" (fun () ->
      List.exists (ended sim) calls ) ;
  List.iteri
    (fun i c -> is_true ~msg:(Printf.sprintf "call %d ended" i) (ended sim c))
    calls

(* The next frame that the peer reads. *)
let peer_frame peer =
  show_frame (Frame.read (Reader.create peer) ~masked:false ~max:125)

let time_tests =
  group "Connection and exact time"
    [ test "a frame 1 ns before the idle timeout comes, and then Close 1001"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          let t0 = Time.now Mono in
          let first = call s (fun () -> Ws.recv c) in
          let t1 = before (t0 +: ws_idle) in
          Sim_support.advance_to sim t1 ;
          is_false ~msg:"no end 1 ns before the idle timeout" (ended sim first) ;
          Flow.write_string peer (client Text "one") ;
          is_true ~msg:"the frame" (await first = Text "one") ;
          let second =
            call s (fun () -> outcome (fun () -> ignore (Ws.recv c)))
          in
          equal string "closed 1001 \"idle timeout\""
            (ends_at sim ~t0:t1 ws_idle second) ;
          equal string "close 1001" (peer_frame peer) )
    ; test "a frame that stops in the middle gets the read timeout" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          let frame = client Text "hello" and next = client Text "two" in
          let t0 = Time.now Mono in
          let first = call s (fun () -> Ws.recv c) in
          (* The first read gets 2 bytes. The next read in the frame waits
             [read_timeout]. *)
          Flow.write_string peer (String.sub frame 0 2) ;
          let t1 = before (t0 +: ws_read) in
          Sim_support.advance_to sim t1 ;
          is_false ~msg:"no end 1 ns before the read timeout" (ended sim first) ;
          Flow.write_string peer (String.sub frame 2 1) ;
          (* A slow frame with no long pause gets no timeout: the frame passed
             the idle timeout and the timeout of the first read. *)
          let t2 = before (t1 +: ws_read) in
          Sim_support.advance_to sim t2 ;
          is_false ~msg:"no end 1 ns before the second read timeout"
            (ended sim first) ;
          Flow.write_string peer (String.sub frame 3 (String.length frame - 3)) ;
          is_true ~msg:"the slow frame" (await first = Text "hello") ;
          (* The next frame stops in the middle. *)
          let reader =
            call s (fun () -> outcome (fun () -> ignore (Ws.recv c)))
          in
          Flow.write_string peer (String.sub next 0 2) ;
          let close_frame = call s (fun () -> peer_frame peer) in
          equal string "close 1001" (ends_at sim ~t0:t2 ws_read close_frame) ;
          (* Then the connection lingers for [close_timeout]. *)
          equal string "closed 1001 \"read timeout\""
            (ends_at sim ~t0:(t2 +: ws_read) ws_close reader) )
    ; test "a send that waits ends at the write timeout and not 1 ns before"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c _peer ->
          let t0 = Time.now Mono in
          (* The peer never reads: only an advance can end the send. *)
          let sender =
            call s (fun () -> outcome (fun () -> Ws.send c (Binary large)))
          in
          equal string "closed 1006 \"\"" (ends_at sim ~t0 ws_write sender) )
    ; test "each frame gets a new write timeout" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c _peer ->
          let t0 = Time.now Mono in
          Ws.send c (Text "one") ;
          let t1 = before (t0 +: ws_write) in
          Sim_support.advance_to sim t1 ;
          (* The peer never reads: the second frame waits from t1. *)
          let sender =
            call s (fun () -> outcome (fun () -> Ws.send c (Binary large)))
          in
          equal string "closed 1006 \"\"" (ends_at sim ~t0:t1 ws_write sender) )
    ; test "close ends at the close timeout and not 1 ns before" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket ~idle:None sim
          @@ fun s c peer ->
          let t0 = Time.now Mono in
          let closer = call s (fun () -> outcome (fun () -> Ws.close c)) in
          equal string "close 1000" (peer_frame peer) ;
          (* The peer does not reply: only an advance can end the close. *)
          equal string "ended" (ends_at sim ~t0 ws_close closer) ;
          equal string "closed 1006 \"\""
            (outcome (fun () -> ignore (Ws.recv c))) )
    ; test "close stops at its deadline if the peer sends without a stop"
        (fun () ->
          (* A flow that always has a frame: a read never waits for a deadline,
             thus only the check of the close deadline stops [close]. The reads
             sleep: until 1 ns before the close deadline at read 10, and until
             the deadline at read 20. The program is then quiescent, and the
             time moves in auto mode. *)
          let frame = client Binary "" and count = 1000 in
          let size = String.length frame in
          let reads = ref 0 and served = ref 0 and at_deadline = ref (-1) in
          let read buf off len =
            if !served >= count * size then 0
            else begin
              incr reads ;
              if !reads = 10 then
                Time.sleep Mono (Span.of_ns (Int64.pred (Span.to_ns ws_close))) ;
              if !reads = 20 then Time.sleep Mono (Span.ns 1) ;
              let pos = !served in
              let n = Int.min len (size - (pos mod size)) in
              Bytes.blit_string frame (pos mod size) buf off n ;
              served := pos + n ;
              if !reads = 20 then at_deadline := !served ;
              n
            end
          in
          let out = Buffer.create 16 in
          let flow =
            memory_flow ~write:(fun b o l -> Buffer.add_subbytes out b o l) read
          in
          let after_close =
            Sim_support.run ~guard:(Span.s 60) (fun _ ->
                let c = Ws.Private.of_flow ~close_timeout:ws_close flow in
                Ws.close c ;
                outcome (fun () -> ignore (Ws.recv c)) )
          in
          equal (list string) ["close 1000"] (frames (Buffer.contents out)) ;
          equal string ~msg:"the connection after close" "closed 1006 \"\""
            after_close ;
          is_true ~msg:"reads 1 ns before the close deadline" (!reads >= 20) ;
          equal int ~msg:"bytes after the close deadline" !at_deadline !served )
    ; test "a fiber in recv stops at the time limit of close" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket ~idle:None sim
          @@ fun s c peer ->
          let t0 = Time.now Mono in
          let reader =
            call s (fun () -> outcome (fun () -> ignore (Ws.recv c)))
          in
          (* The reader waits inside a frame until its read timeout. *)
          Flow.write_string peer (String.sub (client Text "hello") 0 2) ;
          Weft_sim.settle sim ;
          (* The close deadline is earlier: the read ends at it. *)
          let closer = call s (fun () -> outcome (fun () -> Ws.close c)) in
          all_end_at sim ~t0 ws_close [reader; closer] ;
          equal string "closed 1006 \"\"" (await reader) ;
          equal string "ended" (await closer) )
    ; test "a fiber blocked in send stops at the time limit of close" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket ~idle:None sim
          @@ fun s c _peer ->
          let t0 = Time.now Mono in
          (* The peer reads nothing: the send waits until its write timeout. *)
          let sender =
            call s (fun () -> outcome (fun () -> Ws.send c (Binary large)))
          in
          Weft_sim.settle sim ;
          (* The close deadline is earlier: the write ends at it. *)
          let closer = call s (fun () -> outcome (fun () -> Ws.close c)) in
          all_end_at sim ~t0 ws_close [sender; closer] ;
          equal string "closed 1006 \"\"" (await sender) ;
          equal string "ended" (await closer) )
    ; test "a frame that is in part in the buffer gets the read timeout"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_counted_socket ~idle:None sim
          @@ fun s c peer counted ->
          (* One read gets the first frame and 1 byte of the next frame. Without
             an idle timeout, only the read timeout stops the wait. *)
          let input = client Text "one" ^ String.sub (client Text "two") 0 1 in
          Flow.write_string peer input ;
          is_true (Ws.recv c = Text "one") ;
          equal int ~msg:"the bytes in the buffer" (String.length input)
            (Atomic.get counted.read_bytes) ;
          let t0 = Time.now Mono in
          let second =
            call s (fun () -> outcome (fun () -> ignore (Ws.recv c)))
          in
          let close_frame = call s (fun () -> peer_frame peer) in
          equal string "close 1001" (ends_at sim ~t0 ws_read close_frame) ;
          (* Then the connection lingers for [close_timeout]. *)
          equal string "closed 1001 \"read timeout\""
            (ends_at sim ~t0:(t0 +: ws_read) ws_close second) )
    ; test "the default read timeout is 30 s and the default close timeout 5 s"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_socket sim
          @@ fun c peer ->
          in_scope
          @@ fun s ->
          let t0 = Time.now Mono in
          let reader =
            call s (fun () -> outcome (fun () -> ignore (Ws.recv c)))
          in
          (* The frame stops after 2 bytes. *)
          Flow.write_string peer (String.sub (client Text "hello") 0 2) ;
          let close_frame = call s (fun () -> peer_frame peer) in
          equal string "close 1001" (ends_at sim ~t0 (Span.s 30) close_frame) ;
          (* Then the connection lingers for the close timeout. *)
          equal string "closed 1001 \"read timeout\""
            (ends_at sim ~t0:(t0 +: Span.s 30) (Span.s 5) reader) )
    ; test "the default write timeout is 30 s" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_socket sim
          @@ fun c _peer ->
          in_scope
          @@ fun s ->
          let t0 = Time.now Mono in
          (* The peer never reads: only an advance can end the send. *)
          let sender =
            call s (fun () -> outcome (fun () -> Ws.send c (Binary large)))
          in
          equal string "closed 1006 \"\"" (ends_at sim ~t0 (Span.s 30) sender) )
    ; test "the read timeout does not apply between frames" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket ~idle:(Some (Span.s 37)) sim
          @@ fun s c peer ->
          (* A read inside the first frame sets the read timeout. *)
          let frame = client Text "one" in
          let t0 = Time.now Mono in
          let first = call s (fun () -> Ws.recv c) in
          Flow.write_string peer (String.sub frame 0 2) ;
          Weft_sim.settle sim ;
          Flow.write_string peer (String.sub frame 2 (String.length frame - 2)) ;
          is_true (await first = Text "one") ;
          (* The second recv waits past the read timeout. *)
          let second =
            call s (fun () -> outcome (fun () -> ignore (Ws.recv c)))
          in
          Sim_support.advance_to sim (t0 +: ws_read) ;
          is_false ~msg:"no end at the read timeout" (ended sim second) ;
          Flow.write_string peer (client Text "two") ;
          equal string "ended" (await second) )
    ; test "each frame gets a new idle timeout" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          let t0 = Time.now Mono in
          let reader = call s (fun () -> Ws.recv c) in
          (* A Ping 1 ns before the idle timeout. *)
          let t1 = before (t0 +: ws_idle) in
          Sim_support.advance_to sim t1 ;
          Flow.write_string peer (client Ping "") ;
          equal string "pong \"\"" (peer_frame peer) ;
          Sim_support.advance_to sim (before (t1 +: ws_idle)) ;
          Flow.write_string peer (client Text "end") ;
          is_true (await reader = Text "end") )
    ; test "close stops at a shorter idle timeout" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          (* The idle timeout is shorter than the close timeout. *)
          with_timed_socket sim
          @@ fun s c _peer ->
          let t0 = Time.now Mono in
          let closer = call s (fun () -> outcome (fun () -> Ws.close c)) in
          equal string "ended" (ends_at sim ~t0 ws_idle closer) )
    ; test "close keeps the shorter write timeout of a send that waits"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          (* The write timeout is shorter than the close timeout. *)
          with_timed_socket ~close:(Span.s 37) sim
          @@ fun s c _peer ->
          let t0 = Time.now Mono in
          (* The peer reads nothing: the send waits until its write timeout. *)
          let sender =
            call s (fun () -> outcome (fun () -> Ws.send c (Binary large)))
          in
          Weft_sim.settle sim ;
          let closer = call s (fun () -> outcome (fun () -> Ws.close c)) in
          is_closing_now sim c ;
          all_end_at sim ~t0 ws_write [sender; closer] ;
          equal string "closed 1006 \"\"" (await sender) ;
          equal string "ended" (await closer) )
    ; test "close keeps its time limit with a long idle timeout" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          (* The idle timeout is longer than the close timeout. *)
          with_timed_socket ~idle:(Some (Span.s 37)) sim
          @@ fun s c _peer ->
          let t0 = Time.now Mono in
          let closer = call s (fun () -> outcome (fun () -> Ws.close c)) in
          equal string "ended" (ends_at sim ~t0 ws_close closer) )
    ; test
        "close keeps its time limit when the reply to the peer waits for a send"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          let t0 = Time.now Mono in
          (* The peer reads nothing: the send holds the writer until its write
             timeout. *)
          let sender =
            call s (fun () -> outcome (fun () -> Ws.send c (Binary large)))
          in
          Weft_sim.settle sim ;
          (* The reply to the Close frame of the peer waits for the writer. *)
          let reader =
            call s (fun () -> outcome (fun () -> ignore (Ws.recv c)))
          in
          Flow.write_string peer (client Close (code_reason 1000 "")) ;
          is_closing_now sim c ;
          (* The close deadline is earlier: the write ends at it. *)
          let closer = call s (fun () -> outcome (fun () -> Ws.close c)) in
          all_end_at sim ~t0 ws_close [closer; sender; reader] ;
          equal string "ended" (await closer) ;
          equal string "closed 1006 \"\"" (await sender) ;
          equal string "closed 1006 \"\"" (await reader) )
    ; test "a second close does not make the close deadline later" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket ~idle:None sim
          @@ fun s c peer ->
          let t0 = Time.now Mono in
          let first = call s (fun () -> outcome (fun () -> Ws.close c)) in
          equal string "close 1000" (peer_frame peer) ;
          (* The second close comes 1 s after the first. *)
          Sim_support.advance sim (Span.s 1) ;
          let second = call s (fun () -> outcome (fun () -> Ws.close c)) in
          is_false ~msg:"the second close waits" (ended sim second) ;
          (* After a frame, the read waits again, until the first deadline. *)
          Flow.write_string peer (client Text "x") ;
          all_end_at sim ~t0 ws_close [first; second] ;
          equal string "ended" (await first) ;
          equal string "ended" (await second) )
    ; test "a fiber in recv reads the reply to close" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          let reader =
            call s (fun () -> outcome (fun () -> ignore (Ws.recv c)))
          in
          Weft_sim.settle sim ;
          let closer =
            call s (fun () -> outcome (fun () -> Ws.close ~code:1001 c))
          in
          equal string "close 1001" (peer_frame peer) ;
          Flow.write_string peer (client Close (code_reason 1001 "")) ;
          equal string "ended" (await closer) ;
          equal string "closed 1001 \"\"" (await reader) )
    ; test "a send that waits for a different send raises Closed after close"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          (* The peer reads nothing yet: [big] fills the socket buffers. *)
          let big =
            call s (fun () -> outcome (fun () -> Ws.send c (Binary large)))
          in
          Weft_sim.settle sim ;
          (* [late] waits for the writer before the close starts. *)
          let late =
            call s (fun () -> outcome (fun () -> Ws.send c (Text "late")))
          in
          Weft_sim.settle sim ;
          let closer = call s (fun () -> outcome (fun () -> Ws.close c)) in
          is_closing_now sim c ;
          let r = Reader.create peer in
          let next () = Frame.read r ~masked:false ~max:max_int in
          equal int (String.length large) (String.length (next ()).payload) ;
          equal string "close 1000" (show_frame (next ())) ;
          Flow.write_string peer (client Close (code_reason 1000 "")) ;
          equal string "ended" (await big) ;
          equal string "closed 1000 \"\"" (await late) ;
          equal string "ended" (await closer) )
    ; test "a second close waits for the end of the first closing handshake"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          let first =
            call s (fun () -> outcome (fun () -> Ws.close ~code:1001 c))
          in
          let r = Reader.create peer in
          equal string "close 1001"
            (show_frame (Frame.read r ~masked:false ~max:125)) ;
          let second = call s (fun () -> outcome (fun () -> Ws.close c)) in
          is_false ~msg:"the second close waits" (ended sim second) ;
          Flow.write_string peer (client Close (code_reason 1001 "")) ;
          equal string "ended" (await first) ;
          equal string "ended" (await second) ;
          raises End_of_file (fun () -> Frame.read r ~masked:false ~max:125) )
    ; test "a cancel in the linger after a protocol error closes the connection"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          (* The peer does not close, and the time does not advance: only the
             cancel ends the linger. *)
          let reader = call s (fun () -> Ws.recv c) in
          Flow.write_string peer (client Continuation "x") ;
          let r = Reader.create peer in
          equal string "close 1002"
            (show_frame (Frame.read r ~masked:false ~max:125)) ;
          (* The end of the stream shows that the server is in the linger. *)
          is_true (at_end r) ;
          is_false ~msg:"the reader lingers" (ended sim reader) ;
          Sim_support.cancel reader ;
          was_cancelled reader ;
          equal int 1002 (closed_code (fun () -> Ws.recv c)) )
    ; test "the linger after a protocol error discards at most 1 MiB" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          (* The peer sends 3 MiB after a frame that is too large, and does not
             close. The time does not advance: only the limit of 1 MiB stops the
             linger. Then the server closes with unread input, and the peer gets
             a reset. *)
          let reader =
            call s (fun () -> outcome (fun () -> ignore (Ws.recv c)))
          in
          let header =
            of_hex "82ff" ^ "\x00\x00\x00\x00\x01\x40\x00\x00"
            ^ "\x01\x02\x03\x04"
          in
          let writer =
            call s (fun () ->
                match
                  Flow.write_string peer
                    (header ^ String.make (3 * 1024 * 1024) 'x')
                with
                | () ->
                    "all written"
                | exception Flow.Closed ->
                    "reset" )
          in
          equal string "closed 1009 \"message is too large\"" (await reader) ;
          equal string "reset" (await writer) )
    ; test "a cancel between messages does not close" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          let reader = call s (fun () -> Ws.recv c) in
          Weft_sim.settle sim ;
          Sim_support.cancel reader ;
          was_cancelled reader ;
          Flow.write_string peer (client Text "hello") ;
          is_true (Ws.recv c = Text "hello") )
    ; test "a cancel in the middle of a received frame closes with 1006"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          let reader = call s (fun () -> Ws.recv c) in
          let frame = client Text "hello" in
          (* The header of a masked frame of 5 bytes has 6 bytes. *)
          Flow.write_string peer (String.sub frame 0 7) ;
          (* The reader took the header and waits inside the payload. *)
          Weft_sim.settle sim ;
          Sim_support.cancel reader ;
          was_cancelled reader ;
          (* The connection is closed: the write can get a reset. *)
          ( try
              Flow.write_string peer
                ( String.sub frame 7 (String.length frame - 7)
                ^ client Text "next" )
            with Flow.Closed -> () ) ;
          equal int 1006 (closed_code (fun () -> Ws.recv c)) )
    ; test "a cancel in the header of a received frame keeps the connection"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          let reader = call s (fun () -> Ws.recv c) in
          let frame = client Text "hello" in
          Flow.write_string peer (String.sub frame 0 3) ;
          (* The reader holds 3 bytes of the header and waits for the rest. *)
          Weft_sim.settle sim ;
          Sim_support.cancel reader ;
          was_cancelled reader ;
          (* The header is not consumed: the next recv reads the same frame. *)
          Flow.write_string peer (String.sub frame 3 (String.length frame - 3)) ;
          is_true (Ws.recv c = Text "hello") ;
          Flow.write_string peer (client Text "next") ;
          is_true (Ws.recv c = Text "next") )
    ; test
        "a cancel in the wait to send a Pong inside a fragmented message \
         closes with 1006" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_counted_socket sim
          @@ fun s c peer counted ->
          (* The peer reads nothing: [big] keeps the writer. *)
          let big =
            call s (fun () -> outcome (fun () -> Ws.send c (Binary large)))
          in
          Weft_sim.settle sim ;
          let reader = call s (fun () -> Ws.recv c) in
          let input = client ~fin:false Text "Hel" ^ client Ping "p" in
          Flow.write_string peer input ;
          (* The count shows that the reader took the fragment and the Ping.
             Then it waits for the writer to send the Pong. *)
          Weft_sim.settle sim ;
          equal int ~msg:"the reader took the input" (String.length input)
            (Atomic.get counted.read_bytes) ;
          Sim_support.cancel reader ;
          was_cancelled reader ;
          ( try Flow.write_string peer (client Continuation "lo")
            with Flow.Closed -> () ) ;
          (* Without the fix, the next recv waits for the write timeout of
             [big]. The time does not advance. *)
          equal int 1006 (closed_code (fun () -> Ws.recv c)) ;
          equal string "closed 1006 \"\"" (await big) )
    ; test
        "a recv after a cancel in a frame raises Closed, also with frames in \
         the buffer" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_counted_socket sim
          @@ fun s c peer counted ->
          (* The peer reads nothing: [big] keeps the writer. *)
          let big =
            call s (fun () -> outcome (fun () -> Ws.send c (Binary large)))
          in
          Weft_sim.settle sim ;
          let reader = call s (fun () -> Ws.recv c) in
          (* One read takes the three frames. The reader waits for the writer to
             send the Pong, and a complete message stays in the buffer. *)
          let input =
            client ~fin:false Text "Hel" ^ client Ping "p" ^ client Text "after"
          in
          Flow.write_string peer input ;
          Weft_sim.settle sim ;
          equal int ~msg:"the reader took the input" (String.length input)
            (Atomic.get counted.read_bytes) ;
          Sim_support.cancel reader ;
          was_cancelled reader ;
          equal int 1006 (closed_code (fun () -> Ws.recv c)) ;
          equal string "closed 1006 \"\"" (await big) )
    ; test "a Pong that waits for the writer is dropped after a close starts"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          (* The peer reads nothing yet: [big] keeps the writer. *)
          let big =
            call s (fun () -> outcome (fun () -> Ws.send c (Binary large)))
          in
          Weft_sim.settle sim ;
          (* The Pong for the Ping waits for the writer. Then the Close frame of
             [close] waits after it. *)
          let reader =
            call s (fun () -> outcome (fun () -> ignore (Ws.recv c)))
          in
          Flow.write_string peer (client Ping "p") ;
          Weft_sim.settle sim ;
          let closer = call s (fun () -> outcome (fun () -> Ws.close c)) in
          is_closing_now sim c ;
          let r = Reader.create peer in
          let next () = Frame.read r ~masked:false ~max:max_int in
          equal int (String.length large) (String.length (next ()).payload) ;
          equal string "ended" (await big) ;
          (* RFC 6455 §5.5.1: no Pong after our Close frame. *)
          equal string "close 1000" (show_frame (next ())) ;
          Flow.write_string peer (client Close (code_reason 1000 "")) ;
          equal string "ended" (await closer) ;
          equal string "closed 1000 \"\"" (await reader) ;
          raises End_of_file (fun () -> Frame.read r ~masked:false ~max:125) )
    ; test "a cancel inside a fragmented message closes with 1006" (fun () ->
          Sim_support.manual
          @@ fun sim ->
          with_timed_socket sim
          @@ fun s c peer ->
          let reader = call s (fun () -> Ws.recv c) in
          Flow.write_string peer (client ~fin:false Text "Hel") ;
          (* The reader took the fragment and waits for the next frame. *)
          Weft_sim.settle sim ;
          Sim_support.cancel reader ;
          was_cancelled reader ;
          ( try Flow.write_string peer (client Continuation "lo")
            with Flow.Closed -> () ) ;
          equal int 1006 (closed_code (fun () -> Ws.recv c)) )
    ; test
        "the end of the handler does not stop the closing handshake of a stop"
        (fun () ->
          Sim_support.manual
          @@ fun sim ->
          Server_support.with_flows sim
          @@ fun ~server ~client:peer ->
          in_scope
          @@ fun s ->
          let stop = Ivar.create ()
          and conn = Ivar.create ()
          and go = Ivar.create () in
          let handler =
            call s (fun () ->
                run_handler ~stopping:(Ivar.read stop)
                  ~close_timeout:(Span.s 37)
                  (fun c ->
                    Ivar.fill conn c ;
                    sync (Ivar.read go) )
                  server )
          in
          let c = sync (Ivar.read conn) in
          (* The peer does not read yet: the send holds the writer, and the
             Close frame of the stop waits for it. *)
          let sender =
            call s (fun () ->
                try Ws.send c (Binary large) with Ws.Closed _ -> () )
          in
          Weft_sim.settle sim ;
          Ivar.fill stop () ;
          is_closing_now sim c ;
          Ivar.fill go () ;
          (* After the end of the handler, the connection waits for the end of
             the closing handshake. *)
          is_false ~msg:"the handshake waits" (ended sim handler) ;
          let r = Reader.create peer in
          let first = Frame.read r ~masked:false ~max:max_int in
          let second =
            match Frame.read r ~masked:false ~max:125 with
            | f ->
                Flow.write_string peer (client Close f.payload) ;
                show_frame f
            | exception End_of_file ->
                "end of the stream"
          in
          await handler ;
          await sender ;
          equal (list string)
            ["binary"; "close 1001 \"server shutdown\""]
            [opcode_name first.opcode; second] )
    ; test
        "an upgraded connection gets the idle timeout, and the first frame \
         from Flow.prepend" (fun () ->
          (* The handler reads the first frame. Then it waits for a frame until
             the idle timeout. *)
          let first = Ivar.create () in
          let handler req =
            Ws.accept ~close_timeout:ws_close ~write_timeout:ws_write
              ~read_timeout:ws_read ~idle_timeout:ws_idle req (fun c ->
                Ivar.fill first (Ws.recv c) ;
                ignore (Ws.recv c) )
          in
          Sim_support.manual
          @@ fun sim ->
          let t0 = Time.now Mono in
          with_server ~handler
          @@ fun server ->
          let flow = Caravan.Tcp.connect (Caravan.Server.local_addr server) in
          Fun.protect ~finally:(fun () -> Flow.close flow)
          @@ fun () ->
          (* The first frame goes in the write of the request: the connection
             gets it through Flow.prepend. *)
          Flow.write_string flow
            (upgrade_request ~first:(client Text "early") "/") ;
          let r = Buf_reader.of_flow flow in
          equal string "HTTP/1.1 101 Switching Protocols"
            (List.hd (response_head r)) ;
          let r =
            Reader.create (Flow.prepend (Buf_reader.take_buffered r) flow)
          in
          in_scope
          @@ fun s ->
          let close_frame =
            call s (fun () -> show_frame (Frame.read r ~masked:false ~max:125))
          in
          equal string "close 1001" (ends_at sim ~t0 ws_idle close_frame) ;
          is_true ~msg:"the first frame" (Ivar.peek first = Some (Text "early")) )
    ]

(* The server and exact time *)

(* [Server_support.timed_config] with an idle timeout of 41 s: the default close
   timeout of a WebSocket connection is 5 s, the idle timeout of [timed_config].
   The errors go to [errors]. *)
let timed_server_config errors =
  { Server_support.timed_config with
    idle_timeout= Span.s 41
  ; on_error= (fun e _ -> errors := Printexc.to_string e :: !errors) }

(* [session server path] in a call of [s]. [test] gets the call when the
   response head came. *)
let with_session ?reply s server path test =
  let head = Ivar.create () in
  let call =
    call s (fun () ->
        session ?reply server path ~during:(fun () -> Ivar.fill head ()) )
  in
  sync (Ivar.read head) ;
  test call

(* The Close frame of the server in the result of [session]. *)
let server_close s = List.nth (String.split_on_char '\n' s) 5

(* [with_timed_server config test] runs [test sim s server] in manual mode, with
   a server of [server_handler]. *)
let with_timed_server config test =
  Sim_support.manual
  @@ fun sim ->
  with_server ~config ~handler:server_handler
  @@ fun server -> in_scope (fun s -> test sim s server)

let timed_server_tests =
  group "Server and exact time"
    [ test
        "a handler that does not use the connection gets Close 1001, then the \
         cancel" (fun () ->
          let errors = ref [] in
          with_timed_server (timed_server_config errors) (fun sim s server ->
              with_session s server "/wait"
              @@ fun exchange ->
              let t0 = Time.now Mono in
              Caravan.Server.shutdown server ;
              (* The peer replies to the Close frame, and the session ends. The
                 handler waits until the grace. *)
              equal string "close 1001 \"server shutdown\""
                (server_close (await exchange)) ;
              let wait = call s (fun () -> Caravan.Server.wait server) in
              ends_at sim ~t0 Server_support.grace_g wait ) ;
          equal (list string) [] !errors )
    ; test
        "a cancel at the grace while the stop waits for the reply is not an \
         error" (fun () ->
          let errors = ref [] in
          (* The grace of 1 s ends before the close timeout of 2 s of the
             handler. *)
          let config =
            {(timed_server_config errors) with shutdown_grace= Span.s 1}
          in
          with_timed_server config (fun sim s server ->
              with_session ~reply:false s server "/wait-slow-close"
              @@ fun exchange ->
              let t0 = Time.now Mono in
              Caravan.Server.shutdown server ;
              equal string "close 1001 \"server shutdown\""
                (server_close (ends_at sim ~t0 (Span.s 1) exchange)) ) ;
          equal (list string) [] !errors )
    ; test
        "a peer that does not reply to Close 1001 gets the end of the stream \
         at the close timeout" (fun () ->
          let errors = ref [] in
          with_timed_server (timed_server_config errors) (fun sim s server ->
              with_session ~reply:false s server "/echo-fast"
              @@ fun exchange ->
              let t0 = Time.now Mono in
              Caravan.Server.shutdown server ;
              equal string "close 1001 \"server shutdown\""
                (server_close (ends_at sim ~t0 (Span.ms 100) exchange)) ) ;
          equal (list string) [] !errors )
    ; test "a handler that only sends gets the full closing handshake"
        (fun () ->
          let errors = ref [] in
          started := Ivar.create () ;
          with_timed_server (timed_server_config errors) (fun sim s server ->
              with_session ~reply:false s server "/ticker"
              @@ fun exchange ->
              sync (Ivar.read !started) ;
              let t0 = Time.now Mono in
              Caravan.Server.shutdown server ;
              (* The close waits for the reply until the close timeout. The
                 advance goes through each tick of 10 ms. *)
              let out = ends_at sim ~t0 (Span.ms 300) exchange in
              let frames =
                List.filteri (fun i _ -> i > 4) (String.split_on_char '\n' out)
              in
              is_true ~msg:"one or more ticks" (List.hd frames = "text \"tick\"") ;
              equal (list string)
                ["close 1001 \"server shutdown\""]
                (List.filter (fun f -> f <> "text \"tick\"") frames) ;
              equal string "close 1001 \"server shutdown\""
                (List.nth frames (List.length frames - 1)) ) ;
          equal (list string) [] !errors ) ]

(* Races on 4 domains *)

let race_tests =
  group "Races"
    [ test "race: fibers that send at the same time do not mix their frames"
        (fun () ->
          (* Two messages of each sender are larger than the buffers of a
             socket: they need many writes. The total is less than 256 KiB. *)
          let count = 40 in
          let body name i =
            String.make (if i mod 20 = 0 then 60_000 else i) name
          in
          Sim_support.race
          @@ fun sim ->
          with_socket sim
          @@ fun c peer ->
          in_scope
          @@ fun s ->
          let sender name =
            call ~name:"sender" s (fun () ->
                for i = 1 to count do
                  Ws.send c
                    (Binary (Printf.sprintf "%c %d %s" name i (body name i)))
                done )
          in
          let a = sender 'a' and b = sender 'b' in
          let r = Reader.create peer and last = Hashtbl.create 2 in
          for _ = 1 to 2 * count do
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
          await a ;
          await b )
    ; test "race: recv, send and close set deadlines at the same time"
        (fun () ->
          (* A reader inside a slow frame sets its read timeout before each
             read. A sender waits in a write, and a close sets the two deadlines
             of the flow from a different domain. The three calls end at the
             close deadline. The payload of the sender is 200 KiB. *)
          Sim_support.race ~guard:(Span.s 60)
          @@ fun sim ->
          with_socket ~close_timeout:ws_close ~write_timeout:ws_write
            ~read_timeout:ws_read sim
          @@ fun c peer ->
          in_scope
          @@ fun s ->
          let t0 = Time.now Mono in
          let timed f =
            call s (fun () ->
                let r = outcome f in
                (r, Time.diff Mono (Time.now Mono) t0) )
          in
          (* A frame of 100 bytes that comes one byte each 2 s. *)
          let frame = client Text (String.make 100 'x') in
          let _slow =
            call s (fun () ->
                try
                  String.iter
                    (fun ch ->
                      Flow.write_string peer (String.make 1 ch) ;
                      Time.sleep Mono (Span.s 2) )
                    frame
                with Flow.Closed -> () )
          in
          let reader = timed (fun () -> ignore (Ws.recv c)) in
          let sender =
            timed (fun () -> Ws.send c (Binary (String.make (200 * 1024) 'x')))
          in
          (* The first byte of the frame shows that the sender holds the
             writer. *)
          ignore (Flow.read peer (Bytes.create 1) 0 1) ;
          let closer = timed (fun () -> Ws.close c) in
          let closed = ("closed 1006 \"\"", ws_close) in
          equal
            (list (pair string Server_support.span))
            [closed; closed; ("ended", ws_close)]
            (List.map await [reader; sender; closer]) ) ]

let () =
  exit
    (Windtrap.run "websocket"
       [ sha1_tests
       ; base64_tests
       ; frame_tests
       ; utf8_tests
       ; connection_tests
       ; close_tests
       ; socket_tests
       ; handshake_tests
       ; server_tests
       ; time_tests
       ; timed_server_tests
       ; race_tests ] )
