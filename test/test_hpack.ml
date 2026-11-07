(* The tests of HPACK and of its Huffman code (RFC 7541): the Huffman code. *)

open Windtrap
module Error = Caravan_http2.Private.Error
module Huffman = Caravan_http2.Private.Huffman

let of_hex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let to_hex s =
  String.concat ""
    (List.map
       (fun c -> Printf.sprintf "%02x" (Char.code c))
       (List.of_seq (String.to_seq s)) )

let huffman s =
  let b = Buffer.create 16 in
  Huffman.encode b s ; Buffer.contents b

let unhuffman s = Huffman.decode s 0 (String.length s)

let hpack_error ?(sub = "") f =
  match f () with
  | _ ->
      fail ("no Error.Hpack: " ^ sub)
  | exception Error.Hpack m ->
      contains ~sub m

(* The Huffman strings of RFC 7541 Appendix C.4 and C.6. *)
let huffman_vectors =
  [ ("www.example.com", "f1e3c2e5f23a6ba0ab90f4ff")
  ; ("no-cache", "a8eb10649cbf")
  ; ("custom-key", "25a849e95ba97d7f")
  ; ("custom-value", "25a849e95bb8e8b4bf")
  ; ("302", "6402")
  ; ("307", "640eff")
  ; ("private", "aec3771a4b")
  ; ( "Mon, 21 Oct 2013 20:13:21 GMT"
    , "d07abe941054d444a8200595040b8166e082a62d1bff" )
  ; ( "Mon, 21 Oct 2013 20:13:22 GMT"
    , "d07abe941054d444a8200595040b8166e084a62d1bff" )
  ; ("https://www.example.com", "9d29ad171863c78f0b97c8e9ae82ae43d3")
  ; ("gzip", "9bd9ab")
  ; ( "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"
    , "94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007"
    ) ]

let all_bytes = String.init 256 Char.chr

let huffman_tests =
  group "Huffman"
    [ test "the strings of RFC 7541 Appendix C" (fun () ->
          List.iter
            (fun (s, h) ->
              equal string ~msg:s h (to_hex (huffman s)) ;
              equal int ~msg:s (String.length h / 2) (Huffman.encoded_length s) ;
              equal string ~msg:s s (unhuffman (of_hex h)) )
            huffman_vectors )
    ; test "the codes of RFC 7541 Appendix B, from the shortest to the longest"
        (fun () ->
          (* '0' has 5 bits, '\x00' 13 bits, '\x01' 23 bits, '\x02' 28 bits and
             '\n' 30 bits. The padding fills the last byte with 1 bits. *)
          equal string "07" (to_hex (huffman "0")) ;
          equal string "ffc7" (to_hex (huffman "\x00")) ;
          equal string "ffffb1" (to_hex (huffman "\x01")) ;
          equal string "fffffe2f" (to_hex (huffman "\x02")) ;
          equal string "fffffff3" (to_hex (huffman "\n")) ;
          equal string "" (huffman "") ;
          equal int 0 (Huffman.encoded_length "") )
    ; test "the code of each byte is its row of RFC 7541 Appendix B" (fun () ->
          (* 8 copies of a byte give 8 codes and no padding. Thus the length of
             the code in bits is the number of bytes, and the code is the first
             bits. A script computed the MD5 digest from the text of the RFC:
             one line "symbol code length" for each row 0 to 255, with the code
             in hex. EOS has its own tests. *)
          let rows = Buffer.create 4096 in
          for c = 0 to 255 do
            let h = huffman (String.make 8 (Char.chr c)) in
            let len = String.length h and code = ref 0 in
            for i = 0 to len - 1 do
              let bit = (Char.code h.[i / 8] lsr (7 - (i mod 8))) land 1 in
              code := (!code lsl 1) lor bit
            done ;
            Printf.bprintf rows "%d %x %d\n" c !code len
          done ;
          equal string "149ef2d8ddab36e92251d95096e47aa3"
            (Digest.to_hex (Digest.string (Buffer.contents rows))) )
    ; test "each byte decodes, alone and in one string" (fun () ->
          String.iter
            (fun c ->
              let s = String.make 1 c in
              equal string ~msg:(String.escaped s) s (unhuffman (huffman s)) )
            all_bytes ;
          let h = huffman all_bytes in
          equal int (String.length h) (Huffman.encoded_length all_bytes) ;
          equal string all_bytes (unhuffman h) )
    ; test "codes of 5 bits decode to the longest string" (fun () ->
          (* '0' has a code of 5 bits, the shortest. 8 of them fill 5 bytes. *)
          equal string "00000000" (unhuffman (huffman "00000000")) ;
          let s = String.make 300 '0' in
          equal string s (unhuffman (huffman s)) )
    ; test "EOS in a string is an error" (fun () ->
          (* EOS is 30 1 bits. *)
          hpack_error ~sub:"EOS" (fun () -> unhuffman "\xff\xff\xff\xff") ;
          (* 'a' (00011), then EOS, then 1 bits. *)
          hpack_error ~sub:"EOS" (fun () -> unhuffman "\x1f\xff\xff\xff\xff") )
    ; test "a padding of more than 7 bits is an error" (fun () ->
          (* 'a', then 11 1 bits. *)
          hpack_error ~sub:"more than 7 bits" (fun () -> unhuffman "\x1f\xff") ;
          (* Only 8 1 bits. *)
          hpack_error ~sub:"more than 7 bits" (fun () -> unhuffman "\xff") ;
          (* 'a', then 3 1 bits: correct. *)
          equal string "a" (unhuffman "\x1f") )
    ; test "a padding with a 0 bit is an error" (fun () ->
          (* 'a', then 000. *)
          hpack_error ~sub:"not all 1 bits" (fun () -> unhuffman "\x18") ;
          (* 'a', then 110. *)
          hpack_error ~sub:"not all 1 bits" (fun () -> unhuffman "\x1e") )
    ; test "encode_n refuses a length that is not the encoded length" (fun () ->
          let buf = Buffer.create 8 in
          Buffer.add_string buf "x" ;
          (* "a" has 5 bits: one byte. *)
          raises (Invalid_argument "Huffman.encode_n") (fun () ->
              Huffman.encode_n buf "a" 4 ) ;
          raises (Invalid_argument "index out of bounds") (fun () ->
              Huffman.encode_n buf "aa" 1 ) ;
          equal Testable.string "x" (Buffer.contents buf) ;
          Huffman.encode_n buf "a" 1 ;
          equal Testable.string "x\x1f" (Buffer.contents buf) )
    ; test "a range that is not in the string is refused" (fun () ->
          let refused off len =
            raises (Invalid_argument "Huffman.decode") (fun () ->
                Huffman.decode "\x1f\x1f" off len )
          in
          refused (-1) 1 ;
          refused 0 (-1) ;
          refused 0 3 ;
          refused 1 2 ;
          refused 3 0 ;
          equal string "a" (Huffman.decode "\x1f\x1f" 1 1) ;
          equal string "a" (Huffman.decode "\x1f\x1f" 0 1) ;
          equal string "" (Huffman.decode "\x1f\x1f" 1 0) )
    ; prop "a string decodes to itself, at each offset"
        Gen.(
          triple
            (string_of ~size:(int_range 0 300) char)
            (string_of ~size:(int_range 0 3) char)
            (string_of ~size:(int_range 0 3) char) )
        (fun (s, before, after) ->
          let h = huffman s in
          equal int (String.length h) (Huffman.encoded_length s) ;
          let all = before ^ h ^ after in
          equal string s
            (Huffman.decode all (String.length before) (String.length h)) ) ]

let () = exit (Windtrap.run "hpack" [huffman_tests])
