(* The tests of HPACK and of its Huffman code (RFC 7541): the examples of RFC
   7541 Appendix C, the rules of the decoder, the limits of a list, the encoder
   and round trips. *)

open Windtrap
module Error = Caravan_http2.Private.Error
module Huffman = Caravan_http2.Private.Huffman
module Hpack = Caravan_http2.Private.Hpack
module D = Hpack.Decoder

let of_hex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let to_hex s =
  String.concat ""
    (List.map
       (fun c -> Printf.sprintf "%02x" (Char.code c))
       (List.of_seq (String.to_seq s)) )

(* [int_hex flags bits v] is the integer [v] with a prefix of [bits] bits (RFC
   7541 section 5.1), in hex. [flags] holds the other bits of the first byte.
   This is a second form of the encoder of the library. *)
let int_hex flags bits v =
  let max_prefix = (1 lsl bits) - 1 in
  if v < max_prefix then Printf.sprintf "%02x" (flags lor v)
  else
    let rec rest v =
      if v < 128 then Printf.sprintf "%02x" v
      else Printf.sprintf "%02x" ((v mod 128) + 128) ^ rest (v / 128)
    in
    Printf.sprintf "%02x" (flags lor max_prefix) ^ rest (v - max_prefix)

let huffman s =
  let b = Buffer.create 16 in
  Huffman.encode b s ; Buffer.contents b

let unhuffman s = Huffman.decode s 0 (String.length s)

let encode fields =
  let b = Buffer.create 64 in
  Hpack.encode b fields ; Buffer.contents b

let big = 1 lsl 20

let decode ?(max_list_size = big) ?(max_fields = big) d block =
  D.decode d ~max_list_size ~max_fields (of_hex block)

let fields d block =
  match decode d block with `Fields l -> l | `Too_large -> fail "Too_large"

let hpack_error ?(sub = "") f =
  match f () with
  | _ ->
      fail ("no Error.Hpack: " ^ sub)
  | exception Error.Hpack m ->
      contains ~sub m

let field_list = list (pair string string)

(* The result of a decode. *)
let decoded =
  Testable.make
    ~pp:(fun ppf -> function
      | `Too_large ->
          Format.pp_print_string ppf "Too_large"
      | `Fields l ->
          Format.fprintf ppf "Fields [%s]"
            (String.concat "; "
               (List.map (fun (n, v) -> Printf.sprintf "%S: %S" n v) l) ) )
    ~equal:( = )

(* The decoded list and the dynamic table, in the form of RFC 7541 Appendix
   C. *)
let show d block =
  let l = fields d block in
  let b = Buffer.create 256 in
  Buffer.add_string b "Dynamic Table (after decoding):\n" ;
  List.iteri
    (fun i (n, v) ->
      Printf.bprintf b "[%3d] (s = %3d) %s: %s\n" (i + 1)
        (String.length n + String.length v + 32)
        n v )
    (D.entries d) ;
  Printf.bprintf b "      Table size: %3d\nDecoded header list:\n"
    (D.table_size d) ;
  List.iter (fun (n, v) -> Printf.bprintf b "%s: %s\n" n v) l ;
  Buffer.contents b

(* RFC 7541 Appendix C.1. The decoder has an integer with a 5-bit prefix only in
   a table size update, and no integer with an 8-bit prefix. Thus C.1.1 and
   C.1.2 are size updates, and C.1.3 is not here. The value shows in the error
   of an update over the maximum. *)
let c1_tests =
  group "Appendix C.1"
    [ test "C.1.1: 10 with a 5-bit prefix" (fun () ->
          equal string "2a" (int_hex 0x20 5 10) ;
          equal field_list [] (fields (D.create ~max_table_size:10) "2a") ;
          hpack_error ~sub:"update to 10, over 9" (fun () ->
              decode (D.create ~max_table_size:9) "2a" ) )
    ; test "C.1.2: 1337 with a 5-bit prefix" (fun () ->
          equal string "3f9a0a" (int_hex 0x20 5 1337) ;
          equal field_list [] (fields (D.create ~max_table_size:1337) "3f9a0a") ;
          hpack_error ~sub:"update to 1337, over 1336" (fun () ->
              decode (D.create ~max_table_size:1336) "3f9a0a" ) ) ]

let c2_tests =
  group "Appendix C.2"
    [ test "C.2.1: literal with indexing" (fun () ->
          let d = D.create ~max_table_size:4096 in
          expect (show d "400a637573746f6d2d6b65790d637573746f6d2d686561646572")
          @@ __POS_OF__
               {|
            Dynamic Table (after decoding):
            [  1] (s =  55) custom-key: custom-header
                  Table size:  55
            Decoded header list:
            custom-key: custom-header
          |} )
    ; test "C.2.2: literal without indexing" (fun () ->
          let d = D.create ~max_table_size:4096 in
          expect (show d "040c2f73616d706c652f70617468")
          @@ __POS_OF__
               {|
            Dynamic Table (after decoding):
                  Table size:   0
            Decoded header list:
            :path: /sample/path
          |} )
    ; test "C.2.3: literal never indexed" (fun () ->
          let d = D.create ~max_table_size:4096 in
          expect (show d "100870617373776f726406736563726574")
          @@ __POS_OF__
               {|
            Dynamic Table (after decoding):
                  Table size:   0
            Decoded header list:
            password: secret
          |} )
    ; test "C.2.4: indexed" (fun () ->
          let d = D.create ~max_table_size:4096 in
          expect (show d "82")
          @@ __POS_OF__
               {|
            Dynamic Table (after decoding):
                  Table size:   0
            Decoded header list:
            :method: GET
          |} )
    ]

(* C.3 and C.4 give the same lists and tables. *)
let requests (b1, b2, b3) =
  let d = D.create ~max_table_size:4096 in
  expect (show d b1)
  @@ __POS_OF__
       {|
    Dynamic Table (after decoding):
    [  1] (s =  57) :authority: www.example.com
          Table size:  57
    Decoded header list:
    :method: GET
    :scheme: http
    :path: /
    :authority: www.example.com
  |} ;
  expect (show d b2)
  @@ __POS_OF__
       {|
    Dynamic Table (after decoding):
    [  1] (s =  53) cache-control: no-cache
    [  2] (s =  57) :authority: www.example.com
          Table size: 110
    Decoded header list:
    :method: GET
    :scheme: http
    :path: /
    :authority: www.example.com
    cache-control: no-cache
  |} ;
  expect (show d b3)
  @@ __POS_OF__
       {|
    Dynamic Table (after decoding):
    [  1] (s =  54) custom-key: custom-value
    [  2] (s =  53) cache-control: no-cache
    [  3] (s =  57) :authority: www.example.com
          Table size: 164
    Decoded header list:
    :method: GET
    :scheme: https
    :path: /index.html
    :authority: www.example.com
    custom-key: custom-value
  |}

(* C.5 and C.6 give the same lists and tables, with a table of 256 bytes. *)
let responses (b1, b2, b3) =
  let d = D.create ~max_table_size:256 in
  expect (show d b1)
  @@ __POS_OF__
       {|
    Dynamic Table (after decoding):
    [  1] (s =  63) location: https://www.example.com
    [  2] (s =  65) date: Mon, 21 Oct 2013 20:13:21 GMT
    [  3] (s =  52) cache-control: private
    [  4] (s =  42) :status: 302
          Table size: 222
    Decoded header list:
    :status: 302
    cache-control: private
    date: Mon, 21 Oct 2013 20:13:21 GMT
    location: https://www.example.com
  |} ;
  expect (show d b2)
  @@ __POS_OF__
       {|
    Dynamic Table (after decoding):
    [  1] (s =  42) :status: 307
    [  2] (s =  63) location: https://www.example.com
    [  3] (s =  65) date: Mon, 21 Oct 2013 20:13:21 GMT
    [  4] (s =  52) cache-control: private
          Table size: 222
    Decoded header list:
    :status: 307
    cache-control: private
    date: Mon, 21 Oct 2013 20:13:21 GMT
    location: https://www.example.com
  |} ;
  expect (show d b3)
  @@ __POS_OF__
       {|
    Dynamic Table (after decoding):
    [  1] (s =  98) set-cookie: foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1
    [  2] (s =  52) content-encoding: gzip
    [  3] (s =  65) date: Mon, 21 Oct 2013 20:13:22 GMT
          Table size: 215
    Decoded header list:
    :status: 200
    cache-control: private
    date: Mon, 21 Oct 2013 20:13:22 GMT
    location: https://www.example.com
    content-encoding: gzip
    set-cookie: foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1
  |}

(* A script copied the hex dumps from the text of RFC 7541. *)
let c3_to_c6_tests =
  group "Appendix C.3 to C.6"
    [ test "C.3: requests without Huffman" (fun () ->
          requests
            ( "828684410f7777772e6578616d706c652e636f6d"
            , "828684be58086e6f2d6361636865"
            , "828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565" ) )
    ; test "C.4: requests with Huffman" (fun () ->
          requests
            ( "828684418cf1e3c2e5f23a6ba0ab90f4ff"
            , "828684be5886a8eb10649cbf"
            , "828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf" ) )
    ; test "C.5: responses without Huffman, with eviction" (fun () ->
          responses
            ( "4803333032580770726976617465611d4d6f6e2c203231204f637420323031332032303a31333a323120474d546e1768747470733a2f2f7777772e6578616d706c652e636f6d"
            , "4803333037c1c0bf"
            , "88c1611d4d6f6e2c203231204f637420323031332032303a31333a323220474d54c05a04677a69707738666f6f3d4153444a4b48514b425a584f5157454f50495541585157454f49553b206d61782d6167653d333630303b2076657273696f6e3d31"
            ) )
    ; test "C.6: responses with Huffman, with eviction" (fun () ->
          responses
            ( "488264025885aec3771a4b6196d07abe941054d444a8200595040b8166e082a62d1bff6e919d29ad171863c78f0b97c8e9ae82ae43d3"
            , "4883640effc1c0bf"
            , "88c16196d07abe941054d444a8200595040b8166e084a62d1bffc05a839bd9ab77ad94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587316065c003ed4ee5b1063d5007"
            ) ) ]

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

(* The rules of the decoder (RFC 7541 sections 2.3.3, 4, 5 and 6). *)
let size_update v = int_hex 0x20 5 v

let rule_tests =
  group "Decoder rules"
    [ test "an integer over 2^28 is an error" (fun () ->
          let d = D.create ~max_table_size:(1 lsl 30) in
          equal field_list [] (fields d (size_update (1 lsl 28))) ;
          hpack_error ~sub:"over 2^28" (fun () ->
              decode d (size_update ((1 lsl 28) + 1)) ) ;
          (* 2^35 - 1 + 31 in five continuation bytes. *)
          hpack_error ~sub:"over 2^28" (fun () -> decode d "3fffffffff7f") )
    ; test "an integer with more than 5 continuation bytes is an error"
        (fun () ->
          let d = D.create ~max_table_size:4096 in
          (* 31 + 0, with 5 continuation bytes: correct. *)
          equal field_list [] (fields d "3f8080808000") ;
          hpack_error ~sub:"too many bytes" (fun () ->
              decode d "3f808080808000" ) )
    ; test "an integer that ends after the block is an error" (fun () ->
          let d = D.create ~max_table_size:4096 in
          hpack_error ~sub:"ends after the block" (fun () -> decode d "3f") ;
          hpack_error ~sub:"ends after the block" (fun () -> decode d "3f80") ;
          hpack_error ~sub:"ends after the block" (fun () -> decode d "ff") )
    ; test "a string past the end of the block is an error" (fun () ->
          let d = D.create ~max_table_size:4096 in
          (* A literal without indexing, a new name of 1 byte, a value of 2
             bytes with 1 byte in the block. *)
          equal field_list [("a", "bc")] (fields d "000161026263") ;
          hpack_error ~sub:"string of 2 bytes" (fun () -> decode d "0001610262") ;
          hpack_error ~sub:"string of 2 bytes" (fun () -> decode d "000261") ;
          hpack_error ~sub:"string of 3 bytes" (fun () -> decode d "00830000") ;
          hpack_error ~sub:"ends before a string" (fun () -> decode d "00") ;
          hpack_error ~sub:"ends before a string" (fun () -> decode d "000161") ;
          hpack_error ~sub:"ends before a string" (fun () -> decode d "40") )
    ; test "the index 0 is an error" (fun () ->
          let d = D.create ~max_table_size:4096 in
          hpack_error ~sub:"index 0" (fun () -> decode d "80") ;
          hpack_error ~sub:"index 0" (fun () -> decode d "8280") )
    ; test "an index past the two tables is an error" (fun () ->
          let d = D.create ~max_table_size:4096 in
          equal field_list [("www-authenticate", "")] (fields d "bd") ;
          hpack_error ~sub:"index 62 is not" (fun () -> decode d "be") ;
          hpack_error ~sub:"index 62 is not" (fun () -> decode d "7e0161") ;
          hpack_error ~sub:"index 127 is not" (fun () -> decode d "ff00") ;
          (* One entry: 62 is correct, 63 is not. *)
          equal field_list [("a", "b")] (fields d "4001610162") ;
          equal field_list [("a", "b"); ("a", "c")] (fields d "be0f2f0163") ;
          hpack_error ~sub:"index 63 is not" (fun () -> decode d "bf") )
    ; test "a table size update over the maximum is an error" (fun () ->
          let d = D.create ~max_table_size:100 in
          equal field_list [] (fields d (size_update 100)) ;
          hpack_error ~sub:"update to 101, over 100" (fun () ->
              decode d (size_update 101) ) )
    ; test "a table size update after a field is an error" (fun () ->
          let d = D.create ~max_table_size:4096 in
          (* Two updates before the first field are correct. *)
          equal field_list [(":method", "GET")] (fields d "203f8f1f82") ;
          hpack_error ~sub:"after a field" (fun () -> decode d "8220") ;
          hpack_error ~sub:"after a field" (fun () -> decode d "00016101622020") )
    ; test "a table size update evicts the oldest entries" (fun () ->
          let d = D.create ~max_table_size:4096 in
          (* Three entries of 34 bytes. *)
          ignore (fields d "400161016140016201624001630163") ;
          equal int 102 (D.table_size d) ;
          equal field_list [] (fields d (size_update 68)) ;
          equal field_list [("c", "c"); ("b", "b")] (D.entries d) ;
          equal field_list [] (fields d (size_update 67)) ;
          equal field_list [("c", "c")] (D.entries d) ;
          equal int 34 (D.table_size d) ;
          (* The new maximum stays: with an entry of 36 bytes, the table keeps
             only this entry. *)
          equal field_list [("dd", "dd")] (fields d "40026464026464") ;
          equal field_list [("dd", "dd")] (D.entries d) )
    ; test "an entry larger than the table empties the table" (fun () ->
          let d = D.create ~max_table_size:100 in
          (* An empty table stays empty. *)
          let v = String.make 68 'v' in
          ignore (fields d ("400161" ^ int_hex 0 7 68 ^ to_hex v)) ;
          equal int 0 (D.table_size d) ;
          ignore (fields d "4001610161") ;
          equal int 34 (D.table_size d) ;
          (* name + value + 32 = 101. *)
          let v = String.make 68 'v' in
          equal field_list
            [("a", v)]
            (fields d ("400161" ^ int_hex 0 7 68 ^ to_hex v)) ;
          equal int 0 (D.table_size d) ;
          equal field_list [] (D.entries d) ;
          (* name + value + 32 = 100: the entry fits, and evicts the others. *)
          ignore (fields d "4001610161") ;
          let v = String.make 67 'v' in
          ignore (fields d ("400161" ^ int_hex 0 7 67 ^ to_hex v)) ;
          equal int 100 (D.table_size d) ;
          equal field_list [("a", v)] (D.entries d) )
    ; test "the table grows past 16 entries in order" (fun () ->
          let d = D.create ~max_table_size:4096 in
          let n = 40 in
          for i = 1 to n do
            let v = string_of_int i in
            ignore
              (fields d
                 ("4001" ^ to_hex "k" ^ int_hex 0 7 (String.length v) ^ to_hex v) )
          done ;
          equal field_list
            (List.init n (fun i -> ("k", string_of_int (n - i))))
            (D.entries d) ;
          equal field_list
            [("k", "40"); ("k", "1")]
            (fields d (int_hex 0x80 7 62 ^ int_hex 0x80 7 (61 + n))) )
    ; test "a Huffman error in a block is an error" (fun () ->
          let d = D.create ~max_table_size:4096 in
          hpack_error ~sub:"not all 1 bits" (fun () -> decode d "0001618118") )
    ; test "the decoder does not check the names and the values" (fun () ->
          let d = D.create ~max_table_size:4096 in
          equal field_list
            [("Bad Name", "\x00\r\n")]
            (fields d ("0008" ^ to_hex "Bad Name" ^ "03000d0a")) )
    ; test "a negative max_table_size is refused" (fun () ->
          raises
            (Invalid_argument "Hpack.Decoder.create: a negative max_table_size")
            (fun () -> D.create ~max_table_size:(-1) ) ;
          equal int 0 (D.table_size (D.create ~max_table_size:0)) ) ]

let too_large_tests =
  group "Too_large"
    [ test "a list over max_list_size is too large" (fun () ->
          let d = D.create ~max_table_size:4096 in
          (* "a: b" has the size 1 + 1 + 32 = 34. *)
          equal decoded
            (`Fields [("a", "b"); ("a", "c")])
            (decode d ~max_list_size:68 "00016101620001610163") ;
          equal decoded `Too_large
            (decode d ~max_list_size:67 "00016101620001610163") )
    ; test "a list with more than max_fields fields is too large" (fun () ->
          let d = D.create ~max_table_size:4096 in
          equal decoded
            (`Fields [(":method", "GET"); (":path", "/")])
            (decode d ~max_fields:2 "8284") ;
          equal decoded `Too_large (decode d ~max_fields:2 "828486") ;
          equal decoded (`Fields []) (decode d ~max_fields:0 "") ;
          equal decoded `Too_large (decode d ~max_fields:0 "82") )
    ; test "a too large list keeps the dynamic table correct" (fun () ->
          let d = D.create ~max_table_size:4096 in
          (* Block 1: a field of 3000 bytes (an entry of 3032 bytes), then a
             small field, the two with indexing. The first field is over
             max_list_size, thus the decoder keeps no field, but it adds the two
             entries. *)
          let v = String.make 2997 'v' in
          let block1 =
            "4003" ^ to_hex "big" ^ int_hex 0 7 2997 ^ to_hex v ^ "4001610162"
          in
          equal decoded `Too_large (decode d ~max_list_size:2048 block1) ;
          equal int (3032 + 34) (D.table_size d) ;
          (* Block 2 references the two entries. *)
          equal field_list [("big", v); ("a", "b")] (fields d "bfbe") )
    ; test "a too large list continues to decode until the end" (fun () ->
          let d = D.create ~max_table_size:4096 in
          (* The index 0 after the limit is an error. *)
          hpack_error ~sub:"index 0" (fun () -> decode d ~max_fields:1 "828280") ;
          hpack_error ~sub:"not all 1 bits" (fun () ->
              decode d ~max_list_size:1 "820001618118" ) ) ]

let encoder_tests =
  group "Encoder"
    [ test "the blocks of the static rules" (fun () ->
          let check fields hex =
            equal string
              ~msg:(String.concat ", " (List.map fst fields))
              hex
              (to_hex (encode fields))
          in
          (* A size update to 0 only. *)
          check [] "20" ;
          (* Indexed: a field of the static table. *)
          check [(":method", "GET")] "2082" ;
          (* Indexed, then a literal without indexing with the name index 31 and
             a Huffman value. *)
          check
            [(":status", "200"); ("content-type", "text/html")]
            "20880f1087497ca589d34d1f" ;
          (* The second field has the name of the static table, but not the
             value. *)
          check
            [("accept-encoding", "gzip, deflate"); ("accept-encoding", "br")]
            "20900f01026272" ;
          (* Literal never indexed, with the name index, also for the empty
             value of the static table. *)
          check
            [ ("cookie", "a=b")
            ; ("set-cookie", "id=1; Secure")
            ; ("authorization", "") ]
            "201f1103613d621f2889349007ed4dc525b6171f0800" ;
          check [("cookie", "")] "201f1100" ;
          (* The empty value of a name with values in the static table is not
             indexed. The empty value of :authority (1) is. A later value of a
             name is indexed (":status: 404" is 13). *)
          check [(":method", "")] "200200" ;
          (* The name index 1. *)
          check [(":authority", "a")] "20010161" ;
          check [(":authority", ""); (":status", "404")] "20818d" ;
          (* An uppercase name is a new name, also for set-cookie. *)
          check [("Set-Cookie", "")] "200088dc54ad78e7ea62ff00" ;
          (* A new name, with a Huffman name and a Huffman value. *)
          check [("x-new", "value")] "200084f2b545f184ee3a2d2f" ;
          (* A Huffman string of the same length is not shorter. *)
          check [(":path", "/x")] "2004022f78" ;
          (* A length of 300: 127, then 173 and 1. *)
          let zeros = String.make 300 '\x00' in
          check [("x-a", zeros)] ("200003782d617fad01" ^ to_hex zeros) ;
          (* A length of 255: 127, then 128 in 2 bytes. *)
          let zeros = String.make 255 '\x00' in
          check [("x-a", zeros)] ("200003782d617f8001" ^ to_hex zeros) ;
          (* A Huffman string of 188 bytes: 127, then 61. *)
          let a = String.make 300 'a' in
          check [("x-a", a)] ("200003782d61ff3d" ^ to_hex (huffman a)) ) ]

let names =
  [ ":authority"
  ; ":method"
  ; ":path"
  ; ":scheme"
  ; ":status"
  ; "accept-encoding"
  ; "content-type"
  ; "www-authenticate"
  ; "cookie"
  ; "set-cookie"
  ; "authorization" ]

let values = [""; "GET"; "/"; "200"; "gzip, deflate"; "http"]

let fields_gen =
  let open Gen in
  let name =
    frequency
      [ (2, of_list ~pp:Format.pp_print_string names)
      ; (1, string_of ~size:(int_range 0 20) (char_range 'a' 'z')) ]
  in
  let value =
    frequency
      [ (1, of_list ~pp:Format.pp_print_string values)
      ; (2, string_of ~size:(int_range 0 300) char)
      ; (1, string_of ~size:(int_range 0 300) (char_range 'a' 'z')) ]
  in
  list ~size:(int_range 0 12) (pair name value)

(* A block of hostile representations, as a hex string. Its parts: indices near
   the end of the static table, raw and Huffman strings, the 4 literal forms,
   size updates mostly at the start, and a cut block. *)
let hostile_block_gen =
  let open Gen in
  let index =
    frequency [(6, of_list [1; 2; 60; 61; 62; 63; 64]); (1, int_range 0 70)]
  in
  let text =
    let* size = of_list [0; 1; 3; 10; 40; 200] in
    let* s =
      string_of ~size:(constant size)
        (of_list (List.of_seq (String.to_seq "abcxyz-:")))
    in
    let+ h = bool in
    if h then
      let e = huffman s in
      int_hex 0x80 7 (String.length e) ^ to_hex e
    else int_hex 0 7 (String.length s) ^ to_hex s
  in
  let literal flags bits =
    let* i = frequency [(1, constant 0); (1, index)] in
    let* name = text in
    let+ value = text in
    int_hex flags bits i ^ (if i = 0 then name else "") ^ value
  in
  let update =
    let+ v = of_list [0; 32; 100; 200; 256; 257] in
    int_hex 0x20 5 v
  in
  let representation =
    frequency
      [ (12, map (int_hex 0x80 7) index)
      ; (12, literal 0x40 6)
      ; (8, literal 0x00 4)
      ; (4, literal 0x10 4)
      ; (1, update) ]
  in
  let* updates = list ~size:(int_range 0 2) update in
  let* rest = list ~size:(int_range 0 8) representation in
  let block = String.concat "" (updates @ rest) in
  let+ cut = frequency [(19, constant None); (1, map Option.some nat)] in
  match cut with
  | Some n when block <> "" ->
      String.sub block 0 (2 * (n mod (String.length block / 2)))
  | _ ->
      block

let property_tests =
  group "Properties"
    [ prop "the block starts with 0x20, decodes to the list and adds no entry"
        fields_gen (fun l ->
          let block = encode l in
          equal char '\x20' block.[0] ;
          (* With a table of 4096 bytes, an indexed literal adds an entry. Thus
             a table size of 0 shows that the encoder never indexes. *)
          let d = D.create ~max_table_size:4096 in
          equal decoded (`Fields l)
            (D.decode d ~max_list_size:big ~max_fields:big block) ;
          equal int 0 (D.table_size d) ;
          (* The update to 0 is correct also with a table of 0 bytes. *)
          let d = D.create ~max_table_size:0 in
          equal decoded (`Fields l)
            (D.decode d ~max_list_size:big ~max_fields:big block) )
    ; prop ~count:2000
        "hostile blocks give Fields, Too_large or Error.Hpack, and the table \
         stays correct"
        Gen.(list ~size:(int_range 1 6) hostile_block_gen)
        (fun blocks ->
          (* A table of 256 bytes, to evict often. After an error, the
             connection closes, thus the next blocks do not run. *)
          let d = D.create ~max_table_size:256 in
          let fields = ref false
          and too_large = ref false
          and used = ref false in
          ( try
              List.iter
                (fun b ->
                  ( match
                      D.decode d ~max_list_size:512 ~max_fields:8 (of_hex b)
                    with
                  | `Fields l ->
                      fields := !fields || l <> []
                  | `Too_large ->
                      too_large := true ) ;
                  (* The table size is the sum of the entries, and at most
                     256. *)
                  let sizes =
                    List.map
                      (fun (n, v) -> String.length n + String.length v + 32)
                      (D.entries d)
                  in
                  equal int (List.fold_left ( + ) 0 sizes) (D.table_size d) ;
                  is_true (D.table_size d <= 256) ;
                  used := !used || sizes <> [] )
                blocks
            with Error.Hpack _ -> () ) ;
          cover ~share:0.1 "fields" !fields ;
          cover ~share:0.03 "too large" !too_large ;
          cover ~share:0.1 "dynamic table used" !used )
    ; prop ~count:10000 "random bytes give Fields, Too_large or Error.Hpack"
        Gen.(list ~size:(int_range 1 4) (string_of ~size:(int_range 0 40) char))
        (fun blocks ->
          (* A table of 256 bytes, to evict often. After an error, the
             connection closes, thus the next blocks do not run. *)
          let d = D.create ~max_table_size:256 in
          try
            List.iter
              (fun b ->
                match D.decode d ~max_list_size:512 ~max_fields:8 b with
                | `Fields _ | `Too_large ->
                    () )
              blocks
          with Error.Hpack _ -> () ) ]

let () =
  exit
    (Windtrap.run "hpack"
       [ c1_tests
       ; c2_tests
       ; c3_to_c6_tests
       ; huffman_tests
       ; rule_tests
       ; too_large_tests
       ; encoder_tests
       ; property_tests ] )
