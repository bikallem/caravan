open Windtrap
open Caravan_websocket.Private

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

let handshake_tests =
  group "Handshake"
    [ test "accept_key answers the RFC 6455 example" (fun () ->
          equal string "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
            (Caravan_websocket.accept_key "dGhlIHNhbXBsZSBub25jZQ==")) ]

let () = exit (run "websocket" [ sha1_tests; base64_tests; handshake_tests ])
