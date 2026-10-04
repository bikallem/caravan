let mask = 0xffff_ffff
let rol x n = ((x lsl n) lor (x lsr (32 - n))) land mask

let digest s =
  let len = String.length s in
  (* The message, a 0x80 byte, zeros, then its length in bits on 8 bytes,
     padded to whole 64-byte blocks. *)
  let padded = (len + 9 + 63) / 64 * 64 in
  let m = Bytes.make padded '\000' in
  Bytes.blit_string s 0 m 0 len;
  Bytes.set m len '\x80';
  Bytes.set_int64_be m (padded - 8) (Int64.mul (Int64.of_int len) 8L);
  let h = [| 0x67452301; 0xefcdab89; 0x98badcfe; 0x10325476; 0xc3d2e1f0 |] in
  let w = Array.make 80 0 in
  for block = 0 to (padded / 64) - 1 do
    for t = 0 to 15 do
      w.(t) <- Int32.to_int (Bytes.get_int32_be m ((block * 64) + (4 * t))) land mask
    done;
    for t = 16 to 79 do
      w.(t) <- rol (w.(t - 3) lxor w.(t - 8) lxor w.(t - 14) lxor w.(t - 16)) 1
    done;
    let a = ref h.(0) and b = ref h.(1) and c = ref h.(2) and d = ref h.(3)
    and e = ref h.(4) in
    for t = 0 to 79 do
      let f, k =
        if t < 20 then ((!b land !c) lor (lnot !b land !d), 0x5a827999)
        else if t < 40 then (!b lxor !c lxor !d, 0x6ed9eba1)
        else if t < 60 then
          ((!b land !c) lor (!b land !d) lor (!c land !d), 0x8f1bbcdc)
        else (!b lxor !c lxor !d, 0xca62c1d6)
      in
      let temp = (rol !a 5 + f + !e + k + w.(t)) land mask in
      e := !d;
      d := !c;
      c := rol !b 30;
      b := !a;
      a := temp
    done;
    List.iteri
      (fun i x -> h.(i) <- (h.(i) + x) land mask)
      [ !a; !b; !c; !d; !e ]
  done;
  let out = Bytes.create 20 in
  Array.iteri (fun i x -> Bytes.set_int32_be out (4 * i) (Int32.of_int x)) h;
  Bytes.unsafe_to_string out
