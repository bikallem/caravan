let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let encode s =
  let len = String.length s in
  let groups = (len + 2) / 3 in
  let out = Bytes.create (4 * groups) in
  let byte i = if i < len then Char.code s.[i] else 0 in
  for g = 0 to groups - 1 do
    let i = 3 * g in
    let n = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
    for k = 0 to 3 do
      Bytes.set out ((4 * g) + k) alphabet.[(n lsr (18 - (6 * k))) land 63]
    done
  done;
  (* Padding replaces the characters that encode only zeros past the end. *)
  let pad = (3 - (len mod 3)) mod 3 in
  Bytes.fill out (Bytes.length out - pad) pad '=';
  Bytes.unsafe_to_string out

let value = function
| 'A' .. 'Z' as c -> Char.code c - Char.code 'A'
| 'a' .. 'z' as c -> Char.code c - Char.code 'a' + 26
| '0' .. '9' as c -> Char.code c - Char.code '0' + 52
| '+' -> 62
| '/' -> 63
| _ -> -1

let decode s =
  let len = String.length s in
  let pad =
    if len >= 4 && s.[len - 1] = '=' then if s.[len - 2] = '=' then 2 else 1
    else 0
  in
  (* Each of the [data] characters holds 6 bits. The result is full bytes,
     and the 2 or 4 bits that remain at the end must be zero. *)
  let data = len - pad in
  let out = Bytes.create (data * 6 / 8) in
  let rec go i acc bits o =
    if i = data then if acc = 0 then Some (Bytes.unsafe_to_string out) else None
    else
    let v = value s.[i] in
    if v < 0 then None
    else
    let acc = (acc lsl 6) lor v and bits = bits + 6 in
    if bits < 8 then go (i + 1) acc bits o
    else begin
      Bytes.set out o (Char.chr (acc lsr (bits - 8)));
      go (i + 1) (acc land ((1 lsl (bits - 8)) - 1)) (bits - 8) (o + 1)
    end
  in
  if len mod 4 <> 0 then None else go 0 0 0 0
