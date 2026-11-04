let standard =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let url_safe =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

let encode ?(url = false) ?(pad = true) s =
  let alphabet = if url then url_safe else standard in
  let len = String.length s in
  let groups = (len + 2) / 3 in
  let out = Bytes.create (4 * groups) in
  let byte i =
    if i < len then Char.code s.[i]
    else (0 [@mutate off "the bits past the end never reach a character"])
  in
  for g = 0 to groups - 1 do
    let i = 3 * g in
    let n = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
    for k = 0 to 3 do
      Bytes.set out ((4 * g) + k) alphabet.[(n lsr (18 - (6 * k))) land 63]
    done
  done ;
  (* The characters past the end encode only zeros: padding replaces them, or
     the text stops before them. *)
  let extra = (3 - (len mod 3)) mod 3 in
  if pad then (
    Bytes.fill out (Bytes.length out - extra) extra '=' ;
    Bytes.unsafe_to_string out )
  else Bytes.sub_string out 0 (Bytes.length out - extra)

let value ~url = function
  | 'A' .. 'Z' as c ->
      Char.code c - Char.code 'A'
  | 'a' .. 'z' as c ->
      Char.code c - Char.code 'a' + 26
  | '0' .. '9' as c ->
      Char.code c - Char.code '0' + 52
  | '+' when not url ->
      62
  | '/' when not url ->
      63
  | '-' when url ->
      62
  | '_' when url ->
      63
  | _ ->
      -1

let decode ?(url = false) ?(pad = true) s =
  let len = String.length s in
  let padding =
    if pad && len >= 4 && s.[len - 1] = '=' then
      if s.[len - 2] = '=' then 2 else 1
    else 0
  in
  (* Each of the [data] characters holds 6 bits. The result is full bytes, and
     the 2 or 4 bits that remain at the end must be zero. *)
  let data = len - padding in
  let out = Bytes.create (data * 6 / 8) in
  let rec go i acc bits o =
    if i = data then if acc = 0 then Some (Bytes.unsafe_to_string out) else None
    else
      let v = value ~url s.[i] in
      if v < 0 then None
      else
        let acc = (acc lsl 6) lor v and bits = bits + 6 in
        if bits < 8 then go (i + 1) acc bits o
        else begin
          Bytes.set out o (Char.chr (acc lsr (bits - 8))) ;
          go (i + 1) (acc land ((1 lsl (bits - 8)) - 1)) (bits - 8) (o + 1)
        end
  in
  (* With padding, the length is a multiple of 4. With no padding, a length of
     4k+1 holds no full byte in its last character. *)
  if if pad then len mod 4 <> 0 else len mod 4 = 1 then None else go 0 0 0 0
