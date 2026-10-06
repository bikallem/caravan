exception Invalid

(* A byte that can start a sequence of 2 to 4 bytes (Unicode table 3-7). *)
let is_lead c = c >= '\xc2' && c <= '\xf4'

(* For an incorrect decode, [String.get_utf_8_uchar] gives the length of
   the maximal subpart: the longest prefix of a correct sequence (Unicode
   §3.9). Thus, after a lead byte, a maximal subpart that reaches the end
   of the input is an incomplete sequence. A byte that is not a lead byte
   also gives a length of 1, thus we examine the lead byte too. *)
let continue pending s =
  let n = String.length s in
  let rec inside i =
    if i = n
    then ""
    else begin
      let d = String.get_utf_8_uchar s i in
      let len = Uchar.utf_decode_length d in
      if Uchar.utf_decode_is_valid d then inside (i + len)
      else if i + len = n && is_lead s.[i] then String.sub s i len
      else raise Invalid
    end
  in
  if pending = ""
  then inside 0
  else begin
    (* [pending] has 1 to 3 bytes of a sequence of at most 4 bytes, thus
       the sequence ends in the first 3 bytes of [s]. *)
    let p = String.length pending in
    let t = pending ^ String.sub s 0 (Int.min 3 n) in
    let d = String.get_utf_8_uchar t 0 in
    let len = Uchar.utf_decode_length d in
    if Uchar.utf_decode_is_valid d then inside (len - p)
    (* A maximal subpart has at most 3 bytes, thus [t] has at most 3
       bytes, and all of [s] is in [t]. *)
    else if len = String.length t then t
    else raise Invalid
  end
