exception Invalid

(* The length of the sequence that the lead byte [c] starts, 0 if [c] cannot
   start a sequence (Unicode table 3-7). *)
let length c =
  if c < 0x80 then (1
    [@mutate off "only an incorrect pending gives ASCII: need <= 0 with 0"] )
  else if c < 0xc2 then 0
  else if c < 0xe0 then 2
  else if c < 0xf0 then 3
  else if c < 0xf5 then 4
  else 0

(* Whether [b] can be the byte at the position [k] (1 to 3) of a sequence that
   starts with [c] (Unicode table 3-7). *)
let fits c k b =
  if k > 1 then b land 0xc0 = 0x80
  else
    match c with
    | 0xe0 ->
        b >= 0xa0 && b <= 0xbf
    | 0xed ->
        b >= 0x80 && b <= 0x9f
    | 0xf0 ->
        b >= 0x90 && b <= 0xbf
    | 0xf4 ->
        b >= 0x80 && b <= 0x8f
    | _ ->
        b land 0xc0 = 0x80

let ascii8 = 0x8080_8080_8080_8080L

(* A continuation byte, 0x80 to 0xBF. *)
let[@inline] cont b = b land 0xc0 = 0x80

(* The careful path for a sequence that starts at [i] and can end after [stop]:
   it examines only the bytes before [stop]. Gives the bytes of the incomplete
   sequence, or [""] and the position after a complete one in [next]. *)
let tail buf i stop next =
  let c = Bytes.get_uint8 buf i in
  let n = length c in
  if n = 0 then raise Invalid ;
  let avail = Int.min n (stop - i) in
  for k = 1 to avail - 1 do
    if not (fits c k (Bytes.get_uint8 buf (i + k))) then raise Invalid
  done ;
  if avail < n then Bytes.sub_string buf i avail
  else begin
    next := i + n ;
    ""
  end

(* Examines [buf] from [i] to [stop] (excluded), which must be in [buf]. Gives
   the bytes of an incomplete sequence at the end, [""] if there is none. It
   reads no byte at [stop] or after: the unsafe reads are before [stop]. The
   branches follow the stdlib Bytes.is_valid_utf_8. *)
let rec scan buf i stop =
  if i >= stop then ""
  else
    let c = Char.code (Bytes.unsafe_get buf i) and left = stop - i in
    (* With exactly 8 bytes left, the loop of one byte gives the result of the
       step of 8 bytes. With exactly 4 bytes left, [tail] gives the result of
       the main path. *)
    if c < 0x80 then
      if
        (left >= 8) [@mutate off "the same result"]
        && Int64.logand (Bytes.get_int64_le buf i) ascii8
           = (0L [@mutate off "never 1L: the byte loop gives the same result"])
      then scan buf (i + 8) stop
      else scan buf (i + 1) stop
    else if (left < 4) [@mutate off "the same result"] then begin
      let next = ref (0 [@mutate off "tail sets next before scan reads it"]) in
      match tail buf i stop next with
      | "" ->
          scan buf !next stop
      | pending ->
          pending
    end
    else if c < 0xc2 then raise Invalid
    else if c < 0xe0 then
      if cont (Char.code (Bytes.unsafe_get buf (i + 1))) then
        scan buf (i + 2) stop
      else raise Invalid
    else if c < 0xf0 then begin
      let b1 = Char.code (Bytes.unsafe_get buf (i + 1)) in
      let ok1 =
        match c with
        | 0xe0 ->
            b1 >= 0xa0 && b1 <= 0xbf
        | 0xed ->
            b1 >= 0x80 && b1 <= 0x9f
        | _ ->
            cont b1
      in
      if ok1 && cont (Char.code (Bytes.unsafe_get buf (i + 2))) then
        scan buf (i + 3) stop
      else raise Invalid
    end
    else if c < 0xf5 then begin
      let b1 = Char.code (Bytes.unsafe_get buf (i + 1)) in
      let ok1 =
        match c with
        | 0xf0 ->
            b1 >= 0x90 && b1 <= 0xbf
        | 0xf4 ->
            b1 >= 0x80 && b1 <= 0x8f
        | _ ->
            cont b1
      in
      if
        ok1
        && cont (Char.code (Bytes.unsafe_get buf (i + 2)))
        && cont (Char.code (Bytes.unsafe_get buf (i + 3)))
      then scan buf (i + 4) stop
      else raise Invalid
    end
    else raise Invalid

let continue_bytes pending buf off len =
  if off < 0 || len < 0 || off > Bytes.length buf - len then
    invalid_arg "Utf8.continue_bytes: invalid range" ;
  let stop = off + len in
  if pending = "" then scan buf off stop
  else begin
    (* [pending] must be a result of this function: a correct prefix of 1 to 3
       bytes of a sequence. *)
    let c = Char.code pending.[0] and have = String.length pending in
    let need = length c - have in
    if need <= 0 then invalid_arg "Utf8.continue_bytes: pending is not a prefix" ;
    let take = Int.min need len in
    for j = 0 to take - 1 do
      if not (fits c (have + j) (Bytes.get_uint8 buf (off + j))) then
        raise Invalid
    done ;
    if take < need then pending ^ Bytes.sub_string buf off take
    else scan buf (off + need) stop
  end

let continue pending s =
  continue_bytes pending (Bytes.unsafe_of_string s) 0 (String.length s)
