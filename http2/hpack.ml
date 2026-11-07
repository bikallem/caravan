(* HPACK (RFC 7541): the decoder, with its dynamic table, and an encoder with no
   state. *)

(* The static table (RFC 7541 Appendix A). The entry of the index [i] is at [i -
   1], and the comment of each row gives the index. A script copied the rows
   from the text of the RFC. *)
let static =
  [| (":authority", "") (* 1 *)
   ; (":method", "GET") (* 2 *)
   ; (":method", "POST") (* 3 *)
   ; (":path", "/") (* 4 *)
   ; (":path", "/index.html") (* 5 *)
   ; (":scheme", "http") (* 6 *)
   ; (":scheme", "https") (* 7 *)
   ; (":status", "200") (* 8 *)
   ; (":status", "204") (* 9 *)
   ; (":status", "206") (* 10 *)
   ; (":status", "304") (* 11 *)
   ; (":status", "400") (* 12 *)
   ; (":status", "404") (* 13 *)
   ; (":status", "500") (* 14 *)
   ; ("accept-charset", "") (* 15 *)
   ; ("accept-encoding", "gzip, deflate") (* 16 *)
   ; ("accept-language", "") (* 17 *)
   ; ("accept-ranges", "") (* 18 *)
   ; ("accept", "") (* 19 *)
   ; ("access-control-allow-origin", "") (* 20 *)
   ; ("age", "") (* 21 *)
   ; ("allow", "") (* 22 *)
   ; ("authorization", "") (* 23 *)
   ; ("cache-control", "") (* 24 *)
   ; ("content-disposition", "") (* 25 *)
   ; ("content-encoding", "") (* 26 *)
   ; ("content-language", "") (* 27 *)
   ; ("content-length", "") (* 28 *)
   ; ("content-location", "") (* 29 *)
   ; ("content-range", "") (* 30 *)
   ; ("content-type", "") (* 31 *)
   ; ("cookie", "") (* 32 *)
   ; ("date", "") (* 33 *)
   ; ("etag", "") (* 34 *)
   ; ("expect", "") (* 35 *)
   ; ("expires", "") (* 36 *)
   ; ("from", "") (* 37 *)
   ; ("host", "") (* 38 *)
   ; ("if-match", "") (* 39 *)
   ; ("if-modified-since", "") (* 40 *)
   ; ("if-none-match", "") (* 41 *)
   ; ("if-range", "") (* 42 *)
   ; ("if-unmodified-since", "") (* 43 *)
   ; ("last-modified", "") (* 44 *)
   ; ("link", "") (* 45 *)
   ; ("location", "") (* 46 *)
   ; ("max-forwards", "") (* 47 *)
   ; ("proxy-authenticate", "") (* 48 *)
   ; ("proxy-authorization", "") (* 49 *)
   ; ("range", "") (* 50 *)
   ; ("referer", "") (* 51 *)
   ; ("refresh", "") (* 52 *)
   ; ("retry-after", "") (* 53 *)
   ; ("server", "") (* 54 *)
   ; ("set-cookie", "") (* 55 *)
   ; ("strict-transport-security", "") (* 56 *)
   ; ("transfer-encoding", "") (* 57 *)
   ; ("user-agent", "") (* 58 *)
   ; ("vary", "") (* 59 *)
   ; ("via", "") (* 60 *)
   ; ("www-authenticate", "") (* 61 *) |]

let static_count = Array.length static

let error fmt = Printf.ksprintf (fun m -> raise (Error.Hpack m)) fmt

(* RFC 7541 section 4.1: the size of an entry. *)
let entry_size (name, value) = String.length name + String.length value + 32

(* RFC 7541 section 5.1: the integer at [!pos] of [s], with a prefix of [bits]
   bits. The caller makes sure that [!pos] is in [s]. A value over 2^28 is an
   error, and also an encoding with more than 5 continuation bytes: the sixth
   continuation byte has the shift 35. *)
let decode_int s pos bits =
  let max_prefix = (1 lsl bits) - 1 in
  let v = Char.code s.[!pos] land max_prefix in
  incr pos ;
  if v < max_prefix then v
  else
    let rec more v shift =
      if !pos >= String.length s then error "an integer ends after the block" ;
      if shift = 35 then error "an integer has too many bytes" ;
      let b = Char.code s.[!pos] in
      incr pos ;
      let v = v + ((b land 0x7f) lsl shift) in
      if v > 1 lsl 28 then error "an integer is over 2^28" ;
      if b land 0x80 = 0 then v else more v (shift + 7)
    in
    more v 0

(* RFC 7541 section 5.2: the string literal at [!pos] of [s]. *)
let decode_string s pos =
  if !pos >= String.length s then error "the block ends before a string" ;
  let huffman = Char.code s.[!pos] land 0x80 <> 0 in
  let len = decode_int s pos 7 in
  if len > String.length s - !pos then
    error "a string of %d bytes goes past the end of the block" len ;
  let v =
    if huffman then Huffman.decode s !pos len else String.sub s !pos len
  in
  pos := !pos + len ;
  v

module Decoder = struct
  (* The dynamic table is a ring of entries. [first] is the slot of the newest
     entry, and the [count] entries follow it in the ring, from the newest to
     the oldest. [max_size] is the maximum table size of RFC 7541 section 4.2:
     the value of the last table size update, at most [max_table_size].

     The ring grows when it is full, thus its memory follows the use of the
     table, not the setting. The engine does not limit [max_table_size], and a
     ring of [max_table_size / 32] slots from the start can be large. *)
  type t =
    { max_table_size: int
    ; mutable max_size: int
    ; mutable size: int
    ; mutable ring: (string * string) array
    ; mutable first: int
    ; mutable count: int }

  let create ~max_table_size =
    if max_table_size < 0 then
      invalid_arg "Hpack.Decoder.create: a negative max_table_size" ;
    { max_table_size
    ; max_size= max_table_size
    ; size= 0
    ; ring= [||]
    ; first= (0 [@mutate off "the first add sets first"])
    ; count= 0 }

  let table_size t = t.size

  (* [slot t i] is the slot of the entry [i] of the dynamic table, from 0 for
     the newest. *)
  let slot t i = (t.first + i) mod Array.length t.ring

  let get t i = t.ring.(slot t i)

  let entries t = List.init t.count (get t)

  (* RFC 7541 section 4.4: remove the oldest entries until the size is at most
     [limit]. A negative [limit] removes all the entries. *)
  let evict t limit =
    while t.count > 0 && t.size > limit do
      let i = t.count - 1 in
      t.size <- t.size - entry_size (get t i) ;
      (t.ring.(slot t i) <- ("", "") )
      [@mutate off "the clear only lets the GC free the strings"] ;
      t.count <- i
    done

  (* RFC 7541 section 4.4: an entry larger than the table empties the table, and
     the table does not keep the entry. *)
  let add t entry =
    let size = entry_size entry in
    evict t (t.max_size - size) ;
    if size <= t.max_size then begin
      let len = Array.length t.ring in
      if t.count = len then begin
        let ring =
          Array.make
            (max 16 (2 * len)
             [@mutate
               off "the ring length: another length gives the same table"] )
            ("", "")
        in
        for i = 0 to t.count - 1 do
          ring.(i) <- get t i
        done ;
        t.ring <- ring ;
        t.first <- 0
      end ;
      let len = Array.length t.ring in
      t.first <- (t.first + len - 1) mod len ;
      t.ring.(t.first) <- entry ;
      t.count <- t.count + 1 ;
      t.size <- t.size + size
    end

  (* RFC 7541 section 2.3.3: the index space of the two tables. *)
  let find t index =
    if index = 0 then error "the index 0" ;
    if index <= static_count then static.(index - 1)
    else if index - static_count <= t.count then get t (index - static_count - 1)
    else error "the index %d is not in the tables" index

  (* RFC 7541 section 6.2: the name and the value of a literal field. *)
  let literal t s pos bits =
    let index = decode_int s pos bits in
    let name = if index = 0 then decode_string s pos else fst (find t index) in
    (name, decode_string s pos)

  let decode t ~max_list_size ~max_fields block =
    let pos = ref 0
    and fields = ref []
    and list_size = ref 0
    and field_count = ref 0 in
    (* The two counters only increase: after the list is too large, it stays too
       large. *)
    let too_large () =
      !list_size > max_list_size || !field_count > max_fields
    in
    (* The decoder keeps the fields until the list is too large. After that, it
       continues to decode, so that the dynamic table stays correct. *)
    let field f =
      list_size := !list_size + entry_size f ;
      incr field_count ;
      if not (too_large ()) then fields := f :: !fields
    in
    while !pos < String.length block do
      let b = Char.code block.[!pos] in
      if b land 0x80 <> 0 then field (find t (decode_int block pos 7))
      else if b land 0x40 <> 0 then begin
        let f = literal t block pos 6 in
        add t f ; field f
      end
      else if b land 0x20 <> 0 then begin
        (* RFC 7541 section 4.2: an update comes before the first field. *)
        if !field_count > 0 then error "a table size update after a field" ;
        let size = decode_int block pos 5 in
        if size > t.max_table_size then
          error "a table size update to %d, over %d" size t.max_table_size ;
        t.max_size <- size ;
        evict t size
      end
      else field (literal t block pos 4)
    done ;
    if too_large () then `Too_large else `Fields (List.rev !fields)
end

(* The encoder. *)

(* [encode_int buf flags bits v] adds [v] with a prefix of [bits] bits (RFC 7541
   section 5.1). [flags] holds the other bits of the first byte. *)
let encode_int buf flags bits v =
  let max_prefix = (1 lsl bits) - 1 in
  if v < max_prefix then Buffer.add_char buf (Char.unsafe_chr (flags lor v))
  else begin
    Buffer.add_char buf (Char.unsafe_chr (flags lor max_prefix)) ;
    let v = ref (v - max_prefix) in
    while !v >= 0x80 do
      Buffer.add_char buf (Char.unsafe_chr (!v land 0x7f lor 0x80)) ;
      v := !v lsr 7
    done ;
    Buffer.add_char buf (Char.unsafe_chr !v)
  end

(* RFC 7541 section 5.2: Huffman only if it is shorter. *)
let encode_string buf s =
  let n = Huffman.encoded_length s in
  if n < String.length s then begin
    encode_int buf 0x80 7 n ; Huffman.encode_n buf s n
  end
  else begin
    encode_int buf 0 7 (String.length s) ;
    Buffer.add_string buf s
  end

(* For each name of the static table: its first index, and the index of each of
   its values. Only 5 names have more than one value. *)
let static_names =
  let names = Hashtbl.create 64 in
  for i = static_count downto 1 do
    let name, value = static.(i - 1) in
    let values =
      match Hashtbl.find_opt names name with Some (_, l) -> l | None -> []
    in
    Hashtbl.replace names name (i, (value, i) :: values)
  done ;
  names

(* A literal field, with the representation of [flags] (RFC 7541 section 6.2).
   [index] is the index of the name, or 0 for a new name. *)
let encode_literal buf flags index name value =
  if index > 0 then encode_int buf flags 4 index
  else begin
    Buffer.add_char buf (Char.unsafe_chr flags) ;
    encode_string buf name
  end ;
  encode_string buf value

let encode buf fields =
  (* A dynamic table size update to 0: the encoder never uses the dynamic
     table. *)
  Buffer.add_char buf '\x20' ;
  List.iter
    (fun (name, value) ->
      match Hashtbl.find_opt static_names name with
      | None ->
          (* Literal without indexing, with a new name. *)
          encode_literal buf 0x00 0 name value
      | Some (first, values) -> (
        match name with
        | "set-cookie" | "authorization" | "cookie" ->
            (* RFC 7541 section 7.1.3: literal never indexed. The 3 names have
               an index over 14, thus the 4 low bits of the first byte are 1
               bits. *)
            encode_literal buf (0x10 [@mutate off "the 4 low bits are 1 bits"])
              first name value
        | _ -> (
          match List.find_opt (fun (v, _) -> String.equal v value) values with
          | Some (_, i) ->
              (* Indexed. The index is at most 61, thus it fits in the prefix of
                 7 bits. *)
              Buffer.add_char buf (Char.unsafe_chr (0x80 lor i))
          | None ->
              (* Literal without indexing, with the index of the name. *)
              encode_literal buf 0x00 first name value ) ) )
    fields
