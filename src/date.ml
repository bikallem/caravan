let weekdays = [|`Sun; `Mon; `Tue; `Wed; `Thu; `Fri; `Sat|]

let of_utc (u : Weft.Time.utc) =
  `IMF
    ( weekdays.(u.weekday)
    , (u.year, u.month, u.day)
    , (u.hour, u.minute, u.second) )

(* The second of the last call and its string: one string for each second, for
   all domains. A race only computes the same string again. [min_int] is no
   second of [Clock.posix]. *)
let cache = Atomic.make (min_int, "")

let epoch = Weft.Time.posix_of_unix_s 0.

let day_names = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|]

let month_names =
  [| "Jan"
   ; "Feb"
   ; "Mar"
   ; "Apr"
   ; "May"
   ; "Jun"
   ; "Jul"
   ; "Aug"
   ; "Sep"
   ; "Oct"
   ; "Nov"
   ; "Dec" |]

(* A direct write of the 29 bytes, with no Format: the range of [Clock.posix]
   has only years of 4 digits. *)
let to_imf p =
  let (u : Weft.Time.utc) = Weft.Time.to_utc p in
  let b = Bytes.create 29 in
  let text i s = Bytes.blit_string s 0 b i 3 in
  let two i n =
    Bytes.set b i (Char.chr (Char.code '0' + (n / 10))) ;
    Bytes.set b (i + 1) (Char.chr (Char.code '0' + (n mod 10)))
  in
  text 0 day_names.(u.weekday) ;
  Bytes.blit_string ", " 0 b 3 2 ;
  two 5 u.day ;
  Bytes.set b 7 ' ' ;
  text 8 month_names.(u.month - 1) ;
  Bytes.set b 11 ' ' ;
  two 12 (u.year / 100) ;
  two 14 (u.year mod 100) ;
  Bytes.set b 16 ' ' ;
  two 17 u.hour ;
  Bytes.set b 19 ':' ;
  two 20 u.minute ;
  Bytes.set b 22 ':' ;
  two 23 u.second ;
  Bytes.blit_string " GMT" 0 b 25 4 ;
  Bytes.unsafe_to_string b

let of_posix p =
  (* The floor of the second, from the exact nanoseconds: a float can round a
     fraction near 1 s up to the next second. The difference to 0 cannot
     overflow. *)
  let ns = Weft.Span.to_ns (Weft.Time.diff Weft.Clock.Posix p epoch) in
  let q = Int64.div ns 1_000_000_000L in
  let second =
    Int64.to_int (if Int64.rem ns 1_000_000_000L < 0L then Int64.pred q else q)
  in
  let cached_second, cached = Atomic.get cache in
  if second = cached_second then cached
  else begin
    let date = to_imf p in
    Atomic.set cache (second, date) ;
    date
  end
