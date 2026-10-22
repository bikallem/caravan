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
    (* Not Http_date.encode: it formats through the global Format.str_formatter,
       which domains must not share. *)
    let date =
      Format.asprintf "%a" Http_date.pp (of_utc (Weft.Time.to_utc p))
    in
    Atomic.set cache (second, date) ;
    date
  end
