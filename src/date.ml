let weekdays = [|`Sun; `Mon; `Tue; `Wed; `Thu; `Fri; `Sat|]

let of_unix_time t =
  let tm = Unix.gmtime t in
  `IMF
    ( weekdays.(tm.tm_wday)
    , (tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday)
    , (tm.tm_hour, tm.tm_min, tm.tm_sec) )

(* (second, encoded date) of the last call: one string per second for all
   domains. A race only recomputes the same string. *)
let cache = Atomic.make (min_int, "")

let of_posix p =
  (* The exact floor of the second. A float can round a fraction near 1 s up to
     the next second. *)
  let span = Affect_unix.Ptime.to_span p in
  let second = Option.get (Affect_unix.Ptime.Span.to_int_s span) in
  let t = Float.of_int second in
  let cached_second, cached = Atomic.get cache in
  if second = cached_second then cached
  else begin
    (* Not Http_date.encode: it formats through the global Format.str_formatter,
       which domains must not share. *)
    let date = Format.asprintf "%a" Http_date.pp (of_unix_time t) in
    Atomic.set cache (second, date) ;
    date
  end
