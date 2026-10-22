(* Internal. POSIX times as HTTP dates, with the http-date library for the RFC
   9110 formats. *)

val of_utc : Weft.Time.utc -> Http_date.t
(** [of_utc u] is the date [u] as an IMF-fixdate ([`IMF]). *)

val of_posix : Weft.Clock.posix -> string
(** [of_posix p] is the POSIX time [p] encoded as an IMF-fixdate, for example
    ["Sun, 06 Nov 1994 08:49:37 GMT"], for the [Date] field. Fractions of a
    second are dropped: a time before 1970 goes to the second before it. The
    range is the range of [Clock.posix], 1677 to 2262. All domains share one
    cached string: in the second of the previous call, [of_posix] gives it. Two
    domains can compute the string of a new second at the same time, and then
    the result is the same. [of_posix] needs no fiber. *)
