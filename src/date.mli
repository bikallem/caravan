(* Internal. POSIX times as HTTP dates, using the http-date library for the RFC
   9110 formats. *)

val of_unix_time : float -> Http_date.t
(** [of_unix_time t] is the POSIX time [t], in seconds, as an IMF-fixdate
    ([`IMF]). Fractions of a second are dropped. *)

val of_posix : Affect_unix.Ptime.t -> string
(** [of_posix p] is the POSIX time [p] encoded as an IMF-fixdate, for example
    ["Sun, 06 Nov 1994 08:49:37 GMT"], for the [Date] field. Fractions of a
    second are dropped: a time before 1970 goes to the second before it. The
    string is computed at most once per second and shared by all domains: in the
    second of the previous call, [of_posix] gives the cached string. *)
