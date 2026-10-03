(* Internal. Current time and Unix time as HTTP dates, using the http-date
   library for the RFC 9110 formats. *)

val of_unix_time : float -> Http_date.t
(** [of_unix_time t] is the POSIX time [t], in seconds, as an IMF-fixdate
    ([`IMF]). Fractions of a second are dropped. *)

val now : unit -> string
(** [now ()] is the current time encoded as an IMF-fixdate, for example
    ["Sun, 06 Nov 1994 08:49:37 GMT"], for the [Date] header. The string is
    computed at most once per second and shared by all domains. *)

val now_at : float -> string
(** [now_at t] is [now ()] with the clock reading [t]: it returns the cached
    string when [t] falls in the same second as the previous call. *)
