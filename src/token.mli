(* Internal. RFC 9110 tokens, shared by Method and Headers, and the bytes of a
   request target. *)

val is_tchar : char -> bool

val is_token : string -> bool

val is_target_char : char -> bool
(** [is_target_char c] is [true] iff [c] can be in a request target: a visible
    ASCII byte, 0x21 to 0x7e (RFC 9112 §3.2, RFC 3986). HTTP/1.1 and HTTP/2
    share this rule. *)

val is_ws : char -> bool
(** [is_ws c] is [true] iff [c] is a space or a tab (the OWS of RFC 9110). *)
