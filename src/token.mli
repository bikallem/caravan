(* Internal. RFC 9110 tokens, shared by Method and Headers. *)

val is_tchar : char -> bool

val is_token : string -> bool

val is_ws : char -> bool
(** [is_ws c] is [true] iff [c] is a space or a tab (the OWS of RFC 9110). *)
