(* Internal. RFC 9110 tokens, shared by Method and Headers. *)

val is_tchar : char -> bool

val is_token : string -> bool
