(* Internal. The decode of the %XX sequences of a path segment (RFC 3986 §2.1),
   for caravan.router and caravan.static. *)

val decode : string -> string option
(** [decode s] is [s] with each [%XX] replaced by its byte, or [None] for a bad
    sequence. A [+] stays a [+]: this is not the decode of a form. *)
