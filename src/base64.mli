(* Internal. Base64 (RFC 4648 §4 and §5), with or without padding, for
   caravan.websocket and caravan.session. *)

val encode : ?url:bool -> ?pad:bool -> string -> string
(** [encode s] is [s] in base64. [url] (default [false]) uses the alphabet of
    RFC 4648 §5, with [-] and [_]. [pad] (default [true]) pads with [=] to a
    multiple of 4 characters; with [~pad:false], the text has no [=]. *)

val decode : ?url:bool -> ?pad:bool -> string -> string option
(** [decode s] is the bytes that [s] encodes, with the same [url] and [pad] as
    {!encode}, or [None] if [s] is not the canonical encoding of any bytes: a
    bad length, a character outside the alphabet, an [=] where it cannot be, or
    bits that the end leaves over that are not zero. Hence [decode s = Some d]
    implies [encode d = s]. *)
