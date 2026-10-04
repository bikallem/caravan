(** Base64 ({{:https://www.rfc-editor.org/rfc/rfc4648#section-4}RFC 4648
    §4}), with padding. *)

val encode : string -> string
(** [encode s] is [s] in base64, padded with [=] to a multiple of 4
    characters. *)

val decode : string -> string option
(** [decode s] is the bytes that [s] encodes, or [None] if [s] is not the
    canonical encoding of any bytes: its length is not a multiple of 4, a
    character is outside the alphabet, [=] appears before the end or more than
    twice, or the bits that the padding leaves over are not zero. Hence
    [decode s = Some d] implies [encode d = s]. *)
