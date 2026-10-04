(** SHA-1 ({{:https://www.rfc-editor.org/rfc/rfc3174}RFC 3174}).

    Only for the WebSocket opening handshake, which fixes the hash. SHA-1 is
    not collision-resistant; do not use it for anything that relies on that. *)

val digest : string -> string
(** [digest s] is the 20-byte SHA-1 digest of [s]. *)
