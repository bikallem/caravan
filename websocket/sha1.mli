(** SHA-1 ({{:https://www.rfc-editor.org/rfc/rfc3174}RFC 3174}).

    Only for the WebSocket opening handshake, which specifies this hash.
    SHA-1 does not prevent collisions. Do not use it for a task that
    relies on collision resistance. *)

val digest : string -> string
(** [digest s] is the 20-byte SHA-1 digest of [s]. *)
