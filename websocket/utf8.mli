(** UTF-8 in parts, for the text of a fragmented message
    ({{:https://www.rfc-editor.org/rfc/rfc6455#section-8.1}RFC 6455 §8.1}). *)

(** A byte cannot be part of correct UTF-8. *)
exception Invalid

val continue : string -> string -> string
(** [continue pending s] examines the UTF-8 of [s] after the bytes [pending] of
    an incomplete sequence, at most 3 bytes. Its result is the bytes of the
    incomplete sequence at the end of [s], [""] if there is none. For the first
    part, [pending] is [""]. All parts are correct UTF-8 if no call raises and
    the last result is [""].

    @raise Invalid
      at the first byte that no correct UTF-8 can have in its position. *)
