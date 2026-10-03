(** WebSocket ({{:https://www.rfc-editor.org/rfc/rfc6455}RFC 6455}) for
    caravan servers. *)

(** {1:handshake Opening handshake} *)

val accept_key : string -> string
(** [accept_key key] is the [Sec-WebSocket-Accept] value that answers a
    request's [Sec-WebSocket-Key] [key]: the base64 SHA-1 digest of [key]
    followed by the protocol's GUID
    ({{:https://www.rfc-editor.org/rfc/rfc6455#section-4.2.2}RFC 6455
    §4.2.2}). *)

(**/**)

(** Internal modules, exposed for tests. Not part of the public API. *)
module Private : sig
  module Sha1 = Sha1
  module Base64 = Base64
end
