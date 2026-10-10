(* The HTTP/1.1 engine on in-memory connections, for exact checks of the bytes
   that a server writes. *)

open Caravan

(** What the server wrote on one connection. *)
type output =
  { written: string  (** The bytes, in order. *)
  ; fin: bool  (** The server half-closed the connection after [written]. *)
  ; reset: bool  (** The server reset the connection. *) }

val serve : config:Config.t -> Handler.t -> string list -> output list
(** [serve ~config handler inputs] serves each input on its own connection, one
    after the other, with [Private.H1_server.serve]. The client sends all of its
    input at once, and then closes its side. The server does not watch for the
    close: [serve] sets [cancel_on_disconnect] to [false]. Call it outside
    [Unix.main].

    A write after a half-close raises [Failure] in the server. A write after a
    reset raises [Flow.Closed] in the server, and the bytes are dropped. *)
