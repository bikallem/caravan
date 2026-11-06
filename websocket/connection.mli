(* Internal. Messages on a WebSocket connection, server side. The documentation
   is in Caravan_websocket. *)

open Caravan

type t

type message = Text of string | Binary of string

exception Closed of {code: int; reason: string}

val make :
     ?protocol:string
  -> ?max_message:int
  -> ?close_timeout:Weft.Span.t
  -> ?write_timeout:Weft.Span.t
  -> ?read_timeout:Weft.Span.t
  -> ?idle_timeout:Weft.Span.t
  -> Flow.t
  -> t

val protocol : t -> string option

(* [is_closing c] is [true] if a close started on [c] (a Close frame went or
   waits to go), or [c] is closed. *)
val is_closing : t -> bool

(* [is_own_closed c e] is [true] if [e] is a [Closed] exception that [c] raised.
   A [Closed] of a different connection gives [false], also with the same code
   and reason. *)
val is_own_closed : t -> exn -> bool

val recv : t -> message

val send : t -> message -> unit

val ping : t -> string -> unit

val close : ?code:int -> ?reason:string -> t -> unit
