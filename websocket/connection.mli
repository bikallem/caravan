(* Internal. Messages on a WebSocket connection, server side. The
   documentation is in Caravan_websocket. *)

open Caravan

type t
type message = Text of string | Binary of string

exception Closed of { code : int; reason : string }

val make :
  ?protocol:string -> ?max_message:int -> ?close_timeout:Affect_unix.Mtime.Span.t -> Flow.t -> t

val protocol : t -> string option
val recv : t -> message
val send : t -> message -> unit
val ping : t -> string -> unit
val close : ?code:int -> ?reason:string -> t -> unit
