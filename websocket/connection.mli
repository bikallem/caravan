(* Internal. Messages on a WebSocket connection, server side. The
   documentation is in Caravan_websocket. *)

open Caravan

type t
type message = Text of string | Binary of string

exception Closed of { code : int; reason : string }

val make : ?max_message:int -> Flow.t -> t
val recv : t -> message
val send : t -> message -> unit
val ping : t -> string -> unit
