(* Internal. One request and its response on a connection (RFC 9112). *)

exception Writer_failed of exn * Printexc.raw_backtrace

exception Net of exn
(* A write error of the connection, in the writer. *)

val stops : [`Head | `Body] -> exn -> bool
(* [stops phase e] is [true] if the end of the writer with [e] stops a read that
   waits in [phase]. *)

val run :
     Config.t
  -> Writers.t
  -> Conn.t
  -> on_release:(Conn.t -> reusable:bool -> unit)
  -> Caravan.Request.t
  -> Caravan.Response.t
(* [run config writers conn ~on_release req] sends [req] on [conn] and gives the
   final response when its head comes. A job of [writers] writes the request,
   thus [run] can return before the request ends. [on_release conn ~reusable]
   runs one time: at the end of the body, at once for an empty body, at
   Body.close, or at an error. If the connection can stay and the response ends
   before the user code of the request body, it runs later instead, in the fiber
   of the writer, at the end of the writer. The deadlines are on the time line
   of [conn.flow] (Flow.time). *)
