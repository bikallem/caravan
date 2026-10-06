(* Internal. One request and its response on a connection (RFC 9112). *)

exception Writer_failed of exn * Printexc.raw_backtrace

val run :
  Config.t ->
  Writers.t ->
  Conn.t ->
  on_release:(Conn.t -> reusable:bool -> unit) ->
  Caravan.Request.t ->
  Caravan.Response.t
(* [run config writers conn ~on_release req] sends [req] on [conn] and
   gives the final response when its head comes. A job of [writers] writes
   the request, thus [run] can return before the request ends.
   [on_release conn ~reusable] runs one time: at the end of the body, at
   once for an empty body, at Body.close, or at an error. *)
