(** A server in the OS world (Linux only). *)

val run :
     ?config:Caravan.Config.t
  -> ?engine:Caravan.Engine.t
  -> ?domains:int
  -> ?on_start:(Caravan.Server.t -> unit)
  -> Caravan.Handler.t
  -> unit
(** [run ~config ~engine ~domains ~on_start handler] serves [handler] until the
    process gets SIGINT (Ctrl-C) or SIGTERM:
    + It calls [Weft_linux.run ~domains ~signals:[SIGINT; SIGTERM]]. [domains]
      is [Domain.recommended_domain_count ()] by default, at most 62 (the
      default of [Weft_linux.run] is at most 4).
    + It starts the server and calls [on_start] with it, for example to print
      its address.
    + It calls [Caravan.Server.stop_on_signals] in a daemon fiber: the first
      signal stops the server with a grace, the second signal aborts it, and a
      third signal gets its default action.

    It returns when the server stopped. Then the world restores the signal mask,
    and the signals get the action that they had before [run]. Call it outside
    fiber code.

    @raise Weft.Net.Error
      if the address cannot be bound, if the accept of connections failed, or
      for an error of the OS for SIGINT or SIGTERM (see
      {!Caravan.Server.stop_on_signals}). An error for a signal cancels the
      scope of the server: the requests in progress get no grace. *)
