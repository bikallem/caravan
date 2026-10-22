(* Internal. What an error of accept(2) is about. *)

type t =
  [ `Connection
    (** The connection that the server accepts failed, or a signal interrupted
        the call: try again. Linux gives the pending network errors of the new
        socket in this way, and [EPERM] for firewall rules (man 2 accept). *)
  | `Resources
    (** The process or the system has no free descriptors or memory: wait. *)
  | `Listener  (** The listening socket is not usable. *) ]

val classify : Weft.Net.error -> t
(** [classify e] is what [e], raised by [Tcp.Listener.accept], is about. *)
