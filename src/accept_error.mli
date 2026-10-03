(* Internal. What an error from accept(2) is about. *)

type t =
  [ `Connection
  (** The connection being accepted failed, or a signal interrupted the
      call: try again. Linux reports the pending network errors of the new
      socket this way, and [EPERM] for firewall rules (man 2 accept). *)
  | `Resources
  (** The process or system is out of descriptors or memory: wait. *)
  | `Listener  (** The listening socket itself is unusable. *) ]

val classify : Unix.error -> t
(** [classify e] is what [e], raised by accept(2), is about. *)
