(** HTTP protocol versions. *)

type t =
  | Http_1_0
  | Http_1_1
  | Http_2
  | Http_3  (** The type for HTTP versions. *)

val to_string : t -> string
(** [to_string v] is ["HTTP/1.0"], ["HTTP/1.1"], ["HTTP/2"] or ["HTTP/3"]. *)

val of_string : string -> t option
(** [of_string s] is the version written [s] as by {!to_string}, or [None] if
    [s] is not one of these strings. *)

val equal : t -> t -> bool
(** [equal v v'] is [true] iff [v] and [v'] are the same version. *)

val compare : t -> t -> int
(** [compare] orders versions from oldest to newest. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a version with {!to_string}. *)
