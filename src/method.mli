(** HTTP request methods
    ({{:https://www.rfc-editor.org/rfc/rfc9110#section-9} RFC 9110 §9}). *)

(** The type for request methods. Method names are case-sensitive. *)
type t =
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH  (** RFC 5789 *)
  | Other of string
      (** An extension method, a token. {!of_string} never returns [Other] for a
          standard name such as ["GET"]. *)

val of_string : string -> t
(** [of_string s] is the method named [s]. Standard names map to their
    constructor, anything else to [Other s]. Matching is case-sensitive:
    [of_string "get"] is [Other "get"].

    @raise Invalid_argument
      if [s] is not a
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-5.6.2}token}. *)

val to_string : t -> string
(** [to_string m] is the name of [m] as it appears on the wire. *)

val equal : t -> t -> bool
(** [equal m m'] is [true] iff [m] and [m'] have the same name, so
    [equal (Other "GET") GET] is [true]. *)

val compare : t -> t -> int
(** [compare] orders methods by name, consistently with {!equal}. *)

val is_safe : t -> bool
(** [is_safe m] is [true] iff [m] is
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-9.2.1}safe}: [GET],
    [HEAD], [OPTIONS] or [TRACE]. *)

val is_idempotent : t -> bool
(** [is_idempotent m] is [true] iff [m] is
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-9.2.2}idempotent}: a safe
    method, [PUT] or [DELETE]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a method with {!to_string}. *)
