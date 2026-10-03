(** HTTP header fields ({{:https://www.rfc-editor.org/rfc/rfc9110#section-5}
    RFC 9110 §5}).

    Fields keep the order, name case and duplicates they were given, so that a
    proxy can forward them faithfully. Lookups compare names
    case-insensitively. *)

type t
(** The type for ordered collections of header fields. *)

val empty : t
(** [empty] has no fields. *)

val is_empty : t -> bool
(** [is_empty h] is [true] iff [h] has no fields. *)

val length : t -> int
(** [length h] is the number of fields in [h], duplicates included. *)

val of_list : (string * string) list -> t
(** [of_list fields] are the [(name, value)] [fields] in order.

    @raise Invalid_argument if a name or a value is not valid, see
    {!is_valid_name} and {!is_valid_value}. *)

val to_list : t -> (string * string) list
(** [to_list h] are the fields of [h] in order, with their names as given. *)

(** {1:modifying Modifying} *)

val add : string -> string -> t -> t
(** [add name value h] is [h] with the field [(name, value)] appended.

    @raise Invalid_argument if [name] or [value] is not valid. *)

val set : string -> string -> t -> t
(** [set name value h] replaces the first field of [h] named [name] by
    [(name, value)], at the same position, and removes the other fields named
    [name]. If [h] has no such field, [(name, value)] is appended.

    @raise Invalid_argument if [name] or [value] is not valid. *)

val remove : string -> t -> t
(** [remove name h] is [h] without the fields named [name]. *)

(** {1:lookup Lookup} *)

val find : string -> t -> string option
(** [find name h] is the value of the first field of [h] named [name], if
    any. *)

val find_all : string -> t -> string list
(** [find_all name h] are the values of the fields of [h] named [name], in
    order. *)

val mem : string -> t -> bool
(** [mem name h] is [true] iff [h] has a field named [name]. *)

val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f h acc] folds [f] over the fields of [h] in order. *)

val iter : (string -> string -> unit) -> t -> unit
(** [iter f h] applies [f] to the fields of [h] in order. *)

(** {1:lists Comma-separated lists} *)

val tokens : string -> t -> string list
(** [tokens name h] are the elements of the
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1}comma-separated
    lists} in the fields of [h] named [name], in order, with surrounding
    whitespace trimmed and empty elements dropped. For example, with the fields
    [Connection: keep-alive, Upgrade] and [Connection: close], [tokens
    "connection" h] is [["keep-alive"; "Upgrade"; "close"]]. *)

val has_token : string -> string -> t -> bool
(** [has_token name token h] is [true] iff [token] is in [tokens name h],
    compared case-insensitively. For example [has_token "connection" "close"
    h]. *)

val remove_hop_by_hop : t -> t
(** [remove_hop_by_hop h] is [h] without its
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-7.6.1}hop-by-hop
    fields}: [Connection], every field that [Connection] names, and
    [Keep-Alive], [Proxy-Connection], [Proxy-Authenticate],
    [Proxy-Authorization], [TE], [Trailer], [Transfer-Encoding] and [Upgrade].
    A proxy applies it before forwarding a message. *)

(** {1:validity Validity} *)

val is_valid_name : string -> bool
(** [is_valid_name s] is [true] iff [s] is a
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-5.6.2}token}. *)

val is_valid_value : string -> bool
(** [is_valid_value s] is [true] iff [s] is a
    {{:https://www.rfc-editor.org/rfc/rfc9110#section-5.5}field value}: it has
    no control characters other than horizontal tab, so no CR, LF or NUL, and
    it does not start or end with a space or a tab. It may be empty. *)

(** {1:fmt Comparing and formatting} *)

val equal : t -> t -> bool
(** [equal h h'] is [true] iff [h] and [h'] have the same fields in the same
    order, with names compared case-insensitively and values exactly. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats fields one per line as [name: value]. *)
