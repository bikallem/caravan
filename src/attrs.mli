(** Typed attributes.

    Middleware attach attributes to a request or a response, to give typed data
    to the handlers after them or before them. For example, an authentication
    middleware gives the user to the handler, and a router gives the matched
    route to an access log. Attributes never go on the wire: the server and the
    client do not read them.

    A map of attributes is immutable. Thus many domains can read it at the same
    time. A value that is mutable must be synchronized, the same as other state
    that handlers share. *)

(** {1:keys Keys} *)

(** The type for keys of values of type ['a]. *)
type 'a key

val key : ?name:string -> unit -> 'a key
(** [key ~name ()] is a new key. It is different from all other keys, also from
    a key with the same name and type. [name] is only for {!pp} and {!key_name}.

    Make keys at the top level of a module, with a type annotation:
    {[
    let user : string Attrs.key = Attrs.key ~name:"user" ()
    ]} *)

val key_name : 'a key -> string option
(** [key_name k] is the name of [k], if it has one. *)

(** {1:maps Maps} *)

(** The type for maps from keys to values. *)
type t

val empty : t
(** [empty] has no attributes. *)

val is_empty : t -> bool
(** [is_empty m] is [true] iff [m] has no attributes. *)

val add : 'a key -> 'a -> t -> t
(** [add k v m] is [m] with the value [v] for [k]. It replaces the value that
    [m] has for [k], if one. *)

val find : 'a key -> t -> 'a option
(** [find k m] is the value of [k] in [m], if one. *)

val mem : 'a key -> t -> bool
(** [mem k m] is [true] iff [m] has a value for [k]. *)

val remove : 'a key -> t -> t
(** [remove k m] is [m] without a value for [k]. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the names of the keys of a map, sorted and separated by commas.
    A key without a name prints as [_]. It does not print the values. *)
