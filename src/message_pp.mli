(** The format of {!Request.pp} and {!Response.pp}. *)

val pp :
     Format.formatter
  -> (Format.formatter -> unit -> unit)
  -> Headers.t
  -> Attrs.t
  -> unit
(** [pp ppf start h m] prints the first line [start] of a message, then its
    header fields [h], then a line [attrs: ] with {!Attrs.pp} of [m]. It prints
    no line for empty [h] or empty [m], and no line break at the end. *)
