(** HPACK, the header compression of HTTP/2
    ({{:https://www.rfc-editor.org/rfc/rfc7541}RFC 7541}).

    The decoder keeps the dynamic table of a connection. The encoder has no
    state: it never uses the dynamic table. *)

(** The decoder of the header blocks of one connection. *)
module Decoder : sig
  (** The type for decoders. A decoder holds the dynamic table. *)
  type t

  val create : max_table_size:int -> t
  (** [create ~max_table_size] is a decoder with an empty dynamic table.
      [max_table_size] is the limit of the table size updates in bytes, the
      value of SETTINGS_HEADER_TABLE_SIZE that the peer received. A table size
      update in a block sets the maximum table size (RFC 7541 §4.2) to a value
      from 0 to [max_table_size]. Before the first update, the maximum table
      size is [max_table_size].

      @raise Invalid_argument if [max_table_size] is negative. *)

  val table_size : t -> int
  (** [table_size t] is the size of the dynamic table of [t] in bytes (RFC 7541
      §4.1): the sum of name + value + 32 for each entry. For the tests. *)

  val entries : t -> (string * string) list
  (** [entries t] are the entries of the dynamic table of [t], from the newest
      to the oldest. For the tests. *)

  val decode :
       t
    -> max_list_size:int
    -> max_fields:int
    -> string
    -> [`Fields of (string * string) list | `Too_large]
  (** [decode t ~max_list_size ~max_fields block] decodes the complete header
      block [block], and updates the dynamic table of [t]. The result is
      [`Fields l], with the [(name, value)] fields in order. The decoder does
      not check the names and the values.

      The size of a list is the sum of name + value + 32 for each field. If the
      size is over [max_list_size], or if the list has more than [max_fields]
      fields, the result is [`Too_large]. The decoder then stops to keep the
      fields, but it decodes all of the block, so that the dynamic table stays
      correct.

      @raise Error.Hpack
        if the block is not correct: an integer over 2^28, an integer with more
        than 5 continuation bytes, a string or an integer that goes past the end
        of the block, the index 0, an index that is not in the static table or
        in the dynamic table, a table size update over [max_table_size] or after
        a field, or a Huffman string that is not correct (see
        {!Huffman.decode}). The dynamic table can then be incorrect, and the
        connection must close with [COMPRESSION_ERROR]. *)
end

val encode : Buffer.t -> (string * string) list -> unit
(** [encode buf fields] adds the header block of [fields] to [buf].

    The block starts with a dynamic table size update to 0 (the byte [0x20]).
    Then, for each field:
    - [set-cookie], [authorization] and [cookie] are literals never indexed,
      with the index of the name in the static table.
    - A field of the static table is indexed.
    - A field with a name of the static table is a literal without indexing,
      with the index of the name.
    - The other fields are literals without indexing, with the name as a string.

    Each string is a Huffman string if it is shorter, else a plain string.

    The names must be in lowercase, as RFC 9113 §8.2 requires. The comparison
    with the static table and with the 3 names of the never-indexed rule is
    case-sensitive. Thus [encode] gives a name with an uppercase letter as a new
    name, and the value of [Set-Cookie] is then not a literal never indexed.
    [encode] does not check the names and the values. *)
