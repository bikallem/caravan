(** The Huffman code of HPACK
    ({{:https://www.rfc-editor.org/rfc/rfc7541#section-5.2}RFC 7541 §5.2} and
    Appendix B). *)

val encoded_length : string -> int
(** [encoded_length s] is the number of bytes of the Huffman code of [s], with
    the padding. *)

val encode : Buffer.t -> string -> unit
(** [encode buf s] adds the Huffman code of [s] to [buf], with the padding. The
    padding is the most significant bits of EOS, thus all 1 bits. [encode] adds
    {!encoded_length}[ s] bytes. *)

val encode_n : Buffer.t -> string -> int -> unit
(** [encode_n buf s n] is [encode buf s], for the caller that has [n], the value
    of {!encoded_length}[ s].

    @raise Invalid_argument
      if [n] is not {!encoded_length}[ s]. Then [buf] does not change. *)

val decode : string -> int -> int -> string
(** [decode s off len] is the string that the [len] bytes of [s] at [off]
    encode.

    @raise Error.Hpack
      if the bytes contain the code of EOS, or if the padding at the end has
      more than 7 bits, or if the padding has a 0 bit.
    @raise Invalid_argument
      if [off] and [len] do not give a range of bytes in [s]. *)
