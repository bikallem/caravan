(* Internal. A strict decoder of a subset of CBOR (RFC 8949), for WebAuthn:
   integers, byte strings, text strings, arrays, maps, false, true and null. *)

type t =
  | Int of int
  | Bytes of string
  | Text of string
  | Array of t list
  | Map of (t * t) list  (** In the order of the input. *)
  | Bool of bool
  | Null

val decode : string -> int -> (t * int, string) result
(* [decode s pos] is the item at [pos] of [s] and the position after it. The
   errors: a negative [pos]; the end of the input in an item; an indefinite
   length; a tag; a float, undefined or another simple value; the additional
   information 28 to 30; a length over the rest of the input (no allocation for
   it); a depth over 16; a map of more than 64 pairs, or with a key that is not
   an integer or a text; two equal keys in a map; a text that is not UTF-8; an
   integer out of the range of an OCaml int. A length that is not in its
   shortest form is accepted. *)

val decode_all : string -> (t, string) result
(* [decode_all s] is the one item of [s]: bytes after it are an error. *)
