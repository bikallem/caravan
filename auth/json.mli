(* Internal. A strict parse of JSON (RFC 8259) with yojson. *)

val parse : string -> (Yojson.Safe.t, string) result
(** [parse s] is the value of the JSON text [s]. The errors:
    - ["not JSON"]: a text that RFC 8259 does not permit (comments, names with
      no quotes, [NaN] and [Infinity], control characters in a string, escapes
      of other characters, other numbers), a lone surrogate in a [\u] escape, or
      a depth over 32. A check refuses these before yojson.
    - ["not UTF-8"]: invalid UTF-8.
    - ["not JSON: "] and the message of yojson: a text that yojson refuses after
      the check. No such text is known.
    - ["a number that is not finite"]: for example [1e400] or an integer of 400
      digits.
    - ["a name two times"]: an object with two members of the same name. *)
