(** Searches in the HTML text of a page, for the tests of the examples. *)

val after : string -> string -> string
(** [after prefix html] is the text of [html] from the end of the first [prefix]
    to the next double quote. For example, [after {|href="|} html] is the value
    of the first [href].

    @raise Not_found if [html] has no [prefix], or no double quote after it. *)

val csrf_token : string -> string
(** [csrf_token html] is the value of the first field [_csrf] of [html]: the
    text after [{|name="_csrf" value="|}]. *)

val grep_runs : int -> string -> string list
(** [grep_runs n html] is the list of matches of [grep -o '[[:alnum:]_-]\{n\}']
    in an ASCII [html], in order. As grep, it takes the first [n] characters of
    a run of base64url characters, then continues after them. *)
