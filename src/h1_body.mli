(* Internal. HTTP/1.1 request bodies (RFC 9112 §6, §7), read from the
   connection's buffered reader. *)

val fixed : Buf_reader.t -> int -> Body.t
(** [fixed r n] is the body of exactly [n] bytes read from [r], as declared by
    [Content-Length]. Its {!Body.length} is [Some n]. Reads return at most the
    bytes that remain, then [0] without reading [r] again, so that the next
    request on the connection stays in [r].

    Reads raise {!Body.Truncated} if [r] ends before [n] bytes. *)
