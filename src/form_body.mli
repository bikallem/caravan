(* Internal. The read of an [application/x-www-form-urlencoded] body, for
   [Request.form] and caravan.session. *)

val content_type : string -> Headers.t -> string option
(** [content_type media h] is the value of the one [Content-Type] field of [h],
    if its media type, with no parameters, is [media] (ASCII case-insensitive).
    [None] for no field or for more than one. *)

val text : max_bytes:int -> Headers.t -> Body.t -> (string, Form.error) result
(** [text ~max_bytes h b] is the text of the form body [b], with the
    [Content-Type] and size rules of [Request.form]. *)

val read :
     max_bytes:int
  -> max_fields:int
  -> Headers.t
  -> Body.t
  -> (Form.t, Form.error) result
(** [read ~max_bytes ~max_fields h b] is the pairs of the form body [b], with
    the rules of [Request.form]. *)
