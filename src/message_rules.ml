type content =
  | No_content
  | Length_only of int
  | Length of int
  | Unknown_length

(* [Headers.tokens] drops the empty elements, thus a value is never empty. *)
let content_length headers =
  let valid s =
    String.length s <= 18
    && String.for_all (function '0' .. '9' -> true | _ -> false) s
  in
  (* A request has no Content-Length most of the time: [Headers.mem] finds it
     with no allocation. *)
  if not (Headers.mem "content-length" headers) then `Absent
  else
    match Headers.tokens "content-length" headers with
    | n :: rest when valid n && List.for_all (String.equal n) rest ->
        `Length (int_of_string n)
    | _ ->
        `Invalid

let content ~meth r =
  match Response.kind r with
  | Response.Upgrade _ ->
      No_content
  | Response.Body body -> (
      let code = Status.code (Response.status r) in
      if code < 200 || code = 204 || code = 304 then No_content
      else if Method.equal meth Method.CONNECT && code < 300 then
        No_content
        (* RFC 9110 §15.3.6: no content in a 205. RFC 9112 §6.3 would read a 205
           without a framing field to the close. *)
      else if code = 205 then Length_only 0
      else if Method.equal meth Method.HEAD then
        (* A body with no known length, for example the body of a HEAD response
           of the client: the Content-Length of the handler, if it is valid. A
           HEAD response has no body bytes, thus the field cannot change the
           framing of the connection. *)
        match Body.length body with
        | Some n ->
            Length_only n
        | None -> (
          match content_length (Response.headers r) with
          | `Length n ->
              Length_only n
          | `Absent | `Invalid ->
              No_content )
      else
        match Body.length body with
        | Some n ->
            Length n
        | None ->
            Unknown_length )

(* A match on string literals compares words, with no allocation. *)
let is_connection_field = function
  | "content-length"
  | "transfer-encoding"
  | "connection"
  | "keep-alive"
  | "proxy-connection"
  | "te"
  | "upgrade" ->
      true
  | _ ->
      false

(* Three are representation metadata that a 304 does not send (RFC 9110
   §15.4.5): a cache updates its stored fields from a 304 (RFC 9111 §4.3.4), and
   these change the stored response. Content-Range has no meaning on a 304 (RFC
   9110 §14.4). *)
let is_not_on_304 = function
  | "content-type" | "content-encoding" | "content-language" | "content-range"
    ->
      true
  | _ ->
      false
