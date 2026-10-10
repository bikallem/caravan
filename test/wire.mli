(* A strict parser of the HTTP/1.1 responses that a server wrote on one
   connection. It examines only the structure: the status line, the field lines
   and the framing of RFC 9112 6.3. The checks of the field values are for the
   tests. *)

(** What follows the head of a response. *)
type framing =
  | No_body  (** A 1xx (not 101), a 204, a 304, or a response to HEAD. *)
  | Length of int  (** A [Content-Length] body. *)
  | Chunked  (** A chunked body. *)
  | Close_delimited  (** A body that ends where the connection ends. *)
  | Tunnel  (** A 101, or a 2xx to CONNECT: the connection changes. *)

type response =
  { head: string
        (** The raw head, from the status line to the empty line included. *)
  ; status_line: string  (** Without its CRLF. *)
  ; code: int
  ; fields: (string * string) list
        (** The field lines in order, with duplicates. A name is as written. A
            value is without the OWS before and after it. *)
  ; framing: framing
  ; body: string  (** The content. A chunked body is decoded. *) }

type t =
  { responses: response list
        (** The interim (1xx, not 101) responses, then the final one. *)
  ; rest: string
        (** The bytes after the final response. After a [Tunnel], the bytes of
            the tunnel. *)
  ; fin: bool  (** The server half-closed the connection. *)
  ; reset: bool  (** The server reset the connection. *) }

val parse : meth:string -> Memory_server.output -> (t, string) result
(** [parse ~meth out] parses the responses in [out] to a request with the method
    [meth]. It stops after the first response that is not interim. It is an
    error if a head is not complete, if a status line or a field line is
    malformed, if the Content-Length values are not equal digits, if a response
    has both Transfer-Encoding and Content-Length, or if a body is short. *)

val final : t -> response
(** [final t] is the last response of [t]. *)

val find : string -> response -> string option
(** [find name r] is the value of the first field [name] of [r], in any case. *)

val find_all : string -> response -> string list
(** [find_all name r] are the values of the fields [name] of [r], in order. *)
