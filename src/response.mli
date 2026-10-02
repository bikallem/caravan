(** HTTP responses. *)

type t
(** The type for responses. *)

type kind =
| Body of Body.t (** A response with a body, possibly empty. *)
| Upgrade of (Flow.t -> unit)
(** A response that takes over the connection: after the response head is
    sent, the function gets the connection's raw flow and the connection
    closes when it returns. Used for WebSocket and [CONNECT] tunnels. *)
(** The type for what follows a response's head. *)

val make : ?status:Status.t -> ?headers:Headers.t -> Body.t -> t
(** [make ~status ~headers body] is a response with [body]. [status] defaults
    to {!Status.ok}, [headers] to {!Headers.empty}. The protocol engine adds
    framing fields such as [Content-Length]; handlers do not set them. *)

val string :
  ?status:Status.t -> ?headers:Headers.t -> ?content_type:string -> string -> t
(** [string ~status ~headers ~content_type s] is a response whose body is [s].
    Its [Content-Type] is [content_type] if given, else the one in [headers],
    else ["text/plain; charset=utf-8"]. *)

val empty : ?status:Status.t -> ?headers:Headers.t -> unit -> t
(** [empty ~status ~headers ()] is a response without a body. [status]
    defaults to {!Status.no_content}. *)

val stream :
  ?status:Status.t ->
  ?headers:Headers.t ->
  ?length:int ->
  (Body.Writer.t -> unit) ->
  t
(** [stream ~status ~headers ~length produce] is a response whose body
    [produce] writes, see {!Body.of_writer}. *)

val upgrade : ?status:Status.t -> ?headers:Headers.t -> (Flow.t -> unit) -> t
(** [upgrade ~status ~headers f] is a response that hands the connection to [f]
    once its head is sent. [status] defaults to {!Status.switching_protocols};
    a [2xx] status is for accepting a [CONNECT] tunnel.

    @raise Invalid_argument if [status] is neither [101] nor [2xx]. *)

val status : t -> Status.t
(** [status r] is the status of [r]. *)

val headers : t -> Headers.t
(** [headers r] are the header fields of [r]. *)

val kind : t -> kind
(** [kind r] is what follows the head of [r]. *)

val with_status : Status.t -> t -> t
(** [with_status s r] is [r] with status [s]. *)

val with_headers : Headers.t -> t -> t
(** [with_headers h r] is [r] with headers [h]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a response's status and headers, for inspection. *)
