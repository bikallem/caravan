(** A cookie jar for the tests of the examples, as [curl -c jar -b jar] keeps
    it. It is for tests only:
    - It keeps the name and the value of each cookie, with
      {!Caravan.Cookie.parse_set_cookie}.
    - Of the attributes, it reads only [Max-Age]. Each cookie goes with each
      request. *)

(** The type for cookie jars. *)
type t

val create : unit -> t
(** [create ()] is an empty jar. *)

val find : string -> t -> string option
(** [find name jar] is the value of the cookie [name], if [jar] has it. *)

val with_cookies : t -> Caravan.Request.t -> Caravan.Request.t
(** [with_cookies jar req] is [req] with a [Cookie] field of all the cookies of
    [jar], in the order of their first [Set-Cookie]. An empty jar gives [req].
*)

val keep : t -> Caravan.Headers.t -> unit
(** [keep jar headers] keeps the cookies of the [Set-Cookie] fields of
    [headers]. A new value replaces the old value of the name. A [Max-Age] of 0
    or less removes the cookie, as {!Caravan.Cookie.expired} sends it. *)

val fetch : t -> Caravan_testing.t -> Caravan.Request.t -> Caravan_testing.reply
(** [fetch jar t req] sends [with_cookies jar req] with
    {!Caravan_testing.fetch}, then keeps the cookies of the reply. *)
