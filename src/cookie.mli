(** Cookies: the [Cookie] field of a request and the [Set-Cookie] field of a
    response ({{:https://www.rfc-editor.org/rfc/rfc6265}RFC 6265}, with the
    rules of the draft RFC 6265bis and of CHIPS for [SameSite], the name
    prefixes and [Partitioned]). {!Request.cookies}, {!Request.cookie} and
    {!Response.add_cookie} use this module. *)

(** {1:request The Cookie field of a request} *)

val parse : string -> (string * string) list
(** [parse s] is the name and value pairs of the [Cookie] field value [s], in
    order, with duplicates. The parse never fails:
    - The parts between the [;] characters are the pairs. The spaces and tabs at
      the two ends of a part go away, and an empty part gives no pair.
    - The name is the part before the first [=], and the value is the part after
      it, each with no spaces or tabs at its two ends. Then, if the value has
      two or more bytes and starts and ends with a double quote, the two quotes
      go away. The bytes between them stay.
    - A part with no [=], or with an empty name, gives no pair: a server cannot
      look up a cookie with no name.

    For example, [parse "a=1; b=\"2\"; c"] is [[("a", "1"); ("b", "2")]].

    The bytes stay as they are: there is no percent-decode, and a name that is
    not a token stays. Code that sends a name or a value again must check it. *)

(** {1:response The Set-Cookie field of a response} *)

(** The type for the [SameSite] attribute. [`Unspecified] sends no attribute,
    thus the user agent uses its default. *)
type same_site = [`Strict | `Lax | `None | `Unspecified]

(** The type for the cookies of a [Set-Cookie] field. {!make} validates the
    fields. *)
type t = private
  { name: string
  ; value: string
  ; expires: Weft.Clock.posix option
  ; max_age: int option  (** In seconds. *)
  ; domain: string option
  ; path: string
  ; secure: bool
  ; http_only: bool
  ; same_site: same_site
  ; partitioned: bool }

val make :
     ?expires:Weft.Clock.posix
  -> ?max_age:int
  -> ?domain:string
  -> ?path:string
  -> ?secure:bool
  -> ?http_only:bool
  -> ?same_site:same_site
  -> ?partitioned:bool
  -> string
  -> string
  -> t
(** [make name value] is a cookie. The defaults are:
    - [path] is ["/"]. With no [Path], the user agent takes the directory of the
      request path.
    - [http_only] is [true]: a script cannot read the cookie. A cookie that a
      script reads, for example a CSRF token of a double submit, needs
      [~http_only:false].
    - [same_site] is [`Lax].
    - [secure] is [false], so that a test over [http] works. Use [~secure:true]
      in production.
    - No [expires] and no [max_age]: the cookie ends with the session of the
      user agent. The user agent cuts [Expires] and [Max-Age] to 400 days.
    - [partitioned] is [false].

    @raise Invalid_argument
      if one of these conditions is false:
      - [name] is a token (RFC 9110), and [value] has only cookie-octets (RFC
        6265 §4.1.1: no space, double quote, [,], [;], backslash, control
        character, DEL or byte of 0x80 or more). Encode other bytes, for example
        with base64url. An empty value is permitted.
      - [name] and [value] together have 4096 bytes or less, and [domain] and
        [path] each have 1024 bytes or less. A user agent ignores a larger
        cookie or attribute, with no error.
      - [domain] has only ASCII letters, digits, [-], [.] and [_], and its
        labels are not empty. One [.] at the start is permitted.
      - [path] starts with [/], and has only bytes from 0x21 to 0x7E, except
        [;]. Thus a space is not permitted.
      - [max_age] is 0 or more. [0] tells the user agent to remove the cookie.
      - [same_site] [`None] and [partitioned] each need [secure].
      - A name that starts with [__Secure-] needs [secure]. A name that starts
        with [__Host-] needs [secure], the path ["/"] and no [domain]. The
        compare of the prefixes ignores the case. *)

val with_value : string -> t -> t
(** [with_value v c] is [c] with the value [v]. For example, a session
    middleware keeps one cookie and gives a new value for each response.

    @raise Invalid_argument as {!make} does for [v]. *)

val expired : t -> t
(** [expired c] is the cookie that removes [c] from the user agent: [c] with the
    value [""], [Max-Age=0] and [Expires] at the start of 1970. The user agent
    matches a cookie on its name, domain and path, and on its partition for a
    [Partitioned] cookie: thus [expired] keeps these attributes of [c]. *)

val to_string : t -> string
(** [to_string c] is the value of a [Set-Cookie] field for [c]: [name=value],
    then [Expires], [Max-Age], [Domain], [Path], [Secure], [HttpOnly],
    [SameSite] and [Partitioned], in this order, each only if it is set. For
    example, [to_string (make "a" "1")] is
    ["a=1; Path=/; HttpOnly; SameSite=Lax"]. *)

val find : string -> string -> string option
(** [find name s] is the value of the first pair of {!parse}[ s] with the name
    [name], if any, with no copy of the other pairs. *)

val parse_set_cookie : string -> (string * string) option
(** [parse_set_cookie s] is the name and the value of the [Set-Cookie] field
    value [s]: the part before the first [;], with the rules of {!parse}. [None]
    if this part gives no pair. The attributes are not read. For a test, or a
    client that sends a cookie back. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a cookie as {!to_string} does, but with the length of the value
    in place of the value, for example [a=<1 bytes>; Path=/]. Thus a log does
    not get a secret. *)
