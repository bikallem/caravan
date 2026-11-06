(* Internal. Host names to addresses. *)

val numeric : string -> string option
(* [numeric host] is the address of a numeric host as text: IPv4 in dotted
   decimal, or IPv6, with or without brackets. Else it gives [None]. It accepts
   the same texts as inet_pton, and it does no lookup. *)

val resolve : string -> string list
(* [resolve host] gives the addresses of [host] as text, in the order of the
   lookup, each one time. A numeric host (see {!numeric}) gives its address at
   once. Else Weft.Dns.resolve does the lookup on the name pool of the world. It
   is a cancellation point: a cancel ends the wait at once, but the lookup
   continues in its thread until getaddrinfo returns. @raise Error.Error
   (Connect _) if the host has no address, or if the lookup fails: then the text
   names the host. *)
