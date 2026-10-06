(* Internal. Host names to addresses, without a block of the domain. *)

val threads_started : int Atomic.t
(* The number of lookup threads, for tests. *)

val resolve :
  ?getaddrinfo:(string -> string -> Unix.getaddrinfo_option list -> Unix.addr_info list) ->
  host:string ->
  port:int ->
  unit ->
  Unix.sockaddr list
(* [resolve ~host ~port ()] are the addresses of [host]. A numeric host
   (IPv4, or IPv6 in brackets) gives its address at once. Else a system
   thread calls [getaddrinfo]. At most 16 fibers wait for a lookup at the
   same time. A lookup cannot stop before getaddrinfo returns: with 2 or
   more domains, Affect_unix.Unix.main waits for a lookup thread that
   runs at its end.
   @raise Error.Error (Connect _) if the host has no address. *)
