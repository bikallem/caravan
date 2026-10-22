(* Internal. The idle connections of each host and port. *)

type t

val make : ?connect:(host:string -> port:int -> Caravan.Flow.t) -> Config.t -> t
(* [connect] is for tests. By default, [connect] is Connect.connect. The age of
   the idle connections is on the [Mono] clock of the world. *)

val checkout :
  ?reuse:bool -> t -> host:string -> port:int -> Conn.t * [`New | `Reused]
(* [checkout t ~host ~port] is the newest idle connection of the host (in
   lowercase) and the port that is not stale, else a new connection. A
   connection is stale when its age is [idle_timeout] or more, or when a read
   without a wait gives the end of the input, a byte, or a reset. A stale
   connection closes: if it has unread data, the peer gets a reset. With
   [~reuse:false], always a new connection. @raise Invalid_argument after
   [close]. *)

val release : t -> Conn.t -> reusable:bool -> unit
(* [release t c ~reusable] keeps [c] as idle if [reusable], the pool is not
   closed, and its host has less than [max_idle_per_host] idle connections. Else
   it closes [c]. It does no sync, thus a cancelled fiber can call it. *)

val reap : t -> unit
(* [reap t] closes the idle connections whose age is [idle_timeout] or more. *)

val idle_hosts : t -> int
(* The number of hosts with idle connections, for tests. *)

val in_use : t -> int
(* The number of connections in use, for tests. *)

val close : t -> unit
(* [close t] closes the idle connections and the connections in use. It does no
   sync. *)
