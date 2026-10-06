(* Internal. The idle connections of each host and port. *)

type t

val make :
  ?now:(unit -> Affect_unix.Mtime.t) ->
  ?connect:(host:string -> port:int -> Caravan.Flow.t) ->
  Config.t ->
  t
(* [now] and [connect] are for tests. By default, [connect] is
   Connect.connect. *)

val checkout : ?reuse:bool -> t -> host:string -> port:int -> Conn.t * [ `New | `Reused ]
(* [checkout t ~host ~port] is the newest idle connection of the host (in
   lowercase) and the port that is not stale, else a new connection. A
   connection is stale when it is older than [idle_timeout], or when a
   read without a wait gives the end of the input, a byte, or a reset.
   With [~reuse:false], always a new connection.
   @raise Invalid_argument after [close]. *)

val release : t -> Conn.t -> reusable:bool -> unit
(* [release t c ~reusable] keeps [c] as idle if [reusable], the pool is
   not closed, and its host has less than [max_idle_per_host] idle
   connections. Else it closes [c]. *)

val close : t -> unit
(* [close t] closes the idle connections and the connections in use. *)
