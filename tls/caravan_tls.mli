(** TLS for caravan, with ocaml-tls.

    {[
    let () = Mirage_crypto_rng_unix.use_default ()

    let tls =
      match Tls.Config.server ~certificates:(`Single (certs, key)) () with
      | Ok c ->
          c
      | Error (`Msg m) ->
          failwith m

    let () =
      Caravan.Server.run
        ~engine:(Caravan_tls.engine tls Caravan.Server.http1)
        (module Weft_os)
        handler
    ]}

    {1:rng The random number generator}

    ocaml-tls takes its randomness from [Mirage_crypto_rng.default_generator].
    Set it one time, before the first handshake, with
    [Mirage_crypto_rng_unix.use_default ()]. That generator calls the OS for
    each request and keeps no state, thus it is safe on many domains. Do not use
    a Fortuna generator on more than one domain: it is not safe there.

    {1:flow The TLS flow}

    The TLS flow is that of the library [weft-tls]: {!server}, {!client} and
    {!Handshake_error} are [Weft_tls.server], [Weft_tls.client] and
    [Weft_tls.Handshake_error]. The doc of [Weft_tls] gives the rules of the
    flow (concurrent reads and writes, the failures, [close_notify], the budget
    of records with no plaintext) and of the handshake. *)

(** A failure of the handshake: [Weft_tls.Handshake_error]. The text tells the
    cause. *)
exception Handshake_error of string

val server :
  Tls.Config.server -> Caravan.Flow.t -> Caravan.Flow.t * Tls.Core.epoch_data
(** [server config flow] is [Weft_tls.server config flow]: the handshake of a
    server on [flow], then the TLS flow and the data of the session, for example
    its ALPN protocol. *)

val client :
  Tls.Config.client -> Caravan.Flow.t -> Caravan.Flow.t * Tls.Core.epoch_data
(** [client config flow] is [Weft_tls.client config flow]: the handshake of a
    client on [flow]. The authenticator of [config] checks the certificate of
    the server, and the name [peer_name] or the address [ip] of [config].
    Without one of them, the authenticator checks no name: give one for each
    server on the Internet. *)

val https :
     Tls.Config.client
  -> host:string
  -> Caravan.Flow.t
  -> (Caravan.Flow.t, string) result
(** [https config] is the TLS of [caravan.client] for [https] URLs:
    {[
    let () = Mirage_crypto_rng_unix.use_default ()

    let ok = function Ok v -> v | Error (`Msg m) -> failwith m

    let config =
      ok (Tls.Config.client ~authenticator:(ok (Ca_certs.authenticator ())) ())

    let main () =
      Caravan_client.run ~tls:(Caravan_tls.https config)
      @@ fun client -> Caravan_client.get client "https://example.com/"
    ]}

    [Ca_certs.authenticator ()] of the package ca-certs trusts the certificate
    authorities of the system. Caravan does not depend on it.

    For each connection, [https] sets the peer of [config] from [host]:
    - A name: [Tls.Config.peer]. The client sends the name in SNI, and the
      authenticator checks the name of the certificate.
    - An IPv4 address, or an IPv6 address in brackets: [Tls.Config.ip]. No SNI
      goes (RFC 6066 §3), and the authenticator checks the address of the
      certificate.

    An authenticator that is not from [X509.Authenticator] can ignore the name
    or the address. The client certificates of [config] go to each server.

    [Error] tells the cause: a failed handshake (also a failed check of the
    certificate), the end of the connection in the handshake, a [host] that is
    not a name or an address, or an ALPN protocol of the server other than
    ["http/1.1"].

    @raise Invalid_argument
      at [https config], before a connection, if [config] has a peer name or an
      IP address (the URL gives them), or a cached session or ticket (ocaml-tls
      does not compare the name of a cached session with the host, and a resumed
      session skips the check of the certificate). Also for the checks of
      {!engine}: the ALPN list, the lowest version and the generator. *)

val engine : Tls.Config.server -> Caravan.Engine.t -> Caravan.Engine.t
(** [engine config inner] does the TLS handshake of each connection, then calls
    [inner] with the TLS flow and the connection with the scheme [`Https] and
    the ALPN protocol of the session ({!Caravan.Conn_info.with_tls}). Thus a
    request has the scheme ["https"].

    - The handshake has the deadline [header_timeout] of the server, and it ends
      at a stop of the server. Each read of the handshake examines the deadline
      and the stop, also when the client keeps the socket full. A handshake that
      fails or that ends early closes the connection, and the server reports
      nothing: a client can cause each of these failures.
    - After [inner] returns, the engine sends [close_notify] if [inner] did not,
      then the end of the TCP stream, with the deadline [linger_timeout].

    @raise Invalid_argument
      if the ALPN list of [config] has a protocol other than ["http/1.1"]
      (caravan has only HTTP/1.1, and a client that gets ["h2"] sends HTTP/2),
      if the lowest version of [config] is before TLS 1.2 (with TLS 1.0,
      ocaml-tls sends an empty record before each write, and each one costs in
      the budget of the peer), or if no default generator is set (see {!rng}).
*)

(**/**)

module Private : sig
  val https_with :
       client:
         (   Tls.Config.client
          -> Caravan.Flow.t
          -> Caravan.Flow.t * Tls.Core.epoch_data )
    -> Tls.Config.client
    -> host:string
    -> Caravan.Flow.t
    -> (Caravan.Flow.t, string) result
  (* [https_with ~client config] is [https config] with the handshake [client],
     for a test. *)

  val guard :
       Caravan.Flow.t
    -> handshake:bool ref
    -> (bytes -> int -> int -> int)
    -> Caravan.Flow.t
  (* [guard tcp ~handshake read] is the base flow of the handshake of [engine]:
     [tcp], with the reads of [read] while [!handshake]. Each other function
     goes to [tcp]. For a test. *)
end
