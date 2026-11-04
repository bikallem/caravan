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

    {!server} and {!client} give a {!Caravan.Flow.t}. A read and a write can run
    at the same time in two fibers, as the flow contract permits. A reader never
    waits for a TCP write: it waits for a writer only while the writer encrypts
    one record. A response of the session to the peer, for example a TLS 1.3
    KeyUpdate, goes out before the next write.

    After the handshake:
    - A TLS failure of the peer (for example a bad record or an alert) makes a
      read or a write raise [Caravan.Flow.Closed], after the plaintext that came
      before it. The flow sends no alert.
    - The end of the TCP stream with no [close_notify] makes a read raise
      [Caravan.Flow.Closed]: it can be a truncation. A [close_notify] of the
      peer gives the end of the stream (a read gives 0).
    - A session permits 64 small records with no plaintext (for example
      KeyUpdate messages), and one more for each 16 KiB of plaintext. A record
      with no plaintext costs one more for each KiB of its size. More fail the
      flow: ocaml-tls 2.1.3 keeps memory for each KeyUpdate, and for a part of a
      handshake message.
    - After a [close_notify] of the peer, the flow ignores the records that
      follow it.
    - [Caravan.Flow.shutdown_send] sends [close_notify], then the end of the TCP
      stream. [Caravan.Flow.close] sends nothing: it never waits.
    - A write that raises, also for a cancel, can send a part of a record. Then
      the flow is failed, and each later write raises [Caravan.Flow.Closed]. *)

(** A failure of the handshake: a TLS alert, a bad message, a failed check of a
    certificate, a handshake of more than 64 KiB, the end of the connection, or
    too many records with no plaintext in the same read as the end of the
    handshake (see {!flow}). The text tells the cause. *)
exception Handshake_error of string

val server :
  Tls.Config.server -> Caravan.Flow.t -> Caravan.Flow.t * Tls.Core.epoch_data
(** [server config flow] does the handshake of a server on [flow], and gives the
    TLS flow and the data of the session, for example its ALPN protocol. The
    read deadlines and timeouts of [flow] apply to the handshake.

    @raise Handshake_error
      if the handshake fails. For a failure of TLS, the alert goes to the peer
      if the connection is open.
    @raise Caravan.Flow.Closed
    @raise Caravan.Flow.Timeout as a read or a write of [flow]. *)

val client :
  Tls.Config.client -> Caravan.Flow.t -> Caravan.Flow.t * Tls.Core.epoch_data
(** [client config flow] does the handshake of a client on [flow], as {!server}.
    The authenticator of [config] checks the certificate of the server, and the
    name [peer_name] of [config] (see [Tls.Config.client] and
    [Tls.Config.peer]). Without a peer name, the authenticator checks no name:
    give one for each server on the Internet. *)

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
      with the deadline [linger_timeout].

    @raise Invalid_argument
      if the ALPN list of [config] has a protocol other than ["http/1.1"]
      (caravan has only HTTP/1.1, and a client that gets ["h2"] sends HTTP/2),
      if the lowest version of [config] is before TLS 1.2 (with TLS 1.0,
      ocaml-tls sends an empty record before each write, and each one costs in
      the budget of the peer), or if no default generator is set (see {!rng}).
*)
