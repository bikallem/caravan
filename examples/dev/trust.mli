(** The certificate authorities (CAs) that a client of the examples trusts. For
    production, use the package ca-certs: [Ca_certs.authenticator ()] knows the
    trust store of each system. *)

val of_cas : X509.Certificate.t list -> X509.Authenticator.t
(** [of_cas cas] trusts each certificate chain that one of [cas] signed
    ([X509.Authenticator.chain_of_trust]). It reads the time from the [Posix]
    clock of the weft world: in the simulator, the virtual time. Thus the
    handshake must run in fiber code. *)

val of_file : string -> X509.Authenticator.t
(** [of_file path] is {!of_cas} of the CAs of the PEM file [path]. It decodes
    one certificate at a time, as ca-certs does: it skips a certificate that
    does not decode, and trusts the others.

    @raise Failure
      if the file cannot be read or has no certificate that decodes. *)

val system_file : unit -> string
(** [system_file ()] is the file that the environment variable [SSL_CERT_FILE]
    names. If the variable is not set or is empty, the file is
    [/etc/ssl/certs/ca-certificates.crt], for example the trust store of Debian
    and Ubuntu. On other systems, set [SSL_CERT_FILE]. *)

val system : unit -> X509.Authenticator.t
(** [system ()] is [of_file (system_file ())].

    @raise Failure as {!of_file}. *)
