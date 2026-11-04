(** Test certificates for caravan.tls: a CA and a leaf for [localhost], with
    P-256 keys. The keys come from the default generator of mirage-crypto-rng.
*)

val pki : unit -> X509.Certificate.t * X509.Certificate.t * X509.Private_key.t
(** [pki ()] is a new CA, a leaf certificate for [localhost] that the CA signed,
    and the key of the leaf. The certificates are valid from 2020 to 2040. *)

val localhost : [`host] Domain_name.t

val configs :
     ?server_alpn:string list
  -> ?client_alpn:string list
  -> ?peer_name:[`host] Domain_name.t
  -> time:(unit -> Ptime.t option)
  -> unit
  -> Tls.Config.server * Tls.Config.client
(** [configs ~time ()] makes a {!pki}, and gives a server configuration with the
    leaf (ALPN [server_alpn], default [["http/1.1"]]) and a client configuration
    that trusts the CA at the time [time ()]. The peer name of the client is
    [peer_name] (default {!localhost}). *)
