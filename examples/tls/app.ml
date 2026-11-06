(* HTTPS with caravan.tls. The page shows the scheme of the request and the ALPN
   protocol of the TLS connection.

   Run dune exec examples/tls/main.exe. By default, main makes a development
   certificate authority (CA) in memory, and a certificate for localhost,
   127.0.0.1 and app.test. It writes the CA to a file, and prints a curl line
   with the path of that file. In the lines that follow, $CA is that path. If
   the environment variables CERT and KEY name two PEM files, main uses that
   certificate chain and key.

   The request has the scheme https. The server has one ALPN protocol, http/1.1,
   and curl offers it. The response has no Strict-Transport-Security field,
   because the Hsts middleware does not send it for localhost:

   curl -i --cacert $CA https://localhost:8443/

   With no ALPN offer of the client, the page shows alpn: none:

   curl -i --cacert $CA --no-alpn https://localhost:8443/

   For another host, the response has the field Strict-Transport-Security:
   max-age=31536000. The --resolve option sends app.test to 127.0.0.1:

   curl -i --cacert $CA --resolve app.test:8443:127.0.0.1 https://app.test:8443/

   A client that does not trust the CA refuses the certificate. Thus curl gives
   the error 60:

   curl -i https://localhost:8443/

   Never use curl -k (--insecure): curl then accepts each certificate, also the
   certificate of an attacker. Give the CA with --cacert. *)

open Caravan
open Caravan_middleware

let info req =
  let alpn = Option.bind (Request.conn req) Conn_info.alpn in
  Response.string
    (Printf.sprintf "scheme: %s\nalpn: %s\n" (Request.scheme req)
       (Option.value ~default:"none" alpn) )

(* Hsts adds the field only to https requests. By default, it does not add the
   field for localhost: a browser keeps HSTS for a host, and then it refuses
   HTTP for each later development server on localhost. *)
let handler = Hsts.middleware () info
