(* The CAs that a client of the examples trusts. *)

open Weft

let now () = Ptime.of_float_s (Time.posix_to_unix_s (Time.now Posix))

let of_cas cas = X509.Authenticator.chain_of_trust ~time:now cas

let of_file file =
  let text =
    try In_channel.with_open_bin file In_channel.input_all
    with Sys_error m -> failwith ("Trust.of_file: " ^ m)
  in
  (* One certificate at a time: a certificate that does not decode is not
     trusted, but the others are. *)
  let cas =
    X509.Certificate.fold_decode_pem_multiple
      (fun cas -> function Ok ca -> ca :: cas | Error _ -> cas)
      [] text
  in
  if cas = [] then failwith ("Trust.of_file: no certificate in " ^ file) ;
  of_cas (List.rev cas)

let system_file () =
  match Sys.getenv_opt "SSL_CERT_FILE" with
  | None | Some "" ->
      "/etc/ssl/certs/ca-certificates.crt"
  | Some f ->
      f

let system () = of_file (system_file ())
