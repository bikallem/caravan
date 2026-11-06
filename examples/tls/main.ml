(* The TLS example on https://127.0.0.1:8443. With CERT and KEY in the
   environment, main reads the certificate chain and the key from these PEM
   files. If not, it makes a development certificate (never for production). *)

open Example_dev

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let read_file path = In_channel.with_open_bin path In_channel.input_all

(* The configuration of the files of CERT and KEY. *)
let of_files cert key =
  let certs = ok (X509.Certificate.decode_pem_multiple (read_file cert)) in
  let key = ok (X509.Private_key.decode_pem (read_file key)) in
  ok
    (Tls.Config.server
       ~certificates:(`Single (certs, key))
       ~alpn_protocols:["http/1.1"] () )

(* A development certificate. The CA goes to a new file for curl --cacert:
   temp_file makes a file that did not exist, with a new name. The CA is of no
   use after the program ends: its key is only in memory. Thus the program
   removes the file at its end (also at Ctrl-C). *)
let dev () =
  let time = Option.get (Ptime.of_float_s (Unix.gettimeofday ())) in
  let cert =
    Dev_cert.make ~time ~names:["localhost"; "127.0.0.1"; "app.test"]
  in
  let file = Filename.temp_file "caravan-ca" ".pem" in
  Dev_cert.write_ca cert file ;
  at_exit (fun () -> try Sys.remove file with Sys_error _ -> ()) ;
  Printf.printf "Development CA in %s. Try:\ncurl --cacert %s %s\n" file file
    "https://localhost:8443/" ;
  Dev_cert.server_config cert

let () =
  (* The generator of the OS: safe on many domains. *)
  Mirage_crypto_rng_unix.use_default () ;
  let tls =
    match (Sys.getenv_opt "CERT", Sys.getenv_opt "KEY") with
    | Some cert, Some key ->
        of_files cert key
    | None, None ->
        dev ()
    | _ ->
        failwith "set CERT and KEY together"
  in
  Example_common.serve ~port:8443 ~scheme:"https"
    ~engine:(Caravan_tls.engine tls Caravan.Server.http1)
    Example_tls.App.handler
