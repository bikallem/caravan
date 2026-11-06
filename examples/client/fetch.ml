(* Fetches a URL with the client of caravan, and prints the status and the body.
   It reads at most 1 MiB of the body: a larger body is an error.

   An https URL uses the CAs of Trust.system of the dev library: SSL_CERT_FILE,
   else /etc/ssl/certs/ca-certificates.crt. Trust.system is a dev stand-in: in
   production, use the CAs of the system with a library such as ca-certs
   (Ca_certs.authenticator ()).

   dune exec examples/client/fetch.exe -- https://example.com/

   While the gateway (main.exe) runs:

   dune exec examples/client/fetch.exe -- http://127.0.0.1:8080/api/

   While the tls example runs, give its CA ($CA) in SSL_CERT_FILE:

   SSL_CERT_FILE=$CA dune exec examples/client/fetch.exe -- \
   https://localhost:8443/

   A bad URL gives a message and the exit code 2. *)

open Example_dev
open Caravan

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let max_body = 1024 * 1024

(* [request url] is a GET of [url]: scheme://host[:port][/path][?query], with
   the scheme http or https. It is None for another URL. *)
let request url =
  let make scheme authority path =
    let path =
      if String.starts_with ~prefix:"/" path then path else "/" ^ path
    in
    if List.mem (String.lowercase_ascii scheme) ["http"; "https"] then
      Some (Request.make ~scheme ~authority GET path)
    else None
  in
  try Scanf.sscanf url "%[^:]://%[^/?#]%[^#]" make
  with Scanf.Scan_failure _ | End_of_file | Invalid_argument _ -> None

let () =
  let url =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "http://127.0.0.1:8080/api/"
  in
  let bad_url () =
    prerr_endline ("not a URL: " ^ url) ;
    exit 2
  in
  let req = match request url with Some req -> req | None -> bad_url () in
  (* Only an https URL needs the CAs. *)
  let tls =
    if String.lowercase_ascii (Request.scheme req) = "https" then (
      (* The generator of the OS: safe on many domains. *)
      Mirage_crypto_rng_unix.use_default () ;
      Some
        (Caravan_tls.https
           (ok (Tls.Config.client ~authenticator:(Trust.system ()) ())) ) )
    else None
  in
  (* The fiber code gives the result. The program prints it after the run.
     with_response closes the body at the end, also after an error. The client
     raises Invalid_argument for a host or a port that it refuses. *)
  let result =
    Weft.run (module Weft_os)
    @@ fun () ->
    Caravan_client.run ?tls
    @@ fun client ->
    match
      Caravan_client.with_response client req (fun resp ->
          ( Status.code (Response.status resp)
          , Body.to_string ~max:max_body (Caravan_client.body resp) ) )
    with
    | r ->
        Ok r
    | exception Invalid_argument _ ->
        Error None
    | exception (Caravan_client.Error _ as e) ->
        Error (Some (Printexc.to_string e))
    | exception Body.Too_large ->
        Error (Some "the body is larger than 1 MiB")
  in
  match result with
  | Ok (code, body) ->
      Printf.printf "%d\n%s" code body
  | Error None ->
      bad_url ()
  | Error (Some text) ->
      prerr_endline text ; exit 1
