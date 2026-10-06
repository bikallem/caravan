(* Fetches a URL with the client, and prints the status and the body.
   For example: dune exec examples/fetch.exe -- http://localhost:8080/hello *)

let () =
  let url = if Array.length Sys.argv > 1 then Sys.argv.(1) else "http://localhost:8080/hello" in
  Affect_unix.Unix.main @@ fun () ->
  Caravan_client.run @@ fun client ->
  match Caravan_client.get client url with
  | resp ->
      Printf.printf "%d\n%s" (Caravan.Status.code (Caravan.Response.status resp))
        (Caravan.Body.to_string (Caravan_client.body resp))
  | exception Caravan_client.Error e ->
      prerr_endline (Printexc.to_string (Caravan_client.Error e));
      exit 1
