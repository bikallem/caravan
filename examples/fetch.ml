(* Fetches a URL with the client, and prints the status and the body. For
   example: dune exec examples/fetch.exe -- http://localhost:8080/hello *)

let () =
  let url =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "http://localhost:8080/hello"
  in
  (* The fiber code gives the result. The program prints it after the run. *)
  let result =
    Weft_linux.run
    @@ fun () ->
    Caravan_client.run
    @@ fun client ->
    match Caravan_client.get client url with
    | resp ->
        Ok
          ( Caravan.Status.code (Caravan.Response.status resp)
          , Caravan.Body.to_string (Caravan_client.body resp) )
    | exception Caravan_client.Error e ->
        Error (Printexc.to_string (Caravan_client.Error e))
  in
  match result with
  | Ok (code, body) ->
      Printf.printf "%d\n%s" code body
  | Error text ->
      prerr_endline text ; exit 1
