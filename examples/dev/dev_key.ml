(* The key of the sessions during development. *)

let message =
  "SESSION_KEY is not set: the sessions end at a restart. To keep them, set it \
   to 32 random bytes in base64url, for example: export SESSION_KEY=$(openssl \
   rand -base64 32 | tr +/ -_ | tr -d =)"

let session ?(log = prerr_endline) () =
  match Sys.getenv_opt "SESSION_KEY" with
  | Some text when text <> "" -> (
    match Caravan_session.Key.of_base64url text with
    | key ->
        key
    | exception Invalid_argument e ->
        invalid_arg ("SESSION_KEY: " ^ e) )
  | _ ->
      log message ;
      Caravan_session.Key.generate ()
