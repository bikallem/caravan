let content_type media headers =
  match Headers.find_all "content-type" headers with
  | [v] ->
      let t =
        match String.index_opt v ';' with
        | None ->
            v
        | Some i ->
            String.sub v 0 i
      in
      if String.equal (String.lowercase_ascii (String.trim t)) media then Some v
      else None
  | _ ->
      None

let text ~max_bytes headers body =
  if Option.is_none (content_type Form.media_type headers) then
    Error Form.Unsupported_media_type
  else
    match Body.length body with
    (* No read, thus no 100 Continue. *)
    | Some n when n > max_bytes ->
        Error Form.Too_large
    | _ -> (
      match Body.to_string ~max:max_bytes body with
      | exception Body.Too_large ->
          Error Form.Too_large
      | s ->
          Ok s )

let read ~max_bytes ~max_fields headers body =
  Result.bind (text ~max_bytes headers body) (fun s ->
      if Form.count s > max_fields then Error Form.Too_many_fields
      else Ok (Form.parse s) )
