type t =
  { meth: Method.t
  ; version: Version.t
  ; scheme: string
  ; authority: string option
  ; path: string
  ; headers: Headers.t
  ; body: Body.t
  ; conn: Conn_info.t option
  ; attrs: Attrs.t }

(* Characters that would corrupt a request line or a Host field. *)
let check fn what s =
  let bad = function '\x00' .. '\x20' | '\x7f' -> true | _ -> false in
  if String.exists bad s then
    invalid_arg (Printf.sprintf "Request.%s: invalid %s %S" fn what s)

let make ?(version = Version.Http_1_1) ?(scheme = "http") ?authority
    ?(headers = Headers.empty) ?(body = Body.empty) ?conn meth path =
  check "make" "path" path ;
  Option.iter (check "make" "authority") authority ;
  { meth
  ; version
  ; scheme
  ; authority
  ; path
  ; headers
  ; body
  ; conn
  ; attrs= Attrs.empty }

let meth r = r.meth

let version r = r.version

let scheme r = r.scheme

let authority r = r.authority

let path r = r.path

let path_only r =
  match String.index_opt r.path '?' with
  | None ->
      r.path
  | Some i ->
      String.sub r.path 0 i

let query r =
  match String.index_opt r.path '?' with
  | None ->
      None
  | Some i ->
      Some (String.sub r.path (i + 1) (String.length r.path - i - 1))

let query_params r = match query r with None -> [] | Some q -> Form.parse q

let headers r = r.headers

let cookies r =
  List.concat_map Cookie.parse (Headers.find_all "cookie" r.headers)

let cookie name r =
  List.find_map (Cookie.find name) (Headers.find_all "cookie" r.headers)

let body r = r.body

let conn r = r.conn

let attrs r = r.attrs

let find_attr k r = Attrs.find k r.attrs

let with_headers headers r = {r with headers}

let with_body body r = {r with body}

let with_path path r =
  check "with_path" "path" path ;
  {r with path}

let with_authority authority r =
  Option.iter (check "with_authority" "authority") authority ;
  {r with authority}

let with_scheme scheme r =
  {r with scheme= (match scheme with `Http -> "http" | `Https -> "https")}

let is_valid_authority s = H1_parse.is_authority s

(* The value of the one [Content-Type] field of [headers], if its media type,
   with no parameters, is [media]. *)
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

let is_form headers = Option.is_some (content_type Form.media_type headers)

let form ?(max_bytes = 1_048_576) ?(max_fields = 1000) r =
  if max_bytes < 0 then invalid_arg "Request.form: max_bytes < 0" ;
  if max_fields < 0 then invalid_arg "Request.form: max_fields < 0" ;
  if not (is_form r.headers) then Error Form.Unsupported_media_type
  else
    match Body.length r.body with
    (* No read, thus no 100 Continue. *)
    | Some n when n > max_bytes ->
        Error Form.Too_large
    | _ -> (
      match Body.to_string ~max:max_bytes r.body with
      | exception Body.Too_large ->
          Error Form.Too_large
      | s when Form.count s > max_fields ->
          Error Form.Too_many_fields
      | s ->
          Ok (Form.parse s) )

let with_form f r =
  { r with
    body= Body.of_string (Form.encode f)
  ; headers= Headers.set "Content-Type" Form.media_type r.headers }

let multipart_boundary r =
  Option.bind (content_type Multipart.media_type r.headers) Multipart.boundary

(* The limits are checked before the [Content-Type], as in [form]. *)
let check_limits fn limits =
  List.iter
    (fun (what, v) ->
      match v with
      | Some n when n < 0 ->
          invalid_arg (Printf.sprintf "Request.%s: %s < 0" fn what)
      | Some _ | None ->
          () )
    limits

let multipart ?max_parts ?max_header_bytes r ~init f =
  check_limits "multipart"
    [("max_parts", max_parts); ("max_header_bytes", max_header_bytes)] ;
  match multipart_boundary r with
  | None ->
      Error Multipart.Unsupported_media_type
  | Some boundary ->
      Multipart.fold ?max_parts ?max_header_bytes ~boundary r.body ~init f

let multipart_form ?max_parts ?max_header_bytes ?max_bytes r =
  check_limits "multipart_form"
    [ ("max_parts", max_parts)
    ; ("max_header_bytes", max_header_bytes)
    ; ("max_bytes", max_bytes) ] ;
  match multipart_boundary r with
  | None ->
      Error Multipart.Unsupported_media_type
  | Some boundary ->
      Multipart.read_all ?max_parts ?max_header_bytes ?max_bytes ~boundary
        r.body

let with_multipart fields r =
  let boundary, text = Multipart.encode fields in
  { r with
    body= Body.of_string text
  ; headers=
      Headers.set "Content-Type"
        (Multipart.media_type ^ "; boundary=" ^ boundary)
        r.headers }

let with_attrs attrs r = {r with attrs}

let with_attr k v r = {r with attrs= Attrs.add k v r.attrs}

let pp ppf r =
  let start ppf () =
    Format.fprintf ppf "%a %s://%s%s %a" Method.pp r.meth r.scheme
      (Option.value ~default:"" r.authority)
      r.path Version.pp r.version
  in
  Message_pp.pp ppf start r.headers r.attrs
