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

let headers r = r.headers

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

let with_attrs attrs r = {r with attrs}

let with_attr k v r = {r with attrs= Attrs.add k v r.attrs}

let pp ppf r =
  let start ppf () =
    Format.fprintf ppf "%a %s://%s%s %a" Method.pp r.meth r.scheme
      (Option.value ~default:"" r.authority)
      r.path Version.pp r.version
  in
  Message_pp.pp ppf start r.headers r.attrs
