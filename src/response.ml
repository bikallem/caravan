type kind = Body of Body.t | Upgrade of (Flow.t -> unit)

type t = {status: Status.t; headers: Headers.t; kind: kind}

(* A body response's status is final: 1xx are interim and the engine's to send.
   An upgrade switches protocols (101) or opens a tunnel (2xx). *)
let check_status fn status kind =
  let code = Status.code status in
  match kind with
  | Body _ when Status.kind status = `Informational ->
      invalid_arg
        (Printf.sprintf "Response.%s: %d is not a final status" fn code)
  | Upgrade _ when code <> 101 && Status.kind status <> `Success ->
      invalid_arg
        (Printf.sprintf "Response.%s: an upgrade cannot have status %d" fn code)
  | Body _ | Upgrade _ ->
      ()

let v fn status headers kind =
  check_status fn status kind ;
  {status; headers; kind}

let make ?(status = Status.ok) ?(headers = Headers.empty) body =
  v "make" status headers (Body body)

let string ?status ?(headers = Headers.empty) ?content_type s =
  let headers =
    match content_type with
    | Some ct ->
        Headers.set "Content-Type" ct headers
    | None when Headers.mem "Content-Type" headers ->
        headers
    | None ->
        Headers.add "Content-Type" "text/plain; charset=utf-8" headers
  in
  make ?status ~headers (Body.of_string s)

let empty ?(status = Status.no_content) ?headers () =
  make ~status ?headers Body.empty

let stream ?status ?headers ?length produce =
  make ?status ?headers (Body.of_writer ?length produce)

let upgrade ?(status = Status.switching_protocols) ?(headers = Headers.empty) f
    =
  v "upgrade" status headers (Upgrade f)

let status r = r.status

let headers r = r.headers

let kind r = r.kind

let with_status status r =
  check_status "with_status" status r.kind ;
  {r with status}

let with_headers headers r = {r with headers}

let pp ppf r =
  Format.fprintf ppf "@[<v>%a%s@,%a@]" Status.pp r.status
    (match r.kind with Body _ -> "" | Upgrade _ -> " (upgrade)")
    Headers.pp r.headers
