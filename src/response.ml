type kind = Body of Body.t | Upgrade of (Flow.t -> unit)

(* [refused] runs the [on_refused] function of an upgrade, at most one time: the
   copies of a response share its flag. *)
type t =
  { status: Status.t
  ; headers: Headers.t
  ; kind: kind
  ; attrs: Attrs.t
  ; refused: unit -> unit }

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

let v ~refused fn status headers kind =
  check_status fn status kind ;
  {status; headers; kind; attrs= Attrs.empty; refused}

let make ?(status = Status.ok) ?(headers = Headers.empty) body =
  v ~refused:ignore "make" status headers (Body body)

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

let upgrade ?(status = Status.switching_protocols) ?(headers = Headers.empty)
    ?on_refused f =
  let refused =
    match on_refused with
    | None ->
        ignore
    | Some g ->
        let ran = Atomic.make false in
        fun () -> if not (Atomic.exchange ran true) then g ()
  in
  v ~refused "upgrade" status headers (Upgrade f)

let abandon r =
  Weft.Fiber.shield (fun () ->
      match r.kind with Body b -> Body.close b | Upgrade _ -> r.refused () )

let status r = r.status

let headers r = r.headers

let kind r = r.kind

let with_status status r =
  check_status "with_status" status r.kind ;
  {r with status}

let with_headers headers r = {r with headers}

let add_cookie c r =
  {r with headers= Headers.add "Set-Cookie" (Cookie.to_string c) r.headers}

let with_body body r =
  match r.kind with
  | Body _ ->
      {r with kind= Body body}
  | Upgrade _ ->
      invalid_arg "Response.with_body: an upgrade has no body"

let attrs r = r.attrs

let with_attrs attrs r = {r with attrs}

let find_attr k r = Attrs.find k r.attrs

let with_attr k v r = {r with attrs= Attrs.add k v r.attrs}

let pp ppf r =
  let start ppf () =
    Format.fprintf ppf "%a%s" Status.pp r.status
      (match r.kind with Body _ -> "" | Upgrade _ -> " (upgrade)")
  in
  Message_pp.pp ppf start r.headers r.attrs
