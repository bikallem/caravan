open Caravan
module Message_rules = Caravan.Private.Message_rules

type request_head =
  { meth: Method.t
  ; authority: string option
  ; path: string
  ; headers: Headers.t
  ; content_length: int option
  ; expect: [`None | `Continue | `Unknown] }

type refusal = Malformed of string | Misdirected

(* A rule of [Malformed] refuses the fields. The text tells the rule, and it can
   hold a field name, but never a value: a value can be a secret. *)
exception Bad of string

let bad fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt

let is_pseudo name = String.starts_with ~prefix:":" name

let is_upper = function 'A' .. 'Z' -> true | _ -> false

(* The compare of [Headers]: it ignores the case of the ASCII letters, with no
   copy. *)
let equal_ci a b =
  let n = String.length a in
  n = String.length b
  &&
  let rec loop i =
    i = n
    || (Char.lowercase_ascii a.[i] = Char.lowercase_ascii b.[i] && loop (i + 1))
  in
  loop 0

(* RFC 9113 §8.2.2. *)
let is_connection_specific = function
  | "connection"
  | "keep-alive"
  | "proxy-connection"
  | "transfer-encoding"
  | "upgrade" ->
      true
  | _ ->
      false

let check_value name value =
  if not (Headers.is_valid_value value) then bad "an invalid value of %S" name

(* The rules of a regular field of HTTP/2, in a request and in trailers (RFC
   9113 §8.2). [of_fields] checks the names and the values. *)
let check_field name value =
  if String.exists is_upper name then bad "an uppercase field name %S" name ;
  if is_connection_specific name then bad "a connection-specific field %S" name ;
  if name = "te" && not (equal_ci value "trailers") then
    bad "a te field other than trailers"

(* The pseudo-fields come before the regular fields (RFC 9113 §8.3). *)
type pseudo =
  { mutable meth: string option
  ; mutable scheme: string option
  ; mutable authority: string option
  ; mutable path: string option }

let read_pseudo fields =
  let p = {meth= None; scheme= None; authority= None; path= None} in
  let once name old value =
    if Option.is_some old then bad "two %s fields" name ;
    Some value
  in
  let rec loop = function
    | (name, value) :: rest when is_pseudo name ->
        check_value name value ;
        ( match name with
        | ":method" ->
            p.meth <- once name p.meth value
        | ":scheme" ->
            p.scheme <- once name p.scheme value
        | ":authority" ->
            p.authority <- once name p.authority value
        | ":path" ->
            p.path <- once name p.path value
        | ":status" ->
            bad "a :status field in a request"
        | _ ->
            bad "an unknown pseudo-field %S" name ) ;
        loop rest
    | regular ->
        (p, regular)
  in
  loop fields

let meth (p : pseudo) =
  match p.meth with
  | None ->
      bad "no :method"
  | Some m ->
      if not (Caravan.Private.Token.is_token m) then
        bad "a :method that is not a token" ;
      Method.of_string m

(* The path of a request with the method [meth]. *)
let target meth (p : pseudo) =
  if Method.equal meth Method.CONNECT then begin
    (* RFC 9113 §8.5. A host and a port, as in HTTP/1.1. *)
    if Option.is_some p.scheme then bad "a CONNECT with :scheme" ;
    if Option.is_some p.path then bad "a CONNECT with :path" ;
    ( match p.authority with
    | None ->
        bad "a CONNECT with no :authority"
    | Some a ->
        if not (Caravan.Private.H1_parse.is_authority ~port:true a) then
          bad "a CONNECT :authority that is not host:port" ) ;
    ""
  end
  else
    match (p.scheme, p.path) with
    | None, _ ->
        bad "no :scheme"
    | _, None ->
        bad "no :path"
    | Some _, Some path ->
        if path = "" then bad "an empty :path" ;
        (* The bytes of an HTTP/1.1 request target. *)
        if not (String.for_all Caravan.Private.Token.is_target_char path) then
          bad "a :path with a byte that is not visible ASCII" ;
        if
          not
            (path.[0] = '/' || (path = "*" && Method.equal meth Method.OPTIONS))
        then bad "a :path that does not start with /" ;
        path

(* RFC 9113 §8.3.1: the authority is [:authority], else [host]. Each [host]
   field must be equal to it. With no [:authority], an empty [host] is no
   authority, as in HTTP/1.1 (RFC 9110 §7.2). *)
let authority (p : pseudo) headers =
  let hosts = Headers.find_all "host" headers in
  match (p.authority, hosts) with
  | None, [] ->
      None
  | Some a, _ | None, a :: _ ->
      if List.exists (fun h -> not (equal_ci h a)) hosts then
        bad "a host field that is not the authority" ;
      if Option.is_none p.authority && a = "" then None
      else if Request.is_valid_authority a then Some a
      else bad "an invalid authority"

let content_length headers =
  match Message_rules.content_length headers with
  | `Absent ->
      None
  | `Length n ->
      Some n
  | `Invalid ->
      bad "an invalid content-length"

(* The fields of a list. The text of the [Invalid_argument] holds the value,
   thus it is not the text of the refusal. *)
let of_fields l =
  match Headers.of_list l with
  | headers ->
      headers
  | exception Invalid_argument _ ->
      bad "an invalid field name or value"

(* RFC 9110 §10.1.1, as in HTTP/1.1: [100-continue] is the only expectation. *)
let expect headers =
  match Headers.tokens "expect" headers with
  | [] ->
      `None
  | tokens ->
      if List.for_all (fun t -> equal_ci t "100-continue") tokens then `Continue
      else `Unknown

(* RFC 9113 §8.2.3: the cookie fields become one field at the position of the
   first, with "; " between the values. Each value is valid, and an empty value
   adds nothing. Thus the joined value is valid, and [Headers.set] does not
   raise. *)
let join_cookies headers =
  match Headers.find_all "cookie" headers with
  | [] | [_] ->
      headers
  | values ->
      let values = List.filter (fun v -> v <> "") values in
      Headers.set "cookie" (String.concat "; " values) headers

let head fields =
  let p, regular = read_pseudo fields in
  List.iter
    (fun (name, value) ->
      if is_pseudo name then bad "a pseudo-field %S after a regular field" name ;
      check_field name value )
    regular ;
  let headers = join_cookies (of_fields regular) in
  let meth = meth p in
  let path = target meth p in
  let head =
    { meth
    ; authority= authority p headers
    ; path
    ; headers
    ; content_length= content_length headers
    ; expect= expect headers }
  in
  match p.scheme with
  | Some s when not (equal_ci s "http" || equal_ci s "https") ->
      Error Misdirected
  | Some _ | None ->
      Ok head

let request fields =
  match head fields with
  | result ->
      result
  | exception Bad reason ->
      Error (Malformed reason)

let trailers l =
  match
    List.iter
      (fun (name, value) ->
        if is_pseudo name then bad "a pseudo-field %S in trailers" name ;
        check_field name value )
      l ;
    of_fields l
  with
  | headers ->
      Ok headers
  | exception Bad reason ->
      Error reason

type body = No_body | Length of int | Unknown_length

type plan = Send of (string * string) list * body | Fail_500 of string

let response ~meth ~date r =
  let status = Response.status r and headers = Response.headers r in
  match Response.kind r with
  | Response.Upgrade _ ->
      Fail_500 "an upgrade response: the HTTP/2 engine has no upgrades"
  | Response.Body _
    when Method.equal meth Method.CONNECT && Status.kind status = `Success ->
      Fail_500 "a 2xx response to CONNECT: the HTTP/2 engine has no tunnels"
  | Response.Body _ ->
      let code = Status.code status in
      (* RFC 9113 §8.2.2: the fields that [connection] names are for one
         connection. A table keeps a long list of names linear. *)
      let named =
        if not (Headers.mem "connection" headers) then fun _ -> false
        else begin
          let table = Hashtbl.create 8 in
          List.iter
            (fun n -> Hashtbl.replace table (String.lowercase_ascii n) ())
            (Headers.tokens "connection" headers) ;
          Hashtbl.mem table
        end
      in
      let dropped name =
        Message_rules.is_connection_field name
        || named name
        || (code = 304 && Message_rules.is_not_on_304 name)
      in
      let lowercase name =
        if String.exists is_upper name then String.lowercase_ascii name
        else name
      in
      let fields =
        Headers.fold
          (fun name value acc ->
            let name = lowercase name in
            if dropped name then acc else (name, value) :: acc )
          headers []
      in
      (* The date of the engine, if no date field stays. *)
      let date =
        match date with
        | Some d when not (List.exists (fun (n, _) -> n = "date") fields) ->
            [("date", d)]
        | Some _ | None ->
            []
      in
      let length n = [("content-length", string_of_int n)] in
      let length_fields, body =
        match Message_rules.content ~meth r with
        | No_content ->
            ([], No_body)
        | Length_only n ->
            (length n, No_body)
        | Length n ->
            (length n, Length n)
        | Unknown_length ->
            ([], Unknown_length)
      in
      Send
        ( ((":status", string_of_int code) :: date)
          @ List.rev_append fields length_fields
        , body )

let interim code =
  if code < 100 || code > 199 || code = 101 then
    invalid_arg (Printf.sprintf "Message.interim: %d" code) ;
  [(":status", string_of_int code)]
