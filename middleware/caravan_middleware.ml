open Caravan

module Forwarded = struct
  type node = Ip of string | Unix | Unknown

  let is_digit c = c >= '0' && c <= '9'

  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ->
        true
    | _ ->
        false

  (* [ipv4 s] is the four octets of [s], or [None]. An octet has no leading
     zero, because some parsers read it as octal. *)
  let ipv4 s =
    match String.split_on_char '.' s with
    | [_; _; _; _] as parts
      when List.for_all
             (fun p ->
               p <> "" && String.for_all is_digit p
               && (p = "0" || p.[0] <> '0')
               &&
               match int_of_string_opt p with
               | Some n ->
                   n <= 255
               | None ->
                   false )
             parts ->
        Some (List.map int_of_string parts)
    | _ ->
        None

  (* [find_double s] is the index of the first "::" of [s]. *)
  let find_double s =
    let n = String.length s in
    let rec go i =
      if i + 1 >= n then None
      else if s.[i] = ':' && s.[i + 1] = ':' then Some i
      else go (i + 1)
    in
    go 0

  (* [groups ~tail p] are the 16-bit groups of [p], a part of an IPv6 address
     without "::". With [tail], the last group can be an IPv4 address, as two
     groups. *)
  let groups ~tail p =
    let rec go = function
      | [] ->
          Some []
      | [last] when tail && String.contains last '.' -> (
        match ipv4 last with
        | Some [a; b; c; d] ->
            Some [(a lsl 8) lor b; (c lsl 8) lor d]
        | _ ->
            None )
      | g :: rest ->
          let n = String.length g in
          if n >= 1 && n <= 4 && String.for_all is_hex g then
            Option.map (fun r -> int_of_string ("0x" ^ g) :: r) (go rest)
          else None
    in
    if p = "" then Some [] else go (String.split_on_char ':' p)

  (* [ipv6 s] is the eight groups of [s], or [None]. *)
  let ipv6 s =
    match find_double s with
    | None -> (
      match groups ~tail:true s with
      | Some g when List.length g = 8 ->
          Some g
      | _ ->
          None )
    | Some i -> (
        let n = String.length s in
        let right = String.sub s (i + 2) (n - i - 2) in
        (* A second "::" in [right] gives an empty group, thus [None]. *)
        match
          (groups ~tail:false (String.sub s 0 i), groups ~tail:true right)
        with
        | Some l, Some r when List.length l + List.length r <= 7 ->
            Some
              ( l
              @ List.init (8 - List.length l - List.length r) (fun _ -> 0)
              @ r )
        | _ ->
            None )

  (* [text_of_ipv6 g] is the RFC 5952 text of the groups [g]: the longest run of
     two or more zero groups (the first of equal runs) is "::", and the other
     groups are lower-case hexadecimal with no leading zero. An IPv4 address
     mapped to IPv6 is its IPv4 address. *)
  let text_of_ipv6 = function
    | [0; 0; 0; 0; 0; 0xffff; ab; cd] ->
        Printf.sprintf "%d.%d.%d.%d" (ab lsr 8) (ab land 0xff) (cd lsr 8)
          (cd land 0xff)
    | g ->
        let a = Array.of_list g in
        let best =
          ref
            ( (-1 [@mutate off "the start is read only after a run sets it"])
            , (0 [@mutate off "a best run of one zero gives no \"::\""]) )
        and run =
          ref ((-1 [@mutate off "the start of an empty run is not read"]), 0)
        in
        Array.iteri
          (fun i x ->
            if x = 0 then (
              let start, len = !run in
              run := if len = 0 then (i, 1) else (start, len + 1) ;
              if snd !run > snd !best then best := !run )
            else
              run :=
                ((-1 [@mutate off "the start of an empty run is not read"]), 0) )
          a ;
        let start, len = !best in
        let b = Buffer.create 39 in
        (* The hexadecimal digits of a group, with no leading zero. *)
        let hex x =
          let digits = "0123456789abcdef" in
          let rec go shift started =
            if shift >= 0 then (
              let d = (x lsr shift) land 0xf in
              if started || d <> 0 || shift = 0 then
                Buffer.add_char b digits.[d] ;
              go (shift - 4) (started || d <> 0) )
          in
          go 12 false
        in
        let part lo hi =
          for i = lo to hi - 1 do
            if i > lo then Buffer.add_char b ':' ;
            hex a.(i)
          done
        in
        if len < 2 then part 0 8
        else (
          part 0 start ;
          Buffer.add_string b "::" ;
          part (start + len) 8 ) ;
        Buffer.contents b

  (* [normal ip] is the normal form of an address: RFC 5952 for IPv6, and the
     text of the octets for IPv4. A text that is not an address only goes to
     lower case. *)
  let normal ip =
    match ipv4 ip with
    | Some _ ->
        ip
    | None -> (
      match ipv6 ip with
      | Some g ->
          text_of_ipv6 g
      | None ->
          String.lowercase_ascii ip )

  let is_port s = s <> "" && String.for_all is_digit s

  (* [of_value v] is the node of a value of X-Forwarded-For. *)
  let of_value v =
    let v = String.trim v in
    let n = String.length v in
    let v6 ip =
      match ipv6 ip with Some g -> Ip (text_of_ipv6 g) | None -> Unknown
    in
    if String.starts_with ~prefix:"[" v then
      match String.index_opt v ']' with
      | Some i ->
          let after = String.sub v (i + 1) (n - i - 1) in
          if
            after = ""
            || after.[0] = ':'
               && is_port (String.sub after 1 (String.length after - 1))
          then v6 (String.sub v 1 (i - 1))
          else Unknown
      | None ->
          Unknown
    else
      match String.split_on_char ':' v with
      | [ip] ->
          if Option.is_some (ipv4 ip) then Ip ip else Unknown
      | [ip; port] when Option.is_some (ipv4 ip) ->
          if is_port port then Ip ip else Unknown
      | _ ->
          v6 v

  let of_peer = function
    | Weft.Net.Tcp ("", _) ->
        Unknown
    | Tcp (ip, _) ->
        Ip (normal ip)
    | Unix _ ->
        Unix

  let trust_ips ips =
    let ips = List.map normal ips in
    function Ip a -> List.mem a ips | Unix | Unknown -> false

  let client_key : node Attrs.key = Attrs.key ~name:"client" ()

  (* [last_elements limit s] are the last [limit] elements of the list [s]
     (split at ','), the last element first. Empty elements stay, thus the
     positions stay. The cost depends on [limit], not on the length of [s]. *)
  let last_elements limit s =
    let rec go acc k j =
      if k = 0 then List.rev acc
      else
        match String.rindex_from_opt s (j - 1) ',' with
        | None ->
            List.rev (String.trim (String.sub s 0 j) :: acc)
        | Some i ->
            go (String.trim (String.sub s (i + 1) (j - i - 1)) :: acc) (k - 1) i
    in
    go [] limit (String.length s)

  (* [from_right limit name req] are the last [limit] elements of the fields
     [name] of [req], the last element first. The field lines join in order: the
     search starts at the last line, thus it reads no more than [limit] elements
     of any line. *)
  let from_right limit name req =
    let rec go acc k = function
      | [] ->
          acc
      | line :: earlier ->
          let e = last_elements k line in
          let k = k - List.length e in
          if k = 0 then acc @ e else go (acc @ e) k earlier
    in
    go [] limit (List.rev (Headers.find_all name (Request.headers req)))

  let scheme_of v =
    match String.lowercase_ascii v with
    | "http" ->
        Some `Http
    | "https" ->
        Some `Https
    | _ ->
        None

  let valid_host v = if Request.is_valid_authority v then Some v else None

  (* [checked_head check l] is the first element of [l] that [check] accepts
     ([Ok (Some x)]), [Ok None] for an empty [l], or [Error ()], with the
     tail. *)
  let checked_head check = function
    | [] ->
        (Ok None, [])
    | v :: rest -> (
      match check v with
      | Some x ->
          (Ok (Some x), rest)
      | None ->
          (Error (), rest) )

  let middleware ?(proto = true) ?(host = false) ?(limit = 1) ~trusted () =
    if limit < 1 then
      invalid_arg "Caravan_middleware.Forwarded.middleware: limit < 1" ;
    fun next req ->
      match Request.conn req with
      | None ->
          next req
      | Some conn -> (
          let is_trusted node = node <> Unknown && trusted node in
          let peer = of_peer (Conn_info.peer conn) in
          if not (is_trusted peer) then next req
          else
            let fields name on = if on then from_right limit name req else [] in
            (* [walk fors protos hosts scheme authority] takes the next element
               from the right: the node before it is trusted. It goes on while
               the new node is trusted. A bad element gives the client
               [Unknown], never the trusted node before it. *)
            let rec walk fors protos hosts client scheme authority =
              match fors with
              | [] ->
                  (client, scheme, authority)
              | f :: fors -> (
                  let p, protos = checked_head scheme_of protos in
                  let h, hosts = checked_head valid_host hosts in
                  match (p, h) with
                  | Ok p, Ok h ->
                      let c = of_value f in
                      let scheme = if Option.is_some p then p else scheme in
                      let authority =
                        if Option.is_some h then h else authority
                      in
                      if is_trusted c then
                        walk fors protos hosts (Some c) scheme authority
                      else (Some c, scheme, authority)
                  | _ ->
                      (Some Unknown, scheme, authority) )
            in
            let client, scheme, authority =
              walk
                (fields "x-forwarded-for" true)
                (fields "x-forwarded-proto" proto)
                (fields "x-forwarded-host" host)
                None None None
            in
            match client with
            | None ->
                next req
            | Some c ->
                let req = Request.with_attr client_key c req in
                let req =
                  match scheme with
                  | Some s ->
                      Request.with_scheme s req
                  | None ->
                      req
                in
                let req =
                  match authority with
                  | Some a
                    when not (Method.equal (Request.meth req) Method.CONNECT) ->
                      Request.with_authority (Some a) req
                  | _ ->
                      req
                in
                next req )

  let client req =
    match Request.find_attr client_key req with
    | Some n ->
        Some n
    | None ->
        Option.map (fun c -> of_peer (Conn_info.peer c)) (Request.conn req)
end

module Request_id = struct
  let id_key : string Attrs.key = Attrs.key ~name:"request_id" ()

  let is_kept = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '@' ->
        true
    | _ ->
        false

  let max_length = 255

  let clean s =
    let b = Buffer.create (min (String.length s) max_length) in
    String.iter
      (fun c ->
        if is_kept c && Buffer.length b < max_length then Buffer.add_char b c )
      s ;
    Buffer.contents b

  let middleware ?(field = "X-Request-Id") ?(accept = fun _ -> true) ?make () =
    if not (Headers.is_valid_name field) then
      invalid_arg
        ("Caravan_middleware.Request_id.middleware: invalid field name " ^ field) ;
    let counter = Atomic.make 0 in
    let next () = string_of_int (Atomic.fetch_and_add counter 1) in
    fun handler req ->
      let incoming =
        if accept req then
          Option.map clean (Headers.find field (Request.headers req))
        else None
      in
      let id =
        match incoming with
        | Some s when s <> "" ->
            s
        | _ -> (
          match make with
          | None ->
              next ()
          | Some f -> (
            match clean (f req) with "" -> next () | s -> s ) )
      in
      let req =
        Request.with_headers (Headers.set field id (Request.headers req)) req
        |> Request.with_attr id_key id
      in
      let resp = handler req in
      Response.with_headers (Headers.set field id (Response.headers resp)) resp

  let find req = Request.find_attr id_key req
end

module Cancel = Private.Cancel
module H1_error = Private.H1_error

(* [watch_request req] is [req] with a hook on its request body, and a flag that
   becomes [true] when a read of that body raises [H1_error.E] or
   [Body.Truncated]: the condition of [H1_body.failed] in the server. A failure
   of a sink, for example [Body.Too_large] of [to_string], does not count. *)
let watch_request req =
  let failed = Atomic.make false in
  let hook how ~bytes:_ =
    match how with
    | Body.Failed (H1_error.E _ | Body.Truncated) ->
        Atomic.set failed true
    | _ ->
        ()
  in
  (Request.with_body (Body.on_end hook (Request.body req)) req, failed)

(* [request_body_error failed e] is [true] if [e] is the error of a request body
   that failed. The server then sends the answer of that error (for example
   413), or nothing for a body that was cut short, and reports nothing. *)
let request_body_error failed = function
  | H1_error.E _ | Body.Truncated ->
      Atomic.get failed
  | _ ->
      false

module Access_log = struct
  type outcome =
    | Complete
    | Incomplete
    | Failed of exn
    | Cancelled
    | Not_sent
    | Upgraded

  type entry =
    { start: Weft.Clock.posix
    ; duration: Weft.Span.t
    ; meth: Method.t
    ; target: string
    ; version: Version.t
    ; client: Forwarded.node option
    ; request_id: string option
    ; route: string option
    ; status: Status.t option
    ; bytes: int
    ; outcome: outcome }

  (* The responses that the server sends with no body: the framings [No_body]
     and [Length_only]. *)
  let has_no_body meth status =
    Method.equal meth Method.HEAD
    || match Status.code status with 204 | 205 | 304 -> true | _ -> false

  let middleware ?(route = fun _ -> None) ~log () next req =
    let start = Weft.Time.now Posix and t0 = Weft.Time.now Mono in
    let req, request_failed = watch_request req in
    let meth = Request.meth req in
    let connect = Method.equal meth Method.CONNECT in
    let emit ~route ~status ~bytes outcome =
      let entry =
        { start
        ; duration= Weft.Time.diff Mono (Weft.Time.now Mono) t0
        ; meth
        ; target=
            ( if connect then Option.value ~default:"" (Request.authority req)
              else Request.path req )
        ; version= Request.version req
        ; client= Forwarded.client req
        ; request_id= Request_id.find req
        ; route
        ; status
        ; bytes
        ; outcome }
      in
      (* A cancel of the fiber does not stop [log]. Its exception never changes
         the response or the exception of the handler. *)
      Weft.Fiber.shield (fun () -> try log entry with _ -> ())
    in
    (* [failure e] is the status and the outcome of an exception [e] of the
       handler or of the body. *)
    let failure ~status e =
      if Cancel.is_cancelled () then (None, Cancelled)
      else if request_body_error request_failed e then
        match e with
        | H1_error.E err ->
            (Some (H1_error.status err), Failed e)
        | _ ->
            (* The server takes a request body that was cut short as a client
               that left, and sends nothing. *)
            (None, Cancelled)
      else (status, Failed e)
    in
    match next req with
    | resp -> (
        let status = Response.status resp in
        let route = try route resp with _ -> None in
        match Response.kind resp with
        | Response.Upgrade _ ->
            emit ~route ~status:(Some status) ~bytes:0 Upgraded ;
            resp
        | Response.Body b ->
            (* The server refuses a 2xx body to CONNECT, and sends 500. *)
            let refused = connect && Status.kind status = `Success in
            let length = Body.length b in
            let hook how ~bytes =
              let status, outcome =
                match how with
                | _ when refused ->
                    (Some Status.internal_server_error, Not_sent)
                | Body.Abandoned ->
                    ( Some status
                    , if has_no_body meth status then Complete else Not_sent )
                | Ended -> (
                  match length with
                  | Some n when bytes < n ->
                      (Some status, Incomplete)
                  | _ ->
                      (Some status, Complete) )
                | Failed Flow.Closed ->
                    (Some status, Cancelled)
                | Failed e ->
                    (* After the first byte, the head went with the status of
                       the handler. *)
                    let status', outcome = failure ~status:(Some status) e in
                    ((if bytes > 0 then Some status else status'), outcome)
              in
              emit ~route ~status ~bytes outcome
            in
            Response.with_body (Body.on_end hook b) resp )
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        let status, outcome =
          failure ~status:(Some Status.internal_server_error) e
        in
        emit ~route:None ~status ~bytes:0 outcome ;
        Printexc.raise_with_backtrace e bt

  (* [quote s] is [s], or [s] as an OCaml string if it has a byte outside 0x21
     to 0x7E, a '"' or a '\\', or if it is empty or "-". Thus a line is one
     line, and "-" is only a value that is not known. *)
  let quote s =
    if
      s <> "" && s <> "-"
      && String.for_all
           (fun c -> c > ' ' && c < '\x7f' && c <> '"' && c <> '\\')
           s
    then s
    else Printf.sprintf "%S" s

  let or_dash f = function Some x -> f x | None -> "-"

  let line e =
    let client = function
      | Forwarded.Ip a ->
          a
      | Unix ->
          "unix"
      | Unknown ->
          "unknown"
    in
    let ms = Int64.to_float (Weft.Span.to_ns e.duration) /. 1e6 in
    let outcome =
      match e.outcome with
      | Complete ->
          ""
      | Incomplete ->
          " incomplete"
      | Failed x ->
          " failed " ^ quote (Printexc.to_string x)
      | Cancelled ->
          " cancelled"
      | Not_sent ->
          " not-sent"
      | Upgraded ->
          " upgraded"
    in
    Printf.sprintf "%s %s \"%s %s %s\" %s %d %.3fms %s%s"
      (or_dash client e.client)
      (or_dash quote e.request_id)
      (Method.to_string e.meth) (quote e.target)
      (Version.to_string e.version)
      (or_dash (fun s -> string_of_int (Status.code s)) e.status)
      e.bytes ms (or_dash quote e.route) outcome
end

module Error_page = struct
  let plain _ _ =
    Response.string ~status:Status.internal_server_error
      "Internal Server Error\n"

  let middleware ~report ?(page = plain) () next req =
    let req, request_failed = watch_request req in
    match next req with
    | resp ->
        resp
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        if Cancel.is_cancelled () || request_body_error request_failed e then
          Printexc.raise_with_backtrace e bt
        else (report req e bt ; page req e)
end

module Cors = struct
  type origins = Any | Only of string list | Matching of (string -> bool)

  let lower = String.lowercase_ascii

  let permits origins =
    match origins with
    | Any ->
        fun _ -> true
    | Only l ->
        let l = List.map lower l in
        fun o ->
          let o = lower o in
          List.exists (String.equal o) l
    | Matching p ->
        p

  let is_digit c = c >= '0' && c <= '9'

  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let is_scheme_char c =
    is_letter c || is_digit c || c = '+' || c = '-' || c = '.'

  let is_host_char c = is_letter c || is_digit c || c = '-' || c = '_'

  (* [valid_port p] is [true] for 1 to 65535 with no leading zero. *)
  let valid_port p =
    p <> "" && String.for_all is_digit p
    && p.[0] <> '0'
    && match int_of_string_opt p with Some n -> n <= 65535 | None -> false

  (* [valid_host h] is [true] for a name of letters, digits, '.', '-' and '_',
     or an IPv6 literal in brackets in the form that a browser sends (RFC
     5952). *)
  (* [canonical g] is the text that a browser sends for the IPv6 address of the
     groups [g]: RFC 5952, with an IPv4 address mapped to IPv6 in hexadecimal
     (the WHATWG serializer). *)
  let canonical = function
    | [0; 0; 0; 0; 0; 0xffff; ab; cd] ->
        Printf.sprintf "::ffff:%x:%x" ab cd
    | g ->
        Forwarded.text_of_ipv6 g

  (* [valid_name h] is [true] for labels of letters, digits, '-' and '_',
     separated by '.', with an optional '.' at the end. A name whose last label
     is a number must be an IPv4 address in its normal form, as a browser sends
     it (WHATWG "ends in a number"). *)
  let valid_name h =
    let labels = String.split_on_char '.' h in
    let labels =
      match List.rev labels with
      | "" :: (_ :: _ as rest) ->
          List.rev rest
      | _ ->
          labels
    in
    List.for_all (fun l -> l <> "" && String.for_all is_host_char l) labels
    &&
    let last = List.nth labels (List.length labels - 1) in
    if
      String.for_all is_digit last
      || String.starts_with ~prefix:"0x" (lower last)
    then Option.is_some (Forwarded.ipv4 h)
    else true

  (* [valid_host h] is [true] for a name ([valid_name]), or an IPv6 literal in
     brackets as a browser sends it. *)
  let valid_host h =
    match Forwarded.of_value h with
    | Forwarded.Ip _ when String.starts_with ~prefix:"[" h ->
        let ip = String.sub h 1 (String.length h - 2) in
        Option.map canonical (Forwarded.ipv6 ip) = Some ip
    | _ ->
        valid_name h

  (* [valid_origin e] is [true] if [e] is ["null"], or the form of an [Origin]
     field: [scheme "://" host [":" port]], with no default port. A browser
     never sends an [Origin] with a ws or wss scheme. *)
  let valid_origin e =
    e = "null"
    ||
    match String.index_opt e ':' with
    | None ->
        false
    | Some i -> (
        let scheme = lower (String.sub e 0 i) in
        let rest = String.sub e (i + 1) (String.length e - i - 1) in
        scheme <> ""
        && is_letter scheme.[0]
        && String.for_all is_scheme_char scheme
        && scheme <> "ws" && scheme <> "wss"
        && String.starts_with ~prefix:"//" rest
        &&
        let authority = String.sub rest 2 (String.length rest - 2) in
        (* The port follows the last ':' after the host (or after ']'). *)
        let host_end =
          match String.rindex_opt authority ':' with
          | Some j when not (String.contains_from authority j ']') ->
              j
          | _ ->
              String.length authority
        in
        let host = String.sub authority 0 host_end in
        let port =
          if host_end = String.length authority then None
          else
            Some
              (String.sub authority (host_end + 1)
                 (String.length authority - host_end - 1) )
        in
        valid_host host
        &&
        match port with
        | None ->
            true
        | Some p ->
            valid_port p
            && not
                 ( (scheme = "http" && p = "80")
                 || (scheme = "https" && p = "443") ) )

  let safelisted = Method.[GET; HEAD; POST]

  (* [with_vary names h] is [h] with each of [names] in its [Vary] field, once,
     compared case-insensitively. A [Vary: *] stays. *)
  let with_vary names h =
    let have = Headers.tokens "vary" h in
    if List.mem "*" have then h
    else
      let lowered = List.map lower have in
      let missing =
        List.filter (fun n -> not (List.mem (lower n) lowered)) names
      in
      if missing = [] then h
      else Headers.set "Vary" (String.concat ", " (have @ missing)) h

  let cors_prefix = "access-control-"

  (* [has_cors_prefix n i] is [true] if [n] has [cors_prefix] from [i] on,
     case-insensitively. *)
  let rec has_cors_prefix n i =
    i = String.length cors_prefix
    || Char.lowercase_ascii n.[i] = cors_prefix.[i]
       && has_cors_prefix n (i + 1)

  (* [is_cors_field n] compares the prefix case-insensitively, with no
     allocation. *)
  let is_cors_field n =
    String.length n >= String.length cors_prefix && has_cors_prefix n 0

  let invalid what = invalid_arg ("Caravan_middleware.Cors.middleware: " ^ what)

  let preflight_vary =
    "Origin, Access-Control-Request-Method, Access-Control-Request-Headers"

  let middleware ~origins ?(allow_methods = []) ?(allow_headers = [])
      ?(expose_headers = []) ?(credentials = false) ?max_age
      ?(preflight_status = Status.no_content) () =
    ( match origins with
    | Any when credentials ->
        invalid "Any with credentials: any site could read the responses"
    | Only l -> (
      match List.find_opt (fun e -> not (valid_origin e)) l with
      | Some "*" ->
          invalid "\"*\" is not an origin: use Any"
      | Some e when String.contains e '*' ->
          invalid
            (Printf.sprintf "%S is not an origin: use Matching for a pattern" e)
      | Some e ->
          invalid (Printf.sprintf "%S is not an origin" e)
      | None ->
          () )
    | Any | Matching _ ->
        () ) ;
    List.iter
      (fun m ->
        let name = Method.to_string m in
        if not (Headers.is_valid_name name) then
          invalid (Printf.sprintf "%S is not a method" name) ;
        (* The methods that a browser forbids, also in lower case, and a [*]
           that a browser takes as a literal name with credentials. *)
        match String.uppercase_ascii name with
        | "CONNECT" | "TRACE" | "TRACK" | "*" ->
            invalid (name ^ " in allow_methods")
        | _ ->
            () )
      allow_methods ;
    let check_names l =
      List.iter
        (fun n ->
          if not (n = "*" || Headers.is_valid_name n) then
            invalid (Printf.sprintf "%S is not a field name" n) )
        l
    in
    check_names allow_headers ;
    check_names expose_headers ;
    if credentials && List.mem "*" expose_headers then
      invalid "\"*\" in expose_headers with credentials" ;
    ( match max_age with
    | Some n when n < 0 ->
        invalid "a negative max_age"
    | _ ->
        () ) ;
    if Status.kind preflight_status <> `Success then
      invalid "a preflight_status that is not 2xx" ;
    let permitted = permits origins in
    let any_header = List.mem "*" allow_headers in
    let allowed_headers = List.map lower allow_headers in
    let preflight_methods = safelisted @ allow_methods in
    (* For [Any] (thus with no credentials), the value is the constant [*]: each
       actual response gets it, and the response does not vary with the
       origin. *)
    let star =
      match origins with Any -> true | Only _ | Matching _ -> false
    in
    let allow_origin o = if star then "*" else o in
    let expose = String.concat ", " expose_headers in
    (* The fields of a preflight that do not change. *)
    let preflight_fields =
      Headers.of_list
        ( ( if allow_methods = [] then []
            else
              [ ( "Access-Control-Allow-Methods"
                , String.concat ", " (List.map Method.to_string allow_methods)
                ) ] )
        @ ( if credentials then [("Access-Control-Allow-Credentials", "true")]
            else [] )
        @ ( match max_age with
          | Some n ->
              [("Access-Control-Max-Age", string_of_int n)]
          | None ->
              [] )
        @ [("Vary", preflight_vary)] )
    in
    let refused_preflight = Headers.of_list [("Vary", preflight_vary)] in
    let preflight req o =
      let h = Request.headers req in
      let requested = Headers.tokens "access-control-request-headers" h in
      let method_ok =
        match Headers.find "access-control-request-method" h with
        | Some m when Headers.is_valid_name m ->
            List.exists (Method.equal (Method.of_string m)) preflight_methods
        | _ ->
            false
      in
      let headers_ok =
        List.for_all
          (fun n -> any_header || List.mem (lower n) allowed_headers)
          requested
      in
      let headers =
        if permitted o && method_ok && headers_ok then
          let h =
            Headers.set "Access-Control-Allow-Origin" (allow_origin o)
              preflight_fields
          in
          match List.filter Headers.is_valid_name requested with
          | [] ->
              h
          | names ->
              Headers.set "Access-Control-Allow-Headers"
                (String.concat ", " names) h
        else refused_preflight
      in
      Response.empty ~status:preflight_status ~headers ()
    in
    let allow o h =
      let h = Headers.set "Access-Control-Allow-Origin" (allow_origin o) h in
      let h =
        if credentials then
          Headers.set "Access-Control-Allow-Credentials" "true" h
        else h
      in
      if expose_headers = [] then h
      else Headers.set "Access-Control-Expose-Headers" expose h
    in
    let actual origin resp =
      let h = Response.headers resp in
      let h =
        match origin with
        | None ->
            (* A cache can give this response to a later CORS fetch: with the
               constant [*], it has the fields. *)
            if star then allow "*" h else h
        | Some o when permitted o ->
            allow o h
        | Some _ ->
            (* A field of an upstream must not go to this origin. The fold reads
               the fields of the response and removes each CORS field from the
               result. *)
            Headers.fold
              (fun n _ h -> if is_cors_field n then Headers.remove n h else h)
              h h
      in
      let h = if star then h else with_vary ["Origin"] h in
      Response.with_headers h resp
    in
    fun next req ->
      let h = Request.headers req in
      match Headers.find "origin" h with
      | Some o
        when Method.equal (Request.meth req) Method.OPTIONS
             && Headers.mem "access-control-request-method" h ->
          preflight req o
      | origin ->
          actual origin (next req)
end

(* [host_of a] is the host of the authority [a], with no port. An IPv6 literal
   keeps its brackets. [a.example:], with no digit, gives [a.example]. *)
let host_of a =
  if String.starts_with ~prefix:"[" a then
    match String.index_opt a ']' with
    | Some i ->
        String.sub a 0 (i + 1)
    | None ->
        a
  else
    match String.rindex_opt a ':' with Some i -> String.sub a 0 i | None -> a

module Https_redirect = struct
  let middleware ?(status = Status.temporary_redirect) ?port () =
    ( match Status.code status with
    | 301 | 302 | 307 | 308 ->
        ()
    | _ ->
        invalid_arg
          "Caravan_middleware.Https_redirect.middleware: the status is not \
           301, 302, 307 or 308" ) ;
    let port =
      match port with
      | None | Some 443 ->
          ""
      | Some p when p >= 1 && p <= 65535 ->
          ":" ^ string_of_int p
      | Some _ ->
          invalid_arg
            "Caravan_middleware.Https_redirect.middleware: the port is not in \
             1 to 65535"
    in
    fun next req ->
      if Request.scheme req = "https" then next req
      else
        let path = Request.path req in
        match Request.authority req with
        (* [is_valid_authority] refuses an empty host. *)
        | Some a
          when Request.is_valid_authority a
               && String.starts_with ~prefix:"/" path ->
            Response.empty ~status
              ~headers:
                (Headers.of_list
                   [("Location", "https://" ^ host_of a ^ port ^ path)] )
              ()
        | _ ->
            Response.string ~status:Status.bad_request "Bad Request\n"
end

module Hsts = struct
  (* [normal h] is the host [h] in lower case, with no dot at its end. *)
  let normal h =
    let h = String.lowercase_ascii h in
    if String.ends_with ~suffix:"." h then String.sub h 0 (String.length h - 1)
    else h

  (* A host that ends with ".localhost" is a loopback host (RFC 6761). *)
  let excluded hosts host =
    let host = normal host in
    List.exists
      (fun e ->
        host = e
        || (e = "localhost" && String.ends_with ~suffix:".localhost" host) )
      hosts

  let middleware ?(max_age = 31536000) ?(include_subdomains = false)
      ?(preload = false) ?(exclude_hosts = ["localhost"; "127.0.0.1"; "[::1]"])
      () =
    if max_age < 0 then
      invalid_arg "Caravan_middleware.Hsts.middleware: a negative max_age" ;
    if preload && ((not include_subdomains) || max_age < 31536000) then
      invalid_arg
        "Caravan_middleware.Hsts.middleware: preload needs include_subdomains \
         and a max_age of 31536000 or more" ;
    let value =
      "max-age=" ^ string_of_int max_age
      ^ (if include_subdomains then "; includeSubDomains" else "")
      ^ if preload then "; preload" else ""
    in
    List.iter
      (fun e ->
        if host_of e <> e then
          invalid_arg
            (Printf.sprintf
               "Caravan_middleware.Hsts.middleware: the excluded host %S has a \
                port"
               e ) )
      exclude_hosts ;
    let hosts = List.map normal exclude_hosts in
    fun next req ->
      let r = next req in
      let skip =
        Request.scheme req <> "https"
        || ( match Request.authority req with
          | Some a ->
              excluded hosts (host_of a)
          | None ->
              false )
        || Headers.mem "strict-transport-security" (Response.headers r)
      in
      if skip then r
      else
        Response.with_headers
          (Headers.add "Strict-Transport-Security" value (Response.headers r))
          r
end

module Cross_origin = struct
  let forbidden _ = Response.string ~status:Status.forbidden "Forbidden\n"

  (* [same_host origin authority] is [true] if [origin] is [scheme "://" a],
     with a scheme that is not empty, where [a] is [authority] with no case. *)
  let same_host origin = function
    | Some a when a <> "" -> (
      match String.index_opt origin ':' with
      | Some i when i > 0 ->
          let rest = String.length origin - i - 3 in
          rest = String.length a
          && String.sub origin i 3 = "://"
          && String.lowercase_ascii (String.sub origin (i + 3) rest)
             = String.lowercase_ascii a
      | _ ->
          false )
    | _ ->
        false

  let middleware ?(trusted_origins = []) ?(exempt = fun _ -> false)
      ?(deny = forbidden) () =
    List.iter
      (fun e ->
        if e = "null" || not (Cors.valid_origin e) then
          invalid_arg
            (Printf.sprintf
               "Caravan_middleware.Cross_origin.middleware: %S is not an origin"
               e ) )
      trusted_origins ;
    let trusted = Cors.permits (Cors.Only trusted_origins) in
    (* Examined only on a refusal, as Go does. *)
    let excepted req origin =
      exempt req || match origin with Some o -> trusted o | None -> false
    in
    let pass req =
      match Request.meth req with
      | Method.GET | Method.HEAD | Method.OPTIONS ->
          true
      | _ -> (
          let h = Request.headers req in
          match Headers.find "sec-fetch-site" h with
          | Some ("same-origin" | "none") ->
              true
          | Some _ ->
              excepted req (Headers.find "origin" h)
          | None -> (
            match Headers.find "origin" h with
            | None ->
                true
            | Some o as origin ->
                same_host o (Request.authority req) || excepted req origin ) )
    in
    fun next req -> if pass req then next req else deny req
end
