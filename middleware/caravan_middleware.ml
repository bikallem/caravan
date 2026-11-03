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
               p <> ""
               && String.length p <= 3
               && String.for_all is_digit p
               && (p = "0" || p.[0] <> '0')
               && int_of_string p <= 255 )
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
        let best = ref (-1, 0) and run = ref (-1, 0) in
        Array.iteri
          (fun i x ->
            if x = 0 then (
              let start, len = !run in
              run := if len = 0 then (i, 1) else (start, len + 1) ;
              if snd !run > snd !best then best := !run )
            else run := (-1, 0) )
          a ;
        let start, len = !best in
        let hex i = Printf.sprintf "%x" a.(i) in
        let part lo hi =
          String.concat ":" (List.init (hi - lo) (fun k -> hex (lo + k)))
        in
        if len < 2 then part 0 8 else part 0 start ^ "::" ^ part (start + len) 8

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
    if n > 0 && v.[0] = '[' then
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
