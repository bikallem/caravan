open Caravan
open Weft
module Smap = Map.Make (String)
module Base64 = Caravan.Private.Base64
module Date = Caravan.Private.Date

let b64_encode = Base64.encode ~url:true ~pad:false

let b64_decode = Base64.decode ~url:true ~pad:false

(* {1 Keys and signed values} *)

module Key = struct
  type t = string

  let of_string s =
    if String.length s < 32 then
      invalid_arg "Caravan_session.Key.of_string: a key has 32 bytes or more" ;
    s

  let generate () = Mirage_crypto_rng.generate 32

  let to_base64url = b64_encode

  let of_base64url s =
    match b64_decode s with
    | Some k ->
        of_string k
    | None ->
        invalid_arg "Caravan_session.Key.of_base64url: not base64url"
end

module Signed = struct
  (* The text of a MAC: 32 bytes in base64url with no padding. *)
  let mac_length = 43

  let check fn purpose =
    if String.contains purpose '\000' then
      invalid_arg ("Caravan_session.Signed." ^ fn ^ ": a NUL byte in purpose") ;
    if String.starts_with ~prefix:"caravan." purpose then
      invalid_arg
        ( "Caravan_session.Signed." ^ fn
        ^ ": the purposes caravan.* are reserved" )

  (* [raw_mac ~purpose key value] is the 32 bytes of the HMAC. A purpose has no
     NUL, thus the input of the HMAC gives the purpose and the value back. *)
  let raw_mac ~purpose key value =
    Digestif.SHA256.(to_raw_string (hmac_string ~key (purpose ^ "\000" ^ value)))

  let mac ~purpose key value = b64_encode (raw_mac ~purpose key value)

  (* [sign_reserved] is [sign] with no check of the purpose, for the purposes of
     this library. *)
  let sign_reserved ~purpose key value = value ^ "." ^ mac ~purpose key value

  let sign ~purpose key value =
    check "sign" purpose ;
    sign_reserved ~purpose key value

  (* [verify_first ~purpose keys s] is the value of [s], and [true] if the first
     key signed it. A MAC text of another length costs no HMAC. The compare of
     the texts is constant time, and only the canonical text of the MAC is
     accepted. *)
  let verify_first ~purpose keys s =
    match String.rindex_opt s '.' with
    | Some i when String.length s - i - 1 = mac_length ->
        let value = String.sub s 0 i in
        let m = String.sub s (i + 1) mac_length in
        let rec find first = function
          | [] ->
              None
          | key :: rest ->
              if Eqaf.equal (mac ~purpose key value) m then Some (value, first)
              else find false rest
        in
        find true keys
    | _ ->
        None

  let verify ~purpose keys s =
    check "verify" purpose ;
    Option.map fst (verify_first ~purpose keys s)
end

(* {1 Stores} *)

type store =
  { find: string -> (string * Clock.posix) option
  ; create: string -> string -> expires:Clock.posix -> unit
  ; update: string -> string -> expires:Clock.posix -> unit
  ; touch: string -> expires:Clock.posix -> unit
  ; remove: string -> bool }

let store ~find ~create ~update ~touch ~remove =
  {find; create; update; touch; remove}

(* [before a b] is [true] if the time [a] is before [b]. *)
let before a b = Span.compare (Time.diff Posix a b) Span.zero < 0

let min_posix a b = if before a b then a else b

let with_lock m f =
  Fiber.Mutex.lock m ;
  Fun.protect ~finally:(fun () -> Fiber.Mutex.unlock m) f

(* An entry of the memory store: the value, the expiry, and the number of its
   write. *)
type entry = {value: string; expires: Clock.posix; write: int}

let memory_store
    ?(max_sessions =
      (100_000 [@mutate off "a default: a test of it needs 10^5 sessions"]))
    ?(max_bytes =
      (256 * 1024 * 1024) [@mutate off "a default: a test of it needs 256 MiB"])
    () =
  if max_sessions < 1 then
    invalid_arg "Caravan_session.memory_store: max_sessions < 1" ;
  if max_bytes < 1 then
    invalid_arg "Caravan_session.memory_store: max_bytes < 1" ;
  let table : (string, entry) Hashtbl.t = Hashtbl.create 64 in
  (* The writes, in order: a key and the number of its write. Only the last
     write of a key is current: thus each key has at most one current entry. *)
  let writes : (string * int) Queue.t ref = ref (Queue.create ()) in
  let count =
    ref (0 [@mutate off "the first write number: only the order matters"])
  and bytes = ref 0 in
  let lock = Fiber.Mutex.create () in
  let size k e = String.length k + String.length e.value in
  let current (k, n) =
    match Hashtbl.find_opt table k with
    | Some e ->
        e.write = n
    | None ->
        false [@mutate off "a removed key: an eviction of it removes nothing"]
  in
  let drop k =
    match Hashtbl.find_opt table k with
    | Some e ->
        bytes := !bytes - size k e ;
        Hashtbl.remove table k ;
        true
    | None ->
        false
  in
  let put k value expires =
    ignore (drop k) ;
    incr count ;
    let e = {value; expires; write= !count} in
    Hashtbl.replace table k e ;
    bytes := !bytes + size k e ;
    Queue.push (k, !count) !writes ;
    (* Rebuild the queue when the stale entries are more than the table. *)
    if
      ( Queue.length !writes
      > (2 * Hashtbl.length table) + 16 )
      [@mutate off "the bound of a rebuild changes only the cost"]
    then (
      let fresh = Queue.create () in
      Queue.iter (fun w -> if current w then Queue.push w fresh) !writes ;
      writes := fresh )
  in
  (* [evict ~room] pops the head of the queue while it is stale or expired, or
     while a new entry of [room] bytes does not fit: the session with the oldest
     write goes first. *)
  let rec evict ~room =
    match Queue.peek_opt !writes with
    | None ->
        ()
    | Some ((k, _) as w) ->
        let live =
          current w && before (Time.now Posix) (Hashtbl.find table k).expires
        in
        if
          (not live)
          || Hashtbl.length table + 1 > max_sessions
          || !bytes + room > max_bytes
        then (
          ignore (Queue.pop !writes) ;
          if current w then ignore (drop k) ;
          evict ~room )
  in
  let find k =
    with_lock lock (fun () ->
        match Hashtbl.find_opt table k with
        | Some e when before (Time.now Posix) e.expires ->
            Some (e.value, e.expires)
        | Some _ ->
            ignore (drop k) ;
            None
        | None ->
            None )
  in
  (* An entry larger than [max_bytes] is not kept: it does not remove the other
     sessions. *)
  let fits k v = String.length k + String.length v <= max_bytes in
  let create k v ~expires =
    with_lock lock (fun () ->
        if fits k v then (
          evict ~room:(String.length k + String.length v) ;
          put k v expires ) )
  in
  let update k v ~expires =
    with_lock lock (fun () ->
        if Hashtbl.mem table k then (
          ignore (drop k) ;
          if fits k v then (
            evict ~room:(String.length k + String.length v) ;
            put k v expires ) ) )
  in
  let touch k ~expires =
    with_lock lock (fun () ->
        match Hashtbl.find_opt table k with
        | Some e ->
            put k e.value expires
        | None ->
            () )
  in
  let remove k = with_lock lock (fun () -> drop k) in
  {find; create; update; touch; remove}

(* {1 The data of a session} *)

let max_data = 65536

(* [digits s i] is the decimal number at [i] in [s], with only digits, and the
   index after it. [None] for no digit or an overflow. *)
let digits s i =
  let n = String.length s in
  let rec go j acc =
    if j < n && s.[j] >= '0' && s.[j] <= '9' then
      let d = Char.code s.[j] - Char.code '0' in
      if
        ( acc
        > (max_int - d) / 10 )
        [@mutate
          off "at the bound, the number is too large for a length or a time"]
      then None
      else go (j + 1) ((acc * 10) + d)
    else if j = i then None
    else Some (acc, j)
  in
  go i 0

(* The text of the data: a version byte "1", the creation time in ns (0 for a
   time before 1970), "\n", then each name and value as "<length>:<bytes>". *)
let encode created data =
  let b = Buffer.create 256 in
  Buffer.add_char b '1' ;
  let ns = Span.to_ns (Time.diff Posix created Date.epoch) in
  Buffer.add_string b
    (Int64.to_string
       (Int64.max (0L [@mutate off "a clamp for a time before 1970"]) ns) ) ;
  Buffer.add_char b '\n' ;
  let add s =
    Buffer.add_string b (string_of_int (String.length s)) ;
    Buffer.add_char b ':' ;
    Buffer.add_string b s
  in
  Smap.iter (fun k v -> add k ; add v) data ;
  Buffer.contents b

(* [decode s] is the creation time, the data and its size, or [None]. *)
let decode s =
  let n = String.length s in
  let field i =
    match digits s i with
    | Some (len, j) when j < n && s.[j] = ':' && len <= n - j - 1 ->
        Some (String.sub s (j + 1) len, j + 1 + len)
    | _ ->
        None
  in
  let rec pairs i data size =
    if i = n then Some (data, size)
    else
      match field i with
      | None ->
          None
      | Some (k, i) -> (
        match field i with
        | None ->
            None
        | Some (v, i) ->
            pairs i (Smap.add k v data)
              (size + String.length k + String.length v) )
  in
  if n = 0 || s.[0] <> '1' then None
  else
    match digits s 1 with
    | Some (ns, j) when j < n && s.[j] = '\n' -> (
      match Time.add Posix Date.epoch (Span.of_ns (Int64.of_int ns)) with
      | Some created ->
          Option.map
            (fun (d, size) -> (created, d, size))
            (pairs (j + 1) Smap.empty 0)
      | None ->
          None )
    | _ ->
        None

(* {1 The state of a session} *)

(* Where the ID of a session comes from. *)
type origin =
  | Fresh  (** New in this request: the store must create it. *)
  | Stored  (** From the store. *)
  | Stored_old_key  (** From the store, with a cookie of an old key. *)

type state =
  { id: (string * origin) option  (** The session, if any. *)
  ; data: string Smap.t
  ; size: int
  ; created: Clock.posix
  ; changed: bool  (** [set] or [remove] changed the data. *)
  ; old: string list  (** The stored IDs that the request ended. *)
  ; parent: string option
        (** The stored ID that a [renew] ended: the new session exists only if
            this ID still existed in the store. *)
  ; cleared: bool  (** [clear] ended the session. *)
  ; no_store: bool  (** [Csrf.token] gave a token: the response is private. *)
  ; frozen: bool  (** The response is out: no more change. *) }

(* The session of a request, and the keys of the middleware. *)
(* [loaded] is the data of the session at the start of the request. *)
type session = {state: state Atomic.t; keys: Key.t list; loaded: string Smap.t}

let attr : session Attrs.key = Attrs.key ~name:"session" ()

let session_of fn req =
  match Request.find_attr attr req with
  | Some s ->
      s
  | None ->
      invalid_arg
        ( "Caravan_session." ^ fn
        ^ ": the request did not go through the middleware" )

let state_of fn req = (session_of fn req).state

(* [modify fn s f] changes the state with [f] in a CAS loop: a handler can fork
   fibers on other domains. [f] gives [None] for no change. [modify] gives the
   state after the change, or the state that it read if [f] gives [None]. *)
let rec modify fn s f =
  let st = Atomic.get s in
  match f st with
  | None ->
      st
  | Some st' ->
      if st.frozen then
        invalid_arg
          ("Caravan_session." ^ fn ^ ": the response of the request is out") ;
      if Atomic.compare_and_set s st st' then st' else modify fn s f

let change fn req f = ignore (modify fn (state_of fn req) f)

let new_id () = b64_encode (Mirage_crypto_rng.generate 32)

(* [fresh st] is [st] with a session: a new ID if it has none. *)
let fresh st =
  match st.id with
  | Some _ ->
      st
  | None ->
      {st with id= Some (new_id (), Fresh); created= Time.now Posix}

(* [retire st] is the list of the IDs to end, with the stored ID of [st]. *)
let retire st =
  match st.id with
  | Some (id, (Stored | Stored_old_key)) ->
      id :: st.old
  | _ ->
      st.old

let find_any name req =
  Smap.find_opt name (Atomic.get (state_of "find" req)).data

let set_any name value req =
  change "set" req (fun st ->
      let st = fresh st in
      let old =
        match Smap.find_opt name st.data with
        | Some v ->
            String.length name + String.length v
        | None ->
            0
      in
      let size = st.size - old + String.length name + String.length value in
      if size > max_data then
        invalid_arg "Caravan_session.set: the data has more than 64 KiB" ;
      Some {st with data= Smap.add name value st.data; size; changed= true} )

(* The names [caravan.*] are for the libraries of caravan: a value of the
   client, for example a form field, can never set them. *)
let check_name fn name =
  if String.starts_with ~prefix:"caravan." name then
    invalid_arg ("Caravan_session." ^ fn ^ ": the names caravan.* are reserved")

let find name req = check_name "find" name ; find_any name req

let set name value req = check_name "set" name ; set_any name value req

let remove name req =
  check_name "remove" name ;
  change "remove" req (fun st ->
      match Smap.find_opt name st.data with
      | Some v ->
          Some
            { st with
              data= Smap.remove name st.data
            ; size= st.size - String.length name - String.length v
            ; changed= true }
      | None ->
          None )

let renew req =
  change "renew" req (fun st ->
      match st.id with
      | Some _ ->
          let parent =
            match st.id with
            | Some (id, (Stored | Stored_old_key)) ->
                Some id
            | _ ->
                st.parent
          in
          Some {st with id= Some (new_id (), Fresh); old= retire st; parent}
      | None ->
          None )

let clear req =
  change "clear" req (fun st ->
      Some
        { st with
          id= None
        ; data= Smap.empty
        ; size= 0
        ; old= retire st
        ; parent= None
        ; cleared= true } )

let id req =
  let st =
    modify "id" (state_of "id" req) (fun st ->
        match st.id with Some _ -> None | None -> Some (fresh st) )
  in
  (* [fresh] gives an ID. *)
  fst (Option.get st.id)

(* {1 The middleware} *)

let purpose = "caravan.session"

let store_key id = Digestif.SHA256.(to_hex (digest_string id))

(* A cookie value: an ID and its MAC, each of 43 characters. *)
let cookie_length = 43 + 1 + Signed.mac_length

let max_span = Span.s (10 * 365 * 86400)

let middleware ?(cookie = "__Host-session") ?(idle = Span.s 1800)
    ?(absolute = Span.s (8 * 3600)) ?(secure = true) ?(same_site = `Lax) ~keys
    ~store () =
  let bad m = invalid_arg ("Caravan_session.middleware: " ^ m) in
  if keys = [] then bad "no key" ;
  let in_range s =
    Span.compare s Span.zero > 0 && Span.compare s max_span <= 0
  in
  if not (in_range idle && in_range absolute) then
    bad "idle and absolute must be more than 0 and at most 10 years" ;
  ( match Mirage_crypto_rng.default_generator () with
  | _ ->
      ()
  | exception Mirage_crypto_rng.No_default_generator ->
      bad
        "no default generator: call Mirage_crypto_rng_unix.use_default () first"
  ) ;
  let template =
    match Cookie.make ~secure ~same_site cookie "x" with
    | c ->
        c
    | exception Invalid_argument m ->
        bad m
  in
  (* A span is at most 10 years, and a time of the store is a creation time of a
     session: no overflow before 2252. *)
  let add t span =
    match Time.add Posix t span with
    | Some t ->
        t
    | None ->
        t [@mutate off "no overflow for a span of at most 10 years"]
  in
  let half = Span.of_ns (Int64.div (Span.to_ns idle) 2L) in
  let blank now =
    { id= None
    ; data= Smap.empty
    ; size= 0
    ; created= now
    ; changed= false
    ; old= []
    ; parent= None
    ; cleared= false
    ; no_store= false
    ; frozen= false }
  in
  (* [load now value] is the state of the cookie [value], and the expiry of the
     session in the store. A cookie of another length costs no HMAC, and a bad
     signature costs no read of the store. *)
  let load now value =
    let none = (blank now, None) in
    match value with
    | Some v when String.length v = cookie_length -> (
      match Signed.verify_first ~purpose keys v with
      | None ->
          none
      | Some (id, first) -> (
        match store.find (store_key id) with
        | Some (text, expires) when before now expires -> (
          match decode text with
          | Some (created, data, size) when before now (add created absolute) ->
              let origin = if first then Stored else Stored_old_key in
              ( {(blank now) with id= Some (id, origin); data; size; created}
              , Some expires )
          | _ ->
              none )
        | _ ->
            none ) )
    | _ ->
        none
  in
  (* [save now st loaded] writes the state to the store, and gives [false] if a
     renew lost its session: another request ended the old ID first. *)
  let save now st loaded =
    (* Each ended ID goes. A renewed session lives only if its parent was still
       in the store: else another request ended it first. *)
    let alive =
      List.fold_left
        (fun alive id ->
          let found = store.remove (store_key id) in
          if st.parent = Some id then found && alive else alive )
        true st.old
    in
    match st.id with
    | None ->
        true [@mutate off "with no ID, the middleware does not read it"]
    | Some (id, origin) -> (
        let key = store_key id in
        let expires = min_posix (add now idle) (add st.created absolute) in
        match (origin, loaded) with
        | Fresh, _ ->
            if alive then store.create key (encode st.created st.data) ~expires ;
            alive
        | _, _ when st.changed ->
            store.update key (encode st.created st.data) ~expires ;
            true
        | _, Some e when before e (add now half) && before e expires ->
            (* More than half of idle passed, and the limit moves later. *)
            store.touch key ~expires ; true
        | _ ->
            true )
  in
  let no_store r =
    Response.with_headers
      (Headers.set "Cache-Control" "no-store" (Response.headers r))
      r
  in
  let with_cookie c r = no_store (Response.add_cookie c r) in
  fun next req ->
    let now = Time.now Posix in
    let value = Request.cookie cookie req in
    let st, loaded = load now value in
    let s = Atomic.make st in
    let response =
      next (Request.with_attr attr {state= s; keys; loaded= st.data} req)
    in
    (* After the response, no change: a stream producer, an upgrade or a
       background fiber gets Invalid_argument. *)
    let rec freeze () =
      let st = Atomic.get s in
      if Atomic.compare_and_set s st {st with frozen= true} then st
      else freeze ()
    in
    let st = freeze () in
    let kept = save now st loaded in
    let expire () =
      if Option.is_some value then
        with_cookie (Cookie.expired template) response
      else response
    in
    let response =
      match st.id with
      | Some (id, (Fresh | Stored_old_key)) when kept ->
          with_cookie
            (Cookie.with_value
               (Signed.sign_reserved ~purpose (List.hd keys) id)
               template )
            response
      | Some _ when kept ->
          response
      | Some _ ->
          (* A renew after the end of the session: as a clear. *)
          expire ()
      | None ->
          if st.cleared then expire () else response
    in
    (* A page with a token of the session must not go to another user. *)
    if st.no_store then no_store response else response

(* {1 CSRF tokens} *)

module Csrf = struct
  let purpose = "caravan.csrf"

  (* A token is a pad of 32 bytes and the secret xor the pad, in base64url. *)
  let length = 86

  let xor a b =
    String.init (String.length a) (fun i ->
        Char.chr (Char.code a.[i] lxor Char.code b.[i]) )

  (* [secret key id] is the secret of the session [id]. *)
  let secret key id = Signed.raw_mac ~purpose key id

  let token req =
    let {state; keys; _} = session_of "Csrf.token" req in
    (* A change each time: after the response, [modify] raises. *)
    let st =
      modify "Csrf.token" state (fun st ->
          Some {(fresh st) with no_store= true} )
    in
    (* [fresh] gives an ID. *)
    let id = fst (Option.get st.id) in
    let pad = Mirage_crypto_rng.generate 32 in
    b64_encode (pad ^ xor pad (secret (List.hd keys) id))

  let check t req =
    let {state; keys; _} = session_of "Csrf.check" req in
    match (Atomic.get state).id with
    | Some (id, _) when String.length t = length -> (
      (* A canonical text of 86 characters is 64 bytes. *)
      match b64_decode t with
      | Some raw ->
          let s = xor (String.sub raw 0 32) (String.sub raw 32 32) in
          List.exists (fun key -> Eqaf.equal (secret key id) s) keys
      | None ->
          false )
    | _ ->
        false

  let forbidden _ = Response.string ~status:Status.forbidden "Forbidden\n"

  let middleware ?(header = "X-CSRF-Token") ?(field = "_csrf")
      ?(max_bytes = 1_048_576) ?(exempt = fun _ -> false) ?(deny = forbidden) ()
      =
    let bad m = invalid_arg ("Caravan_session.Csrf.middleware: " ^ m) in
    if not (Caravan.Private.Token.is_token header) then
      bad "the header is not a field name" ;
    if field = "" then bad "the field is empty" ;
    if max_bytes < 0 then bad "a negative max_bytes" ;
    fun next req ->
      (* For each method: a wrong order of the middleware shows on a GET. *)
      let {state; _} = session_of "Csrf.middleware" req in
      match Request.meth req with
      | Method.GET | Method.HEAD | Method.OPTIONS ->
          next req
      | _ when exempt req ->
          next req
      | _ when Option.is_none (Atomic.get state).id ->
          (* No session, thus no token: no read of the body. *)
          deny req
      | _ -> (
          let h = Request.headers req in
          match Headers.find header h with
          | Some t ->
              if check t req then next req else deny req
          | None -> (
            match
              Caravan.Private.Form_body.text ~max_bytes h (Request.body req)
            with
            | Error Form.Unsupported_media_type ->
                deny req
            | Error e ->
                let status = Form.error_status e in
                Response.string ~status (Status.reason status ^ "\n")
            | Ok text ->
                (* The next handler, or [deny], gets the same bytes, read. *)
                let h =
                  h
                  |> Headers.remove "Transfer-Encoding"
                  |> Headers.remove "Expect"
                  |> Headers.set "Content-Length"
                       (string_of_int (String.length text))
                in
                let req =
                  Request.with_headers h
                    (Request.with_body (Body.of_string text) req)
                in
                let good =
                  match Form.find_text field text with
                  | Some t ->
                      check t req
                  | None ->
                      false
                in
                if good then next req else deny req ) )
end

module Private = struct
  let find = find_any

  let find_loaded name req = Smap.find_opt name (session_of "find" req).loaded

  let set = set_any
end
