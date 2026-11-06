(* The tests of caravan.session, in the simulator: the virtual wall clock moves
   the idle and absolute limits. Each test sets a new Fortuna generator with a
   fixed seed first. The simulator runs on one OS thread, thus Fortuna is
   correct here. *)

open Windtrap
open Caravan
open Weft
module M = Caravan_session

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"caravan session tests"
       (module Mirage_crypto_rng.Fortuna) )

let key1 = M.Key.of_string (String.make 32 'a')

let key2 = M.Key.of_string (String.make 32 'b')

let is_invalid = function Invalid_argument _ -> true | _ -> false

let made (_ : Handler.middleware) = ()

(* The time of the tests: 2025-06-15. *)
let t0 = 1_750_000_000.

(* [sim f] runs [f] in the simulator at [t0], with a seeded generator. *)
let sim ?domains f =
  seed () ;
  Sim_support.run ?domains (fun s ->
      Weft_sim.set_posix s (Time.posix_of_unix_s t0) ;
      f s )

let at s seconds = Weft_sim.set_posix s (Time.posix_of_unix_s (t0 +. seconds))

(* {1 A store that counts its calls} *)

type counts =
  { mutable finds: int
  ; mutable creates: int
  ; mutable updates: int
  ; mutable touches: int
  ; mutable removes: int }

(* [counting ?honest ()] is a store in a table, and its counts. With
   [~honest:false], [find] gives a key after its expiry too. *)
let counting ?(honest = true) () =
  let table = Hashtbl.create 8 in
  let c = {finds= 0; creates= 0; updates= 0; touches= 0; removes= 0} in
  let before a b = Span.compare (Time.diff Posix a b) Span.zero < 0 in
  let store =
    M.store
      ~find:(fun k ->
        c.finds <- c.finds + 1 ;
        match Hashtbl.find_opt table k with
        | Some (v, e) when (not honest) || before (Time.now Posix) e ->
            Some (v, e)
        | _ ->
            None )
      ~create:(fun k v ~expires ->
        c.creates <- c.creates + 1 ;
        Hashtbl.replace table k (v, expires) )
      ~update:(fun k v ~expires ->
        c.updates <- c.updates + 1 ;
        if Hashtbl.mem table k then Hashtbl.replace table k (v, expires) )
      ~touch:(fun k ~expires ->
        c.touches <- c.touches + 1 ;
        match Hashtbl.find_opt table k with
        | Some (v, _) ->
            Hashtbl.replace table k (v, expires)
        | None ->
            () )
      ~remove:(fun k ->
        c.removes <- c.removes + 1 ;
        let existed = Hashtbl.mem table k in
        Hashtbl.remove table k ; existed )
  in
  (store, c, table)

let writes c = c.creates + c.updates + c.touches + c.removes

(* {1 A browser} *)

let cookie_name = "__Host-session"

let contains sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

(* [set_cookies r] are the Set-Cookie fields of [r]. *)
let set_cookies r = Headers.find_all "set-cookie" (Response.headers r)

(* [session_cookie headers] is the value of the session cookie that a response
   with [headers] sets, if any. *)
let session_cookie headers =
  List.find_map
    (fun f ->
      match Cookie.parse_set_cookie f with
      | Some (n, v) when n = cookie_name ->
          Some v
      | _ ->
          None )
    (Headers.find_all "set-cookie" headers)

let cookie_of r = session_cookie (Response.headers r)

let req ?cookie path =
  let headers =
    match cookie with
    | Some v ->
        Headers.of_list [("Cookie", cookie_name ^ "=" ^ v)]
    | None ->
        Headers.empty
  in
  Request.make ~headers Method.GET path

let body r =
  match Response.kind r with
  | Response.Body b ->
      Body.to_string b
  | Response.Upgrade _ ->
      fail "an upgrade"

(* A handler that tells "user", and that sets it on /login, renews on /renew and
   clears on /logout. *)
let app req =
  ( match Request.path req with
  | "/login" ->
      M.set "user" "ada" req
  | "/renew" ->
      M.renew req
  | "/logout" ->
      M.clear req
  | "/logout-flash" ->
      M.clear req ; M.set "flash" "bye" req
  | _ ->
      () ) ;
  Response.string
    ( Option.value ~default:"-" (M.find "user" req)
    ^ " "
    ^ Option.value ~default:"-" (M.find "flash" req) )

let mw ?(keys = [key1]) ?idle ?absolute store =
  M.middleware ?idle ?absolute ~keys ~store ()

(* An ID of 43 characters, as the middleware makes, that no store has. *)
let planted_id = String.make 43 'p'

(* {1 Forged values} *)

let b64 = Caravan.Private.Base64.encode ~url:true ~pad:false

let unb64 s = Option.get (Caravan.Private.Base64.decode ~url:true ~pad:false s)

let hmac key s = Digestif.SHA256.(to_raw_string (hmac_string ~key s))

(* The bytes of [key1]. *)
let key1_bytes = String.make 32 'a'

(* [forge id] is the value of a session cookie for [id], signed with [key1]. *)
let forge id = id ^ "." ^ b64 (hmac key1_bytes ("caravan.session\000" ^ id))

(* {1 Signed values} *)

let signed_tests =
  group "Signed"
    [ test "sign and verify, keys and purposes" (fun () ->
          let s = M.Signed.sign ~purpose:"p" key1 "a.b" in
          equal (option string) (Some "a.b")
            (M.Signed.verify ~purpose:"p" [key1] s) ;
          equal (option string) None (M.Signed.verify ~purpose:"p" [key2] s) ;
          equal (option string) (Some "a.b")
            (M.Signed.verify ~purpose:"p" [key2; key1] s) ;
          equal (option string) None (M.Signed.verify ~purpose:"q" [key1] s) ;
          equal (option string) None (M.Signed.verify ~purpose:"p" [key1] "a.b") ;
          equal (option string) None
            (M.Signed.verify ~purpose:"p" [key1] "nodot") ;
          (* A changed value, and a changed MAC. *)
          equal (option string) None
            (M.Signed.verify ~purpose:"p" [key1] ("x" ^ s)) ;
          let n = String.length s in
          let flip c = if c = 'A' then 'B' else 'A' in
          equal (option string) None
            (M.Signed.verify ~purpose:"p" [key1]
               (String.sub s 0 (n - 1) ^ String.make 1 (flip s.[n - 1])) ) ;
          (* The MAC has 43 characters: 32 bytes, no padding. *)
          let mac = String.sub s 4 (n - 4) in
          equal int 43 (String.length mac) )
    ; test "only the exact text of the MAC" (fun () ->
          (* The last of 43 characters carries 4 bits: the 2 bits after them
             must be 0. A text with them set decodes to the same bytes in a
             lenient decoder. *)
          let s = M.Signed.sign ~purpose:"p" key1 "v" in
          let n = String.length s in
          let last = s.[n - 1] in
          let alphabet =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
          in
          let i = String.index alphabet last in
          let other = alphabet.[i lor 1] in
          if other <> last then
            equal (option string) None
              (M.Signed.verify ~purpose:"p" [key1]
                 (String.sub s 0 (n - 1) ^ String.make 1 other) ) )
    ; test "known answers" (fun () ->
          (* Computed with Python: hmac, hashlib and
             base64.urlsafe_b64encode. *)
          equal string "v.JzyOnGRbK6qOPeTO4B2ex76i_8AEaapr13h2-AcOBXg"
            (M.Signed.sign ~purpose:"p" key1 "v") ;
          List.iter
            (fun (bytes, text) ->
              equal ~msg:text string text
                (M.Key.to_base64url (M.Key.of_string bytes)) ;
              equal ~msg:text string text
                (M.Key.to_base64url (M.Key.of_base64url text)) )
            [ ( "\xfb\xff\xbf" ^ String.init 31 Char.chr
              , "-_-_AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHg" )
            ; ( String.init 33 (fun i -> Char.chr (200 + i))
              , "yMnKy8zNzs_Q0dLT1NXW19jZ2tvc3d7f4OHi4-Tl5ufo" )
            ; ( "\x3e\xff\xff01234567890123456789xyzw\xf8\x00\x01\x02\x03"
              , "Pv__MDEyMzQ1Njc4OTAxMjM0NTY3ODl4eXp3-AABAgM" ) ] ;
          (* Texts that are not the canonical base64url of bytes: the error of
             the decoder, not of an index. *)
          let not_b64 = function
            | Invalid_argument m ->
                m = "Caravan_session.Key.of_base64url: not base64url"
            | _ ->
                false
          in
          List.iter
            (fun t ->
              raises_match ~msg:t not_b64 (fun () ->
                  ignore (M.Key.of_base64url t) ) )
            [ (* A length of 4k+1. *)
              String.make 45 'a'
            ; (* A bad character. *)
              "+" ^ String.make 42 'a'
            ; "=" ^ String.make 42 'a'
            ; (* 2 bits after the last byte (3 characters at the end). *)
              "Pv__MDEyMzQ1Njc4OTAxMjM0NTY3ODl4eXp3-AABAgN"
            ; (* 4 bits after the last byte (2 characters at the end). *)
              "-_-_AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh" ] )
    ; test "the names caravan.* are reserved" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let r =
                mw store
                  (fun req ->
                    List.iter
                      (fun f -> raises_match is_invalid f)
                      [ (fun () -> M.set "caravan.auth.user" "x" req)
                      ; (fun () -> ignore (M.find "caravan.x" req))
                      ; (fun () -> M.remove "caravan." req) ] ;
                    M.set "caravan" "1" req ;
                    M.set "xcaravan.a" "2" req ;
                    M.Private.set "caravan.a" "3" req ;
                    equal (option string) (Some "3")
                      (M.Private.find "caravan.a" req) ;
                    equal (option string) (Some "1") (M.find "caravan" req) ;
                    equal (option string) (Some "2") (M.find "xcaravan.a" req) ;
                    Response.string "" )
                  (req "/")
              in
              (* The values made a session: a new cookie. *)
              is_some (cookie_of r) ) )
    ; test "the purposes caravan.* are reserved" (fun () ->
          raises_match is_invalid (fun () ->
              ignore (M.Signed.sign ~purpose:"caravan.session" key1 "v") ) ;
          raises_match is_invalid (fun () ->
              ignore (M.Signed.verify ~purpose:"caravan.csrf" [key1] "v.x") ) ;
          let s = M.Signed.sign ~purpose:"caravan" key1 "v" in
          equal (option string) (Some "v")
            (M.Signed.verify ~purpose:"caravan" [key1] s) ;
          ignore (M.Signed.sign ~purpose:"caravanx.a" key1 "v") )
    ; test "a purpose with NUL is refused" (fun () ->
          raises_match is_invalid (fun () ->
              ignore (M.Signed.sign ~purpose:"a\000b" key1 "v") ) ;
          raises_match is_invalid (fun () ->
              ignore (M.Signed.verify ~purpose:"a\000b" [key1] "v.x") ) )
    ; test "keys" (fun () ->
          raises_match is_invalid (fun () ->
              ignore (M.Key.of_string (String.make 31 'a')) ) ;
          let k = M.Key.of_base64url (M.Key.to_base64url key1) in
          equal string (M.Key.to_base64url key1) (M.Key.to_base64url k) ;
          equal int 43 (String.length (M.Key.to_base64url key1)) ;
          (* Not base64url, a non-canonical text, and a short key. *)
          List.iter
            (fun t ->
              raises_match ~msg:t is_invalid (fun () ->
                  ignore (M.Key.of_base64url t) ) )
            [ String.make 43 '='
            ; String.sub (M.Key.to_base64url key1) 0 42 ^ "B"
            ; String.make 41 'a'
            ; "YWFh" ] ;
          sim (fun _ ->
              equal int 43
                (String.length (M.Key.to_base64url (M.Key.generate ()))) ) ) ]

(* {1 The middleware} *)

let session_tests =
  group "Sessions"
    [ test "a new session sets a cookie and no-store" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let h =
                mw store (fun req ->
                    M.set "user" "ada" req ;
                    Response.string
                      ~headers:
                        (Headers.of_list
                           [("Cache-Control", "public, max-age=60")] )
                      "x" )
              in
              let r = h (req "/") in
              equal int 1 c.creates ;
              ( match set_cookies r with
              | [f] ->
                  is_true ~msg:f
                    (String.ends_with
                       ~suffix:"; Path=/; Secure; HttpOnly; SameSite=Lax" f ) ;
                  is_true ~msg:f
                    (String.starts_with ~prefix:"__Host-session=" f)
              | l ->
                  fail (string_of_int (List.length l) ^ " Set-Cookie fields") ) ;
              equal (option string) (Some "no-store")
                (Headers.find "cache-control" (Response.headers r)) ;
              equal int 1
                (List.length
                   (Headers.find_all "cache-control" (Response.headers r)) ) ) )
    ; test "the next request finds the data, with no cookie and no write"
        (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let h = mw store app in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              let r = h (req ~cookie "/") in
              equal string "ada -" (body r) ;
              equal (option string) None (cookie_of r) ;
              equal (option string) None
                (Headers.find "cache-control" (Response.headers r)) ;
              equal int 1 (writes c) ) )
    ; test "an anonymous request makes no session" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let r = mw store app (req "/") in
              equal string "- -" (body r) ;
              equal (list string) [] (set_cookies r) ;
              equal int 0 (writes c + c.finds) ) )
    ; test "a forged cookie, and a signed cookie of no session" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let h = mw store app in
              (* A bad signature: the store is not read. *)
              let r = h (req ~cookie:"abc.xyz" "/") in
              equal string "- -" (body r) ;
              equal int 0 c.finds ;
              (* A bad MAC of the full length: no read of the store either. *)
              equal string "- -"
                (body
                   (h (req ~cookie:(planted_id ^ "." ^ String.make 43 'A') "/")) ) ;
              equal int 0 c.finds ;
              (* No Set-Cookie for a bad cookie, or for a signed ID of no
                 session. *)
              equal (list string) [] (set_cookies r) ;
              let planted = forge planted_id in
              equal (list string) [] (set_cookies (h (req ~cookie:planted "/"))) ;
              (* A signed ID that the store does not have: a login gets a new ID
                 (no fixation). *)
              let r = h (req ~cookie:planted "/login") in
              equal int 2 c.finds ;
              let fresh = Option.get (cookie_of r) in
              is_false ~msg:"a new ID"
                (String.starts_with ~prefix:planted_id fresh) ;
              equal string "ada -" (body (h (req ~cookie:fresh "/"))) ) )
    ; test "renew gives a new ID, ends the old one, and keeps the data"
        (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let h = mw store app in
              let old = Option.get (cookie_of (h (req "/login"))) in
              let r = h (req ~cookie:old "/renew") in
              let fresh = Option.get (cookie_of r) in
              is_true ~msg:"a new ID" (old <> fresh) ;
              equal int 1 c.removes ;
              equal string "ada -" (body (h (req ~cookie:fresh "/"))) ;
              equal string "- -" (body (h (req ~cookie:old "/"))) ;
              (* With no session, renew does nothing. *)
              equal (list string) [] (set_cookies (h (req "/renew"))) ) )
    ; test "clear ends the session and expires the cookie" (fun () ->
          sim (fun _ ->
              let store, _, table = counting () in
              let h = mw store app in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              let r = h (req ~cookie "/logout") in
              equal (option string) (Some "") (cookie_of r) ;
              is_true ~msg:"Max-Age=0"
                (List.exists (contains "Max-Age=0") (set_cookies r)) ;
              equal int 0 (Hashtbl.length table) ;
              equal string "- -" (body (h (req ~cookie "/"))) ;
              (* A clear with no session and no cookie does nothing. *)
              equal (list string) [] (set_cookies (h (req "/logout"))) ) )
    ; test "clear, then set: a new session with a new ID" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let h = mw store app in
              let old = Option.get (cookie_of (h (req "/login"))) in
              let r = h (req ~cookie:old "/logout-flash") in
              let fresh = Option.get (cookie_of r) in
              is_true ~msg:"a new ID" (fresh <> "" && fresh <> old) ;
              equal string "- bye" (body (h (req ~cookie:fresh "/"))) ;
              equal string "- -" (body (h (req ~cookie:old "/"))) ) )
    ; test "a slow request does not bring back a cleared session" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let go = Ivar.create () and started = Ivar.create () in
              (* The slow request has the session of the cookie, then waits for
                 the logout. *)
              let slow req =
                equal string "ada" (Option.get (M.find "user" req)) ;
                Ivar.fill started () ;
                Ivar.read go |> sync ;
                M.set "late" "x" req ;
                Response.string "slow"
              in
              let h =
                mw store (fun req ->
                    if Request.path req = "/slow" then slow req else app req )
              in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              Fiber.scope (fun s ->
                  let a =
                    Fiber.fork_promise s (fun () -> h (req ~cookie "/slow"))
                  in
                  sync (Ivar.read started) ;
                  ignore (h (req ~cookie "/logout")) ;
                  Ivar.fill go () ;
                  ignore (Promise.await a) ) ;
              equal string "- -" (body (h (req ~cookie "/"))) ) )
    ; test "a slow renew after a logout gives no session" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let go = Ivar.create () and started = Ivar.create () in
              let h =
                mw store (fun req ->
                    if Request.path req = "/slow-renew" then (
                      Ivar.fill started () ;
                      sync (Ivar.read go) ;
                      M.renew req ;
                      Response.string "renewed" )
                    else app req )
              in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              Fiber.scope (fun s ->
                  let a =
                    Fiber.fork_promise s (fun () ->
                        h (req ~cookie "/slow-renew") )
                  in
                  sync (Ivar.read started) ;
                  ignore (h (req ~cookie "/logout")) ;
                  Ivar.fill go () ;
                  let r = Promise.await a in
                  (* The renew acts as a clear: an expired cookie. *)
                  equal (option string) (Some "") (cookie_of r) ;
                  (* One create: the login. The renew made no session. *)
                  equal int 1 c.creates ) ;
              equal string "- -" (body (h (req ~cookie "/"))) ) )
    ; test "a slow renew after a logout, with the memory store" (fun () ->
          sim (fun _ ->
              let store = M.memory_store () in
              let go = Ivar.create () and started = Ivar.create () in
              let h =
                mw store (fun req ->
                    if Request.path req = "/slow-renew" then (
                      Ivar.fill started () ;
                      sync (Ivar.read go) ;
                      M.renew req ;
                      Response.string "renewed" )
                    else app req )
              in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              Fiber.scope (fun s ->
                  let a =
                    Fiber.fork_promise s (fun () ->
                        h (req ~cookie "/slow-renew") )
                  in
                  sync (Ivar.read started) ;
                  ignore (h (req ~cookie "/logout")) ;
                  Ivar.fill go () ;
                  equal (option string) (Some "") (cookie_of (Promise.await a)) ) ;
              (* A renew of a live session works. *)
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              let fresh = Option.get (cookie_of (h (req ~cookie "/renew"))) in
              equal string "ada -" (body (h (req ~cookie:fresh "/"))) ) )
    ; test "a clear then a set make a new session, also after a logout"
        (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let go = Ivar.create () and started = Ivar.create () in
              let h =
                mw store (fun req ->
                    if Request.path req = "/slow-relogin" then (
                      Ivar.fill started () ;
                      sync (Ivar.read go) ;
                      M.clear req ;
                      M.set "user" "bob" req ;
                      Response.string "relogin" )
                    else app req )
              in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              Fiber.scope (fun s ->
                  let a =
                    Fiber.fork_promise s (fun () ->
                        h (req ~cookie "/slow-relogin") )
                  in
                  sync (Ivar.read started) ;
                  ignore (h (req ~cookie "/logout")) ;
                  Ivar.fill go () ;
                  let fresh = Option.get (cookie_of (Promise.await a)) in
                  is_true ~msg:"a new cookie" (fresh <> "" && fresh <> cookie) ;
                  equal int 2 c.creates ;
                  equal string "bob -" (body (h (req ~cookie:fresh "/"))) ) ) )
    ; test "no touch when the limit does not move" (fun () ->
          (* With idle 30 min and absolute 1 h, the expiry stops at 1 h: the
             requests near the end make no write. *)
          sim (fun s ->
              let store, c, _ = counting () in
              let h = mw ~absolute:(Span.s 3600) store app in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              List.iter
                (fun t ->
                  at s t ;
                  equal ~msg:(string_of_float t) string "ada -"
                    (body (h (req ~cookie "/"))) )
                [600.; 1200.; 1800.; 2400.; 3000.; 3300.; 3599.] ;
              (* Touches at 1200 s (to 3000 s) and 2400 s (to 3600 s). *)
              equal int 2 c.touches ) )
    ; test "the idle limit" (fun () ->
          sim (fun s ->
              let store, c, _ = counting () in
              let h = mw store app in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              (* At 10 min, no touch: less than half of 30 min. *)
              at s 600. ;
              equal string "ada -" (body (h (req ~cookie "/"))) ;
              equal int 0 c.touches ;
              (* At 20 min, a touch moves the limit to 50 min, with no
                 cookie. *)
              at s 1200. ;
              let r = h (req ~cookie "/") in
              equal string "ada -" (body r) ;
              equal (list string) [] (set_cookies r) ;
              equal int 1 c.touches ;
              at s 2900. ;
              equal string "ada -" (body (h (req ~cookie "/"))) ;
              (* 31 min after the last touch: no session. *)
              at s (2900. +. 1860.) ;
              equal string "- -" (body (h (req ~cookie "/"))) ) )
    ; test "the idle limit with a store that ignores expiry" (fun () ->
          sim (fun s ->
              let store, _, _ = counting ~honest:false () in
              let h = mw store app in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              at s 1801. ;
              equal string "- -" (body (h (req ~cookie "/"))) ) )
    ; test "the absolute limit" (fun () ->
          sim (fun s ->
              let store, _, _ = counting ~honest:false () in
              let h = mw store app in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              (* A request each 20 min keeps the idle limit. *)
              let rec go t =
                if t < 8. *. 3600. then (
                  at s t ;
                  equal ~msg:(string_of_float t) string "ada -"
                    (body (h (req ~cookie "/"))) ;
                  go (t +. 1200.) )
              in
              go 1200. ;
              at s ((8. *. 3600.) +. 1.) ;
              equal string "- -" (body (h (req ~cookie "/"))) ) )
    ; test "a handler that raises writes nothing" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let h = mw store (fun req -> M.set "a" "b" req ; failwith "h") in
              raises_match
                (function Failure _ -> true | _ -> false)
                (fun () -> ignore (h (req "/"))) ;
              equal int 0 (writes c) ) )
    ; test "a store that fails after the handler abandons the response"
        (fun () ->
          sim (fun _ ->
              let failing =
                M.store
                  ~find:(fun _ -> None)
                  ~create:(fun _ _ ~expires:_ -> failwith "store")
                  ~update:(fun _ _ ~expires:_ -> failwith "store")
                  ~touch:(fun _ ~expires:_ -> failwith "store")
                  ~remove:(fun _ -> failwith "store")
              in
              let refused = ref 0 in
              let h =
                mw failing (fun req ->
                    M.set "a" "b" req ;
                    Response.upgrade ~on_refused:(fun () -> incr refused) ignore )
              in
              raises (Failure "store") (fun () -> ignore (h (req "/"))) ;
              equal int ~msg:"the upgrade" 1 !refused ;
              (* A body response that goes nowhere closes. *)
              let closes = ref 0 in
              let h =
                mw failing (fun req ->
                    M.set "a" "b" req ;
                    Response.make
                      (Body.of_source
                         ~close:(fun () -> incr closes)
                         (fun _ _ _ -> 0) ) )
              in
              raises (Failure "store") (fun () -> ignore (h (req "/"))) ;
              equal int ~msg:"the body" 1 !closes ) )
    ; test "a release that fails does not hide the error of the store"
        (fun () ->
          sim (fun _ ->
              let failing =
                M.store
                  ~find:(fun _ -> None)
                  ~create:(fun _ _ ~expires:_ -> failwith "store")
                  ~update:(fun _ _ ~expires:_ -> failwith "store")
                  ~touch:(fun _ ~expires:_ -> failwith "store")
                  ~remove:(fun _ -> failwith "store")
              in
              let h =
                mw failing (fun req ->
                    M.set "a" "b" req ;
                    Response.upgrade
                      ~on_refused:(fun () -> failwith "release")
                      ignore )
              in
              raises (Failure "store") (fun () -> ignore (h (req "/"))) ) )
    ; test "a cancel in a write of the store still releases the upgrade"
        (fun () ->
          sim (fun _ ->
              let slow =
                M.store
                  ~find:(fun _ -> None)
                  ~create:(fun _ _ ~expires:_ -> Time.sleep Mono (Span.s 1))
                  ~update:(fun _ _ ~expires:_ -> ())
                  ~touch:(fun _ ~expires:_ -> ())
                  ~remove:(fun _ -> false)
              in
              let refused = ref 0 in
              let h =
                mw slow (fun req ->
                    M.set "a" "b" req ;
                    Response.upgrade
                      ~on_refused:(fun () ->
                        (* Outside a shield, this sleep raises the cancel. *)
                        Time.sleep Mono (Span.ms 1) ;
                        incr refused )
                      ignore )
              in
              Fiber.scope (fun s ->
                  let p =
                    Fiber.fork_promise s (fun () -> ignore (h (req "/")))
                  in
                  Time.sleep Mono (Span.ms 10) ;
                  Promise.cancel p ;
                  match Promise.await p with
                  | () ->
                      ()
                  | exception Promise.Cancelled ->
                      () ) ;
              equal int 1 !refused ) )
    ; test "an old key verifies, and the cookie is signed again" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let cookie =
                Option.get (cookie_of (mw store app (req "/login")))
              in
              let h = mw ~keys:[key2; key1] store app in
              let r = h (req ~cookie "/") in
              equal string "ada -" (body r) ;
              let again = Option.get (cookie_of r) in
              is_true ~msg:"a new signature" (again <> cookie) ;
              equal (option string) (Some "no-store")
                (Headers.find "cache-control" (Response.headers r)) ;
              (* The new cookie needs no old key. *)
              equal string "ada -"
                (body (mw ~keys:[key2] store app (req ~cookie:again "/"))) ) )
    ; test "id makes a session" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let got = ref "" in
              let h =
                mw store (fun req ->
                    got := M.id req ;
                    Response.string (M.id req) )
              in
              let r = h (req "/") in
              equal int 1 c.creates ;
              equal int 43 (String.length !got) ;
              equal string !got (body r) ;
              is_true ~msg:"a cookie" (Option.is_some (cookie_of r)) ) )
    ; test "a change after the response raises" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let h =
                mw store (fun req ->
                    Response.stream (fun w ->
                        match M.set "late" "x" req with
                        | () ->
                            Body.Writer.write_string w "set"
                        | exception Invalid_argument _ ->
                            Body.Writer.write_string w "frozen" ) )
              in
              equal string "frozen" (body (h (req "/"))) ) )
    ; test "the data has at most 64 KiB" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let h =
                mw store (fun req ->
                    M.set "a" (String.make 65535 'x') req ;
                    raises_match is_invalid (fun () -> M.set "b" "yy" req) ;
                    (* A new value of the same name replaces the old size. *)
                    M.set "a" (String.make 65533 'x') req ;
                    M.set "b" "y" req ;
                    Response.string "ok" )
              in
              ignore (h (req "/")) ) )
    ; test "forked fibers on 4 domains" (fun () ->
          sim ~domains:4 (fun _ ->
              let store, _, _ = counting () in
              let h =
                mw store (fun req ->
                    Fiber.scope (fun s ->
                        for d = 0 to 3 do
                          Fiber.fork s (fun () ->
                              for i = 0 to 49 do
                                M.set (Printf.sprintf "%d-%d" d i) "v" req ;
                                Fiber.yield ()
                              done )
                        done ) ;
                    let n = ref 0 in
                    for d = 0 to 3 do
                      for i = 0 to 49 do
                        if M.find (Printf.sprintf "%d-%d" d i) req <> None then
                          incr n
                      done
                    done ;
                    Response.string (string_of_int !n) )
              in
              equal string "200" (body (h (req "/"))) ) )
    ; test "a set on a session from the store is saved" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let h =
                mw store (fun req ->
                    if Request.path req = "/flash" then M.set "flash" "hi" req ;
                    app req )
              in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              let r = h (req ~cookie "/flash") in
              equal (list string) [] (set_cookies r) ;
              equal int 1 c.updates ;
              equal string "ada hi" (body (h (req ~cookie "/"))) ) )
    ; test "set then clear in one request makes no session" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let h =
                mw store (fun req ->
                    M.set "a" "b" req ; M.clear req ; Response.string "x" )
              in
              equal (list string) [] (set_cookies (h (req "/"))) ;
              equal int 0 (writes c) ) )
    ; test "the exact time bounds" (fun () ->
          sim (fun s ->
              (* A store that ignores expiry: the middleware checks the
                 limits. *)
              let store, _, _ = counting ~honest:false () in
              let h = mw store app in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              (* At 1799 s, the session is there, and a touch moves the limit to
                 3599 s: the limit is not later than that. *)
              at s 1799. ;
              equal string "ada -" (body (h (req ~cookie "/"))) ;
              (* At 1801 s, the touch of 1799 s keeps the session, and this
                 request does not move the limit: less than half of idle
                 passed. *)
              at s 1801. ;
              equal string "ada -" (body (h (req ~cookie "/"))) ;
              at s (1799. +. 1800.) ;
              equal string "- -" (body (h (req ~cookie "/"))) ;
              (* The absolute limit, with a long idle limit. *)
              at s 0. ;
              let h = mw ~idle:(Span.s 36000) store app in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              at s 28799. ;
              equal string "ada -" (body (h (req ~cookie "/"))) ;
              at s 28800. ;
              equal string "- -" (body (h (req ~cookie "/"))) ) )
    ; test "the size of the data, also after a reload" (fun () ->
          let max = 65536 in
          sim (fun _ ->
              let store, _, _ = counting () in
              let h =
                mw store (fun req ->
                    ( match Request.path req with
                    | "/big" ->
                        (* "a" and max bytes: one more than the limit. *)
                        raises_match is_invalid (fun () ->
                            M.set "a" (String.make max 'x') req ) ;
                        (* A remove gives its bytes back. *)
                        M.set "a" (String.make (max - 6) 'x') req ;
                        M.remove "a" req ;
                        (* "b" and max - 6 bytes: 5 bytes stay free. *)
                        M.set "b" (String.make (max - 6) 'x') req
                    | "/clear" ->
                        (* After a clear, the size is 0 again: "a" and max - 1
                           bytes fit. *)
                        M.clear req ;
                        M.set "a" (String.make (max - 1) 'x') req
                    | _ ->
                        (* The size of the data from the store is exact: "c" and
                           "zzzz" take the 5 free bytes, "d" one more. *)
                        M.set "c" "zzzz" req ;
                        raises_match is_invalid (fun () -> M.set "d" "" req) ) ;
                    Response.string "ok" )
              in
              let cookie = Option.get (cookie_of (h (req "/big"))) in
              equal string "ok" (body (h (req ~cookie "/"))) ;
              let cookie = Option.get (cookie_of (h (req "/big"))) in
              equal string "ok" (body (h (req ~cookie "/clear"))) ) )
    ; test "a store value that does not decode gives no session" (fun () ->
          sim (fun _ ->
              let store, _, table = counting () in
              let h = mw store app in
              let id = String.make 43 'c' in
              let cookie = forge id in
              let key = Digestif.SHA256.(to_hex (digest_string id)) in
              let far = Time.posix_of_unix_s (t0 +. 1000.) in
              let ns = "1750000000000000000" in
              let get v =
                Hashtbl.replace table key (v, far) ;
                body (h (req ~cookie "/"))
              in
              equal string "ada -" (get ("1" ^ ns ^ "\n4:user3:ada")) ;
              equal string "123456789 -"
                (get ("1" ^ ns ^ "\n4:user9:123456789")) ;
              List.iter
                (fun v -> equal ~msg:(String.escaped v) string "- -" (get v))
                [ ""
                ; "2" ^ ns ^ "\n4:user3:ada"
                ; "1" ^ ns
                ; "1x\n"
                ; "1" ^ ns ^ "x\n4:user3:ada"
                ; "199999999999999999999\n4:user3:ada"
                ; "1" ^ ns ^ "\n4:user3:ad"
                ; "1" ^ ns ^ "\n4:user3xada"
                ; "1" ^ ns ^ "\n4:user"
                ; "1" ^ ns ^ "\n4:user3"
                ; "1" ^ ns ^ "\n4"
                ; "1" ^ ns ^ "\n:user"
                ; "1" ^ ns ^ "\n4611686018427387903:user"
                ; "1" ^ ns ^ "\n4611686018427387904:user" ] ) )
    ; test "errors when the middleware is made" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let bad f =
                raises_match is_invalid (fun () ->
                    ignore (f () : Handler.middleware) )
              in
              bad (fun () -> M.middleware ~keys:[] ~store ()) ;
              bad (fun () -> M.middleware ~secure:false ~keys:[key1] ~store ()) ;
              bad (fun () ->
                  M.middleware ~idle:Span.zero ~keys:[key1] ~store () ) ;
              bad (fun () ->
                  M.middleware
                    ~absolute:(Span.s (11 * 365 * 86400))
                    ~keys:[key1] ~store () ) ;
              bad (fun () -> M.middleware ~cookie:"a b" ~keys:[key1] ~store ()) ;
              (* 10 years is the largest span. *)
              let ten_years = 10 * 365 * 86400 in
              made
                (M.middleware ~idle:(Span.s ten_years)
                   ~absolute:(Span.s ten_years) ~keys:[key1] ~store () ) ;
              bad (fun () ->
                  M.middleware
                    ~idle:(Span.s (ten_years + 1))
                    ~keys:[key1] ~store () ) ;
              made
                (M.middleware ~secure:false ~cookie:"sid" ~keys:[key1] ~store ()) ;
              Mirage_crypto_rng.unset_default_generator () ;
              Fun.protect ~finally:seed (fun () ->
                  bad (fun () -> M.middleware ~keys:[key1] ~store ()) ) ) )
    ; test "a request that did not go through the middleware" (fun () ->
          List.iter
            (fun f -> raises_match is_invalid (fun () -> f (req "/")))
            [ (fun r -> ignore (M.find "a" r))
            ; M.set "a" "b"
            ; M.remove "a"
            ; M.renew
            ; M.clear
            ; (fun r -> ignore (M.id r)) ] )
    ; test "remove" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let h =
                mw store (fun req ->
                    M.remove "a" req ;
                    if Request.path req = "/set" then M.set "user" "ada" req ;
                    if Request.path req = "/rm" then M.remove "user" req ;
                    Response.string
                      (Option.value ~default:"-" (M.find "user" req)) )
              in
              (* With no session, remove does nothing. *)
              equal (list string) [] (set_cookies (h (req "/"))) ;
              let cookie = Option.get (cookie_of (h (req "/set"))) in
              equal string "-" (body (h (req ~cookie "/rm"))) ;
              equal int 1 c.updates ;
              equal string "-" (body (h (req ~cookie "/"))) ) ) ]

(* {1 The memory store} *)

let memory_tests =
  group "Memory store"
    [ test "the eviction removes the oldest write" (fun () ->
          sim (fun s ->
              let store = M.memory_store ~max_sessions:3 () in
              let h = mw store app in
              let a = Option.get (cookie_of (h (req "/login"))) in
              at s 10. ;
              let b = Option.get (cookie_of (h (req "/login"))) in
              at s 20. ;
              let c = Option.get (cookie_of (h (req "/login"))) in
              (* A new write of a: b is now the oldest. *)
              at s 1000. ;
              ignore (h (req ~cookie:a "/")) ;
              let d = Option.get (cookie_of (h (req "/login"))) in
              equal (list string)
                ["ada -"; "- -"; "ada -"; "ada -"]
                (List.map (fun k -> body (h (req ~cookie:k "/"))) [a; b; c; d]) ) )
    ; test "an expired session goes, and remove" (fun () ->
          sim (fun s ->
              let store = M.memory_store () in
              let h = mw ~idle:(Span.s 60) store app in
              let a = Option.get (cookie_of (h (req "/login"))) in
              at s 61. ;
              equal string "- -" (body (h (req ~cookie:a "/"))) ;
              let b = Option.get (cookie_of (h (req "/login"))) in
              ignore (h (req ~cookie:b "/logout")) ;
              equal string "- -" (body (h (req ~cookie:b "/"))) ) )
    ; test "many writes of one session" (fun () ->
          sim (fun s ->
              let store = M.memory_store ~max_sessions:2 () in
              let h = mw ~idle:(Span.s 100) store app in
              let a = Option.get (cookie_of (h (req "/login"))) in
              (* Each request after half of idle touches a: 400 writes in 6.7 h,
                 before the absolute limit. *)
              for i = 1 to 400 do
                at s (float_of_int (i * 60)) ;
                ignore (h (req ~cookie:a "/"))
              done ;
              equal string "ada -" (body (h (req ~cookie:a "/"))) ;
              let b = Option.get (cookie_of (h (req "/login"))) in
              equal string "ada -" (body (h (req ~cookie:b "/"))) ;
              equal string "ada -" (body (h (req ~cookie:a "/"))) ;
              (* After the rebuilds of the queue, the eviction still works: two
                 new sessions remove a and b. *)
              let c = Option.get (cookie_of (h (req "/login"))) in
              let d = Option.get (cookie_of (h (req "/login"))) in
              equal (list string)
                ["- -"; "- -"; "ada -"; "ada -"]
                (List.map (fun k -> body (h (req ~cookie:k "/"))) [a; b; c; d]) ) )
    ; test "a rebuild of the queue keeps the old sessions" (fun () ->
          (* x has one write. Many writes of y, at the same time, rebuild the
             queue: x must stay in it, and go first at an eviction. *)
          sim (fun _ ->
              let store = M.memory_store ~max_sessions:3 () in
              let h =
                mw store (fun req ->
                    if Request.path req = "/flash" then M.set "flash" "hi" req ;
                    app req )
              in
              let x = Option.get (cookie_of (h (req "/login"))) in
              let y = Option.get (cookie_of (h (req "/login"))) in
              for _ = 1 to 40 do
                ignore (h (req ~cookie:y "/flash"))
              done ;
              let z = Option.get (cookie_of (h (req "/login"))) in
              let w = Option.get (cookie_of (h (req "/login"))) in
              equal (list string)
                ["- -"; "ada hi"; "ada -"; "ada -"]
                (List.map (fun k -> body (h (req ~cookie:k "/"))) [x; y; z; w]) ) )
    ; test "an expired session leaves the table at its read" (fun () ->
          (* b has a long idle limit, a a short one. When a expired, its read
             removes it: then a new session needs no eviction, and b stays. *)
          sim (fun s ->
              let store = M.memory_store ~max_sessions:2 () in
              let long = mw ~idle:(Span.s 3600) store app in
              let short = mw ~idle:(Span.s 60) store app in
              let b = Option.get (cookie_of (long (req "/login"))) in
              at s 10. ;
              let a = Option.get (cookie_of (short (req "/login"))) in
              at s 100. ;
              equal string "- -" (body (short (req ~cookie:a "/"))) ;
              ignore (long (req "/login")) ;
              equal string "ada -" (body (long (req ~cookie:b "/"))) ) )
    ; test "an update of the memory store" (fun () ->
          sim (fun _ ->
              let store = M.memory_store () in
              let h =
                mw store (fun req ->
                    if Request.path req = "/flash" then M.set "flash" "hi" req ;
                    app req )
              in
              let cookie = Option.get (cookie_of (h (req "/login"))) in
              ignore (h (req ~cookie "/flash")) ;
              equal string "ada hi" (body (h (req ~cookie "/"))) ;
              (* An update after a remove writes nothing. *)
              ignore (h (req ~cookie "/logout")) ;
              equal string "- -" (body (h (req ~cookie "/"))) ) )
    ; test "the byte budget of the memory store" (fun () ->
          (* A session is a key of 64 bytes (SHA-256 in hex) and 32 bytes of
             value ("1", 19 digits, "\n", "4:user3:ada"): 96 bytes. 3 of them
             fill 288 bytes exactly. "/flash" adds 11 bytes ("5:flash2:hi"). *)
          let flash h k = ignore (h (req ~cookie:k "/flash")) in
          let handler req =
            if Request.path req = "/flash" then M.set "flash" "hi" req ;
            app req
          in
          let show h ks = List.map (fun k -> body (h (req ~cookie:k "/"))) ks in
          sim (fun s ->
              let store = M.memory_store ~max_bytes:288 () in
              let h = mw store handler in
              let login t =
                at s t ;
                Option.get (cookie_of (h (req "/login")))
              in
              let a = login 0. in
              let b = login 1. in
              let c = login 2. in
              equal (list string) ["ada -"; "ada -"; "ada -"] (show h [a; b; c]) ;
              let d = login 3. in
              equal (list string)
                ["- -"; "ada -"; "ada -"; "ada -"]
                (show h [a; b; c; d]) ) ;
          (* An update that fits needs no eviction; one that does not fit
             removes the oldest session. *)
          sim (fun _ ->
              let store = M.memory_store ~max_bytes:(288 + 11) () in
              let h = mw store handler in
              let login () = Option.get (cookie_of (h (req "/login"))) in
              let a = login () in
              let b = login () in
              let c = login () in
              flash h c ;
              equal (list string)
                ["ada -"; "ada -"; "ada hi"]
                (show h [a; b; c]) ;
              flash h b ;
              equal (list string) ["- -"; "ada hi"; "ada hi"] (show h [a; b; c]) ) ;
          (* Touches do not count the bytes of a session two times. *)
          sim (fun s ->
              let store = M.memory_store ~max_bytes:288 () in
              let h = mw ~idle:(Span.s 100) store handler in
              let login () = Option.get (cookie_of (h (req "/login"))) in
              let a = login () in
              let b = login () in
              let c = login () in
              for i = 1 to 10 do
                at s (float_of_int (i * 60)) ;
                ignore (show h [a; b; c])
              done ;
              equal (list string) ["ada -"; "ada -"; "ada -"] (show h [a; b; c]) ;
              (* A new session removes only the oldest one. *)
              let d = login () in
              equal (list string)
                ["- -"; "ada -"; "ada -"; "ada -"]
                (show h [a; b; c; d]) ) ;
          (* The room of a new session is its key and its value: with 330 bytes,
             a fourth session (384 bytes) removes the oldest. *)
          sim (fun _ ->
              let store = M.memory_store ~max_bytes:330 () in
              let h = mw store handler in
              let login () = Option.get (cookie_of (h (req "/login"))) in
              let a = login () in
              let b = login () in
              let c = login () in
              let d = login () in
              equal (list string)
                ["- -"; "ada -"; "ada -"; "ada -"]
                (show h [a; b; c; d]) ) )
    ; test "bad arguments" (fun () ->
          raises_match is_invalid (fun () ->
              ignore (M.memory_store ~max_sessions:0 ()) ) ;
          raises_match is_invalid (fun () ->
              ignore (M.memory_store ~max_bytes:0 ()) ) ;
          (* A session larger than the budget is not kept, and the other
             sessions stay. *)
          sim (fun _ ->
              let store = M.memory_store ~max_bytes:100 () in
              let h =
                mw store (fun req ->
                    if Request.path req = "/big" then
                      M.set "big" (String.make 200 'x') req ;
                    app req )
              in
              let a = Option.get (cookie_of (h (req "/login"))) in
              let big = Option.get (cookie_of (h (req "/big"))) in
              equal (list string) ["ada -"; "- -"]
                (List.map (fun k -> body (h (req ~cookie:k "/"))) [a; big]) ;
              (* An update that gets too large ends that session. *)
              ignore (h (req ~cookie:a "/big")) ;
              equal string "- -" (body (h (req ~cookie:a "/"))) ) ;
          ignore (M.memory_store ~max_sessions:1 ~max_bytes:1 ()) ) ]

(* {1 The encoding of the data} *)

let data_tests =
  group "Data"
    [ (let gen =
         Gen.list ~size:(Gen.int_range 0 5)
           (Gen.pair
              (Gen.string_of ~size:(Gen.int_range 0 6)
                 (Gen.of_list ['a'; ':'; '1'; '\n'; '\000']) )
              (Gen.string_of ~size:(Gen.int_range 0 6)
                 (Gen.of_list ['b'; ':'; '9'; '\n']) ) )
       in
       prop ~count:100 "the data comes back" gen (fun pairs ->
           sim (fun _ ->
               let store, _, _ = counting () in
               let h =
                 mw store (fun req ->
                     if Request.path req = "/set" then
                       List.iter (fun (k, v) -> M.set k v req) pairs ;
                     Response.string
                       (String.concat ","
                          (List.map
                             (fun (k, _) ->
                               Option.value ~default:"?" (M.find k req) )
                             pairs ) ) )
               in
               match cookie_of (h (req "/set")) with
               | None ->
                   equal int 0 (List.length pairs)
               | Some cookie ->
                   let expected =
                     String.concat ","
                       (List.map
                          (fun (k, _) -> List.assoc k (List.rev pairs))
                          pairs )
                   in
                   equal string expected (body (h (req ~cookie "/"))) ) ) ) ]

(* {1 CSRF tokens} *)

let xor a b =
  String.init (String.length a) (fun i ->
      Char.chr (Char.code a.[i] lxor Char.code b.[i]) )

(* [unmask t] is the secret of the token [t]. *)
let unmask t =
  let r = unb64 t in
  xor (String.sub r 0 32) (String.sub r 32 32)

(* [token_of ~key ~purpose cookie] is a token made by hand for the session of
   [cookie], with the raw key [key], and a pad of zeros. *)
let token_of ~key ~purpose cookie =
  let id = List.hd (String.split_on_char '.' cookie) in
  b64 (String.make 32 '\000' ^ hmac key (purpose ^ "\000" ^ id))

(* [plant table id] puts an empty session [id] in the table of a counting
   store. *)
let plant table id =
  Hashtbl.replace table
    Digestif.SHA256.(to_hex (digest_string id))
    ("11750000000000000000\n", Time.posix_of_unix_s (t0 +. 1000.))

(* [request ?cookie ?fields ?body meth path] is a request with the session
   cookie [cookie] and the fields [fields]. *)
let request ?cookie ?(fields = []) ?body meth path =
  let fields =
    match cookie with
    | Some v ->
        ("Cookie", cookie_name ^ "=" ^ v) :: fields
    | None ->
        fields
  in
  Request.make ~headers:(Headers.of_list fields) ?body meth path

let code r = Status.code (Response.status r)

let cache_control r = Headers.find "cache-control" (Response.headers r)

let form_type = ("Content-Type", "application/x-www-form-urlencoded")

(* A GET gives a token, another method gives "ok" and the body. *)
let token_app req =
  match Request.meth req with
  | Method.GET ->
      Response.string (M.Csrf.token req)
  | _ ->
      Response.string ("ok " ^ Body.to_string (Request.body req))

(* [csrf ?csrf ?keys store] is the session middleware on [store], then [csrf]
   (default [M.Csrf.middleware ()]), then [token_app]. *)
let csrf ?(csrf = M.Csrf.middleware ()) ?keys store =
  mw ?keys store (csrf token_app)

(* [login h] is the cookie and a token of a new session of [h]. *)
let login h =
  let r = h (request Method.GET "/") in
  (Option.get (cookie_of r), body r)

(* [post ?cookie ?token ?fields ?text h] is the status of a POST to [h], with
   the token [token] in the header X-CSRF-Token, and the body [text]. *)
let post ?cookie ?token ?(fields = []) ?text h =
  let fields =
    match token with Some t -> ("X-CSRF-Token", t) :: fields | None -> fields
  in
  let body = Option.map Body.of_string text in
  code (h (request ?cookie ~fields ?body Method.POST "/"))

let csrf_tests =
  group "Csrf"
    [ test "a token, and its check in the same request" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let t1 = ref "" and t2 = ref "" in
              let r =
                mw store
                  (fun req ->
                    t1 := M.Csrf.token req ;
                    t2 := M.Csrf.token req ;
                    is_true ~msg:"t1" (M.Csrf.check !t1 req) ;
                    is_true ~msg:"t2" (M.Csrf.check !t2 req) ;
                    Response.string "" )
                  (request Method.GET "/")
              in
              equal int 86 (String.length !t1) ;
              equal int 64 (String.length (unb64 !t1)) ;
              is_true ~msg:"two texts" (!t1 <> !t2) ;
              equal string (unmask !t1) (unmask !t2) ;
              (* A new session: its cookie, its write, and no cache. *)
              is_some (cookie_of r) ;
              equal int 1 c.creates ;
              equal (option string) (Some "no-store") (cache_control r) ) )
    ; test "the secret is the HMAC of the ID" (fun () ->
          sim (fun _ ->
              let store, c, table = counting () in
              plant table planted_id ;
              let r =
                mw store token_app
                  (request ~cookie:(forge planted_id) Method.GET "/")
              in
              (* Python: hmac.new(b"a" * 32, b"caravan.csrf\0" + b"p" * 43,
                 sha256), in base64url. *)
              equal string "QLEGjeCI_HhcPFvWcrlAFIz35f9zNtL5JlTGG6hcKdM"
                (b64 (unmask (body r))) ;
              (* A session that exists: no cookie and no write, but no cache:
                 the page has the token of the user. *)
              equal (list string) [] (set_cookies r) ;
              equal int 0 (writes c) ;
              equal (option string) (Some "no-store") (cache_control r) ;
              let r =
                mw store
                  (fun _ -> Response.string "")
                  (request ~cookie:(forge planted_id) Method.GET "/")
              in
              equal (option string) None (cache_control r) ) )
    ; test "a token is good in its session only" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let h = csrf store in
              let cookie, t = login h in
              equal int 200 (post ~cookie ~token:t h) ;
              let other, t' = login h in
              equal int 403 (post ~cookie:other ~token:t h) ;
              equal int 200 (post ~cookie:other ~token:t' h) ;
              (* No session: no token is good. *)
              equal int 403 (post ~token:t h) ) )
    ; test "check with no session makes no session" (fun () ->
          sim (fun _ ->
              let store, c, _ = counting () in
              let r =
                mw store
                  (fun req ->
                    is_false (M.Csrf.check (String.make 86 'A') req) ;
                    Response.string "" )
                  (request Method.GET "/")
              in
              equal (list string) [] (set_cookies r) ;
              equal (option string) None (cache_control r) ;
              equal int 0 (writes c + c.finds) ) )
    ; test "renew ends the tokens" (fun () ->
          sim (fun _ ->
              let store, _, table = counting () in
              plant table planted_id ;
              let old = ref "" and fresh = ref "" in
              let r =
                mw store
                  (fun req ->
                    old := M.Csrf.token req ;
                    M.renew req ;
                    is_false ~msg:"old" (M.Csrf.check !old req) ;
                    fresh := M.Csrf.token req ;
                    is_true ~msg:"fresh" (M.Csrf.check !fresh req) ;
                    Response.string "" )
                  (request ~cookie:(forge planted_id) Method.GET "/")
              in
              let cookie = Option.get (cookie_of r) in
              let h = csrf store in
              equal int 200 (post ~cookie ~token:!fresh h) ;
              equal int 403 (post ~cookie ~token:!old h) ) )
    ; test "clear ends the tokens, and a later token makes a session" (fun () ->
          sim (fun _ ->
              let store, _, table = counting () in
              plant table planted_id ;
              let old = ref "" and fresh = ref "" in
              let r =
                mw store
                  (fun req ->
                    old := M.Csrf.token req ;
                    M.clear req ;
                    is_false ~msg:"old" (M.Csrf.check !old req) ;
                    fresh := M.Csrf.token req ;
                    is_true ~msg:"fresh" (M.Csrf.check !fresh req) ;
                    Response.string "" )
                  (request ~cookie:(forge planted_id) Method.GET "/")
              in
              let cookie = Option.get (cookie_of r) in
              is_false ~msg:"a new ID"
                (String.starts_with ~prefix:planted_id cookie) ;
              equal int 200 (post ~cookie ~token:!fresh (csrf store)) ) )
    ; test "token uses the first key" (fun () ->
          sim (fun _ ->
              let store, _, table = counting () in
              plant table planted_id ;
              let r =
                mw ~keys:[key2; key1] store token_app
                  (request ~cookie:(forge planted_id) Method.GET "/")
              in
              equal string
                (hmac (String.make 32 'b') ("caravan.csrf\000" ^ planted_id))
                (unmask (body r)) ) )
    ; test "the keys of a rotation" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let cookie, t = login (csrf ~keys:[key1] store) in
              let post keys token = post ~cookie ~token (csrf ~keys store) in
              equal int 200 (post [key2; key1] t) ;
              (* A token of a key that is not in the list. *)
              equal int 403
                (post [key2; key1]
                   (token_of ~key:(String.make 32 'c') ~purpose:"caravan.csrf"
                      cookie ) ) ;
              (* A token made by hand with key1 is good. *)
              equal int 200
                (post [key1]
                   (token_of ~key:key1_bytes ~purpose:"caravan.csrf" cookie) ) ;
              (* The purpose of the session cookie is not that of a token. *)
              equal int 403
                (post [key1]
                   (token_of ~key:key1_bytes ~purpose:"caravan.session" cookie) ) ) )
    ; test "bad texts" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let h = csrf store in
              let cookie, t = login h in
              let flip i =
                String.mapi
                  (fun j ch ->
                    if i = j then if ch = 'A' then 'B' else 'A' else ch )
                  t
              in
              (* The last character has 2 bits of data and 4 zero bits: it is
                 one of A, Q, g and w. The next character is not canonical. *)
              let noncanonical =
                String.sub t 0 85
                ^ String.make 1 (Char.chr (Char.code t.[85] + 1))
              in
              List.iter
                (fun (label, v) ->
                  equal ~msg:label int 403 (post ~cookie ~token:v h) )
                [ ("85", String.sub t 0 85)
                ; ("87", t ^ "A")
                ; ("pad", String.sub t 0 84 ^ "==")
                ; ("noncanonical", noncanonical)
                ; ("a bit of the pad", flip 3)
                ; ("a bit of the masked part", flip 60)
                ; ("not base64url", String.sub t 0 85 ^ "+")
                ; ("empty", "") ] ;
              equal int 200 (post ~cookie ~token:t h) ) )
    ; test "token after the response raises" (fun () ->
          sim (fun _ ->
              let store, _, table = counting () in
              plant table planted_id ;
              List.iter
                (fun cookie ->
                  let raised = ref false in
                  let r =
                    mw store
                      (fun req ->
                        (* A second token after the response raises too. *)
                        if Option.is_some cookie then ignore (M.Csrf.token req) ;
                        Response.stream (fun _ ->
                            try ignore (M.Csrf.token req)
                            with Invalid_argument _ -> raised := true ) )
                      (request ?cookie Method.GET "/")
                  in
                  ignore (body r) ;
                  is_true !raised )
                [None; Some (forge planted_id)] ) )
    ; test "bad setup" (fun () ->
          List.iter
            (fun (label, f) ->
              raises_match ~msg:label is_invalid (fun () -> made (f ())) )
            [ ("header a b", fun () -> M.Csrf.middleware ~header:"a b" ())
            ; ("header empty", fun () -> M.Csrf.middleware ~header:"" ())
            ; ("field", fun () -> M.Csrf.middleware ~field:"" ())
            ; ("max_bytes", fun () -> M.Csrf.middleware ~max_bytes:(-1) ()) ] ;
          made (M.Csrf.middleware ~max_bytes:0 ()) )
    ; test "no session middleware raises, also on a GET" (fun () ->
          List.iter
            (fun m ->
              raises_match is_invalid (fun () ->
                  ignore (M.Csrf.middleware () token_app (request m "/")) ) )
            Method.[GET; POST] ;
          raises_match is_invalid (fun () ->
              ignore (M.Csrf.token (request Method.GET "/")) ) ;
          raises_match is_invalid (fun () ->
              ignore (M.Csrf.check "x" (request Method.GET "/")) ) )
    ; test "the methods" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let h = csrf store in
              let cookie, _ = login h in
              List.iter
                (fun m ->
                  equal ~msg:(Method.to_string m) int 200
                    (code (h (request ~cookie m "/"))) )
                Method.[GET; HEAD; OPTIONS] ;
              List.iter
                (fun m ->
                  equal ~msg:(Method.to_string m) int 403
                    (code (h (request ~cookie m "/"))) )
                Method.[POST; PUT; DELETE; PATCH; TRACE; Other "GET"] ;
              let r = h (request ~cookie Method.POST "/") in
              equal string "Forbidden\n" (body r) ) )
    ; test "the header" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let h = csrf store in
              let cookie, t = login h in
              (* The body is not read: the handler gets the JSON. *)
              let json = {|{"a":1}|} in
              let r =
                h
                  (request ~cookie
                     ~fields:
                       [ ("X-CSRF-Token", t)
                       ; ("Content-Type", "application/json") ]
                     ~body:(Body.of_string json) Method.POST "/" )
              in
              equal string ("ok " ^ json) (body r) ;
              (* A bad header does not fall to a good form field. *)
              let text = "_csrf=" ^ t in
              List.iter
                (fun v ->
                  equal ~msg:v int 403
                    (post ~cookie ~token:v ~fields:[form_type] ~text h) )
                ["bad"; ""] ;
              (* The first field decides. *)
              equal int 200
                (post ~cookie ~token:t ~fields:[("X-CSRF-Token", "bad")] h) ;
              equal int 200 (post ~cookie ~fields:[form_type] ~text h) ) )
    ; test "a form" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let seen = ref [] in
              let field name hs = (name, Headers.find name hs) in
              let h =
                mw store
                  (M.Csrf.middleware () (fun req ->
                       match Request.meth req with
                       | Method.GET ->
                           Response.string (M.Csrf.token req)
                       | _ ->
                           let hs = Request.headers req in
                           seen :=
                             [ field "content-length" hs
                             ; field "transfer-encoding" hs
                             ; field "expect" hs
                             ; field "content-type" hs ] ;
                           Response.string
                             ( match Request.form req with
                             | Ok p ->
                                 Form.encode p
                             | Error _ ->
                                 "error" ) ) )
              in
              let cookie, t = login h in
              let text = "a=x/y~z&_csrf=" ^ t ^ "&b=%2F" in
              let ct = "Application/X-WWW-Form-URLencoded ; charset=utf-8" in
              let r =
                h
                  (request ~cookie
                     ~fields:
                       [ ("Content-Type", ct)
                       ; ("Transfer-Encoding", "chunked")
                       ; ("Expect", "100-continue") ]
                     ~body:(Body.of_string text) Method.POST "/" )
              in
              equal int 200 (code r) ;
              equal
                (list (pair string (option string)))
                [ ("content-length", Some (string_of_int (String.length text)))
                ; ("transfer-encoding", None)
                ; ("expect", None)
                ; ("content-type", Some ct) ]
                !seen ;
              equal string
                (Form.encode [("a", "x/y~z"); ("_csrf", t); ("b", "/")])
                (body r) ;
              (* The same bytes go to the handler. *)
              let r =
                csrf store
                  (request ~cookie ~fields:[form_type]
                     ~body:(Body.of_string text) Method.POST "/" )
              in
              equal string ("ok " ^ text) (body r) ) )
    ; test "a form with no good token" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let h = csrf store in
              let cookie, t = login h in
              List.iter
                (fun text ->
                  equal ~msg:text int 403
                    (post ~cookie ~fields:[form_type] ~text h) )
                ["a=1"; "_csrf=bad"; "_csrf=bad&_csrf=" ^ t; "_CSRF=" ^ t; ""] ;
              let text = "_csrf=" ^ t in
              List.iter
                (fun (label, fields) ->
                  equal ~msg:label int 403 (post ~cookie ~fields ~text h) )
                [ ("two types", [form_type; form_type])
                ; ("an empty type", [("Content-Type", "")])
                ; ("no type", [])
                ; ("json", [("Content-Type", "application/json")])
                ; ( "multipart"
                  , [("Content-Type", "multipart/form-data; boundary=x")] ) ] ;
              equal int 200 (post ~cookie ~fields:[form_type] ~text h) ;
              (* An encoded name matches too. *)
              equal int 200
                (post ~cookie ~fields:[form_type] ~text:("%5Fcsrf=" ^ t) h) ) )
    ; test "deny gets the form" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let csrf =
                M.Csrf.middleware
                  ~deny:(fun req ->
                    Response.string ~status:Status.forbidden
                      ( match Request.form req with
                      | Ok p ->
                          Form.encode p
                      | Error _ ->
                          "error" ) )
                  ()
              in
              let h = mw store (csrf token_app) in
              let cookie, _ = login h in
              let r =
                h
                  (request ~cookie ~fields:[form_type]
                     ~body:(Body.of_string "a=1&_csrf=bad")
                     Method.POST "/" )
              in
              equal string "a=1&_csrf=bad" (body r) ) )
    ; test "the size of a form" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let h = csrf store in
              let cookie, t = login h in
              let good = "_csrf=" ^ t in
              let n = String.length good in
              let post csrf body =
                code
                  (mw store (csrf token_app)
                     (request ~cookie ~fields:[form_type] ~body Method.POST "/") )
              in
              equal int 200
                (post (M.Csrf.middleware ~max_bytes:n ()) (Body.of_string good)) ;
              equal int 413
                (post
                   (M.Csrf.middleware ~max_bytes:(n - 1) ())
                   (Body.of_string good) ) ;
              (* A declared length over the limit: no read. *)
              let read = ref false in
              let source =
                Body.of_source ~length:(n + 1) (fun _ _ _ ->
                    read := true ;
                    0 )
              in
              equal int 413 (post (M.Csrf.middleware ~max_bytes:n ()) source) ;
              is_false ~msg:"no read" !read ;
              let r =
                mw store
                  (M.Csrf.middleware ~max_bytes:(n - 1) () token_app)
                  (request ~cookie ~fields:[form_type]
                     ~body:(Body.of_string good) Method.POST "/" )
              in
              equal string "Content Too Large\n" (body r) ;
              (* The default: 1 MiB. [pad size] is a form of [size] bytes. *)
              let pad size = good ^ "&p=" ^ String.make (size - n - 3) 'x' in
              let default = M.Csrf.middleware () in
              equal int 200 (post default (Body.of_string (pad 1_048_576))) ;
              equal int 413 (post default (Body.of_string (pad 1_048_577))) ) )
    ; test "no session: no read of the body" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let read = ref false in
              let source =
                Body.of_source (fun _ _ _ ->
                    read := true ;
                    0 )
              in
              let r =
                csrf store
                  (request ~fields:[form_type] ~body:source Method.POST "/")
              in
              equal int 403 (code r) ;
              is_false ~msg:"no read" !read ) )
    ; test "the options" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let cookie, t = login (csrf store) in
              let h =
                csrf
                  ~csrf:(M.Csrf.middleware ~header:"X-Token" ~field:"t" ())
                  store
              in
              equal int 200 (post ~cookie ~fields:[("X-Token", t)] h) ;
              equal int 403 (post ~cookie ~token:t h) ;
              equal int 200
                (post ~cookie ~fields:[form_type] ~text:("t=" ^ t) h) ;
              equal int 403
                (post ~cookie ~fields:[form_type] ~text:("_csrf=" ^ t) h) ) )
    ; test "exempt and deny" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let csrf =
                M.Csrf.middleware
                  ~exempt:(fun r -> Request.path_only r = "/login")
                  ~deny:(fun _ -> Response.string ~status:Status.not_found "")
                  ()
              in
              let h = mw store (csrf token_app) in
              (* A login form with no session. *)
              equal int 200 (code (h (request Method.POST "/login?x=1"))) ;
              equal int 404 (code (h (request Method.POST "/other"))) ;
              (* exempt runs on each POST, before the check. deny runs only on a
                 refusal. Neither runs on a GET. *)
              let csrf =
                M.Csrf.middleware
                  ~exempt:(fun _ -> failwith "exempt")
                  ~deny:(fun _ -> failwith "deny")
                  ()
              in
              let h = mw store (csrf token_app) in
              let cookie, t = login h in
              equal int 200 (code (h (request ~cookie Method.GET "/"))) ;
              raises_match
                (function Failure m -> m = "exempt" | _ -> false)
                (fun () -> ignore (post ~cookie ~token:t h)) ;
              let csrf =
                M.Csrf.middleware ~deny:(fun _ -> failwith "deny") ()
              in
              let h = mw store (csrf token_app) in
              equal int 200 (post ~cookie ~token:t h) ;
              raises_match
                (function Failure m -> m = "deny" | _ -> false)
                (fun () -> ignore (post ~cookie h)) ) )
    ; test "an exception of the body goes out" (fun () ->
          sim (fun _ ->
              let store, _, _ = counting () in
              let h = csrf store in
              let cookie, _ = login h in
              let source = Body.of_source (fun _ _ _ -> failwith "body") in
              raises_match
                (function Failure m -> m = "body" | _ -> false)
                (fun () ->
                  ignore
                    (h
                       (request ~cookie ~fields:[form_type] ~body:source
                          Method.POST "/" ) ) ) ) ) ]

let wire_tests =
  group "On the wire"
    [ test "a login and a later request through the server" (fun () ->
          seed () ;
          let store = M.memory_store () in
          Caravan_testing.run (mw store app) (fun t ->
              let r = Caravan_testing.get t "/login" in
              let cookie = Option.get (session_cookie r.headers) in
              let r =
                Caravan_testing.get
                  ~headers:
                    (Headers.of_list [("Cookie", cookie_name ^ "=" ^ cookie)])
                  t "/"
              in
              equal string "ada -" r.body ) )
    ; test "Cross_origin, the session and Csrf through the server" (fun () ->
          seed () ;
          let app =
            Handler.compose
              [ Caravan_middleware.Cross_origin.middleware ()
              ; mw (M.memory_store ())
              ; M.Csrf.middleware () ]
              token_app
          in
          Caravan_testing.run app (fun t ->
              let r = Caravan_testing.get t "/" in
              let cookie = Option.get (session_cookie r.headers) in
              let token = r.body in
              let post fields text =
                (Caravan_testing.fetch t
                   (Request.make
                      ~headers:
                        (Headers.of_list
                           ( ("Cookie", cookie_name ^ "=" ^ cookie)
                           :: form_type :: fields ) )
                      ~body:(Body.of_string text) Method.POST "/" ) )
                  .status
              in
              let same = [("Sec-Fetch-Site", "same-origin")] in
              equal int 200 (Status.code (post same ("_csrf=" ^ token))) ;
              equal int 403 (Status.code (post same "a=1")) ;
              equal int 403
                (Status.code
                   (post [("Sec-Fetch-Site", "cross-site")] ("_csrf=" ^ token)) ) ) )
    ]

let () =
  exit
    (Windtrap.run "session"
       [ signed_tests
       ; session_tests
       ; memory_tests
       ; data_tests
       ; csrf_tests
       ; wire_tests ] )
