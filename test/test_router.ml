(* The tests of caravan.router. Most tests call the handler of the router with a
   request, with no server. The tests of the bytes on the wire use
   Caravan_testing. *)

open Windtrap
open Caravan
module R = Caravan_router
module T = Caravan_testing

let status = Testable.make ~pp:Status.pp ~equal:Status.equal

let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

(* [reply s] is a handler that gives the body [s]. *)
let reply s _ = Response.string s

let body r =
  match Response.kind r with
  | Response.Body b ->
      Body.to_string b
  | Response.Upgrade _ ->
      fail "an upgrade"

let call ?(meth = Method.GET) h path = h (Request.make meth path)

(* [check ?meth h path code text] makes sure that [h] gives [code] and the body
   [text] to [meth path]. *)
let check ?meth h path code text =
  let r = call ?meth h path in
  let msg = path in
  equal ~msg status (Status.v code) (Response.status r) ;
  equal ~msg string text (body r)

let allow r = Headers.find "allow" (Response.headers r)

(* [echo f] is a handler that gives [f req] as its body. *)
let echo f req = Response.string (f req)

(* [show_list l] shows each string in quotes, thus [[""]] is not [[]]. *)
let show_list l =
  "[" ^ String.concat "; " (List.map (Printf.sprintf "%S") l) ^ "]"

(* [result f] is ["ok"], or ["invalid"] if [f ()] raises [Invalid_argument]. *)
let result f =
  match f () with _ -> "ok" | exception Invalid_argument _ -> "invalid"

(* [rejects cases] makes sure that [R.handler] raises [Invalid_argument] for the
   routes of each case. *)
let rejects cases =
  List.iter
    (fun (label, routes) ->
      raises_match ~msg:label is_invalid_arg (fun () -> R.handler routes) )
    cases

let match_tests =
  group "Match"
    [ test "a literal route, and 404 for other paths" (fun () ->
          let h = R.handler [R.get "/a" (reply "a")] in
          check h "/a" 200 "a" ;
          check h "/b" 404 "Not Found\n" ;
          check h "/" 404 "Not Found\n" )
    ; test "parameters" (fun () ->
          let h =
            R.handler
              [ R.get "/users/:id/posts/:p"
                  (echo (fun req ->
                       R.param "id" req ^ " "
                       ^ String.concat ","
                           (List.map (fun (n, v) -> n ^ "=" ^ v) (R.params req))
                       ^ " "
                       ^ Option.value ~default:"none" (R.find_param "x" req) )
                  ) ]
          in
          check h "/users/42/posts/7" 200 "42 id=42,p=7 none" )
    ; test "the router decodes each segment after the split" (fun () ->
          let h = R.handler [R.get "/users/:id" (echo (R.param "id"))] in
          check h "/users/a%20b" 200 "a b" ;
          check h "/users/a%2Fb" 200 "a/b" ;
          check h "/users/a%2fb" 200 "a/b" ;
          check h "/users/a+b" 200 "a+b" )
    ; test "a bad path gets 400, also with a not_found" (fun () ->
          let h =
            R.handler ~not_found:(reply "nf")
              [ R.get "/users/:id" (echo (R.param "id"))
              ; R.get "/static/**" (reply "s") ]
          in
          List.iter
            (fun p -> check h p 400 "Bad Request\n")
            [ "/users/%zz"
            ; "/users/%4"
            ; "/users/%"
            ; "/a#x"
            ; "/static/../x"
            ; "/static/%2e%2e/x"
            ; "/static/%2E%2E/x"
            ; "/static/./x"
            ; "/static/a%00b" ] ;
          let runs = ref 0 in
          let mw : Handler.middleware = fun next req -> incr runs ; next req in
          let h =
            R.handler [R.group ~middleware:[mw] "/g" [R.get "/:x" (reply "x")]]
          in
          let r = call h "/g/%zz" in
          equal status Status.bad_request (Response.status r) ;
          is_true (Response.find_attr R.template r = None) ;
          equal int 0 !runs )
    ; test "a strict trailing slash" (fun () ->
          let h =
            R.handler
              [R.get "/users" (reply "list"); R.get "/a/:id" (reply "id")]
          in
          check h "/users" 200 "list" ;
          check h "/users/" 404 "Not Found\n" ;
          check h "/a/" 404 "Not Found\n" )
    ; test "a literal comes before a parameter, in each order of the list"
        (fun () ->
          let lit = R.get "/a/b" (reply "lit") in
          let par = R.get "/a/:x" (echo (R.param "x")) in
          List.iter
            (fun routes ->
              let h = R.handler routes in
              check h "/a/b" 200 "lit" ; check h "/a/c" 200 "c" )
            [[lit; par]; [par; lit]] )
    ; test "the search backtracks" (fun () ->
          let h =
            R.handler
              [ R.get "/a/:x/c" (echo (fun req -> "first " ^ R.param "x" req))
              ; R.get "/a/b/d" (reply "second") ]
          in
          check h "/a/b/c" 200 "first b" ;
          check h "/a/b/d" 200 "second" )
    ; test "the rest matches zero or more segments" (fun () ->
          let rest req = show_list (R.rest req) in
          let h = R.handler [R.get "/static/**" (echo rest)] in
          check h "/static" 200 "[]" ;
          check h "/static/" 200 {|[""]|} ;
          check h "/static/a/b%20c" 200 {|["a"; "b c"]|} ;
          let h =
            R.handler
              [R.get "/static/**" (echo rest); R.get "/static" (reply "own")]
          in
          check h "/static" 200 "own" ;
          check h "/static/x" 200 {|["x"]|} )
    ; test "a literal of a pattern is decoded" (fun () ->
          let h = R.handler [R.get "/a%20b" (reply "ok")] in
          check h "/a%20b" 200 "ok" )
    ; test "the query has no effect" (fun () ->
          let h = R.handler [R.get "/a" (reply "a")] in
          check h "/a?x=1" 200 "a" )
    ; test "a path that does not start with / goes to not_found" (fun () ->
          let h =
            R.handler ~not_found:(reply "nf") [R.get "/**" (reply "all")]
          in
          check ~meth:Method.OPTIONS h "*" 200 "nf" )
    ; test "a rest route at the root takes the paths of other methods"
        (fun () ->
          let h =
            R.handler [R.get "/**" (reply "all"); R.post "/api/x" (reply "x")]
          in
          check h "/api/x" 200 "all" ;
          let r = call ~meth:Method.DELETE h "/zzz" in
          equal status Status.method_not_allowed (Response.status r) )
    ; test "a not_found gets the request" (fun () ->
          let h =
            R.handler ~not_found:(echo Request.path) [R.get "/a" (reply "a")]
          in
          check h "/b?q" 200 "/b?q" ) ]

let method_tests =
  group "Methods"
    [ test "405 with Allow" (fun () ->
          let h = R.handler [R.get "/a" (reply "a")] in
          let r = call ~meth:Method.POST h "/a" in
          equal status Status.method_not_allowed (Response.status r) ;
          equal (option string) (Some "GET, HEAD, OPTIONS") (allow r) ;
          is_true (Response.find_attr R.template r = None) )
    ; test "an extension method in Allow" (fun () ->
          let h =
            R.handler
              [ R.get "/a" (reply "a")
              ; R.route (Method.Other "PURGE") "/a" (reply "p") ]
          in
          check ~meth:(Method.Other "PURGE") h "/a" 200 "p" ;
          equal (option string) (Some "GET, HEAD, OPTIONS, PURGE")
            (allow (call ~meth:Method.PUT h "/a")) )
    ; test "the order of Allow" (fun () ->
          let h =
            R.handler
              (List.map
                 (fun m -> R.route m "/a" (reply ""))
                 Method.[Other "PURGE"; PATCH; Other "LOCK"; DELETE; PUT; GET] )
          in
          equal (option string)
            (Some "GET, HEAD, PUT, DELETE, OPTIONS, PATCH, LOCK, PURGE")
            (allow (call ~meth:Method.POST h "/a")) )
    ; test "an Other method with a standard name is that method" (fun () ->
          let h =
            R.handler
              [ R.get "/a" (reply "a")
              ; R.route (Method.Other "OPTIONS") "/a" (reply "o") ]
          in
          check ~meth:Method.OPTIONS h "/a" 200 "o" ;
          equal (option string) (Some "GET, HEAD, OPTIONS")
            (allow (call ~meth:Method.POST h "/a")) ;
          let h =
            R.handler
              [ R.get "/a/b" (reply "b")
              ; R.route (Method.Other "GET") "/a/:x" (reply "x") ]
          in
          check h "/a/c" 200 "x" ;
          equal (option string) (Some "GET, HEAD, OPTIONS")
            (allow (call ~meth:Method.POST h "/a/b")) )
    ; test "Allow is the union of the nodes that match" (fun () ->
          let h =
            R.handler
              [R.get "/a/:x" (echo (R.param "x")); R.post "/a/b" (reply "post")]
          in
          equal (option string) (Some "GET, HEAD, POST, OPTIONS")
            (allow (call ~meth:Method.PUT h "/a/b")) ;
          equal (option string) (Some "GET, HEAD, OPTIONS")
            (allow (call ~meth:Method.POST h "/a/c")) ;
          check h "/a/b" 200 "b" ;
          check ~meth:Method.POST h "/a/b" 200 "post" )
    ; test "HEAD gets the GET route" (fun () ->
          let h =
            R.handler
              [R.get "/a" (echo (fun r -> Method.to_string (Request.meth r)))]
          in
          check ~meth:Method.HEAD h "/a" 200 "HEAD" )
    ; test "a HEAD route comes before the GET route of its node" (fun () ->
          let h =
            R.handler
              [R.get "/a" (reply "get"); R.route Method.HEAD "/a" (reply "head")]
          in
          check ~meth:Method.HEAD h "/a" 200 "head" ;
          let h =
            R.handler
              [ R.get "/a/b" (reply "lit get")
              ; R.route Method.HEAD "/a/:x" (reply "param head") ]
          in
          check ~meth:Method.HEAD h "/a/b" 200 "lit get" )
    ; test "OPTIONS" (fun () ->
          let h = R.handler [R.get "/a" (reply "a")] in
          let r = call ~meth:Method.OPTIONS h "/a" in
          equal status Status.no_content (Response.status r) ;
          equal (option string) (Some "GET, HEAD, OPTIONS") (allow r) ;
          is_true (Response.find_attr R.template r = None) ;
          let h =
            R.handler
              [R.get "/a" (reply "a"); R.route Method.OPTIONS "/a" (reply "o")]
          in
          check ~meth:Method.OPTIONS h "/a" 200 "o" )
    ; test "any" (fun () ->
          let h =
            R.handler [R.any "/p/**" (reply "any"); R.get "/p/x" (reply "get")]
          in
          check ~meth:Method.DELETE h "/p/x" 200 "any" ;
          check h "/p/x" 200 "get" ;
          check h "/p/y" 200 "any" ;
          let h =
            R.handler
              [ R.any "/a" (reply "any")
              ; R.route Method.HEAD "/a" (reply "head")
              ; R.get "/a" (reply "get") ]
          in
          check ~meth:Method.HEAD h "/a" 200 "head" ;
          check ~meth:Method.POST h "/a" 200 "any" ;
          let h =
            R.handler [R.any "/a" (reply "any"); R.get "/a" (reply "get")]
          in
          check ~meth:Method.HEAD h "/a" 200 "get" ) ]

let group_tests =
  group "Groups"
    [ test "the order of the middleware, and what it sees" (fun () ->
          let log = Buffer.create 16 in
          let mw name : Handler.middleware =
           fun next req ->
            Buffer.add_string log
              ( name ^ "("
              ^ Option.value ~default:"-" (R.find_param "id" req)
              ^ ")" ) ;
            next req
          in
          let h =
            R.handler
              [ R.group
                  ~middleware:[mw "a"]
                  "/api"
                  [ R.group
                      ~middleware:[mw "b"; mw "c"]
                      "/v1"
                      [ R.get "/users/:id"
                          (echo (fun req ->
                               Buffer.add_string log "h" ;
                               Option.get (Request.find_attr R.template req) )
                          ) ] ] ]
          in
          check h "/api/v1/users/7" 200 "/api/v1/users/:id" ;
          equal string "a(7)b(7)c(7)h" (Buffer.contents log) )
    ; test "the pattern \"\" in a group is the prefix" (fun () ->
          let h =
            R.handler
              [ R.group "/users"
                  [R.get "" (reply "list"); R.get "/:id" (echo (R.param "id"))]
              ]
          in
          check h "/users" 200 "list" ;
          check h "/users/3" 200 "3" ;
          check h "/users/" 404 "Not Found\n" )
    ; test "the middleware of a group does not run for 404, 405 and OPTIONS"
        (fun () ->
          let runs = ref 0 in
          let mw : Handler.middleware = fun next req -> incr runs ; next req in
          let h =
            R.handler [R.group ~middleware:[mw] "/g" [R.get "/a" (reply "a")]]
          in
          ignore (call h "/g/b") ;
          ignore (call ~meth:Method.POST h "/g/a") ;
          ignore (call ~meth:Method.OPTIONS h "/g/a") ;
          equal int 0 !runs ;
          check h "/g/a" 200 "a" ;
          equal int 1 !runs )
    ; test "an outer middleware reads the template of the response" (fun () ->
          let seen = ref [] in
          let log : Handler.middleware =
           fun next req ->
            let r = next req in
            seen :=
              Option.value ~default:"none" (Response.find_attr R.template r)
              :: !seen ;
            r
          in
          let h =
            Handler.compose [log] (R.handler [R.get "/users/:id" (reply "u")])
          in
          ignore (call h "/users/1") ;
          ignore (call h "/nope") ;
          equal (list string) ["none"; "/users/:id"] !seen )
    ; test "a router in a route replaces the parameters and the rest" (fun () ->
          let inner =
            R.handler
              [ R.get "/outer/:a/**"
                  (echo (fun req ->
                       show_list (List.map fst (R.params req))
                       ^ " " ^ R.param "a" req ) )
              ; R.get "/outer/:a/x"
                  (echo (fun req ->
                       if result (fun () -> R.rest req) = "ok" then "rest"
                       else "no rest" ) ) ]
          in
          let outer = R.handler [R.get "/outer/:b/**" inner] in
          check outer "/outer/1/2" 200 {|["a"] 1|} ;
          check outer "/outer/1/x" 200 "no rest" ;
          let template path = Response.find_attr R.template (call outer path) in
          equal (option string) (Some "/outer/:a/x") (template "/outer/1/x") ;
          let inner = R.handler [R.get "/o/:a/x" (reply "x")] in
          let outer = R.handler [R.get "/o/:b/**" inner] in
          let r = call outer "/o/1/y" in
          equal status Status.not_found (Response.status r) ;
          equal (option string) (Some "/o/:b/**")
            (Response.find_attr R.template r) ) ]

let build_tests =
  group "Build errors"
    [ test "incorrect patterns and prefixes" (fun () ->
          let h = reply "" in
          rejects
            [ ("users", [R.get "users" h])
            ; ("top-level \"\"", [R.get "" h])
            ; ("x in a group", [R.group "/g" [R.get "x" h]])
            ; ("/a/**/b", [R.get "/a/**/b" h])
            ; ("/:", [R.get "/:" h])
            ; ("/:a/:a", [R.get "/:a/:a" h])
            ; ("/:a-b", [R.get "/:a-b" h])
            ; ("/a/*", [R.get "/a/*" h])
            ; ("/a/x**", [R.get "/a/x**" h])
            ; ("/a%zz", [R.get "/a%zz" h])
            ; ("/a b", [R.get "/a b" h])
            ; ("/a?b", [R.get "/a?b" h])
            ; ("/a#b", [R.get "/a#b" h])
            ; ("/a\\tb", [R.get "/a\tb" h])
            ; ("/..", [R.get "/.." h])
            ; ("/%2e", [R.get "/%2e" h])
            ; ("/a%00", [R.get "/a%00" h])
            ; ("CONNECT", [R.route Method.CONNECT "/" h])
            ; ("Other CONNECT", [R.route (Method.Other "CONNECT") "/" h])
            ; ("prefix api", [R.group "api" [R.get "/a" h]])
            ; ("prefix /api/", [R.group "/api/" [R.get "/a" h]])
            ; ("/:a in prefix and pattern", [R.group "/:a" [R.get "/:a" h]])
            ; ("/x/** prefix", [R.group "/x/**" [R.get "/y" h]])
            ; ("a group with no routes", [R.group "/a b/**/c" []]) ] )
    ; test "the text of a build error has the pattern" (fun () ->
          raises_match
            (function
              | Invalid_argument m ->
                  String.starts_with ~prefix:{|Caravan_router: "/a"|} m
              | _ ->
                  false )
            (fun () -> R.handler [R.route (Method.Other "a b") "/a" (reply "")]) )
    ; test "one method and one shape" (fun () ->
          let h = reply "" in
          rejects
            [ ("/a/:x and /a/:y", [R.get "/a/:x" h; R.get "/a/:y" h])
            ; ("through a group", [R.group "/a" [R.get "/b" h]; R.get "/a/b" h])
            ; ("Other GET", [R.route (Method.Other "GET") "/a" h; R.get "/a" h])
            ; ("two any", [R.any "/a" h; R.any "/a" h]) ] ;
          let h =
            R.handler
              [ R.get "/a/:x" (echo (R.param "x"))
              ; R.post "/a/:y" (echo (R.param "y")) ]
          in
          check h "/a/1" 200 "1" ;
          check ~meth:Method.POST h "/a/2" 200 "2" )
    ; test "param and rest of a route that has none" (fun () ->
          let h =
            R.handler
              [ R.get "/a"
                  (echo (fun req ->
                       result (fun () -> R.param "nope" req)
                       ^ " "
                       ^ result (fun () -> R.rest req) ) ) ]
          in
          check h "/a" 200 "invalid invalid" ) ]

(* The reference: a naive matcher with the rules of the mli. A route is (id,
   method or None for any, segments). *)
module Reference = struct
  type seg = L of string | P | Rest

  (* [key pattern path] is the priority key of [pattern] for [path], or None if
     it does not match. At each position: 0 for the end, 1 a literal, 2 a
     parameter, 3 the rest. *)
  let key pattern path =
    let rec go pat path =
      match (pat, path) with
      | [], [] ->
          Some [0]
      | [Rest], _ ->
          Some [3]
      | L s :: pat, s' :: path when s = s' ->
          Option.map (fun k -> 1 :: k) (go pat path)
      | P :: pat, s' :: path when s' <> "" ->
          Option.map (fun k -> 2 :: k) (go pat path)
      | _ ->
          None
    in
    go pattern path

  (* The result: `Route id, `Allow methods, or `Not_found. *)
  let run routes meth path =
    let matching =
      List.filter_map
        (fun (id, m, segs) ->
          Option.map (fun k -> (k, id, m, segs)) (key segs path) )
        routes
    in
    let nodes =
      List.sort_uniq compare
        (List.map (fun (k, _, _, segs) -> (k, segs)) matching)
    in
    let routes_of node =
      List.filter (fun (k, _, _, segs) -> (k, segs) = node) matching
    in
    let pick node =
      let rs = routes_of node in
      let find p = List.find_opt (fun (_, _, m, _) -> p m) rs in
      match find (fun m -> m = Some meth) with
      | Some (_, id, _, _) ->
          Some id
      | None -> (
          let get =
            if meth = Method.HEAD then find (fun m -> m = Some Method.GET)
            else None
          in
          match get with
          | Some (_, id, _, _) ->
              Some id
          | None ->
              Option.map (fun (_, id, _, _) -> id) (find (fun m -> m = None)) )
    in
    match List.find_map pick nodes with
    | Some id ->
        `Route id
    | None when nodes = [] ->
        `Not_found
    | None ->
        `Allow (List.filter_map (fun (_, _, m, _) -> m) matching)
end

let property_tests =
  (* A pattern: 0 to 3 segments of "a", "b", "" or a parameter, then maybe "**".
     A parameter gets the name of its position. *)
  let pattern_gen =
    Gen.map
      (fun (segs, rest) ->
        let segs =
          List.mapi
            (fun i s -> if s = ":p" then s ^ string_of_int i else s)
            segs
        in
        "/" ^ String.concat "/" (segs @ if rest then ["**"] else []) )
      (Gen.pair
         (Gen.list ~size:(Gen.int_range 0 3) (Gen.of_list ["a"; "b"; ":p"; ""]))
         Gen.bool )
  in
  let meth_gen =
    Gen.of_list [Some Method.GET; Some Method.POST; Some Method.HEAD; None]
  in
  let routes_gen =
    Gen.list ~size:(Gen.int_range 1 8) (Gen.pair meth_gen pattern_gen)
  in
  let path_gen =
    Gen.map
      (fun l -> "/" ^ String.concat "/" l)
      (Gen.list ~size:(Gen.int_range 0 4) (Gen.of_list ["a"; "b"; "c"; ""]))
  in
  let req_gen =
    Gen.pair
      (Gen.of_list [Method.GET; Method.POST; Method.HEAD; Method.PUT])
      path_gen
  in
  (* The reference splits the same strings as the router. *)
  let split p =
    String.split_on_char '/' (String.sub p 1 (String.length p - 1))
  in
  let of_text = function
    | "**" ->
        Reference.Rest
    | s when String.starts_with ~prefix:":" s ->
        Reference.P
    | s ->
        Reference.L s
  in
  (* The order of Allow for the methods of the generators only. *)
  let order = function
    | Method.GET ->
        0
    | HEAD ->
        1
    | POST ->
        2
    | PUT ->
        3
    | OPTIONS ->
        5
    | _ ->
        9
  in
  let show = function
    | `Route id ->
        "route " ^ id
    | `Not_found ->
        "404"
    | `Allow a ->
        "allow " ^ a
  in
  group "Laws"
    [ prop "the router agrees with a naive matcher"
        (Gen.pair routes_gen (Gen.list ~size:(Gen.int_range 1 10) req_gen))
        (fun (routes, reqs) ->
          (* Each route is (id, method, pattern, reference segments). Drop a
             route with the method and the shape of an earlier one. *)
          let routes =
            List.mapi
              (fun id (m, p) ->
                (string_of_int id, m, p, List.map of_text (split p)) )
              routes
            |> List.fold_left
                 (fun acc ((_, m, _, segs) as r) ->
                   if
                     List.exists
                       (fun (_, m', _, segs') ->
                         segs' = segs && Option.equal Method.equal m m' )
                       acc
                   then acc
                   else acc @ [r] )
                 []
          in
          let h =
            R.handler
              (List.map
                 (fun (id, m, p, _) ->
                   match m with
                   | Some m ->
                       R.route m p (reply id)
                   | None ->
                       R.any p (reply id) )
                 routes )
          in
          let reference =
            List.map (fun (id, m, _, segs) -> (id, m, segs)) routes
          in
          List.iter
            (fun (meth, p) ->
              let r = h (Request.make meth p) in
              let got =
                match Status.code (Response.status r) with
                | 200 ->
                    `Route (body r)
                | 404 ->
                    `Not_found
                | _ ->
                    `Allow (Option.value ~default:"" (allow r))
              in
              let expected =
                match Reference.run reference meth (split p) with
                | `Allow ms ->
                    let ms =
                      ( if List.exists (Method.equal Method.GET) ms then
                          [Method.HEAD]
                        else [] )
                      @ (Method.OPTIONS :: ms)
                    in
                    `Allow
                      ( List.sort_uniq
                          (fun a b -> compare (order a) (order b))
                          ms
                      |> List.map Method.to_string |> String.concat ", " )
                | (`Route _ | `Not_found) as x ->
                    x
              in
              equal
                ~msg:(Method.to_string meth ^ " " ^ p)
                string (show expected) (show got) )
            reqs ) ]

let wire_tests =
  group "On the wire"
    [ test "HEAD with the GET route" (fun () ->
          let produced = Atomic.make 0 in
          let h =
            R.handler
              [ R.get "/a" (reply "hello")
              ; R.get "/s" (fun _ ->
                    Response.stream (fun w ->
                        Atomic.incr produced ;
                        Body.Writer.write_string w "x" ) ) ]
          in
          T.run h (fun t ->
              let a = T.fetch t (Request.make Method.HEAD "/a") in
              equal status Status.ok a.status ;
              equal (option string) (Some "5")
                (Headers.find "content-length" a.headers) ;
              equal string "" a.body ;
              let s = T.fetch t (Request.make Method.HEAD "/s") in
              equal status Status.ok s.status ;
              equal (option string) None
                (Headers.find "content-length" s.headers) ;
              equal (option string) None
                (Headers.find "transfer-encoding" s.headers) ;
              equal string "" s.body ) ;
          equal int 0 (Atomic.get produced) )
    ; test "OPTIONS *, CONNECT and a query" (fun () ->
          let h =
            R.handler [R.get "/a" (reply "a"); R.any "/**" (reply "any")]
          in
          T.run h (fun t ->
              equal status Status.not_found
                (T.fetch t (Request.make Method.OPTIONS "*")).status ;
              equal status Status.not_found
                (T.fetch t
                   (Request.make ~authority:"127.0.0.1:80" Method.CONNECT "") )
                  .status ;
              equal string "a" (T.get t "/a?x=1").body ) ) ]

let () =
  exit
    (Windtrap.run "router"
       [ match_tests
       ; method_tests
       ; group_tests
       ; build_tests
       ; property_tests
       ; wire_tests ] )
