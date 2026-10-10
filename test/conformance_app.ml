open Caravan

(* From testbed/app/index.html of https://github.com/cispa/http-conformance
   (commit 24f75be). Copyright (c) 2023 Jannis Rautenstrauch, MIT license: see
   LICENSE.http-conformance. *)
let page =
  {|<!DOCTYPE html>
<html>
  <head>
    <title>Test Document!</title>
  </head>
  <body>
    <div>
        <p>Hello world!</p>
    </div>
  </body>
</html>

|}

let allow = Headers.of_list [("Allow", "GET, HEAD, OPTIONS")]

let rec read_to_end flow buf =
  if Flow.read flow buf 0 (Bytes.length buf) > 0 then read_to_end flow buf

let tunnel flow = read_to_end flow (Bytes.create 4096)

let connect () =
  Response.upgrade ~status:Status.ok
    ~headers:
      (Headers.of_list
         [("Content-Length", "0"); ("Transfer-Encoding", "chunked")] )
    tunnel

(* RFC 9110 13.2.2 for [/], which has a representation and no validator. The
   page has no Last-Modified, so If-Modified-Since and If-Unmodified-Since have
   no effect. *)
let root req =
  let h = Request.headers req and meth = Request.meth req in
  let is_get_or_head = Method.(equal meth GET || equal meth HEAD) in
  match (Headers.find "If-Match" h, Headers.find "If-None-Match" h) with
  | Some v, _ when String.trim v <> "*" ->
      Response.empty ~status:Status.precondition_failed ()
  | _, Some v when String.trim v = "*" ->
      if is_get_or_head then Response.empty ~status:Status.not_modified ()
      else Response.empty ~status:Status.precondition_failed ()
  | _ ->
      if is_get_or_head then Response.string ~content_type:"text/html" page
      else Response.empty ~headers:allow ()

let upgrade req =
  match Headers.find "Upgrade" (Request.headers req) with
  | Some offered ->
      let protocol = String.trim (List.hd (String.split_on_char ',' offered)) in
      Response.upgrade
        ~headers:(Headers.of_list [("Upgrade", protocol)])
        (fun _ -> ())
  | None ->
      Response.string "no upgrade"

let fixture req =
  match Request.path_only req with
  | "/fixture/no-content" ->
      Some
        (Response.make ~status:Status.no_content
           ~headers:
             (Headers.of_list
                [("Content-Length", "5"); ("Transfer-Encoding", "chunked")] )
           (Body.of_string "hello") )
  | "/fixture/not-modified" ->
      Some (Response.make ~status:Status.not_modified (Body.of_string "hello"))
  | "/fixture/reset-content" ->
      Some (Response.string ~status:Status.reset_content "hello")
  | "/fixture/stream" ->
      Some
        (Response.stream
           ~headers:(Headers.of_list [("Content-Type", "text/plain")])
           (fun w -> Body.Writer.write_string w "hello") )
  | "/fixture/upgrade-required" ->
      Some
        (Response.string ~status:Status.upgrade_required
           ~headers:(Headers.of_list [("Upgrade", "example/1")])
           "upgrade required" )
  | "/fixture/upgrade" ->
      Some (upgrade req)
  | "/fixture/fail" ->
      failwith "the fixture fails"
  | _ ->
      None

let handler ~fixtures req =
  match Request.meth req with
  | Method.Other _ ->
      Response.string ~status:Status.not_implemented "not implemented"
  | CONNECT ->
      connect ()
  | TRACE | POST | PUT | PATCH | DELETE ->
      Response.string ~status:Status.method_not_allowed ~headers:allow
        "method not allowed"
  | GET | HEAD | OPTIONS -> (
      if Request.path_only req = "/" then root req
      else
        match if fixtures then fixture req else None with
        | Some r ->
            r
        | None ->
            Response.string ~status:Status.not_found "not found" )
