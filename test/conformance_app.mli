(** The application that the conformance tests serve (plan D1). It obeys the
    rules that the application controls, so that each violation that the tests
    find is a violation of caravan. It examines the method before the path:

    - An unknown method: 501.
    - CONNECT: a 200 tunnel. The handler sets [Content-Length: 0] and
      [Transfer-Encoding: chunked]. The engine must drop them. The tunnel reads
      to the end and sends nothing.
    - TRACE, POST, PUT, PATCH, DELETE: 405 with [Allow: GET, HEAD, OPTIONS].
    - GET, HEAD, OPTIONS on [/]: the preconditions of RFC 9110 13.2.2, then 200
      with {!page} for GET and HEAD, and 204 with [Allow] for OPTIONS.
    - GET, HEAD, OPTIONS on another path: 404, or a fixture. *)

val page : string
(** [page] is the page at [/]: the index.html of the upstream test bed. *)

val handler : fixtures:bool -> Caravan.Handler.t
(** [handler ~fixtures] is the application. With [fixtures], these paths give
    responses that the rules must examine (plan 4.3):

    - [/fixture/no-content]: a 204. The handler sets [Content-Length: 5] and
      [Transfer-Encoding: chunked] and gives the body [hello].
    - [/fixture/not-modified]: a 304 with the body [hello] and the fields
      [Content-Type], [Content-Encoding], [Content-Language] and
      [Content-Range], which the engine must not send.
    - [/fixture/reset-content]: a 205 with the body [hello].
    - [/fixture/stream]: [hello] as a stream with no length.
    - [/fixture/upgrade-required]: a 426 with [Upgrade: example/1].
    - [/fixture/upgrade]: a 101 to the first protocol that the request offers in
      [Upgrade], and the tunnel ends at once. Without [Upgrade]: 200.
    - [/fixture/fail]: the handler raises [Failure]. *)
