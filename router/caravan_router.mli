(** Routes: a handler for each method and path pattern.

    {[
    let app =
      Caravan_router.(
        handler
          [ get "/" home
          ; group ~middleware:[auth] "/users"
              [get "" list_users; get "/:id" (fun req -> show (param "id" req))]
          ; get "/static/**" (fun req -> file (rest req)) ] )
    ]}

    {1:patterns Patterns}

    A pattern starts with [/], and the router splits it at each [/]. Thus [/a]
    and [/a/] are different patterns, and a request for [/a/] does not match
    [/a]. A segment is one of these:
    - [:name], a parameter. It matches one segment that is not empty. A name has
      the characters [a-z A-Z 0-9 _].
    - [**], the rest. It is only the last segment. It matches zero or more
      segments: [/static] and [/static/a/b] match [/static/**].
    - Other text, a literal. The router decodes its [%XX] sequences.

    The router decodes each segment of the path of a request after the split.
    Thus [%2F] stays in one segment, and [+] stays [+]. Parameters are bytes,
    not checked UTF-8. A decoded value can contain [/] and [..] (from [%2F]),
    thus a parameter or the rest is not a safe file path.

    {1:match Match}

    The order of the routes in the list has no effect. At each segment, the
    router tries the route that ends there, then a literal, then a parameter,
    then the rest. The first route in this order that matches the path and the
    method gets the request. For one pattern, the method of the request comes
    first, then the [GET] route for a [HEAD] request, then an {!any} route.

    If routes match the path but not the method, the router gives
    [204 No Content] to [OPTIONS] and [405 Method Not Allowed] to the other
    methods, with an [Allow] field. If no route matches the path, the router
    calls [not_found].

    The router gives [400 Bad Request], before the match and before [not_found],
    to a path with a bad [%] sequence, with a segment [.] or [..] (also after
    the decode), with a decoded NUL, or with a [#].

    A path that does not start with [/], for example the [*] of [OPTIONS *],
    goes to [not_found]. *)

(** The type for routes. *)
type route

val route : Caravan.Method.t -> string -> Caravan.Handler.t -> route
(** [route meth pattern h] is a route for [meth] and [pattern] to [h]. *)

val get : string -> Caravan.Handler.t -> route
(** [get] is [route GET]. *)

val post : string -> Caravan.Handler.t -> route
(** [post] is [route POST]. *)

val put : string -> Caravan.Handler.t -> route
(** [put] is [route PUT]. *)

val delete : string -> Caravan.Handler.t -> route
(** [delete] is [route DELETE]. *)

val patch : string -> Caravan.Handler.t -> route
(** [patch] is [route PATCH]. *)

val any : string -> Caravan.Handler.t -> route
(** [any pattern h] is a route for each method that has no route of its own for
    the same pattern. For [HEAD], a [GET] route of the pattern comes first.
    Patterns that differ only in the names of their parameters are the same
    pattern. *)

val group :
  ?middleware:Caravan.Handler.middleware list -> string -> route list -> route
(** [group ~middleware prefix routes] puts [prefix] before the pattern of each
    route of [routes]. A prefix is [""], or a pattern without a [/] at the end.
    In a group, the pattern [""] is the prefix itself:
    [group "/users" [get "" h]] is [get "/users" h].

    [middleware] wraps the handler of each route of the group. The first
    middleware is outermost, and an outer group is outside an inner group. The
    middleware sees the parameters and the {!template} of the request. It does
    not run for a 400, a 404, a 405 or an automatic [OPTIONS]. Thus a middleware
    for all requests, for example CORS with its preflight requests, wraps the
    result of {!handler}.

    The router applies a middleware one time for each route. A middleware that
    makes state when it is applied gets one copy of the state for each route. *)

val handler : ?not_found:Caravan.Handler.t -> route list -> Caravan.Handler.t
(** [handler ~not_found routes] is a handler that gives each request to its
    route. [not_found] gets a request that matches no route
    ({!Caravan.Handler.not_found} by default).

    @raise Invalid_argument
      with the pattern in the text, for:
      - A pattern that does not start with [/], after the prefixes of its
        groups. A prefix that is not [""] and does not start with [/], or that
        ends with [/].
      - A bad [%] sequence, a [?], a [#], a space or a control character.
      - A segment with [*] that is not [**], or a [**] that is not last.
      - A bad parameter name, or one name two times in a pattern.
      - A segment [.] or [..], also after the decode, or a decoded NUL.
      - A [CONNECT] route: the path of a [CONNECT] request is empty.
      - Two routes with one method and one pattern, also with different names of
        parameters ([/a/:x] and [/a/:y]), or two {!any} routes with one pattern.
        [Other "GET"] is [GET].
      - A method of [Other] with a name that is not a token.
      - A bad prefix of a group, also of a group with no routes. *)

(** {1:request Request data} *)

val param : string -> Caravan.Request.t -> string
(** [param name req] is the value of the parameter [name] of the route of [req].
    It is not a safe file path (see {!patterns}).

    @raise Invalid_argument if the route has no parameter [name]. *)

val find_param : string -> Caravan.Request.t -> string option
(** [find_param name req] is the value of the parameter [name], if the route of
    [req] has one. *)

val params : Caravan.Request.t -> (string * string) list
(** [params req] are the names and values of the parameters of the route of
    [req], in the order of the pattern. The values are not safe file paths (see
    {!patterns}). *)

val rest : Caravan.Request.t -> string list
(** [rest req] are the decoded segments that the [**] of the route of [req]
    matched. A segment can contain [/] (from [%2F]) or [\ ], and a segment can
    be empty: the rest of [/s//etc] for [/s/**] is [[""; "etc"]], thus
    [String.concat "/"] of it starts with [/]. The list is not a safe file path.

    @raise Invalid_argument if the route has no [**]. *)

val template : string Caravan.Attrs.key
(** The full pattern of the matched route, for example ["/users/:id"]. The
    router puts it on the request and on the response. A 400, a 404, a 405 and
    an automatic [OPTIONS] of the router have no template. When a router is the
    handler of a route of another router, the response keeps the template of the
    inner router. A response of the inner router with no template, for example a
    404 or a 405, gets the template of the outer route. *)
