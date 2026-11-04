(** Static files: a handler and a middleware that serve the files of a
    directory.

    {[
    let app =
      Caravan.Handler.compose
        [Caravan_static.middleware ~prefix:"/assets" ~root:"public" ()]
        handler
    ]}

    The functions read the files with [Weft.File]. In production, each file
    operation runs on the thread pool of weft, thus a read does not stop the
    domain. Each response has an [ETag] and a [Last-Modified] field, and the
    functions do the preconditions of RFC 9110 §13.2.2: [If-Match],
    [If-Unmodified-Since], [If-None-Match] and [If-Modified-Since]. They give
    [304 Not Modified] and [412 Precondition Failed]. They also serve one byte
    range of a file (see {!ranges}).

    {1:root The root}

    The functions trust [root] and the files in it:
    - They follow symbolic links. A link in [root] can point to a file out of
      [root]. Do not let a client make links or files in [root].
    - A FIFO in [root] stops a thread of the pool in the open.

    {1:cost The cost}

    A file lookup is an open of the file on the pool of weft. Thus each [GET]
    and [HEAD] under the prefix of {!middleware}, with a path that the rules
    permit, costs at least one job of the pool, also when the next handler gets
    the request. With the prefix ["/"], almost each [GET] of the application
    costs this job. Give a prefix, for example ["/static"], or use {!dir} on a
    route of [caravan.router].

    Each response keeps an fd open until its body ends, also for a slow client.
    Thus the process needs about two fds for each connection: give
    [RLIMIT_NOFILE] at least [2 * max_connections] and a margin. With no free
    fd, the open raises [Weft.File.Error] with [No_resources].

    {1:paths Paths}

    {!dir} and {!middleware} refuse a path if a decoded segment:
    - Is [.] or [..], or ends with [.] or a space.
    - Contains [/] (from [%2F]), a backslash, [:], or a control byte.
    - Contains [~] and then a digit. Windows opens [.git] from [GIT~1].
    - Contains a code point that HFS+ ignores in names: U+200C to U+200F, U+202A
      to U+202E, U+206A to U+206F, or U+FEFF. HFS+ opens [.git] from
      [U+200C.git].
    - Is not valid UTF-8, or has more than 255 bytes.
    - Has a reserved name of Windows before its first dot, with no case and with
      no spaces around it: [CON], [PRN], [AUX], [NUL], [CONIN$], [CONOUT$], and
      [COM] or [LPT] with a digit or a superscript 1, 2 or 3. Thus [con.txt] and
      [CON .txt] are refused.
    - Starts with [.], if [dotfiles] is [false] (the default). Thus [.git] and
      [.env] are refused.
    - Is empty, and is not the last segment. Thus [//a] is refused, and [a/] is
      a directory.

    The rules are the same on each OS, thus a site acts the same in the
    simulator, on Linux and on Windows. {!dir} gives [404 Not Found] for a
    refused path, and {!middleware} gives the request to the next handler: the
    response tells nothing about the rules.

    {1:dirs Directories}

    A path that ends with [/] gets the [index] file of the directory. A path of
    a directory with an [index] file and with no [/] at the end gets
    [301 Moved Permanently], with a relative [Location]: the last segment of the
    path, a [/], and the query. If the directory has no [index] file, the path
    has no file: no redirect and no list of the directory. Thus an application
    route [/api] still gets its requests when [root] has a directory [api].

    {1:body The body}

    The fd of the file closes at the end of the body: the server closes each
    response body, also for [HEAD]. A middleware that drops a response of these
    functions must close its body ([Caravan.Body.close]). If not, the fd stays
    open. If the file gets shorter after the response starts, the body ends
    before its [Content-Length]: the server reports [Length_mismatch] and closes
    the connection. If the file gets longer, the body stops at the first size.

    {1:ranges Ranges}

    A [GET] with a [Range] field of one byte range gets [206 Partial Content],
    with [Content-Range] (RFC 9110 §14). A 200 and a 206 have
    [Accept-Ranges: bytes]. The rules:
    - The preconditions come first: a 304 or a 412 has no range.
    - [HEAD] ignores [Range] (RFC 9110 §14.2), and an empty file ignores it.
    - A [Range] field with more than one line, with an incorrect element, or
      with a unit that is not [bytes] (with no case), or with more than 100
      elements, is ignored. A space or a tab around an element is permitted, but
      not around [-].
    - More than one satisfiable range gives the full file, with no
      [multipart/byteranges] (RFC 9110 §14.2 lets a server ignore [Range]).
    - No satisfiable range gives [416 Range Not Satisfiable], with only
      [Content-Range: bytes */size] and no body.
    - A large number does not overflow: a large first byte is not satisfiable, a
      large last byte is the end of the file, and a large suffix is the whole
      file, as a 206.
    - [If-Range] matches with one strong entity tag that is the [ETag], or with
      one date that is the second of [Last-Modified]. A date matches only if
      this second is before the second of the response: a heuristic for a strong
      date (RFC 9110 §8.8.2.2). If [If-Range] does not match, the response is
      the full file.

    {1:validators Validators}

    The [ETag] is strong, from the size and the modification time in
    nanoseconds. Two versions with the same size and time have the same tag:
    some file systems have a coarse time, for example 2 s for FAT. A
    modification time after the time of the response gives the time of the
    response as [Last-Modified]. *)

val content_type : string -> string
(** [content_type name] is the media type for the extension of the file name
    [name], with no case, as [Filename.extension] gives it. Thus [.txt] (a name
    that starts with its only dot) has no extension. Text types get
    [; charset=utf-8]. An unknown extension gives ["application/octet-stream"].
    A [.gz] file is [application/gzip], with no [Content-Encoding]. *)

val file :
     ?content_type:string
  -> ?cache_control:string
  -> string
  -> Caravan.Request.t
  -> Caravan.Response.t
(** [file path req] is a response with the regular file at [path], for a [GET]
    or a [HEAD] request. [path] is a path of the OS: do not make it from the
    text of a request (use {!dir}).
    - [content_type] is the value of [Content-Type] (default {!val-content_type}
      of [path]).
    - [cache_control] is the value of [Cache-Control], if any, also on a 304.
    - The response of a 304 has the [Content-Type] and the [Content-Length] of
      the file, for a middleware: the engine does not send them.

    The response has [X-Content-Type-Options: nosniff]. If [path] is not a
    regular file, or if the OS refuses the open (for example [EACCES]), the
    response is [404 Not Found]. Another method gets [405 Method Not Allowed]
    with [Allow: GET, HEAD].

    @raise Invalid_argument
      if [content_type] or [cache_control] is not a correct field value. The fd
      closes.
    @raise Weft.File.Error
      for other errors of the file, for example [No_resources]. *)

val dir :
     ?index:string option
  -> ?dotfiles:bool
  -> ?content_type:(string -> string)
  -> ?cache_control:(string -> string option)
  -> root:string
  -> string list
  -> Caravan.Request.t
  -> Caravan.Response.t
(** [dir ~root segments req] serves the file at the decoded [segments] under
    [root], as {!file}, for example [Caravan_router.rest req] of a route
    [/static/**]. The redirect of a directory uses the path of [req] (see
    {!dirs}).
    - [index] (default [Some "index.html"]) is the file of a directory.
    - [dotfiles] (default [false]) lets a segment start with a dot.
    - [content_type rel] is the [Content-Type] of the file [rel], the path of
      the file under [root], for example ["css/a.css"] (default
      {!val-content_type}).
    - [cache_control rel] is the [Cache-Control] of the file [rel], if any
      (default none). For example [immutable] for files with a hash in the name,
      and [no-cache] for [index.html].

    A refused path (see {!paths}) or no file gives [404 Not Found]. With no
    segment, the path of [req] tells if the path ends with [/].

    @raise Invalid_argument
      if [root] is empty, or if a value of [content_type] or [cache_control] is
      not a correct field value. An exception of a callback goes on. In each
      case, the fd closes.
    @raise Weft.File.Error as {!file}. *)

val middleware :
     ?prefix:string
  -> ?index:string option
  -> ?dotfiles:bool
  -> ?content_type:(string -> string)
  -> ?cache_control:(string -> string option)
  -> root:string
  -> unit
  -> Caravan.Handler.middleware
(** [middleware ~prefix ~root ()] serves the files of [root] at the paths under
    [prefix] (default ["/"]), as {!dir}. [/static/a.txt] is the file [a.txt] for
    the prefix [/static], and [/static] is [root]. The prefix is compared with
    the path of the request before the decode.

    The next handler gets the request if its method is not [GET] or [HEAD], if
    its path is not under [prefix], if a [%] sequence is bad, if the path is
    refused (see {!paths}), or if there is no file. A [GET] or a [HEAD] with a
    permitted path under [prefix] costs a job of the pool (see {!cost}).

    @raise Invalid_argument
      if [root] is empty, or if [prefix] does not start with [/], or ends with
      [/] and is not ["/"].
    @raise Invalid_argument
      in the handler, as {!dir}, for a value of a callback.
    @raise Weft.File.Error as {!file}, in the handler. *)
