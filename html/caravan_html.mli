(** HTML combinators: elements and attributes, rendered into a [Buffer].

    {[
    let page ~user items =
      let open Caravan_html in
      list
        [ doctype
        ; El.html
            ~at:[At.lang "en"]
            [ El.head [El.meta ~at:[At.v "charset" "utf-8"] (); El.title "Items"]
            ; El.body
                [ El.h1 [txtf "Items of %s" user]
                ; El.ul (List.map (fun i -> El.li [txt i]) items) ] ] ]

    let handler _req = Caravan_html.response (page ~user:"ada" ["a"; "b"])
    ]}

    {1:safety Safety}

    The library is safe by default for data of a user in text and in the values
    of {!At.v} and its helpers:
    - Text and attribute values escape [&], [<], [>], the double quote and the
      single quote. NUL and ESC become U+FFFD.
    - A URL attribute keeps only a relative URL or the schemes [http], [https],
      [mailto] and [tel] (see {!At.v}).
    - {!At.v} refuses the names whose values run as script.
    - {!El.script} and {!El.style} refuse a text that can close the element.

    These items are not safe for data of a user. The library cannot find them:
    - {!unsafe_raw}, {!At.unsafe}, and the text of {!El.script} and {!El.style}.
    - A URL with a permitted scheme in [base href], [script src], [link href],
      [form action] or the URLs of htmx: it loads code or sends data to a host
      of the user.
    - The attributes of client frameworks that run as script. Some examples:
      Alpine [x-*], Vue [v-*], Knockout [data-bind], Unpoly [up-on-*], and the
      [data-*] expressions of Datastar. Also [{{ }}] in text, if Vue or
      AngularJS mounts on the page.
    - A value of {!At.v} that an attribute of {!At.unsafe} makes a URL or a
      script. For example, the [to] value of an SVG [set] element with
      [attributeName="href"], or the [content] value of a [meta] element with
      [http-equiv="refresh"].
    - [style] (CSS injection), [is] (a custom element), and [id] or [name] (DOM
      clobbering).
    - The list attributes [srcset] and [ping] get no check of the scheme. No
      browser runs a [javascript:] URL from them.

    The escaping is safe only in UTF-8. {!response} sends
    [text/html; charset=utf-8]. If you send {!to_string} yourself, send the same
    [Content-Type].

    The library does not check the content model of HTML5: [El.p [El.div []]] is
    permitted.

    {1:errors Errors}

    The constructors do all checks. A bad name, a duplicate attribute or a bad
    text raises [Invalid_argument] when the code builds the node, never during a
    render. A URL with a refused scheme does not raise: it is usually data of a
    user.

    {1:concurrency Concurrency}

    The nodes are immutable. Many domains can render one node at the same time.
*)

(** The type for nodes: an element, text, or a list of nodes. *)
type t

(** The type for attributes. *)
type attr

(** Attributes. *)
module At : sig
  val v : string -> string -> attr
  (** [v name value] is the attribute [name="value"]. [value] is escaped.

      A URL attribute: [href], [src], [action], [formaction], [poster], [cite],
      [data], [background], [xlink:href], and [hx-get], [hx-post], [hx-put],
      [hx-patch], [hx-delete] (also with a [data-] prefix). The ASCII case of
      the name is ignored. If [value] has a scheme other than [http], [https],
      [mailto] or [tel], the value is ["about:invalid#caravan"]. The scheme is
      the one that the URL parser of the browser finds: the leading bytes 0x00
      to 0x20 and all tab, LF and CR bytes are ignored. A value with no scheme
      is relative and stays.

      @raise Invalid_argument
        if [name] is empty or has a byte that is not in 0x21 to 0x7E, or one of
        [< > / = &], the double quote and the single quote. Also for a name
        whose value runs as script, with the ASCII case ignored:
        - A name that starts with [on], [@] or [:].
        - [srcdoc], [attributeName], [http-equiv].
        - The htmx names that start with [hx-on], and [hx-vars], also with a
          [data-] prefix.
        - The hyperscript names [_], [script] and [data-script].
        - [hx-trigger] with an open square bracket in [value] (a filter).
        - [hx-vals], [hx-headers] and [hx-request], if [value] has a byte that
          is not ASCII whitespace and the first such byte is not [{] (the value
          is not a JSON object). htmx runs a [js:] value as script. These htmx
          names also with a [data-] prefix.

      Use {!unsafe} for these names. *)

  val unsafe : string -> string -> attr
  (** [unsafe name value] is the attribute [name="value"]. [value] is escaped,
      but it gets no other check. Do not give it data of a user.

      @raise Invalid_argument if [name] is not a valid name (see {!v}). *)

  val bool : string -> attr
  (** [bool name] is the attribute [name] with no value, for example [disabled].

      @raise Invalid_argument as {!v}, and for [class]. *)

  val none : attr
  (** [none] adds no attribute, for example
      [if editable then At.none else At.readonly]. *)

  val data : string -> string -> attr
  (** [data name value] is [v ("data-" ^ name) value].

      @raise Invalid_argument as {!v}, and if [name] is empty. *)

  val aria : string -> string -> attr
  (** [aria name value] is [v ("aria-" ^ name) value].

      @raise Invalid_argument as {!v}, and if [name] is empty. *)

  val class_ : string -> attr
  (** [class_ c] is [v "class" c].

      An element joins the values of all of its [class] attributes, in order,
      with one space. It drops the empty values. If no value stays, the element
      has no [class] attribute. For each other name, a duplicate raises
      [Invalid_argument] when the element is built. The names compare with the
      ASCII case ignored. *)

  val classes : string list -> attr
  (** [classes l] is [class_] of the non-empty strings of [l], joined with one
      space. *)

  (** {1 Attributes with a value}

      Each one is [v] with its name. *)

  val id : string -> attr

  val href : string -> attr

  val src : string -> attr

  val action : string -> attr

  val method_ : string -> attr

  val name : string -> attr

  val value : string -> attr

  val type_ : string -> attr

  val for_ : string -> attr

  val alt : string -> attr

  val title : string -> attr

  val lang : string -> attr

  val rel : string -> attr

  val placeholder : string -> attr

  val style : string -> attr

  (** {1 Boolean attributes} *)

  val checked : attr

  val disabled : attr

  val selected : attr

  val required : attr

  val hidden : attr

  val multiple : attr

  val readonly : attr

  val autofocus : attr
end

(** {1 Nodes} *)

val txt : string -> t
(** [txt s] is the text [s], escaped (see {!safety}). *)

val txtf : ('a, unit, string, t) format4 -> 'a
(** [txtf fmt ...] is [txt (Printf.sprintf fmt ...)]. *)

val list : t list -> t
(** [list l] is the nodes of [l], in order, with no element around them. *)

val empty : t
(** [empty] is no node. *)

val unsafe_raw : string -> t
(** [unsafe_raw s] is [s] with no escape and no check. Do not give it data of a
    user. *)

val doctype : t
(** [doctype] is [<!DOCTYPE html>]. *)

(** Elements.

    Each function of this module raises [Invalid_argument] if [at] has a
    duplicate attribute (see {!At.class_}). *)
module El : sig
  val element : string -> ?at:attr list -> t list -> t
  (** [element name ~at children] is the element [name] with the attributes [at]
      and the children [children]. Use it for an element with no function in
      this module, for example a custom element.

      If [name] is [pre] or [listing] (ASCII case ignored), an LF comes after
      the start tag. The HTML parser drops it, thus a leading LF of the text
      stays.

      @raise Invalid_argument
        if [name] does not start with an ASCII letter, or has a byte that is not
        an ASCII letter, a digit or [-]. Also for these names, with the ASCII
        case ignored:
        - The void elements (use {!void}), and [param], [keygen], [basefont],
          [bgsound], [frame], [image].
        - [script], [style], [title], [textarea], [iframe] (use their
          functions), and [xmp], [noembed], [noframes], [plaintext].
        - [svg] and [math] with a {!script} or {!style} in the children, at any
          depth: in this foreign content, the parser reads their text as markup.
  *)

  val void : string -> ?at:attr list -> unit -> t
  (** [void name ~at ()] is the void element [name], with no end tag.

      @raise Invalid_argument
        if [name] is not [area], [base], [br], [col], [embed], [hr], [img],
        [input], [link], [meta], [source], [track] or [wbr] (ASCII case
        ignored). *)

  val script : ?at:attr list -> string -> t
  (** [script ~at text] is a [script] element with the text [text], with no
      escape. Do not put data of a user in [text]: use {!json_script}.

      @raise Invalid_argument
        if [text] contains NUL, ESC, [<!--], or [</] followed by an ASCII letter
        (thus [</script], also in a string of the script: write [<\/]). *)

  val style : ?at:attr list -> string -> t
  (** [style ~at text] is a [style] element with the text [text], with no
      escape.

      @raise Invalid_argument
        as {!script}, and if [text] contains [<] followed by an ASCII letter,
        [!] or [?]. *)

  val json_script : ?at:attr list -> string -> t
  (** [json_script ~at json] is [<script type="application/json">] with the JSON
      text [json]. It replaces [<], [>], [&], NUL and ESC with their [\u]
      escapes. These bytes occur only in the strings of a valid JSON text, thus
      a JSON parser gets the same value. A script of the page reads it, for
      example with [JSON.parse(document.getElementById("data").textContent)].

      @raise Invalid_argument if [at] has a [type] attribute (a duplicate). *)

  val title : ?at:attr list -> string -> t
  (** [title ~at text] is a [title] element with the text [text], escaped. *)

  val textarea : ?at:attr list -> string -> t
  (** [textarea ~at text] is a [textarea] element with the text [text], escaped,
      after an LF that the parser drops (see {!element}). *)

  val iframe : ?at:attr list -> unit -> t
  (** [iframe ~at ()] is an [iframe] element with no content. *)

  (** {1 Elements with children}

      Each one is [element] with its name. A name that is an OCaml keyword gets
      a trailing [_]. *)

  val html : ?at:attr list -> t list -> t

  val head : ?at:attr list -> t list -> t

  val body : ?at:attr list -> t list -> t

  val header : ?at:attr list -> t list -> t

  val footer : ?at:attr list -> t list -> t

  val main : ?at:attr list -> t list -> t

  val nav : ?at:attr list -> t list -> t

  val section : ?at:attr list -> t list -> t

  val article : ?at:attr list -> t list -> t

  val aside : ?at:attr list -> t list -> t

  val h1 : ?at:attr list -> t list -> t

  val h2 : ?at:attr list -> t list -> t

  val h3 : ?at:attr list -> t list -> t

  val h4 : ?at:attr list -> t list -> t

  val h5 : ?at:attr list -> t list -> t

  val h6 : ?at:attr list -> t list -> t

  val hgroup : ?at:attr list -> t list -> t

  val address : ?at:attr list -> t list -> t

  val search : ?at:attr list -> t list -> t

  val p : ?at:attr list -> t list -> t

  val div : ?at:attr list -> t list -> t

  val span : ?at:attr list -> t list -> t

  val a : ?at:attr list -> t list -> t

  val ul : ?at:attr list -> t list -> t

  val ol : ?at:attr list -> t list -> t

  val li : ?at:attr list -> t list -> t

  val dl : ?at:attr list -> t list -> t

  val dt : ?at:attr list -> t list -> t

  val dd : ?at:attr list -> t list -> t

  val menu : ?at:attr list -> t list -> t

  val strong : ?at:attr list -> t list -> t

  val em : ?at:attr list -> t list -> t

  val b : ?at:attr list -> t list -> t

  val i : ?at:attr list -> t list -> t

  val u : ?at:attr list -> t list -> t

  val s : ?at:attr list -> t list -> t

  val small : ?at:attr list -> t list -> t

  val mark : ?at:attr list -> t list -> t

  val code : ?at:attr list -> t list -> t

  val kbd : ?at:attr list -> t list -> t

  val samp : ?at:attr list -> t list -> t

  val pre : ?at:attr list -> t list -> t

  val blockquote : ?at:attr list -> t list -> t

  val q : ?at:attr list -> t list -> t

  val cite : ?at:attr list -> t list -> t

  val abbr : ?at:attr list -> t list -> t

  val time : ?at:attr list -> t list -> t

  val sub : ?at:attr list -> t list -> t

  val sup : ?at:attr list -> t list -> t

  val ins : ?at:attr list -> t list -> t

  val del : ?at:attr list -> t list -> t

  val figure : ?at:attr list -> t list -> t

  val figcaption : ?at:attr list -> t list -> t

  val picture : ?at:attr list -> t list -> t

  val video : ?at:attr list -> t list -> t

  val audio : ?at:attr list -> t list -> t

  val object_ : ?at:attr list -> t list -> t

  val canvas : ?at:attr list -> t list -> t

  val map : ?at:attr list -> t list -> t

  val svg : ?at:attr list -> t list -> t

  val math : ?at:attr list -> t list -> t

  val table : ?at:attr list -> t list -> t

  val caption : ?at:attr list -> t list -> t

  val thead : ?at:attr list -> t list -> t

  val tbody : ?at:attr list -> t list -> t

  val tfoot : ?at:attr list -> t list -> t

  val tr : ?at:attr list -> t list -> t

  val th : ?at:attr list -> t list -> t

  val td : ?at:attr list -> t list -> t

  val colgroup : ?at:attr list -> t list -> t

  val form : ?at:attr list -> t list -> t

  val button : ?at:attr list -> t list -> t

  val label : ?at:attr list -> t list -> t

  val select : ?at:attr list -> t list -> t

  val option : ?at:attr list -> t list -> t

  val optgroup : ?at:attr list -> t list -> t

  val fieldset : ?at:attr list -> t list -> t

  val legend : ?at:attr list -> t list -> t

  val datalist : ?at:attr list -> t list -> t

  val output : ?at:attr list -> t list -> t

  val progress : ?at:attr list -> t list -> t

  val meter : ?at:attr list -> t list -> t

  val details : ?at:attr list -> t list -> t

  val summary : ?at:attr list -> t list -> t

  val dialog : ?at:attr list -> t list -> t

  val template : ?at:attr list -> t list -> t

  val slot : ?at:attr list -> t list -> t

  val noscript : ?at:attr list -> t list -> t

  (** {1 Void elements}

      Each one is [void] with its name. *)

  val area : ?at:attr list -> unit -> t

  val base : ?at:attr list -> unit -> t

  val br : ?at:attr list -> unit -> t

  val col : ?at:attr list -> unit -> t

  val embed : ?at:attr list -> unit -> t

  val hr : ?at:attr list -> unit -> t

  val img : ?at:attr list -> unit -> t

  val input : ?at:attr list -> unit -> t

  val link : ?at:attr list -> unit -> t

  val meta : ?at:attr list -> unit -> t

  val source : ?at:attr list -> unit -> t

  val track : ?at:attr list -> unit -> t

  val wbr : ?at:attr list -> unit -> t
end

(** {1 Render} *)

val add_to_buffer : Buffer.t -> t -> unit
(** [add_to_buffer b n] writes the HTML of [n] at the end of [b]. *)

val to_string : t -> string
(** [to_string n] is the HTML of [n]. *)

val response :
     ?status:Caravan.Status.t
  -> ?headers:Caravan.Headers.t
  -> t
  -> Caravan.Response.t
(** [response ~status ~headers n] is {!Caravan.Response.string} of
    [to_string n], with the [Content-Type] [text/html; charset=utf-8]. It
    replaces a [Content-Type] of [headers]. *)
