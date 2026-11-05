(* HTML combinators. The constructors do all checks. An element writes its start
   tag at construction, thus a render only copies strings and escapes text. *)

type t = {raw: bool; write: Buffer.t -> unit}

type attr = No_attr | Attr of {name: string; value: string option}

let invalid fn fmt =
  Printf.ksprintf (fun m -> invalid_arg ("Caravan_html." ^ fn ^ ": " ^ m)) fmt

(* ASCII case folding of the two sides, with no allocation. *)

let lower = function
  | 'A' .. 'Z' as c ->
      Char.unsafe_chr (Char.code c + 32)
  | c ->
      c

let rec prefix_from ~off p s i =
  i = String.length p
  || (lower s.[off + i] = lower p.[i] && prefix_from ~off p s (i + 1))

(* [prefix_at ~off p s]: [s] has the pattern [p] at [off], ASCII case
   ignored. *)
let prefix_at ~off p s =
  String.length s - off >= String.length p && prefix_from ~off p s 0

(* [equal_at ~off p s]: the end of [s] from [off] is [p], ASCII case ignored. *)
let equal_at ~off p s =
  String.length s - off = String.length p && prefix_from ~off p s 0

let equal_ci p s = equal_at ~off:0 p s

let rec mem_ci s = function [] -> false | p :: l -> equal_ci p s || mem_ci s l

let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

(* Escaping. *)

let special = function
  | '&' | '<' | '>' | '"' | '\'' | '\x00' | '\x1b' ->
      true
  | _ ->
      false

let entity = function
  | '&' ->
      "&amp;"
  | '<' ->
      "&lt;"
  | '>' ->
      "&gt;"
  | '"' ->
      "&quot;"
  | '\'' ->
      "&#39;"
  | _ ->
      (* NUL and ESC. *)
      "\xEF\xBF\xBD"

let escape_into b s =
  let n = String.length s in
  let rec go start i =
    if i = n then Buffer.add_substring b s start (i - start)
    else
      let c = String.unsafe_get s i in
      if special c then (
        Buffer.add_substring b s start (i - start) ;
        Buffer.add_string b (entity c) ;
        go (i + 1) (i + 1) )
      else go start (i + 1)
  in
  go 0 0

(* Nodes. *)

let txt s = {raw= false; write= (fun b -> escape_into b s)}

let txtf fmt = Printf.ksprintf txt fmt

let list l =
  { raw= List.exists (fun n -> n.raw) l
  ; write= (fun b -> List.iter (fun n -> n.write b) l) }

let empty = {raw= false; write= ignore}

let unsafe_raw s = {raw= false; write= (fun b -> Buffer.add_string b s)}

let doctype = unsafe_raw "<!DOCTYPE html>"

let add_to_buffer b n = n.write b

let to_string n =
  let b = Buffer.create 16384 in
  n.write b ; Buffer.contents b

let response ?status ?headers n =
  Caravan.Response.string ?status ?headers
    ~content_type:"text/html; charset=utf-8" (to_string n)

(* Attributes. *)

let valid_attr_name s =
  s <> ""
  && String.for_all
       (function
         | '"' | '\'' | '<' | '>' | '/' | '=' | '&' ->
             false
         | c ->
             c >= '\x21' && c <= '\x7e' )
       s

(* The offset after a [data-] prefix: htmx also reads [data-hx-*]. *)
let data_off s = if prefix_at ~off:0 "data-" s then 5 else 0

let script_name s =
  let off = data_off s in
  prefix_at ~off:0 "on" s
  || s.[0] = '@'
  || s.[0] = ':'
  || mem_ci s
       ["srcdoc"; "attributename"; "http-equiv"; "_"; "script"; "data-script"]
  || prefix_at ~off "hx-on" s || equal_at ~off "hx-vars" s

let url_name s =
  mem_ci s
    [ "href"
    ; "src"
    ; "action"
    ; "formaction"
    ; "poster"
    ; "cite"
    ; "data"
    ; "background"
    ; "xlink:href" ]
  ||
  let off = data_off s in
  List.exists
    (fun p -> equal_at ~off p s)
    ["hx-get"; "hx-post"; "hx-put"; "hx-patch"; "hx-delete"]

(* The scheme of a URL, as the WHATWG parser finds it: the leading bytes 0x00 to
   0x20 go, and tab, LF and CR go everywhere. [None]: a relative URL. *)
let scheme v =
  let n = String.length v in
  let rec skip i = if i < n && v.[i] <= ' ' then skip (i + 1) else i in
  let b = Buffer.create 16 in
  let rec go i =
    if i = n then None
    else
      match v.[i] with
      | '\t' | '\n' | '\r' ->
          go (i + 1)
      | ':' when Buffer.length b > 0 ->
          Some (Buffer.contents b)
      | c
        when is_letter c
             || Buffer.length b > 0
                && (is_digit c || c = '+' || c = '-' || c = '.') ->
          Buffer.add_char b (lower c) ;
          go (i + 1)
      | _ ->
          None
  in
  go (skip 0)

let safe_url v =
  match scheme v with
  | None ->
      true
  | Some s ->
      List.mem s ["http"; "https"; "mailto"; "tel"]

let blocked_url = "about:invalid#caravan"

let url_value ~url value =
  if url && not (safe_url value) then blocked_url else value

let ascii_space = function
  | ' ' | '\t' | '\n' | '\x0c' | '\r' ->
      true
  | _ ->
      false

(* An htmx value that runs as script. [hx-vals], [hx-headers] and [hx-request]
   must be a JSON object, [hx-trigger] must have no filter. *)
let script_value name v =
  let off = data_off name in
  if equal_at ~off "hx-trigger" name then String.contains v '['
  else if
    List.exists
      (fun p -> equal_at ~off p name)
      ["hx-vals"; "hx-headers"; "hx-request"]
  then
    let n = String.length v in
    let rec skip i = if i < n && ascii_space v.[i] then skip (i + 1) else i in
    let i = skip 0 in
    i < n && v.[i] <> '{'
  else false

let check_name fn name =
  if not (valid_attr_name name) then invalid fn "%S: a bad attribute name" name

let checked_name fn name =
  check_name fn name ;
  if script_name name then
    invalid fn "%S: the value runs as script; use At.unsafe" name

let attr fn name value =
  checked_name fn name ;
  if script_value name value then
    invalid fn "%S: the value runs as script; use At.unsafe" name ;
  Attr {name; value= Some (url_value ~url:(url_name name) value)}

(* An attribute of this module, with a constant name that is valid and is not a
   script name: the checks of the name run once. *)
let named name =
  let url = url_name name in
  fun value -> Attr {name; value= Some (url_value ~url value)}

module At = struct
  let unsafe name value =
    check_name "At.unsafe" name ;
    Attr {name; value= Some value}

  let v name value = attr "At.v" name value

  let bool name =
    checked_name "At.bool" name ;
    if equal_ci "class" name then invalid "At.bool" "\"class\" needs a value" ;
    Attr {name; value= None}

  let none = No_attr

  let prefixed fn prefix name value =
    if name = "" then invalid fn "an empty name" ;
    attr fn (prefix ^ name) value

  let data name value = prefixed "At.data" "data-" name value

  let aria name value = prefixed "At.aria" "aria-" name value

  let id = named "id"

  let class_ = named "class"

  let classes l = class_ (String.concat " " (List.filter (( <> ) "") l))

  let href = named "href"

  let src = named "src"

  let action = named "action"

  let method_ = named "method"

  let name = named "name"

  let value = named "value"

  let type_ = named "type"

  let for_ = named "for"

  let alt = named "alt"

  let title = named "title"

  let lang = named "lang"

  let rel = named "rel"

  let placeholder = named "placeholder"

  let style = named "style"

  let checked = bool "checked"

  let disabled = bool "disabled"

  let selected = bool "selected"

  let required = bool "required"

  let hidden = bool "hidden"

  let multiple = bool "multiple"

  let readonly = bool "readonly"

  let autofocus = bool "autofocus"
end

(* The start tag of an element, with its attributes, and an LF after it if [lf].
   The values of all [class] attributes join, at the position of the first one.
   Another duplicate name raises. *)
let start_tag ~lf fn name at =
  let b = Buffer.create 64 in
  Buffer.add_char b '<' ;
  Buffer.add_string b name ;
  let classes =
    List.filter_map
      (function
        | Attr {name; value= Some v} when equal_ci "class" name && v <> "" ->
            Some v
        | _ ->
            None )
      at
  in
  let rec add_attrs seen class_done = function
    | [] ->
        ()
    | No_attr :: rest ->
        add_attrs seen class_done rest
    | Attr {name= n; value} :: rest ->
        if equal_ci "class" n then (
          if (not class_done) && classes <> [] then (
            Buffer.add_string b " class=\"" ;
            escape_into b (String.concat " " classes) ;
            Buffer.add_char b '"' ) ;
          add_attrs seen true rest )
        else (
          if mem_ci n seen then invalid fn "%S: a duplicate attribute" n ;
          Buffer.add_char b ' ' ;
          Buffer.add_string b n ;
          Option.iter
            (fun v ->
              Buffer.add_string b "=\"" ;
              escape_into b v ;
              Buffer.add_char b '"' )
            value ;
          add_attrs (n :: seen) class_done rest )
  in
  add_attrs [] false at ;
  Buffer.add_char b '>' ;
  if lf then Buffer.add_char b '\n' ;
  Buffer.contents b

let void_names =
  [ "area"
  ; "base"
  ; "br"
  ; "col"
  ; "embed"
  ; "hr"
  ; "img"
  ; "input"
  ; "link"
  ; "meta"
  ; "source"
  ; "track"
  ; "wbr" ]

let refused_names =
  void_names
  @ ["param"; "keygen"; "basefont"; "bgsound"; "frame"; "image"]
  @ ["script"; "style"; "xmp"; "iframe"; "noembed"; "noframes"; "plaintext"]
  @ ["title"; "textarea"]

let valid_element_name s =
  s <> ""
  && is_letter s.[0]
  && String.for_all (fun c -> is_letter c || is_digit c || c = '-') s

(* The parser drops one LF after these start tags. *)
let lf_name name = mem_ci name ["pre"; "listing"]

let foreign_name name = mem_ci name ["svg"; "math"]

(* An element with children, from its start and end tags. *)
let with_children ~foreign fn name start close children =
  let raw = List.exists (fun n -> n.raw) children in
  if raw && foreign then
    invalid fn "%S: a script or style element in foreign content" name ;
  { raw
  ; write=
      (fun b ->
        Buffer.add_string b start ;
        List.iter (fun n -> n.write b) children ;
        Buffer.add_string b close ) }

let void_node fn name at =
  let s = start_tag ~lf:false fn name at in
  {raw= false; write= (fun b -> Buffer.add_string b s)}

(* An element with no children: the start tag, [content] and the end tag, made
   at construction. *)
let fixed ~raw fn name at content =
  let s =
    String.concat "" [start_tag ~lf:false fn name at; content; "</"; name; ">"]
  in
  {raw; write= (fun b -> Buffer.add_string b s)}

(* An element with a text, escaped at render. *)
let text_element ~lf fn name at text =
  let start = start_tag ~lf fn name at in
  let close = "</" ^ name ^ ">" in
  { raw= false
  ; write=
      (fun b ->
        Buffer.add_string b start ;
        escape_into b text ;
        Buffer.add_string b close ) }

(* A script or a style text that can close its element, or start an escape state
   of the parser. [style] also refuses a tag start. *)
let bad_raw_text ~style s =
  let n = String.length s in
  let at i c = i < n && s.[i] = c in
  let rec go i =
    i < n
    &&
    match s.[i] with
    | '\x00' | '\x1b' ->
        true
    | '<' ->
        (at (i + 1) '/' && i + 2 < n && is_letter s.[i + 2])
        || (at (i + 1) '!' && at (i + 2) '-' && at (i + 3) '-')
        || style
           && i + 1 < n
           && (is_letter s.[i + 1] || s.[i + 1] = '!' || s.[i + 1] = '?')
        || go (i + 1)
    | _ ->
        go (i + 1)
  in
  go 0

let json_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '<' ->
          Buffer.add_string b "\\u003c"
      | '>' ->
          Buffer.add_string b "\\u003e"
      | '&' ->
          Buffer.add_string b "\\u0026"
      | '\x00' ->
          Buffer.add_string b "\\u0000"
      | '\x1b' ->
          Buffer.add_string b "\\u001b"
      | c ->
          Buffer.add_char b c )
    s ;
  Buffer.contents b

let opt = Option.value ~default:[]

module El = struct
  let element name ?at children =
    if not (valid_element_name name) then
      invalid "El.element" "%S: a bad element name" name ;
    if mem_ci name refused_names then
      invalid "El.element" "%S: use El.void or the function of the element" name ;
    let fn = "El.element" in
    with_children ~foreign:(foreign_name name) fn name
      (start_tag ~lf:(lf_name name) fn name (opt at))
      ("</" ^ name ^ ">")
      children

  let void name ?at () =
    if not (mem_ci name void_names) then
      invalid "El.void" "%S: not a void element" name ;
    void_node "El.void" name (opt at)

  let raw_text fn ~style name ?at text =
    if bad_raw_text ~style text then invalid fn "the text can close the element" ;
    fixed ~raw:true fn name (opt at) text

  let script ?at text = raw_text "El.script" ~style:false "script" ?at text

  let style ?at text = raw_text "El.style" ~style:true "style" ?at text

  let json_script ?at json =
    fixed ~raw:true "El.json_script" "script"
      (At.type_ "application/json" :: opt at)
      (json_escape json)

  let title ?at text = text_element ~lf:false "El.title" "title" (opt at) text

  let textarea ?at text =
    text_element ~lf:true "El.textarea" "textarea" (opt at) text

  let iframe ?at () = fixed ~raw:false "El.iframe" "iframe" (opt at) ""

  (* The functions of this module, with the parts that do not change made
     once. *)
  let el name =
    let fn = "El." ^ name in
    let lf = lf_name name in
    let foreign = foreign_name name in
    let bare = start_tag ~lf fn name [] in
    let close = "</" ^ name ^ ">" in
    fun ?at children ->
      let start =
        match at with
        | None | Some [] ->
            bare
        | Some at ->
            start_tag ~lf fn name at
      in
      with_children ~foreign fn name start close children

  let vd name =
    let fn = "El." ^ name in
    let bare = void_node fn name [] in
    fun ?at () ->
      match at with None | Some [] -> bare | Some at -> void_node fn name at

  let html = el "html"

  let head = el "head"

  let body = el "body"

  let header = el "header"

  let footer = el "footer"

  let main = el "main"

  let nav = el "nav"

  let section = el "section"

  let article = el "article"

  let aside = el "aside"

  let h1 = el "h1"

  let h2 = el "h2"

  let h3 = el "h3"

  let h4 = el "h4"

  let h5 = el "h5"

  let h6 = el "h6"

  let hgroup = el "hgroup"

  let address = el "address"

  let search = el "search"

  let p = el "p"

  let div = el "div"

  let span = el "span"

  let a = el "a"

  let ul = el "ul"

  let ol = el "ol"

  let li = el "li"

  let dl = el "dl"

  let dt = el "dt"

  let dd = el "dd"

  let menu = el "menu"

  let strong = el "strong"

  let em = el "em"

  let b = el "b"

  let i = el "i"

  let u = el "u"

  let s = el "s"

  let small = el "small"

  let mark = el "mark"

  let code = el "code"

  let kbd = el "kbd"

  let samp = el "samp"

  let pre = el "pre"

  let blockquote = el "blockquote"

  let q = el "q"

  let cite = el "cite"

  let abbr = el "abbr"

  let time = el "time"

  let sub = el "sub"

  let sup = el "sup"

  let ins = el "ins"

  let del = el "del"

  let figure = el "figure"

  let figcaption = el "figcaption"

  let picture = el "picture"

  let video = el "video"

  let audio = el "audio"

  let object_ = el "object"

  let canvas = el "canvas"

  let map = el "map"

  let svg = el "svg"

  let math = el "math"

  let table = el "table"

  let caption = el "caption"

  let thead = el "thead"

  let tbody = el "tbody"

  let tfoot = el "tfoot"

  let tr = el "tr"

  let th = el "th"

  let td = el "td"

  let colgroup = el "colgroup"

  let form = el "form"

  let button = el "button"

  let label = el "label"

  let select = el "select"

  let option = el "option"

  let optgroup = el "optgroup"

  let fieldset = el "fieldset"

  let legend = el "legend"

  let datalist = el "datalist"

  let output = el "output"

  let progress = el "progress"

  let meter = el "meter"

  let details = el "details"

  let summary = el "summary"

  let dialog = el "dialog"

  let template = el "template"

  let slot = el "slot"

  let noscript = el "noscript"

  let area = vd "area"

  let base = vd "base"

  let br = vd "br"

  let col = vd "col"

  let embed = vd "embed"

  let hr = vd "hr"

  let img = vd "img"

  let input = vd "input"

  let link = vd "link"

  let meta = vd "meta"

  let source = vd "source"

  let track = vd "track"

  let wbr = vd "wbr"
end
