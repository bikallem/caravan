(* The tests of caravan.html: escaping, the URL check against a model of the
   WHATWG steps, the refused names and texts, duplicates, the elements and the
   response. *)

open Windtrap
open Caravan
module H = Caravan_html
module At = H.At
module El = H.El

let render = H.to_string

let invalid ?(msg = "") f =
  match f () with
  | _ ->
      fail ("no Invalid_argument: " ^ msg)
  | exception Invalid_argument _ ->
      ()

(* The reference escape, a byte at a time. *)
let ref_escape s =
  String.concat ""
    (List.map
       (function
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
         | '\x00' | '\x1b' ->
             "\xEF\xBF\xBD"
         | c ->
             String.make 1 c )
       (List.of_seq (String.to_seq s)) )

let href s = render (El.a ~at:[At.href s] [])

let blocked = {|<a href="about:invalid#caravan"></a>|}

let kept s = {|<a href="|} ^ ref_escape s ^ {|"></a>|}

(* [At.v n v] on a [p] element must give the value [shown]. *)
let p_attr ?(shown = "") n v =
  let shown = if shown = "" then ref_escape v else shown in
  equal string
    ~msg:(String.escaped (n ^ " " ^ v))
    ({|<p |} ^ n ^ {|="|} ^ shown ^ {|"></p>|})
    (render (El.p ~at:[At.v n v] []))

(* The model of the URL check: the WHATWG steps, in a second form. *)
let model_safe v =
  let n = String.length v in
  let rec first i =
    if i < n && Char.code v.[i] <= 0x20 then first (i + 1) else i
  in
  let i = first 0 in
  let v =
    String.to_seq (String.sub v i (n - i))
    |> Seq.filter (fun c -> not (c = '\t' || c = '\n' || c = '\r'))
    |> String.of_seq
  in
  match String.index_opt v ':' with
  | None ->
      true
  | Some k ->
      let s = String.sub v 0 k in
      let scheme_char j c =
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' ->
            true
        | '0' .. '9' | '+' | '-' | '.' ->
            j > 0
        | _ ->
            false
      in
      let ok = ref (s <> "") in
      String.iteri (fun j c -> if not (scheme_char j c) then ok := false) s ;
      let is_scheme = !ok in
      (not is_scheme)
      || List.mem (String.lowercase_ascii s) ["http"; "https"; "mailto"; "tel"]

let escape_tests =
  group "Escape"
    [ test "text: the five characters, NUL and ESC" (fun () ->
          equal string "&lt;a &amp; &#39;b&#39; &quot;c&quot;&gt;"
            (render (H.txt "<a & 'b' \"c\">")) ;
          equal string "a\xEF\xBF\xBDb\xEF\xBF\xBDc"
            (render (H.txt "a\x00b\x1bc")) ;
          equal string "plain" (render (H.txt "plain")) ;
          equal string "" (render (H.txt "")) )
    ; test "attribute values" (fun () ->
          equal string
            {|<p title="&quot;x&quot; &lt;&amp;&gt; &#39;" id="a&#39;b"></p>|}
            (render (El.p ~at:[At.title "\"x\" <&> '"; At.id "a'b"] [])) ;
          equal string "<p title=\"\xEF\xBF\xBD\"></p>"
            (render (El.p ~at:[At.title "\x1b"] [])) )
    ; test "property: text and attribute values against the reference"
        (fun () ->
          let rng = Random.State.make [|7|] in
          let alphabet = "ab<>&\"'\x00\x1b\xEF;#é " in
          for _ = 1 to 3000 do
            let s =
              String.init (Random.State.int rng 12) (fun _ ->
                  alphabet.[Random.State.int rng (String.length alphabet)] )
            in
            let out = render (H.txt s) in
            equal string ~msg:(String.escaped s) (ref_escape s) out ;
            (* The byte checks are a check of the reference too: the output is
               equal to it. *)
            String.iteri
              (fun i c ->
                is_true ~msg:(String.escaped out)
                  (not (String.contains "<>\"'\x00\x1b" c)) ;
                if c = '&' then
                  is_true ~msg:(String.escaped out)
                    (List.exists
                       (fun e ->
                         i + String.length e <= String.length out
                         && String.sub out i (String.length e) = e )
                       ["&amp;"; "&lt;"; "&gt;"; "&quot;"; "&#39;"] ) )
              out ;
            equal string ~msg:(String.escaped s)
              ({|<p title="|} ^ ref_escape s ^ {|"></p>|})
              (render (El.p ~at:[At.title s] []))
          done ) ]

let url_tests =
  group "URL"
    [ test "known payloads are blocked" (fun () ->
          List.iter
            (fun v -> equal string ~msg:(String.escaped v) blocked (href v))
            [ "javascript:alert(1)"
            ; "JaVaScRiPt:alert(1)"
            ; " JaVa\tScript:x"
            ; "\x01javascript:x"
            ; "\x0bjavascript:x"
            ; "\x00javascript:x"
            ; "java\nscript:x"
            ; "java\rscript:x"
            ; "javascript\t:x"
            ; "vbscript:x"
            ; "data:text/html,<script>alert(1)</script>"
            ; "file:///etc/passwd"
            ; "a1+.-:x" ] )
    ; test "permitted values stay" (fun () ->
          List.iter
            (fun v -> equal string ~msg:(String.escaped v) (kept v) (href v))
            [ "/a:b"
            ; "//h/x"
            ; "?q"
            ; "#f"
            ; ""
            ; "   "
            ; "HTTPS://x"
            ; "http://x/?a=1&b=2"
            ; "mailto:a@b"
            ; "TEL:+1"
            ; "&#106;avascript:x"
            ; "1javascript:x"
            ; ":x"
            ; "java script:x"
            ; "java\x00script:x"
            ; "jav\xC3\xA9:x"
            ; "javascript" ] )
    ; test "each URL name, in each case and with data- for htmx" (fun () ->
          List.iter
            (fun n ->
              List.iter
                (fun n -> p_attr ~shown:"about:invalid#caravan" n "javascript:x")
                [n; String.uppercase_ascii n] )
            [ "href"
            ; "src"
            ; "action"
            ; "formaction"
            ; "poster"
            ; "cite"
            ; "data"
            ; "background"
            ; "xlink:href"
            ; "hx-get"
            ; "hx-post"
            ; "hx-put"
            ; "hx-patch"
            ; "hx-delete"
            ; "data-hx-get"
            ; "data-hx-post"
            ; "data-hx-put"
            ; "data-hx-patch"
            ; "data-hx-delete" ] ;
          List.iter
            (fun n -> p_attr n "javascript:x")
            [ "title"
            ; "hrefx"
            ; "xhref"
            ; "data-href"
            ; "data-src"
            ; "hx-gets"
            ; "x-hx-get" ] ;
          equal string {|<p data-hx-get="about:invalid#caravan"></p>|}
            (render (El.p ~at:[At.data "hx-get" "javascript:x"] [])) ;
          equal string {|<img src="about:invalid#caravan">|}
            (render (El.img ~at:[At.src "javascript:x"] ())) ;
          equal string {|<form action="about:invalid#caravan"></form>|}
            (render (El.form ~at:[At.action "javascript:x"] [])) )
    ; test "At.unsafe and other helpers do no URL check" (fun () ->
          equal string {|<img src="data:image/png;base64,AA==">|}
            (render
               (El.img ~at:[At.unsafe "src" "data:image/png;base64,AA=="] ()) ) )
    ; test "property: the URL check against the model" (fun () ->
          let rng = Random.State.make [|11|] in
          let pick l = List.nth l (Random.State.int rng (List.length l)) in
          let schemes =
            [ "javascript"
            ; "http"
            ; "https"
            ; "mailto"
            ; "tel"
            ; "data"
            ; "a"
            ; "x1"
            ; "1x"
            ; "" ]
          in
          let noise =
            [ "\t"
            ; "\n"
            ; "\r"
            ; " "
            ; "\x00"
            ; "\x01"
            ; "\x0b"
            ; "/"
            ; ":"
            ; "+"
            ; "."
            ; "-"
            ; "\xC3\xA9"
            ; "#"
            ; "?"
            ; "&" ]
          in
          for _ = 1 to 5000 do
            let mixed s =
              String.map
                (fun c ->
                  if Random.State.bool rng then Char.uppercase_ascii c else c )
                s
            in
            let b = Buffer.create 32 in
            for _ = 1 to Random.State.int rng 3 do
              Buffer.add_string b (pick noise)
            done ;
            String.iter
              (fun c ->
                Buffer.add_char b c ;
                if Random.State.int rng 6 = 0 then
                  Buffer.add_string b (pick noise) )
              (mixed (pick schemes)) ;
            if Random.State.int rng 5 > 0 then Buffer.add_char b ':' ;
            for _ = 1 to Random.State.int rng 4 do
              Buffer.add_string b (pick noise)
            done ;
            let v = Buffer.contents b in
            equal string ~msg:(String.escaped v)
              (if model_safe v then kept v else blocked)
              (href v)
          done ) ]

let names_tests =
  group "Names"
    [ test "script attributes raise in At.v, At.bool, At.data, At.aria"
        (fun () ->
          List.iter
            (fun n ->
              invalid ~msg:n (fun () -> At.v n "x") ;
              invalid ~msg:n (fun () -> At.bool n) )
            [ "onclick"
            ; "ONLOAD"
            ; "On"
            ; "@click"
            ; ":href"
            ; "srcdoc"
            ; "SRCDOC"
            ; "attributeName"
            ; "http-equiv"
            ; "HTTP-EQUIV"
            ; "hx-on:click"
            ; "hx-on"
            ; "HX-ON::after"
            ; "data-hx-on:click"
            ; "DATA-hx-on-click"
            ; "hx-vars"
            ; "data-hx-vars"
            ; "_"
            ; "script"
            ; "SCRIPT"
            ; "data-script" ] ;
          invalid (fun () -> At.data "hx-on:click" "x") ;
          invalid (fun () -> At.data "hx-vars" "x") ;
          invalid (fun () -> At.data "hx-vals" "js:{}") ;
          invalid (fun () -> At.data "hx-headers" "x") ;
          invalid (fun () -> At.data "hx-request" "js:1") ;
          invalid (fun () -> At.data "script" "x") ;
          invalid (fun () -> At.data "x y" "x") ;
          invalid (fun () -> At.data "" "x") ;
          invalid (fun () -> At.aria "" "x") ;
          invalid (fun () -> At.aria "a b" "x") ;
          invalid (fun () -> At.bool "class") ;
          invalid (fun () -> At.bool "CLASS") )
    ; test "names close to the refused names pass" (fun () ->
          List.iter
            (fun n -> p_attr n "x")
            [ "o"
            ; "xon"
            ; "data-on"
            ; "hx-o"
            ; "hx-get-on"
            ; "x-hx-on"
            ; "hx-varsx"
            ; "__"
            ; "scripts"
            ; "x-script"
            ; "data-scripts"
            ; "hx-requests"
            ; "an"
            ; "data-hx-o"
            ; "data-hx-var"
            ; "srcdocx"
            ; "http-equi"
            ; "a@"
            ; "a:b" ] ;
          equal string {|<p data-x="1" aria-label="l"></p>|}
            (render (El.p ~at:[At.data "x" "1"; At.aria "label" "l"] [])) ;
          equal string {|<p data-on="x" disabled></p>|}
            (render (El.p ~at:[At.data "on" "x"; At.bool "disabled"] [])) )
    ; test "htmx values that run as script" (fun () ->
          List.iter
            (fun (n, v) -> invalid ~msg:(n ^ " " ^ v) (fun () -> At.v n v))
            [ ("hx-trigger", "click[x]")
            ; ("HX-TRIGGER", "[")
            ; ("data-hx-trigger", "keyup[key=='Enter']")
            ; ("hx-vals", "js:{a:1}")
            ; ("hx-vals", " JS:{}")
            ; ("hx-vals", "javascript:{}")
            ; ("hx-vals", "\xC2\xA0js:{}")
            ; ("hx-headers", "x")
            ; ("hx-request", "js: alert(1)")
            ; ("data-hx-request", "javascript:1")
            ; ("HX-REQUEST", "x")
            ; ("data-hx-vals", "js:{}")
            ; ("data-hx-headers", "\t[1]") ] ;
          List.iter
            (fun (n, v) -> p_attr n v)
            [ ("hx-trigger", "click")
            ; ("hx-trigger", "keyup changed delay:500ms")
            ; ("hx-vals", {|{"a":1}|})
            ; ("hx-vals", " \t\n\x0c\r{}")
            ; ("hx-vals", "")
            ; ("hx-vals", "  ")
            ; ("hx-headers", {|{"X":"js:"}|})
            ; ("hx-request", {|{"timeout":100}|})
            ; ("hx-request", " ")
            ; ("data-hx-vals", "{}")
            ; ("x-trigger", "[")
            ; ("hx-valsx", "js:") ] )
    ; test "At.unsafe permits the script names" (fun () ->
          equal string {|<p onclick="go(&#39;x&#39;)" hx-vals="js:{}"></p>|}
            (render
               (El.p
                  ~at:
                    [At.unsafe "onclick" "go('x')"; At.unsafe "hx-vals" "js:{}"]
                  [] ) ) )
    ; test "bad attribute names" (fun () ->
          List.iter
            (fun n ->
              invalid ~msg:(String.escaped n) (fun () -> At.v n "x") ;
              invalid ~msg:(String.escaped n) (fun () -> At.unsafe n "x") ;
              invalid ~msg:(String.escaped n) (fun () -> At.bool n) )
            [ ""
            ; "a b"
            ; "a=b"
            ; "a\"b"
            ; "a'b"
            ; "a<b"
            ; "a>b"
            ; "a/b"
            ; "a&b"
            ; "a\x00"
            ; "a\x7f"
            ; "\xC3\xA9"
            ; "a\tb" ] ;
          List.iter (fun n -> p_attr n "x") ["!"; "~"; "x-on:click"; "a.b"; "_x"] )
    ; test "element names" (fun () ->
          List.iter
            (fun n -> invalid ~msg:n (fun () -> El.element n []))
            [ ""
            ; "1x"
            ; "-x"
            ; "a b"
            ; "a_b"
            ; "a:b"
            ; "\xC3\xA9"
            ; "br"
            ; "BR"
            ; "Img"
            ; "wbr"
            ; "param"
            ; "KEYGEN"
            ; "basefont"
            ; "bgsound"
            ; "frame"
            ; "image"
            ; "script"
            ; "Script"
            ; "STYLE"
            ; "xmp"
            ; "iframe"
            ; "noembed"
            ; "noframes"
            ; "plaintext"
            ; "Title"
            ; "textarea" ] ;
          equal string "<my-el></my-el><clipPath></clipPath><x1></x1>"
            (render
               (H.list
                  [ El.element "my-el" []
                  ; El.element "clipPath" []
                  ; El.element "x1" [] ] ) ) )
    ; test "void" (fun () ->
          List.iter
            (fun n -> invalid ~msg:n (fun () -> El.void n ()))
            ["div"; "param"; ""; "brx"; "b"] ;
          equal string {|<BR><img alt="a">|}
            (render
               (H.list [El.void "BR" (); El.void "img" ~at:[At.alt "a"] ()]) ) ;
          invalid (fun () -> El.void "br" ~at:[At.id "a"; At.id "b"] ()) ;
          (* The functions with no attributes, and with an empty list. *)
          equal string "<br><br><x-y></x-y><p></p>"
            (render
               (H.list
                  [ El.br ()
                  ; El.br ~at:[] ()
                  ; El.element "x-y" ~at:[] []
                  ; El.p ~at:[] [] ] ) ) ;
          (* The exception names the function. *)
          match El.img ~at:[At.id "a"; At.id "b"] () with
          | _ ->
              fail "no Invalid_argument"
          | exception Invalid_argument m ->
              equal string {|Caravan_html.El.img: "id": a duplicate attribute|}
                m ) ]

let duplicate_tests =
  group "Duplicates"
    [ test "class values join from all sources" (fun () ->
          equal string {|<div class="a b c d e" id="x"></div>|}
            (render
               (El.div
                  ~at:
                    [ At.class_ "a"
                    ; At.v "CLASS" "b"
                    ; At.class_ ""
                    ; At.none
                    ; At.id "x"
                    ; At.unsafe "Class" "c"
                    ; At.classes ["d"; ""; "e"] ]
                  [] ) ) ;
          equal string {|<div id="x" class="a"></div>|}
            (render (El.div ~at:[At.id "x"; At.class_ "a"] [])) ;
          equal string {|<div class="&lt;"></div>|}
            (render (El.div ~at:[At.class_ "<"] [])) )
    ; test "empty class values drop" (fun () ->
          equal string "<div></div>" (render (El.div ~at:[At.class_ ""] [])) ;
          equal string "<div></div>"
            (render (El.div ~at:[At.classes [""; ""]] [])) ;
          equal string "<div></div>"
            (render (El.div ~at:[At.classes []; At.none] [])) ;
          equal string {|<div id="a"></div>|}
            (render (El.div ~at:[At.class_ ""; At.id "a"; At.class_ ""] [])) )
    ; test "other duplicates raise" (fun () ->
          invalid (fun () -> El.div ~at:[At.id "a"; At.v "ID" "b"] []) ;
          invalid (fun () -> El.div ~at:[At.bool "hidden"; At.hidden] []) ;
          invalid (fun () ->
              El.div ~at:[At.id "a"; At.class_ "c"; At.none; At.id "a"] [] ) ;
          invalid (fun () ->
              El.element "x-y" ~at:[At.data "a" "1"; At.unsafe "DATA-A" "2"] [] ) ;
          invalid (fun () -> El.json_script ~at:[At.type_ "x"] "{}") ;
          invalid (fun () -> El.script ~at:[At.src "/a"; At.src "/b"] "") ;
          invalid (fun () -> El.title ~at:[At.id "a"; At.id "a"] "") ;
          invalid (fun () -> El.textarea ~at:[At.id "a"; At.id "a"] "") ;
          invalid (fun () -> El.iframe ~at:[At.id "a"; At.id "a"] ()) ) ]

let raw_text_tests =
  group "Raw text"
    [ test "script and style refuse texts that close them" (fun () ->
          List.iter
            (fun t ->
              invalid ~msg:(String.escaped t) (fun () -> El.script t) ;
              invalid ~msg:(String.escaped t) (fun () -> El.style t) )
            [ "x</script>"
            ; "x</SCRIPT"
            ; "x</noscript>"
            ; "x</style"
            ; "</a"
            ; "x</Z"
            ; "a<!--"
            ; "<!--"
            ; "a\x00"
            ; "a\x1b" ] ;
          List.iter
            (fun t -> invalid ~msg:t (fun () -> El.style t))
            ["a<img"; "a<Z"; "a<!x"; "a<!"; "a<?x"; "<?"] )
    ; test "permitted texts, with no escape" (fun () ->
          equal string
            "<script>if (a < b && c > d) { s = \"</\" + 1; t = '<!-' }</script>"
            (render
               (El.script "if (a < b && c > d) { s = \"</\" + 1; t = '<!-' }") ) ;
          equal string "<script><\\/script></script>"
            (render (El.script "<\\/script>")) ;
          equal string "<script>a<!</script>" (render (El.script "a<!")) ;
          equal string "<script>a<!-</script>" (render (El.script "a<!-")) ;
          equal string "<script>a</</script>" (render (El.script "a</")) ;
          equal string "<script>a</1</script>" (render (El.script "a</1")) ;
          equal string "<script>a<img</script>" (render (El.script "a<img")) ;
          equal string
            "<style>@media (width < 600px) { a > b { c: 'x' } }</style>"
            (render (El.style "@media (width < 600px) { a > b { c: 'x' } }")) ;
          equal string "<style>a<</style>" (render (El.style "a<")) ;
          equal string "<style>a< b</style>" (render (El.style "a< b")) ;
          equal string {|<script src="/a.js" defer></script>|}
            (render (El.script ~at:[At.src "/a.js"; At.bool "defer"] "")) )
    ; test "svg and math refuse a script or style child at any depth" (fun () ->
          invalid (fun () -> El.svg [El.style "x"]) ;
          invalid (fun () -> El.math [El.script "x"]) ;
          invalid (fun () ->
              El.svg [El.element "g" [H.list [H.empty; El.script "x"]]] ) ;
          invalid (fun () -> El.element "MATH" [El.div [El.script "x"]]) ;
          invalid (fun () -> El.element "Svg" [El.json_script "{}"]) ;
          equal string "<svg><g><p>x</p></g></svg>"
            (render (El.svg [El.element "g" [El.p [H.txt "x"]]])) ;
          equal string "<div><svg></svg><script>x</script></div>"
            (render (El.div [El.svg []; El.script "x"])) ;
          equal string "<svg><x></svg>" (render (El.svg [H.unsafe_raw "<x>"])) ;
          (* Only script and style are raw: an SVG title is permitted. *)
          equal string
            "<svg><title>t</title><br><textarea>\n\
             </textarea><iframe></iframe></svg>"
            (render
               (El.svg
                  [El.title "t"; El.void "br" (); El.textarea ""; El.iframe ()] ) ) )
    ; test "json_script" (fun () ->
          equal string
            {|<script type="application/json">{"a":"\u003c/script\u003e\u003c!--\u0026\u0000\u001b"}</script>|}
            (render (El.json_script "{\"a\":\"</script><!--&\x00\x1b\"}")) ;
          equal string {|<script type="application/json" id="d">[1]</script>|}
            (render (El.json_script ~at:[At.id "d"] "[1]")) )
    ; test "property: json_script output has no special byte" (fun () ->
          let rng = Random.State.make [|3|] in
          let alphabet = "a<>&\x00\x1b\"\\/!-" in
          for _ = 1 to 2000 do
            let s =
              String.init (Random.State.int rng 16) (fun _ ->
                  alphabet.[Random.State.int rng (String.length alphabet)] )
            in
            let out = render (El.json_script s) in
            let pre = {|<script type="application/json">|} in
            let body =
              String.sub out (String.length pre)
                ( String.length out - String.length pre
                - String.length "</script>" )
            in
            String.iter
              (fun c ->
                is_true ~msg:(String.escaped s)
                  (not (String.contains "<>&\x00\x1b" c)) )
              body
          done ) ]

let lf_tests =
  group "Leading LF"
    [ test "textarea, pre and listing write an LF after the start tag" (fun () ->
          equal string "<textarea>\n\nabc</textarea>"
            (render (El.textarea "\nabc")) ;
          equal string "<textarea name=\"t\">\na&lt;b</textarea>"
            (render (El.textarea ~at:[At.name "t"] "a<b")) ;
          equal string "<pre>\n\nx</pre>" (render (El.pre [H.txt "\nx"])) ;
          equal string "<PRE class=\"c\">\n</PRE>"
            (render (El.element "PRE" ~at:[At.class_ "c"] [])) ;
          equal string "<listing>\nx</listing>"
            (render (El.element "listing" [H.txt "x"])) ;
          equal string "<p>x</p>" (render (El.p [H.txt "x"])) ) ]

let elements : (string * (?at:H.attr list -> H.t list -> H.t)) list =
  [ ("html", El.html)
  ; ("head", El.head)
  ; ("body", El.body)
  ; ("header", El.header)
  ; ("footer", El.footer)
  ; ("main", El.main)
  ; ("nav", El.nav)
  ; ("section", El.section)
  ; ("article", El.article)
  ; ("aside", El.aside)
  ; ("h1", El.h1)
  ; ("h2", El.h2)
  ; ("h3", El.h3)
  ; ("h4", El.h4)
  ; ("h5", El.h5)
  ; ("h6", El.h6)
  ; ("hgroup", El.hgroup)
  ; ("address", El.address)
  ; ("search", El.search)
  ; ("p", El.p)
  ; ("div", El.div)
  ; ("span", El.span)
  ; ("a", El.a)
  ; ("ul", El.ul)
  ; ("ol", El.ol)
  ; ("li", El.li)
  ; ("dl", El.dl)
  ; ("dt", El.dt)
  ; ("dd", El.dd)
  ; ("menu", El.menu)
  ; ("strong", El.strong)
  ; ("em", El.em)
  ; ("b", El.b)
  ; ("i", El.i)
  ; ("u", El.u)
  ; ("s", El.s)
  ; ("small", El.small)
  ; ("mark", El.mark)
  ; ("code", El.code)
  ; ("kbd", El.kbd)
  ; ("samp", El.samp)
  ; ("blockquote", El.blockquote)
  ; ("q", El.q)
  ; ("cite", El.cite)
  ; ("abbr", El.abbr)
  ; ("time", El.time)
  ; ("sub", El.sub)
  ; ("sup", El.sup)
  ; ("ins", El.ins)
  ; ("del", El.del)
  ; ("figure", El.figure)
  ; ("figcaption", El.figcaption)
  ; ("picture", El.picture)
  ; ("video", El.video)
  ; ("audio", El.audio)
  ; ("object", El.object_)
  ; ("canvas", El.canvas)
  ; ("map", El.map)
  ; ("svg", El.svg)
  ; ("math", El.math)
  ; ("table", El.table)
  ; ("caption", El.caption)
  ; ("thead", El.thead)
  ; ("tbody", El.tbody)
  ; ("tfoot", El.tfoot)
  ; ("tr", El.tr)
  ; ("th", El.th)
  ; ("td", El.td)
  ; ("colgroup", El.colgroup)
  ; ("form", El.form)
  ; ("button", El.button)
  ; ("label", El.label)
  ; ("select", El.select)
  ; ("option", El.option)
  ; ("optgroup", El.optgroup)
  ; ("fieldset", El.fieldset)
  ; ("legend", El.legend)
  ; ("datalist", El.datalist)
  ; ("output", El.output)
  ; ("progress", El.progress)
  ; ("meter", El.meter)
  ; ("details", El.details)
  ; ("summary", El.summary)
  ; ("dialog", El.dialog)
  ; ("template", El.template)
  ; ("slot", El.slot)
  ; ("noscript", El.noscript) ]

let voids : (string * (?at:H.attr list -> unit -> H.t)) list =
  [ ("area", El.area)
  ; ("base", El.base)
  ; ("br", El.br)
  ; ("col", El.col)
  ; ("embed", El.embed)
  ; ("hr", El.hr)
  ; ("img", El.img)
  ; ("input", El.input)
  ; ("link", El.link)
  ; ("meta", El.meta)
  ; ("source", El.source)
  ; ("track", El.track)
  ; ("wbr", El.wbr) ]

let element_tests =
  group "Elements"
    [ test "each element function renders its own tag" (fun () ->
          List.iter
            (fun (n, f) ->
              equal string ~msg:n
                (Printf.sprintf {|<%s id="i"><b>x</b>y</%s>|} n n)
                (render
                   (f ?at:(Some [At.id "i"]) [El.b [H.txt "x"]; H.txt "y"]) ) ;
              equal string ~msg:n
                (Printf.sprintf "<%s></%s>" n n)
                (render (f ?at:None [])) )
            elements ;
          equal string "<pre>\n</pre>" (render (El.pre [])) )
    ; test "each void function renders its own tag" (fun () ->
          List.iter
            (fun (n, f) ->
              equal string ~msg:n
                (Printf.sprintf {|<%s id="i">|} n)
                (render (f ?at:(Some [At.id "i"]) ())) ;
              equal string ~msg:n (Printf.sprintf "<%s>" n)
                (render (f ?at:None ())) )
            voids )
    ; test "the attribute helpers" (fun () ->
          equal string
            {|<input id="i" name="n" value="v" type="t" alt="a" title="ti" lang="l" rel="r" placeholder="p" style="s" method="m" for="f" checked disabled selected required hidden multiple readonly autofocus>|}
            (render
               (El.input
                  ~at:
                    [ At.id "i"
                    ; At.name "n"
                    ; At.value "v"
                    ; At.type_ "t"
                    ; At.alt "a"
                    ; At.title "ti"
                    ; At.lang "l"
                    ; At.rel "r"
                    ; At.placeholder "p"
                    ; At.style "s"
                    ; At.method_ "m"
                    ; At.for_ "f"
                    ; At.checked
                    ; At.disabled
                    ; At.selected
                    ; At.required
                    ; At.hidden
                    ; At.multiple
                    ; At.readonly
                    ; At.autofocus ]
                  () ) ) ;
          equal string
            {|<a href="/x"></a><img src="/y"><form action="/z"></form>|}
            (render
               (H.list
                  [ El.a ~at:[At.href "/x"] []
                  ; El.img ~at:[At.src "/y"] ()
                  ; El.form ~at:[At.action "/z"] [] ] ) ) )
    ; test "nodes" (fun () ->
          equal string "<!DOCTYPE html>" (render H.doctype) ;
          equal string "3 &lt;b&gt;" (render (H.txtf "%d %s" 3 "<b>")) ;
          equal string "ab"
            (render (H.list [H.txt "a"; H.empty; H.list []; H.txt "b"])) ;
          equal string "" (render H.empty) ;
          equal string "<b>&</b>" (render (H.unsafe_raw "<b>&</b>")) ;
          equal string "<title>a&lt;/title&gt;</title>"
            (render (El.title "a</title>")) ;
          equal string {|<title id="t"></title>|}
            (render (El.title ~at:[At.id "t"] "")) ;
          equal string {|<iframe src="/f"></iframe>|}
            (render (El.iframe ~at:[At.src "/f"] ())) ;
          let b = Buffer.create 4 in
          Buffer.add_string b "pre:" ;
          H.add_to_buffer b (El.i [H.txt "x"]) ;
          H.add_to_buffer b (El.i [H.txt "y"]) ;
          equal string "pre:<i>x</i><i>y</i>" (Buffer.contents b) ;
          let big = El.ul (List.init 5000 (fun i -> El.li [H.txtf "%d" i])) in
          (* Larger than the first buffer of to_string. *)
          equal string
            ( "<ul>"
            ^ String.concat "" (List.init 5000 (Printf.sprintf "<li>%d</li>"))
            ^ "</ul>" )
            (render big) ) ]

let response_tests =
  group "Response"
    [ test "response" (fun () ->
          let r = H.response (El.p [H.txt "x"]) in
          equal int 200 (Status.code (Response.status r)) ;
          equal (option string) (Some "text/html; charset=utf-8")
            (Headers.find "content-type" (Response.headers r)) ;
          let r =
            H.response ~status:Status.not_found
              ~headers:
                (Headers.of_list [("Content-Type", "text/plain"); ("X-A", "1")])
              H.empty
          in
          equal int 404 (Status.code (Response.status r)) ;
          equal (option string) (Some "text/html; charset=utf-8")
            (Headers.find "content-type" (Response.headers r)) ;
          equal (option string) (Some "1")
            (Headers.find "x-a" (Response.headers r)) )
    ; test "a page at the client" (fun () ->
          let page = H.(list [doctype; El.p [txt "<hi>"]]) in
          Caravan_testing.run
            (fun _ -> H.response page)
            (fun t ->
              let r = Caravan_testing.get t "/" in
              equal int 200 (Status.code r.status) ;
              equal (option string) (Some "text/html; charset=utf-8")
                (Headers.find "content-type" r.headers) ;
              equal string "<!DOCTYPE html><p>&lt;hi&gt;</p>" r.body ) ) ]

let () =
  exit
    (Windtrap.run "html"
       [ escape_tests
       ; url_tests
       ; names_tests
       ; duplicate_tests
       ; raw_text_tests
       ; lf_tests
       ; element_tests
       ; response_tests ] )
