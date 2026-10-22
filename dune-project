(lang dune 3.20)

(name caravan)

(generate_opam_files true)

(source
 (github bikallem/caravan))

(authors "Bikal Gurung <gbikal@gmail.com>")

(maintainers "Bikal Gurung <gbikal@gmail.com>")

(license MPL-2.0)

(package
 (name caravan)
 (synopsis "HTTP/1.1 server, client and WebSocket built on weft")
 (description
  "Caravan is an HTTP/1.1 server, client and WebSocket library for OCaml 5, built on the weft concurrency library. It streams bodies, shuts down gracefully, and gives connections to WebSocket and CONNECT tunnels. The design leaves room for HTTP/2, HTTP/3 and a reverse proxy.")
 (depends
  (ocaml
   (>= 5.5.0))
  (http-date
   (>= 0.2))
  weft
  (windtrap
   (and
    :with-test
    (>= 0.2.0)))
  (ppx_windtrap
   (and
    :with-test
    (>= 0.2.0)))
  (odoc :with-doc)))
