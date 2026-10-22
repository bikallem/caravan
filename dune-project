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
 (synopsis "HTTP server built on affect, growing into a proxy")
 (description
  "Caravan is an HTTP/1.1 server for OCaml 5 built on the affect concurrency model. It streams bodies, shuts down gracefully, and hands connections over for WebSocket and CONNECT tunnels; it is designed to grow into HTTP/2, HTTP/3 and a reverse proxy.")
 (depends
  (ocaml
   (>= 5.5.0))
  (affect
   (>= 0.0.0))
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
