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
 (synopsis "A web programming library for OCaml 5, built on weft")
 (description
  "Caravan is a web programming library for OCaml 5, built on the weft concurrency library. It has an HTTP/1.1 server, client and WebSocket, and a framework layer similar to ASP.NET Core: routing, middleware, sessions, authentication, compression, static files, TLS and a test host that runs in the weft simulator. HTTP/2, HTTP/3 and a reverse proxy are not there yet.")
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
  (odoc :with-doc)
  (mirage-crypto-rng :with-test)
  (mirage-crypto-ec :with-test)
  (ptime :with-test)
  (tls
   (and
    :with-test
    (>= 2.1)))
  (weft-tls :with-test)
  (digestif :with-test)
  (eqaf :with-test)
  (mirage-crypto-pk :with-test)
  (yojson :with-test)
  (zarith :with-test)
  (decompress :with-test))
 (depopts
  ; caravan.tls
  weft-tls
  (tls
   (>= 2.1))
  x509
  domain-name
  mirage-crypto-rng
  ; caravan.session
  digestif
  eqaf
  ; caravan.auth
  mirage-crypto-pk
  mirage-crypto-ec
  yojson
  zarith
  ; caravan.compress
  decompress)
 (conflicts
  (tls
   (< 2.1))))
