(lang dune 3.20)

(name affect-http)

(generate_opam_files true)

(authors "Bikal Gurung <gbikal@gmail.com>")

(maintainers "Bikal Gurung <gbikal@gmail.com>")

(license MPL-2.0)

(package
 (name affect-http)
 (synopsis "HTTP/1.1 server and client library built on affect")
 (description
  "affect-http is an HTTP/1.1 server and client library using the affect concurrency model.")
 (depends
  (ocaml
   (>= 5.5.0))
  (affect
   (>= 0.0.0))
  (odoc :with-doc)))
