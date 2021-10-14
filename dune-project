(lang dune 2.9)
(using menhir 2.0)
(name ocaml_of_michelson)
(version 0.0.0)
(generate_opam_files true)
(cram enable)

(source (github aigarashi/ocaml_of_michelson))
(license MIT)
(authors "Atsushi Igarashi <igarashi@kuis.kyoto-u.ac.jp>")
(maintainers "Atsushi Igarashi <igarashi@kuis.kyoto-u.ac.jp>")

(package
 (name ocaml_of_michelson)
 (synopsis "A toy translater from Michelson to OCaml")
 (description "A toy translater from Michelson to OCaml")
 (depends (menhir :build)
          (cppo :build))
)
