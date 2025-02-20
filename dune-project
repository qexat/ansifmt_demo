(lang dune 3.17)

(name ansifmt_demo)

(generate_opam_files true)

(source
 (github qexat/ansifmt_demo))

(authors "Qexat <contact@qexat.com>")

(maintainers "Qexat <contact@qexat.com>")

(license MIT)

(package
 (name ansifmt_demo)
 (synopsis
  "A very simple project that makes use of ansifmt's tokenization-based formatting.")
 (description
  "A very simple project that makes use of ansifmt's tokenization-based formatting.n")
 (depends
  ocaml
  (ansifmt
   (>= 0.3))))
