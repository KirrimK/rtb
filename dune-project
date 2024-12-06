(lang dune 3.16)

(using menhir 3.0)

(name rtb)

(generate_opam_files true)

(source
 (github KirrimK/rtb))

(authors "Rémy B.")

(maintainers "Rémy B.")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name rtb)
 (synopsis "Rémy's Tiny Basic interpreter")
 (description "An interpreter for Tiny Basic made in OCaml")
 (depends
    ocaml
    dune
    (menhir (>= 3.0))
    dune-build-info
  )
 (tags
  (tiny-basic interpreter)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
