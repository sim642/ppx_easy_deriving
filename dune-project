(lang dune 2.8)
(using mdx 0.1)

(name ppx_easy_deriving)

(generate_opam_files true)

(source (github sim642/ppx_easy_deriving))
(authors "Simmo Saan <simmo.saan@gmail.com>")
(maintainers "Simmo Saan <simmo.saan@gmail.com>")
(license MIT)

(package
 (name ppx_easy_deriving)
 (synopsis "Easily define PPX derivers without boilerplate and runtime overhead")
 (depends
  (ocaml (>= 4.08))
  (ppxlib (>= 0.30.0))
  ppx_deriving
  either
  (mdx :with-test)))

(package
 (name ppx_easy_deriving-bench)
 (synopsis "Benchmarks for ppx_easy_deriving")
 (depends
  (ppx_easy_deriving (= :version))
  (ppx_type_directed_value :with-test)
  (benchmark :with-test)
  (ppx_compare :with-test)))
