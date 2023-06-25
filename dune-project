(lang dune 2.8)
(name ppx_easy_deriving)
(generate_opam_files true)
(source (github sim642/ppx_easy_deriving))
(authors "Simmo Saan <simmo.saan@gmail.com>")
(maintainers "Simmo Saan <simmo.saan@gmail.com>")

(package
 (name ppx_easy_deriving)
 (depends
  (ppxlib (>= 0.30.0))
  ppx_deriving
  either))

(package
 (name ppx_easy_deriving-bench)
 (depends
  (ppx_easy_deriving (= :version))
  (ppx_type_directed_value :with-test)
  (benchmark :with-test)
  (ppx_compare :with-test)))
