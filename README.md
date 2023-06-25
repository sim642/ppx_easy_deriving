# ppx_easy_deriving

[![ci workflow status](https://github.com/sim642/ppx_easy_deriving/actions/workflows/ci.yml/badge.svg)](https://github.com/sim642/ppx_easy_deriving/actions/workflows/ci.yml)
[![GitHub release status](https://img.shields.io/github/v/release/sim642/ppx_easy_deriving)](https://github.com/sim642/ppx_easy_deriving/releases)
[![opam package status](https://badgen.net/opam/v/ppx_easy_deriving)](https://opam.ocaml.org/packages/ppx_easy_deriving)

Easily define PPX derivers without boilerplate and runtime overhead.


## Installation
```console
opam install ppx_easy_deriving
```

## Other libraries

* [ppx_type_directed_value](https://github.com/janestreet/ppx_type_directed_value) — has runtime overhead on every call of derived function.
* [refl](https://github.com/thierry-martinez/refl) — requires runtime representations of types.
