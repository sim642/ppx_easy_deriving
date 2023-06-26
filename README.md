# ppx_easy_deriving

[![ci workflow status](https://github.com/sim642/ppx_easy_deriving/actions/workflows/ci.yml/badge.svg)](https://github.com/sim642/ppx_easy_deriving/actions/workflows/ci.yml)
[![GitHub release status](https://img.shields.io/github/v/release/sim642/ppx_easy_deriving)](https://github.com/sim642/ppx_easy_deriving/releases)
[![opam package status](https://badgen.net/opam/v/ppx_easy_deriving)](https://opam.ocaml.org/packages/ppx_easy_deriving)

Easily define PPX derivers without boilerplate and runtime overhead.


## Installation
```console
opam install ppx_easy_deriving
```

## Examples

### Equal
The following shows multiple ways of defining a PPX deriver for a standard `equal` function with various trade-offs.

#### Simple
The simplest way is to define `equal` (here called `easy_equal2`) for binary product and sum types, i.e. pairs and eithers.
Using an isomorphism, all algebraic datatypes can be handled using the simple binary constructs and their base cases (unit and empty type).
This is very similar to the simple derivers of [ppx_type_directed_value](https://github.com/janestreet/ppx_type_directed_value), but with lower runtime overhead.

<!-- $MDX file=example/ppx_easy_equal/ppx_easy_equal.ml,part=easy_equal2 -->
```ocaml
module EasyEqual2Arg: Simple.Variant.S =
struct
  (** Name of deriver. *)
  let name = "easy_equal2"

  (** Type of derived [easy_equal2] function for type [t]. *)
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]

  (** [easy_equal2] function for [unit] type. *)
  let unit ~loc = [%expr fun () () -> true]

  (** [easy_equal2] function for a pair of types with given [easy_equal2] functions [e1] and [e2]. *)
  let both ~loc e1 e2 = [%expr fun (l1, l2) (r1, r2) -> [%e e1] l1 r1 && [%e e2] l2 r2]

  (** [easy_equal2] function for the empty type. *)
  let empty ~loc = [%expr fun _ _ -> true]

  (** [easy_equal2] function for an either of types with given [easy_equal2] functions [e1] and [e2]. *)
  let either ~loc e1 e2 =
    [%expr fun l r ->
      match l, r with
      | Either.Left l, Either.Left r -> [%e e1] l r
      | Either.Right l, Either.Right r -> [%e e2] l r
      | _, _ -> false
    ]

  (** Apply isomorphism to given [easy_equal2] function.
      This allows {!both} and {!either} to be applied to tuples, records and (polymorphic) variants of arbitrary size. *)
  let apply_iso ~loc easy_equal2 ~f ~f':_ =
    [%expr fun l r -> [%e easy_equal2] ([%e f] l) ([%e f] r)]
end

(* Create full deriver from simple deriver and register it with ppxlib. *)
module EasyEqual2Deriver = Deriver.Make (Simple.Variant.Reduce (EasyEqual2Arg))
let _ = EasyEqual2Deriver.register ()
```

#### Product
The isomorphisms of simple deriver definitions still incur some runtime overhead.
A more efficient way to define `equal` (here called `easy_equal`) is to define it for entire products and sums in one go, instead of decomposing them to binary reductions.
These definitions are more involved, especially `variant` since it also handles inline records in constructor arguments.
However, the deriver defined this way is as efficient as a dedicated `equal` deriver, e.g. from [ppx_deriving.eq](https://github.com/ocaml-ppx/ppx_deriving#plugins-eq-and-ord) or [ppx_compare](https://github.com/janestreet/ppx_compare).

<!-- $MDX file=example/ppx_easy_equal/ppx_easy_equal.ml,part=easy_equal -->
```ocaml
module EasyEqualArg: Product.Variant.S =
struct
  (** Name of deriver. *)
  let name = "easy_equal"

  (** Type of derived [easy_equal] function for type [t]. *)
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]

  (** [easy_equal] function body for a product type with list of given [easy_equal] functions for the elements. *)
  let product_body ~loc es pel per =
    let body =
      let esl = Pat_exp.to_exps ~loc pel in
      let esr = Pat_exp.to_exps ~loc per in
      Util.map3 (fun e l r ->
          (* Apply [easy_equal] function per element. *)
          [%expr [%e e] [%e l] [%e r]]
        ) es esl esr
    in
    (* Reduce per-element [easy_equal] results to a result for the entire product. *)
    Util.reduce ~unit:[%expr true] ~both:(fun acc x ->
        [%expr [%e acc] && [%e x]]
      ) body

  (** [easy_equal] function for a product type with list of given [easy_equal] functions for the elements.
      [pe_create] allows creating patterns and expressions of the actual product type. *)
  let product ~loc ~pe_create es =
    let pel = pe_create ~prefix:"l" in
    let per = pe_create ~prefix:"r" in
    let body = product_body ~loc es pel per in
    let pl = Pat_exp.to_pat ~loc pel in
    let pr = Pat_exp.to_pat ~loc per in
    [%expr fun [%p pl] [%p pr] -> [%e body]]

  (** [easy_equal] function for a (polymorphic) variant type with list of given [easy_equal] functions for the (polymorphic) variant constructors. *)
  let variant ~loc ces =
    let cases = List.map (fun (c, c2, _es, es2) ->
        let pel = c ~prefix:"l" in
        let per = c ~prefix:"r" in
        let pel2 = c2 ~prefix:"l" in
        let per2 = c2 ~prefix:"r" in
        let body = product_body ~loc es2 pel2 per2 in
        let pa = Pat_exp.to_pat ~loc pel in
        let pb = Pat_exp.to_pat ~loc per in
        (* Equality case for one constructor with given product argument. *)
        case ~lhs:[%pat? [%p pa], [%p pb]]
          ~guard:None
          ~rhs:body
      ) ces
    in
    let fallback =
      (* Inequality case as fallback. *)
      case ~lhs:[%pat? _, _]
        ~guard:None
        ~rhs:[%expr false]
    in
    let body = pexp_match ~loc [%expr l, r] (cases @ [fallback]) in
    [%expr fun l r -> [%e body]]
end

(* Create full deriver from product deriver and register it with ppxlib. *)
module EasyEqualDeriver = Deriver.Make (Product.Variant.Make (EasyEqualArg))
let _ = EasyEqualDeriver.register ()
```

#### Performance
These two easy derivers are benchmarked against [ppx_deriving.eq](https://github.com/ocaml-ppx/ppx_deriving#plugins-eq-and-ord), [ppx_compare](https://github.com/janestreet/ppx_compare), [ppx_type_directed_value](https://github.com/janestreet/ppx_type_directed_value) and [refl](https://github.com/thierry-martinez/refl).
The benchmark involves an equality check on a 60-field record where only the last fields differ.

The table shows the product version (ppx_easy_deriving) being on par with ppx_deriving and ppx_compare.
The simple version (ppx_easy_deriving2) is notably slower but still faster than ppx_type_directed_value (ppx_type_directed_equal) and refl.

|                         |       Rate |  refl | ppx_type_directed_equal | ppx_easy_deriving2 | ppx_deriving | ppx_compare | ppx_easy_deriving |
| -----------------------:| ----------:| -----:| -----------------------:| ------------------:| ------------:| -----------:| -----------------:|
|                    refl |   605433/s |    -- |                    -73% |               -91% |         -97% |        -97% |              -97% |
| ppx_type_directed_equal |  2245269/s |  271% |                      -- |               -65% |         -90% |        -90% |              -90% |
|      ppx_easy_deriving2 |  6412029/s |  959% |                    186% |                 -- |         -71% |        -72% |              -72% |
|            ppx_deriving | 22347280/s | 3591% |                    895% |               249% |           -- |         -1% |               -1% |
|             ppx_compare | 22595512/s | 3632% |                    906% |               252% |           1% |          -- |               -0% |
|       ppx_easy_deriving | 22639836/s | 3639% |                    908% |               253% |           1% |          0% |                -- |

See the full benchmark in [`example/ppx_easy_equal_bench/`](./example/ppx_easy_equal_bench/).

### Lattice
[`example/ppx_lattice/`](./example/ppx_lattice/) contains examples of additional product deriver helper functors by defining a deriver for product lattices with the following signature:
<!-- $MDX file=example/ppx_lattice_test/ppx_lattice_test.ml,part=lattice -->
```ocaml
module type Lattice =
sig
  type t

  (* reduce2-like function *)
  val leq: t -> t -> bool (* reduce by conjunction *)

  (* map2-like function *)
  val join: t -> t -> t

  (* create-like function *)
  val bot: unit -> t

  (* reduce-like function *)
  val is_bot: t -> bool (* reduce by conjunction *)

  (* map-like function *)
  val relift: t -> t (* not really a lattice operation *)
end
```

## Other libraries

* [ppx_type_directed_value](https://github.com/janestreet/ppx_type_directed_value) — has runtime overhead on every call of derived function.
* [refl](https://github.com/thierry-martinez/refl) — requires runtime representations of types.
