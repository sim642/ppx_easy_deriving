module type Lattice =
sig
  type t
  val leq: t -> t -> bool
  val join: t -> t -> t
  val bot: unit -> t
  val is_bot: t -> bool

  val easy_equal: t -> t -> bool
  val easy_equal2: t -> t -> bool
end

module type Lattice_derived =
sig
  type t [@@deriving lattice, easy_equal, easy_equal2]
end

module Lattice_derived_test1 (L: Lattice): Lattice_derived = L
module Lattice_derived_test2 (L: Lattice_derived): Lattice = L

module Unit: Lattice =
struct
  type t = unit [@@deriving lattice, easy_equal, easy_equal2]
end

let _ = [%is_bot: unit]
let _ = [%bot: unit]
let _ = [%join: unit]
let _ = [%leq: unit]
let _ = [%easy_equal: unit]
let _ = [%easy_equal2: unit]

module Direct (L1: Lattice): Lattice =
struct
  type t = L1.t [@@deriving lattice, easy_equal, easy_equal2]
end

module Tuple2 (L1: Lattice) (L2: Lattice): Lattice =
struct
  type t = L1.t * L2.t [@@deriving lattice, easy_equal, easy_equal2]
end

module Tuple3 (L1: Lattice) (L2: Lattice) (L3: Lattice): Lattice =
struct
  type t = L1.t * L2.t * L3.t [@@deriving lattice, easy_equal, easy_equal2]
end

module Record1 (L1: Lattice): Lattice =
struct
  type t = {
    f1: L1.t;
  } [@@deriving lattice, easy_equal, easy_equal2]
end

module Record2 (L1: Lattice) (L2: Lattice): Lattice =
struct
  type t = {
    f1: L1.t;
    f2: L2.t;
  } [@@deriving lattice, easy_equal, easy_equal2]
end

module Record3 (L1: Lattice) (L2: Lattice) (L3: Lattice): Lattice =
struct
  type t = {
    f1: L1.t;
    f2: L2.t;
    f3: L3.t;
  } [@@deriving lattice, easy_equal, easy_equal2]
end

(* TODO: move out of ppx_lattice_test *)
module Variant (L1: Lattice) (L2: Lattice) (L3: Lattice) (L4: Lattice) (L5: Lattice) =
struct
  type t =
    | C1
    | C2 of L1.t * L2.t
    | C3 of {f1: L3.t; f2: L4.t}
    | C4 of L5.t
  [@@deriving easy_equal, easy_equal2]
end

module PolyVariant (L1: Lattice) (L2: Lattice) (L5: Lattice) =
struct
  type t = [
    | `C1
    | `C2 of L1.t * L2.t
    | `C4 of L5.t
  ] [@@deriving easy_equal, easy_equal2]
end

module PolyVariantInherit (L1: Lattice) (L2: Lattice) (L5: Lattice) =
struct
  module PV = PolyVariant (L1) (L2) (L5)
  type u = PV.t [@@deriving easy_equal, easy_equal2]
  type t = [
    | PV.t
    | `C5
    | u (* actually same as PV.t, so this case should be last and unreachable *)
  ] [@@deriving easy_equal, easy_equal2]
end
