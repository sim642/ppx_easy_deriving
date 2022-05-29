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

module Unit: Lattice =
struct
  type t = unit [@@deriving lattice, easy_equal, easy_equal2]
end

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
  [@@deriving easy_equal]
end

module PolyVariant (L1: Lattice) (L2: Lattice) (L5: Lattice) =
struct
  type t = [
    | `C1
    | `C2 of L1.t * L2.t
    | `C4 of L5.t
  ] [@@deriving easy_equal]
end
