module type Lattice =
sig
  type t
  val leq: t -> t -> bool
  val join: t -> t -> t
  val bot: unit -> t
  val is_bot: t -> bool
  val relift: t -> t
end

module type Lattice_derived =
sig
  type t [@@deriving lattice]
end

module Lattice_derived_test1 (L: Lattice): Lattice_derived = L
module Lattice_derived_test2 (L: Lattice_derived): Lattice = L

module Unit: Lattice =
struct
  type t = unit [@@deriving lattice]
end

let _ = [%is_bot: unit]
let _ = [%bot: unit]
let _ = [%join: unit]
let _ = [%leq: unit]

module Direct (L1: Lattice): Lattice =
struct
  type t = L1.t [@@deriving lattice]
end

module Tuple2 (L1: Lattice) (L2: Lattice): Lattice =
struct
  type t = L1.t * L2.t [@@deriving lattice]
end

module Tuple3 (L1: Lattice) (L2: Lattice) (L3: Lattice): Lattice =
struct
  type t = L1.t * L2.t * L3.t [@@deriving lattice]
end

module Record1 (L1: Lattice): Lattice =
struct
  type t = {
    f1: L1.t;
  } [@@deriving lattice]
end

module Record2 (L1: Lattice) (L2: Lattice): Lattice =
struct
  type t = {
    f1: L1.t;
    f2: L2.t;
  } [@@deriving lattice]
end

module Record3 (L1: Lattice) (L2: Lattice) (L3: Lattice): Lattice =
struct
  type t = {
    f1: L1.t;
    f2: L2.t;
    f3: L3.t;
  } [@@deriving lattice]
end
