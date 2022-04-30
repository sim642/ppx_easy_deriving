module type Lattice =
sig
  type t
  val leq: t -> t -> bool
  val join: t -> t -> t
  val bot: unit -> t
end

module Unit: Lattice =
struct
  type t = unit [@@deriving lattice]
end

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
