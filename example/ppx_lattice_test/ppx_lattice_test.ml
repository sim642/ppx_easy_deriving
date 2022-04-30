module type Leq =
sig
  type t
  val leq: t -> t -> bool
  val join: t -> t -> t
end

module Prod (L1: Leq) (L2: Leq) (L3: Leq): Leq =
struct
  type t = L1.t * L2.t * L3.t [@@deriving leq, join]
  (* type t = L1.t * L2.t [@@deriving leq] *)
end
