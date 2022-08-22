module type S =
sig
  val register: unit -> Ppxlib.Deriving.t
end

module type Deriver =
sig
  module type S = S

  module Make (_: Intf.S): S
end
