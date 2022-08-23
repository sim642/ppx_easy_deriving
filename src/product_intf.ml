open Ppxlib

module type S =
sig
  include Intf.Base
  val product: loc:location -> pe_create:(prefix:string -> PatExp.t) -> expression list -> expression
end

module Variant =
struct
  module type S =
  sig
    include S
    val variant: loc:location -> ((prefix:string -> PatExp.t) * (prefix:string -> PatExp.t) * expression * expression list) list -> expression
  end
end

module type Product =
sig
  module type S = S

  module Make (P: S): Intf.S

  module Variant :
  sig
    module type S = Variant.S

    module Make (PV: S): Intf.S
  end
end
