open Ppxlib

module type Base =
sig
  include Intf.Base
  val apply_iso: loc:location -> expression -> f:expression -> f':expression -> expression
end

module Product =
struct
  module type S =
  sig
    include Base
    val unit: loc:location -> expression
    val both: loc:location -> expression -> expression -> expression
  end
end

module Variant =
struct
  module type S =
  sig
    include Product.S
    val empty: loc:location -> expression
    val either: loc:location -> expression -> expression -> expression
  end
end

module type Simple =
sig
  module Product :
  sig
    module type S = Product.S

    module Reduce (P: S): Intf.S
  end

  module Variant :
  sig
    module type S = Variant.S

    module Reduce (V: S): Intf.S
  end
end
