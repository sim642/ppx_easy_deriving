open Ppxlib

module type Base =
sig
  val name: string
  val typ: loc:location -> core_type -> core_type
end

module Tuple =
struct
  module type S =
  sig
    include Base
    val tuple: loc:location -> expression list -> expression
  end
end

module Record =
struct
  module type S =
  sig
    include Base
    val record: loc:location -> (longident * expression) list -> expression
  end
end

module Product =
struct
  module type S =
  sig
    include Base
    val product: loc:location -> pe_create:(prefix:string -> PatExp.t) -> expression list -> expression
  end
end

module Variant =
struct
  module type S =
  sig
    include Tuple.S
    include Record.S
    val variant: loc:location -> ((prefix:string -> PatExp.t) * (prefix:string -> PatExp.t) * expression * expression list) list -> expression
  end
end

module ProductVariant =
struct
  module type S =
  sig
    include Product.S
    val variant: loc:location -> ((prefix:string -> PatExp.t) * (prefix:string -> PatExp.t) * expression * expression list) list -> expression
  end
end

module type S = Variant.S
