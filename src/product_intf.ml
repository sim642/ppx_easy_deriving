open Ppxlib

module type S =
sig
  include Intf.Base
  val product: loc:location -> pe_create:(prefix:string -> PatExp.t) -> expression list -> expression
end

module Reduce1 =
struct
  module type S =
  sig
    include Intf.Base
    val unit: loc:location -> expression
    val both: loc:location -> expression -> expression -> expression
  end
end

module Reduce2 =
struct
  module type S =
  sig
    include Intf.Base
    val unit: loc:location -> expression
    val both: loc:location -> expression -> expression -> expression
  end
end

module Create =
struct
  module type S =
  sig
    include Intf.Base
  end
end

module Map2 =
struct
  module type S =
  sig
    val name: string
  end
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

  module Reduce1 :
  sig
    module type S = Reduce1.S

    module Make (R1: S): Intf.S
  end

  module Reduce2 :
  sig
    module type S = Reduce2.S

    module Make (R2: S): Intf.S
  end

  module Create :
  sig
    module type S = Create.S

    module Make (C: S): Intf.S
  end

  module Map2 :
  sig
    module type S = Map2.S

    module Make (M2: S): Intf.S
  end

  module Variant :
  sig
    module type S = Variant.S

    module Make (PV: S): Intf.S
  end
end
