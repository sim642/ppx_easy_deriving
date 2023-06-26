open Ppxlib

module type Base =
sig
  include Intf.Base
  val apply_iso: loc:location -> expression -> f:expression -> f':expression -> expression
  (** Apply isomorphism to derived value/function.

      @param f isomorphism from given to target type.
      @param f' inverse isomorphism from target to given type. *)
end

module Product =
struct
  module type S =
  sig
    include Base
    val unit: loc:location -> expression
    (** Derived value/function for [unit] type. *)

    val both: loc:location -> expression -> expression -> expression
    (** Compose derived values/functions in a product into derived value/function for the pair. *)
  end
end

module Variant =
struct
  module type S =
  sig
    include Product.S
    val empty: loc:location -> expression
    (** Derived value/function for the empty type. *)

    val either: loc:location -> expression -> expression -> expression
    (** Compose derived values/functions in a variant into derived value/function for the either. *)
  end
end

module type Simple =
sig

  (** Simple product derivers. *)
  module Product :
  sig
    module type S = Product.S
    (** Simple product deriver interface. *)

    module Reduce (P: S): Intf.S
    (** Make deriver from simple product deriver by isomorphism and reduction. *)
  end

  (** Simple variant derivers. *)
  module Variant :
  sig
    module type S = Variant.S
    (** Simple variant deriver interface. *)

    module Reduce (V: S): Intf.S
    (** Make deriver from simple variant deriver by isomorphism and reduction. *)
  end
end
