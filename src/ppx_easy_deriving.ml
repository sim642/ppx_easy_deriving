open Ppxlib

module PatExp = PatExp
module Util = Util
module Simple = Simple

include Intf

(* module type Deriver = Deriver.S *)
module Deriver = Deriver.Make


module Product =
struct
  include Product

  module Make (P: S): Intf.S =
  struct
    include P

    let record ~loc les =
      let ls = List.map fst les in
      let es = List.map snd les in
      let pe_create ~prefix = PatExp.create_record ~prefix ls in
      P.product ~loc ~pe_create es

    let tuple ~loc es =
      let n = List.length es in
      let pe_create ~prefix = PatExp.create_tuple ~prefix n in
      P.product ~loc ~pe_create es

    let variant ~loc _ = Ast_builder.Default.pexp_extension ~loc (Location.error_extensionf ~loc "Product.Make no variant")
  end
end

module ProductVariant =
struct
  include ProductVariant

  module Make (PV: S): Intf.S =
  struct
    include Product.Make (PV)

    let variant = PV.variant
  end
end
