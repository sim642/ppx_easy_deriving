open Ppxlib

include Product_intf

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

module Variant =
struct
  include Variant

  module Make (PV: S): Intf.S =
  struct
    include Make (PV)

    let variant = PV.variant
  end
end
