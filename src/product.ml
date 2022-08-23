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

module Reduce1 =
struct
  include Reduce1

  module Make (R1: S): Intf.S =
  struct
    module P: Simple.Product.S =
    struct
      let name = R1.name
      let typ ~loc t = [%type: [%t t] -> [%t R1.typ ~loc t]]
      let unit ~loc = [%expr fun () -> [%e R1.unit ~loc]]
      let both ~loc e1 e2 = [%expr fun (a, b) -> [%e R1.both ~loc [%expr [%e e1] a] [%expr [%e e2] b]]]
      let apply_iso ~loc e f _ =
        [%expr fun a -> [%e e] ([%e f] a)]
    end

    include Simple.Product.Reduce (P)
  end
end

module Reduce2 =
struct
  include Reduce2

  module Make (R2: S): Intf.S =
  struct
    module P: Simple.Product.S =
    struct
      let name = R2.name
      let typ ~loc t = [%type: [%t t] -> [%t t] -> [%t R2.typ ~loc t]]
      let unit ~loc = [%expr fun () () -> [%e R2.unit ~loc]]
      let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> [%e R2.both ~loc [%expr [%e e1] a1 a2] [%expr [%e e2] b1 b2]]]
      let apply_iso ~loc e f _ =
        [%expr fun a b -> [%e e] ([%e f] a) ([%e f] b)]
    end

    include Simple.Product.Reduce (P)
  end
end

module Create =
struct
  include Create

  module Make (C: S): Intf.S =
  struct
    module P: Simple.Product.S =
    struct
      let name = C.name
      let typ ~loc t = [%type: [%t C.typ ~loc t] -> [%t t]]
      let unit ~loc = [%expr fun _ -> ()]
      let both ~loc e1 e2 = [%expr fun a -> ([%e e1] a, [%e e2] a)]
      let apply_iso ~loc e _ f' =
        [%expr fun a -> [%e f'] ([%e e] a)]
    end

    include Simple.Product.Reduce (P)
  end
end

module Map2 =
struct
  include Map2

  module Make (M2: S): Intf.S =
  struct
    module P: Simple.Product.S =
    struct
      let name = M2.name
      let typ ~loc t = [%type: [%t t] -> [%t t] -> [%t t]]
      let unit ~loc = [%expr fun () () -> ()]
      let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> ([%e e1] a1 a2, [%e e2] b1 b2)]
      let apply_iso ~loc e f f' =
        [%expr fun a b -> [%e f'] ([%e e] ([%e f] a) ([%e f] b))]
    end

    include Simple.Product.Reduce (P)
  end
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
