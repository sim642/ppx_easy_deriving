open Ppxlib

include Product_intf

module Make (P: S): Intf.S =
struct
  include P

  let tuple ~loc es =
    let n = List.length es in
    let pe_create ~prefix = PatExp.create_tuple ~prefix n in
    P.product ~loc ~pe_create es

  let record ~loc les =
    let ls = List.map fst les in
    let es = List.map snd les in
    let pe_create ~prefix = PatExp.create_record ~prefix ls in
    P.product ~loc ~pe_create es

  let variant ~loc _ = Ast_builder.Default.pexp_extension ~loc (Location.error_extensionf ~loc "Product.Make no variant")
end

module Reduce =
struct
  include Reduce

  module Conjunctive =
  struct
    include Conjunctive

    module Make (C: S): Reduce.S =
    struct
      let name = C.name
      let typ ~loc _ = [%type: bool]
      let unit ~loc = [%expr true]
      let both ~loc e1 e2 = [%expr [%e e1] && [%e e2]]
    end
  end
end

module Reduce1 =
struct
  include Reduce1

  module Make (R1: S): Intf.S =
  struct
    module P = (* TODO: signature *)
    struct
      let name = R1.name
      let typ ~loc t = [%type: [%t t] -> [%t R1.typ ~loc t]]
      (* let unit ~loc = [%expr fun () -> [%e R1.unit ~loc]]
      let both ~loc e1 e2 = [%expr fun (a, b) -> [%e R1.both ~loc [%expr [%e e1] a] [%expr [%e e2] b]]]
      let apply_iso ~loc e f _ =
        [%expr fun a -> [%e e] ([%e f] a)] *)

      let product_body ~loc es pea =
        let esa = PatExp.to_exps ~loc pea in
        let body = List.map2 (fun e ea ->
            [%expr [%e e] [%e ea]]
          ) es esa
        in
        Util.reduce ~unit:(R1.unit ~loc) ~both:(fun acc x ->
            R1.both ~loc acc x
          ) body

      let product ~loc ~pe_create es =
        let pea = pe_create ~prefix:"a" in
        let pa = PatExp.to_pat ~loc pea in
        let body = product_body ~loc es pea in
        [%expr fun [%p pa] -> [%e body]]
    end

    include Make (P)
  end
end

module Reduce2 =
struct
  include Reduce2

  module Make (R2: S): Intf.S =
  struct
    module P =
    struct
      let name = R2.name
      let typ ~loc t = [%type: [%t t] -> [%t t] -> [%t R2.typ ~loc t]]
      (* let unit ~loc = [%expr fun () () -> [%e R2.unit ~loc]]
      let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> [%e R2.both ~loc [%expr [%e e1] a1 a2] [%expr [%e e2] b1 b2]]]
      let apply_iso ~loc e f _ =
        [%expr fun a b -> [%e e] ([%e f] a) ([%e f] b)] *)

      let product_body ~loc es pea peb =
        let esa = PatExp.to_exps ~loc pea in
        let esb = PatExp.to_exps ~loc peb in
        let body = List.map2 (fun e (ea, eb) ->
            [%expr [%e e] [%e ea] [%e eb]]
          ) es (List.combine esa esb)
        in
        Util.reduce ~unit:(R2.unit ~loc) ~both:(fun acc x ->
            R2.both ~loc acc x
          ) body

      let product ~loc ~pe_create es =
        let pea = pe_create ~prefix:"a" in
        let peb = pe_create ~prefix:"b" in
        let pa = PatExp.to_pat ~loc pea in
        let pb = PatExp.to_pat ~loc peb in
        let body = product_body ~loc es pea peb in
        [%expr fun [%p pa] [%p pb] -> [%e body]]
    end

    include Make (P)
  end
end

module Create =
struct
  include Create

  module Make (C: S): Intf.S =
  struct
    module P =
    struct
      let name = C.name
      let typ ~loc t = [%type: [%t C.typ ~loc t] -> [%t t]]
      (* let unit ~loc = [%expr fun _ -> ()]
      let both ~loc e1 e2 = [%expr fun a -> ([%e e1] a, [%e e2] a)]
      let apply_iso ~loc e _ f' =
        [%expr fun a -> [%e f'] ([%e e] a)] *)

      let tuple ~loc es =
        let body =
          match es with
          | [] -> [%expr ()]
          | [e] -> [%expr [%e e] a]
          | _ :: _ ->
            Ast_builder.Default.pexp_tuple ~loc @@ List.map (fun e ->
                [%expr [%e e] a]
              ) es
        in
        [%expr fun a -> [%e body]]

      let record ~loc les =
        let body =
          Ast_builder.Default.pexp_record ~loc (List.map (fun (l, e) ->
              (Ast_builder.Default.Located.mk ~loc l, [%expr [%e e] a])
            ) les) None
        in
        [%expr fun a -> [%e body]]

      let variant ~loc _ = Ast_builder.Default.pexp_extension ~loc (Location.error_extensionf ~loc "Product.Create.Make no variant")
    end

    include P

    (* include Simple.Product.Reduce (P) *)
  end
end

module Map2 =
struct
  include Map2

  module Make (M2: S): Intf.S =
  struct
    module P =
    struct
      let name = M2.name
      let typ ~loc t = [%type: [%t t] -> [%t t] -> [%t t]]
      (* let unit ~loc = [%expr fun () () -> ()]
      let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> ([%e e1] a1 a2, [%e e2] b1 b2)]
      let apply_iso ~loc e f f' =
        [%expr fun a b -> [%e f'] ([%e e] ([%e f] a) ([%e f] b))] *)

      let tuple ~loc es =
        let n = List.length es in
        let pea = PatExp.create_tuple ~prefix:"a" n in
        let peb = PatExp.create_tuple ~prefix:"b" n in
        let pa = PatExp.to_pat ~loc pea in
        let pb = PatExp.to_pat ~loc peb in
        let esa = PatExp.to_exps ~loc pea in
        let esb = PatExp.to_exps ~loc peb in
        let comps = List.map2 (fun e (ea, eb) ->
            [%expr [%e e] [%e ea] [%e eb]]
          ) es (List.combine esa esb)
        in
        let body =
          match comps with
          | [] -> [%expr ()]
          | [comp] -> [%expr [%e comp]]
          | _ :: _ ->
            Ast_builder.Default.pexp_tuple ~loc comps
        in
        [%expr fun [%p pa] [%p pb] -> [%e body]]

      let record ~loc les =
        (* let body =
          Ast_builder.Default.pexp_record ~loc (List.map (fun (l, e) ->
              (Ast_builder.Default.Located.mk ~loc l, [%expr [%e e] a])
            ) les) None
        in *)
        let ls = List.map fst les in
        let es = List.map snd les in
        let pea = PatExp.create_record ~prefix:"a" ls in
        let peb = PatExp.create_record ~prefix:"b" ls in
        let pa = PatExp.to_pat ~loc pea in
        let pb = PatExp.to_pat ~loc peb in
        let esa = PatExp.to_exps ~loc pea in
        let esb = PatExp.to_exps ~loc peb in
        let fields = List.map2 (fun e (ea, eb) ->
            [%expr [%e e] [%e ea] [%e eb]]
          ) es (List.combine esa esb)
        in
        let body = Ast_builder.Default.pexp_record ~loc (List.map (fun (l, e) ->
            (Ast_builder.Default.Located.mk ~loc l, e)
          ) (List.combine ls fields)) None
        in
        [%expr fun [%p pa] [%p pb] -> [%e body]]

      let variant ~loc _ = Ast_builder.Default.pexp_extension ~loc (Location.error_extensionf ~loc "Product.Map2.Make no variant")
    end

    (* include Simple.Product.Reduce (P) *)
    include P
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
