open Ppxlib
include Simple_intf

module Product =
struct
  include Simple_intf.Product

  module Reduce (P: S) =
  struct
    include P

    let record ~loc les =
      let ls = List.map fst les in
      let es = List.map snd les in
      let body =
        es
        |> Util.reduce ~unit:(P.unit ~loc) ~both:(P.both ~loc)
      in
      let f =
        let pe = PatExp.create_record ~prefix:"f" ls in
        let pe' = PatExp.create_nested_tuple ~prefix:"f" (List.length ls) in
        [%expr fun [%p PatExp.to_pat ~loc pe] -> [%e PatExp.to_exp ~loc pe']]
      in
      let f' =
        let pe = PatExp.create_record ~prefix:"f'" ls in
        let pe' = PatExp.create_nested_tuple ~prefix:"f'" (List.length ls) in
        [%expr fun [%p PatExp.to_pat ~loc pe'] -> [%e PatExp.to_exp ~loc pe]]
      in
      P.apply_iso ~loc body f f'

    let tuple ~loc es =
      let n = List.length es in
      let body =
        es
        |> Util.reduce ~unit:(P.unit ~loc) ~both:(P.both ~loc)
      in
      let f =
        let pe = PatExp.create_tuple ~prefix:"f" n in
        let pe' = PatExp.create_nested_tuple ~prefix:"f" n in
        [%expr fun [%p PatExp.to_pat ~loc pe] -> [%e PatExp.to_exp ~loc pe']]
      in
      let f' =
        let pe = PatExp.create_tuple ~prefix:"f'" n in
        let pe' = PatExp.create_nested_tuple ~prefix:"f'" n in
        [%expr fun [%p PatExp.to_pat ~loc pe'] -> [%e PatExp.to_exp ~loc pe]]
      in
      match es with
      (* | [] | [_] -> assert false *)
      | [] -> P.unit ~loc
      | [e] -> e
      | [_; _] -> body (* avoid trivial iso *)
      | _ :: _ :: _ :: _ -> P.apply_iso ~loc body f f'

    let variant ~loc _ = Ast_builder.Default.pexp_extension ~loc (Location.error_extensionf ~loc "Simple.Product.Reduce no variant")
  end
end

module Variant =
struct
  include Variant

  module Reduce (V: S) =
  struct
    include Product.Reduce (V)

    let variant ~loc ces =
      (* let ls = List.map fst les in *)
      let es = List.map (fun (_, _, e, _) -> e) ces in
      let body =
        es
        |> Util.reduce ~unit:(V.empty ~loc) ~both:(V.either ~loc)
      in
      let open Ast_builder.Default in
      let f =
        let cases =
          List.fold_right (fun (c, c2, _, _) acc ->
              let pe = c ~prefix:"a" in
              let pe2 = c2 ~prefix:"a" in
              let pe2' = PatExp.create_nested_variant ~len:(List.length ces) ~i:(List.length acc) pe2 in
              case ~lhs:(PatExp.to_pat ~loc pe)
                ~guard:None
                ~rhs:(PatExp.to_exp ~loc pe2')
                :: acc
            ) (List.rev ces) []
        in
        pexp_function ~loc (List.rev cases)
      in
      let f' =
        let cases =
          List.fold_right (fun (c, c2, _, _) acc ->
              let pe = c ~prefix:"a" in
              let pe2 = c2 ~prefix:"a" in
              let pe2' = PatExp.create_nested_variant ~len:(List.length ces) ~i:(List.length acc) pe2 in
              case ~lhs:(PatExp.to_pat ~loc pe2')
                ~guard:None
                ~rhs:(PatExp.to_exp ~loc pe)
                :: acc
            ) (List.rev ces) []
        in
        pexp_function ~loc cases
      in
      V.apply_iso ~loc body f f'
  end
end
