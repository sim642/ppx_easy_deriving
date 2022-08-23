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
        let body =
          PatExp.to_exps ~loc pe
          |> List.rev
          |> (function
            | (last::others) -> List.fold_left (fun acc field ->
              [%expr ([%e field], [%e acc])]
            ) last others
            | [] -> assert false
          )
        in
        [%expr fun [%p PatExp.to_pat ~loc pe] -> [%e body]]
      in
      let f' =
        let pe = PatExp.create_record ~prefix:"f'" ls in
        let pat =
          PatExp.to_pats ~loc pe
          |> List.rev
          |> (function
            | (last::others) -> List.fold_left (fun acc field ->
              [%pat? ([%p field], [%p acc])]
            ) last others
            | [] -> assert false
          )
        in
        let body =
          PatExp.to_exp ~loc pe
        in
        [%expr fun [%p pat] -> [%e body]]
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
        let body =
          PatExp.to_exps ~loc pe
          |> List.rev
          |> (function
            | (last::others) -> List.fold_left (fun acc field ->
              [%expr ([%e field], [%e acc])]
            ) last others
            | [] -> Ast_builder.Default.pexp_extension ~loc (Location.error_extensionf ~loc "Simple.Product.Reduce empty tuple")
          )
        in
        [%expr fun [%p PatExp.to_pat ~loc pe] -> [%e body]]
      in
      let f' =
        let pe = PatExp.create_tuple ~prefix:"f'" n in
        let pat =
          PatExp.to_pats ~loc pe
          |> List.rev
          |> (function
            | (last::others) -> List.fold_left (fun acc field ->
              [%pat? ([%p field], [%p acc])]
            ) last others
            | [] -> Ast_builder.Default.ppat_extension ~loc (Location.error_extensionf ~loc "Simple.Product.Reduce empty tuple")
          )
        in
        let body =
          PatExp.to_exp ~loc pe
        in
        [%expr fun [%p pat] -> [%e body]]
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
              let i =
                if List.length acc = List.length ces - 1 then
                  PatExp.to_exp ~loc pe2
                else
                  [%expr Either.Left [%e PatExp.to_exp ~loc pe2]]
              in
              let rhs = List.fold_right (fun _ acc' ->
                  [%expr Either.Right [%e acc']]
                ) acc i
              in
              case ~lhs:(PatExp.to_pat ~loc pe)
                ~guard:None
                ~rhs
                :: acc
            ) (List.rev ces) []
        in
        pexp_function ~loc cases
      in
      let f' =
        let cases =
          List.fold_right (fun (c, c2, _, _) acc ->
              let pe = c ~prefix:"a" in
              let pe2 = c2 ~prefix:"a" in
              let i =
                if List.length acc = List.length ces - 1 then
                  PatExp.to_pat ~loc pe2
                else
                  [%pat? Either.Left [%p PatExp.to_pat ~loc pe2]]
              in
              let lhs = List.fold_right (fun _ acc' ->
                  [%pat? Either.Right [%p acc']]
                ) acc i
              in
              case ~lhs
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
