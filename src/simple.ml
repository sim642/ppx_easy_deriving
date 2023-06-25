open Ppxlib
open Ast_builder.Default

include Simple_intf

module Product =
struct
  include Simple_intf.Product

  module Reduce (P: S) =
  struct
    include P

    let tuple ~loc es =
      let body = Util.reduce ~unit:(P.unit ~loc) ~both:(P.both ~loc) es in
      match es with
      | []
      | [_]
      | [_; _] -> body (* avoid trivial iso *)
      | _ :: _ :: _ :: _ ->
        let n = List.length es in
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
        P.apply_iso ~loc body ~f ~f'

    let record ~loc les =
      let es = List.map snd les in
      let body = Util.reduce ~unit:(P.unit ~loc) ~both:(P.both ~loc) es in
      let ls = List.map fst les in
      let n = List.length ls in
      let f =
        let pe = PatExp.create_record ~prefix:"f" ls in
        let pe' = PatExp.create_nested_tuple ~prefix:"f" n in
        [%expr fun [%p PatExp.to_pat ~loc pe] -> [%e PatExp.to_exp ~loc pe']]
      in
      let f' =
        let pe = PatExp.create_record ~prefix:"f'" ls in
        let pe' = PatExp.create_nested_tuple ~prefix:"f'" n in
        [%expr fun [%p PatExp.to_pat ~loc pe'] -> [%e PatExp.to_exp ~loc pe]]
      in
      P.apply_iso ~loc body ~f ~f'

    let variant ~loc _ =
      Ast_builder.Default.pexp_extension ~loc (Location.error_extensionf ~loc "Simple.Product.Reduce: no variant")
  end
end

module Variant =
struct
  include Variant

  module Reduce (V: S) =
  struct
    include Product.Reduce (V)

    let variant ~loc ces =
      let n = List.length ces in
      let es = List.map (fun (_, _, e, _) -> e) ces in
      let body = Util.reduce ~unit:(V.empty ~loc) ~both:(V.either ~loc) es in
      let f =
        let (_, cases) =
          List.fold_left (fun (i, acc) (c, c2, _, _) ->
              let pe = c ~prefix:"a" in
              let pe2 = c2 ~prefix:"a" in
              let pe2' = PatExp.create_nested_variant ~len:n ~i pe2 in
              let acc' =
                case ~lhs:(PatExp.to_pat ~loc pe)
                  ~guard:None
                  ~rhs:(PatExp.to_exp ~loc pe2')
                  :: acc
              in
              (i + 1, acc')
            ) (0, []) ces
        in
        pexp_function ~loc (List.rev cases)
      in
      let f' =
        let (_, cases) =
          List.fold_left (fun (i, acc) (c, c2, _, _) ->
              let pe = c ~prefix:"a" in
              let pe2 = c2 ~prefix:"a" in
              let pe2' = PatExp.create_nested_variant ~len:n ~i pe2 in
              let acc' =
                case ~lhs:(PatExp.to_pat ~loc pe2')
                  ~guard:None
                  ~rhs:(PatExp.to_exp ~loc pe)
                  :: acc
              in
              (i + 1, acc')
            ) (0, []) ces
        in
        pexp_function ~loc (List.rev cases)
      in
      V.apply_iso ~loc body ~f ~f'
  end
end
