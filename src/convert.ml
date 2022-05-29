open Ppxlib


module type Arg2 =
sig
  include Intf.Base
  val both: loc:location -> expression -> expression -> expression
  val apply_iso: loc:location -> expression -> expression -> expression -> expression
end

module type Arg22 =
sig
  include Arg2
  val empty: loc:location -> expression
  val sum: loc:location -> expression -> expression -> expression
end

module type Product =
sig
  val product: loc:location -> pe_create:(prefix:string -> PatExp.t) -> expression list -> expression
end

module type ArgProduct =
sig
  include Intf.Base
  include Product
  include Intf.Variant (* TODO: separate *)
end


module MakeArg2 (Arg2: Arg2): Intf.S =
struct
  include Arg2

  let record ~loc les =
    let ls = List.map fst les in
    let es = List.map snd les in
    let body =
      es
      |> Util.reduce ~unit:(Arg2.unit ~loc) ~both:(Arg2.both ~loc)
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
    Arg2.apply_iso ~loc body f f'

  let tuple ~loc es =
    let n = List.length es in
    let body =
      es
      |> Util.reduce ~unit:(Arg2.unit ~loc) ~both:(Arg2.both ~loc)
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
          | [] -> assert false
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
          | [] -> assert false
        )
      in
      let body =
        PatExp.to_exp ~loc pe
      in
      [%expr fun [%p pat] -> [%e body]]
    in
    match es with
    (* | [] | [_] -> assert false *)
    | [] -> Arg2.unit ~loc
    | [e] -> e
    | [_; _] -> body (* avoid trivial iso *)
    | _ :: _ :: _ :: _ -> Arg2.apply_iso ~loc body f f'

  let variant ~loc:_ _ = failwith "TODO"
end

module MakeArg22 (Arg22: Arg22): Intf.S =
struct
  include MakeArg2 (Arg22)

  let variant ~loc ces =
    (* let ls = List.map fst les in *)
    let es = List.map (fun (_, _, e, _) -> e) ces in
    let body =
      es
      |> Util.reduce ~unit:(Arg22.empty ~loc) ~both:(Arg22.sum ~loc)
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
    Arg22.apply_iso ~loc body f f'
end

module MakeArgProduct (ArgProduct: ArgProduct): Intf.S =
struct
  include ArgProduct

  let record ~loc les =
    let ls = List.map fst les in
    let es = List.map snd les in
    let pe_create ~prefix = PatExp.create_record ~prefix ls in
    ArgProduct.product ~loc ~pe_create es

  let tuple ~loc es =
    let n = List.length es in
    let pe_create ~prefix = PatExp.create_tuple ~prefix n in
    ArgProduct.product ~loc ~pe_create es

  let variant = ArgProduct.variant
end
