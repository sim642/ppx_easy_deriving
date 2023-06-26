open Ppxlib
open Ast_builder.Default
open Ppx_easy_deriving

module EasyEqualArg: Product.Variant.S =
struct
  let name = "easy_equal"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]

  let product_body ~loc es pel per =
    let body =
      let esl = PatExp.to_exps ~loc pel in
      let esr = PatExp.to_exps ~loc per in
      Util.map3 (fun e l r ->
          [%expr [%e e] [%e l] [%e r]]
        ) es esl esr
    in
    Util.reduce ~unit:[%expr true] ~both:(fun acc x ->
        [%expr [%e acc] && [%e x]]
      ) body

  let product ~loc ~pe_create es =
    let pel = pe_create ~prefix:"l" in
    let per = pe_create ~prefix:"r" in
    let body = product_body ~loc es pel per in
    let pl = PatExp.to_pat ~loc pel in
    let pr = PatExp.to_pat ~loc per in
    [%expr fun [%p pl] [%p pr] -> [%e body]]

  let variant ~loc ces =
    let cases = List.map (fun (c, c2, _es, es2) ->
        let pel = c ~prefix:"l" in
        let per = c ~prefix:"r" in
        let pel2 = c2 ~prefix:"l" in
        let per2 = c2 ~prefix:"r" in
        let body = product_body ~loc es2 pel2 per2 in
        let pa = PatExp.to_pat ~loc pel in
        let pb = PatExp.to_pat ~loc per in
        case ~lhs:[%pat? [%p pa], [%p pb]]
          ~guard:None
          ~rhs:body
      ) ces
    in
    let fallback =
      case ~lhs:[%pat? _, _]
        ~guard:None
        ~rhs:[%expr false]
    in
    let body = pexp_match ~loc [%expr l, r] (cases @ [fallback]) in
    [%expr fun l r -> [%e body]]
end

module EasyEqualDeriver = Deriver.Make (Product.Variant.Make (EasyEqualArg))
let _ = EasyEqualDeriver.register ()


module EasyEqual2Arg: Simple.Variant.S =
struct
  let name = "easy_equal2"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]
  let unit ~loc = [%expr fun () () -> true]
  let both ~loc e1 e2 = [%expr fun (l1, l2) (r1, r2) -> [%e e1] l1 r1 && [%e e2] l2 r2]
  let empty ~loc = [%expr fun _ _ -> true]
  let either ~loc e1 e2 =
    [%expr fun l r ->
      match l, r with
      | Either.Left l, Either.Left r -> [%e e1] l r
      | Either.Right l, Either.Right r -> [%e e2] l r
      | _, _ -> false
    ]
  let apply_iso ~loc easy_equal2 ~f ~f':_ =
    [%expr fun l r -> [%e easy_equal2] ([%e f] l) ([%e f] r)]
end

module EasyEqual2Deriver = Deriver.Make (Simple.Variant.Reduce (EasyEqual2Arg))
let _ = EasyEqual2Deriver.register ()
