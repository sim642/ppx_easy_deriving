open Ppxlib
open Ppx_easy_deriving

module EasyEqualArg: Convert.ArgProduct =
struct
  let name = "easy_equal"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]
  let unit ~loc = [%expr fun () () -> true]

  let product_body ~loc es pea peb =
    let esa = PatExp.to_exps ~loc pea in
    let esb = PatExp.to_exps ~loc peb in
    let body = List.map2 (fun e (ea, eb) ->
        [%expr [%e e] [%e ea] [%e eb]]
      ) es (List.combine esa esb)
    in
    Util.reduce ~unit:[%expr true] ~both:(fun acc x ->
        [%expr [%e acc] && [%e x]]
      ) body

  let product ~loc ~pe_create es =
    let pea = pe_create ~prefix:"a" in
    let peb = pe_create ~prefix:"b" in
    let pa = PatExp.to_pat ~loc pea in
    let pb = PatExp.to_pat ~loc peb in
    let body = product_body ~loc es pea peb in
    [%expr fun [%p pa] [%p pb] -> [%e body]]

  let variant ~loc ces =
    let open Ast_builder.Default in
    ces
    |> List.map (fun (c, c2, _es, es2) ->
        let pea = c ~prefix:"a" in
        let peb = c ~prefix:"b" in
        let pea2 = c2 ~prefix:"a" in
        let peb2 = c2 ~prefix:"b" in
        let pa = PatExp.to_pat ~loc pea in
        let pb = PatExp.to_pat ~loc peb in
        let body = product_body ~loc es2 pea2 peb2 in
        case ~lhs:[%pat? [%p pa], [%p pb]]
          ~guard:None
          ~rhs:body
      )
    |> (fun cases -> [%expr fun x y -> [%e pexp_match ~loc [%expr x, y] (cases @ [case ~lhs:[%pat? _, _] ~guard:None ~rhs:[%expr false]])]])
end

module EasyEqualDeriver = Make (Convert.MakeArgProduct (EasyEqualArg))
let _ = EasyEqualDeriver.register ()


module EasyEqual2Arg: Convert.Arg2 =
struct
  let name = "easy_equal2"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]
  let unit ~loc = [%expr fun () () -> true]
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> [%e e1] a1 a2 && [%e e2] b1 b2]
  let apply_iso ~loc leq f _ =
    [%expr fun a b -> [%e leq] ([%e f] a) ([%e f] b)]
end

module EasyEqual2Deriver = Make (Convert.MakeArg2 (EasyEqual2Arg))
let _ = EasyEqual2Deriver.register ()
