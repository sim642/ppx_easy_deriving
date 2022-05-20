open Ppxlib
open Ppx_easy_deriving

module PatExp =
struct
  type t = Record of (longident * string) list
  let create_record ~prefix ls =
    Record (List.mapi (fun i l -> (l, prefix ^ string_of_int i)) ls)
  let to_pat ~loc = function
    | Record xs ->
      let open Ppxlib in
      let open Ast_builder.Default in
      ppat_record ~loc (List.map (fun (l, x) ->
          (Located.mk ~loc l, ppat_var ~loc (Located.mk ~loc x))
        ) xs) Closed
  let to_exps ~loc = function
    | Record xs ->
      let open Ppxlib in
      let open Ast_builder.Default in
      List.map (fun (_, x) -> pexp_ident ~loc {loc; txt = Lident x}) xs
end

module EasyEqualArg: Arg =
struct
  let name = "easy_equal"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]
  let unit ~loc = [%expr fun () () -> true]
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> [%e e1] a1 a2 && [%e e2] b1 b2]
  let apply_iso ~loc leq f _ =
    [%expr fun a b -> [%e leq] ([%e f] a) ([%e f] b)]

  let product ~loc ls es =
    let pea = PatExp.create_record ~prefix:"a" ls in
    let peb = PatExp.create_record ~prefix:"b" ls in
    let pa = PatExp.to_pat ~loc pea in
    let pb = PatExp.to_pat ~loc peb in
    let esa = PatExp.to_exps ~loc pea in
    let esb = PatExp.to_exps ~loc peb in
    let body = List.map2 (fun e (ea, eb) ->
        [%expr [%e e] [%e ea] [%e eb]]
      ) es (List.combine esa esb)
    in
    let body = List.fold_left (fun acc x ->
        [%expr [%e acc] && [%e x]]
      ) [%expr true] body
    in
    [%expr fun [%p pa] [%p pb] -> [%e body]]

end

module EasyEqualDeriver = Make (EasyEqualArg)
let _ = EasyEqualDeriver.register ()
