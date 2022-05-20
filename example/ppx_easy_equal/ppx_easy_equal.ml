open Ppxlib
open Ppx_easy_deriving

module EasyEqualArg: Arg =
struct
  let name = "easy_equal"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]
  let unit ~loc = [%expr fun () () -> true]
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> [%e e1] a1 a2 && [%e e2] b1 b2]
  let apply_iso ~loc leq f _ =
    [%expr fun a b -> [%e leq] ([%e f] a) ([%e f] b)]

  let record ~loc ls es =
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

  let tuple ~loc n es =
    let pea = PatExp.create_tuple ~prefix:"a" n in
    let peb = PatExp.create_tuple ~prefix:"b" n in
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
