open Ppxlib
open Ast_builder.Default

type t = Record of (longident * string) list
let create_record ~prefix ls =
  Record (List.mapi (fun i l -> (l, prefix ^ string_of_int i)) ls)
let to_pat ~loc = function
  | Record xs ->
    ppat_record ~loc (List.map (fun (l, x) ->
        (Located.mk ~loc l, ppat_var ~loc (Located.mk ~loc x))
      ) xs) Closed
let to_exps ~loc = function
  | Record xs ->
    List.map (fun (_, x) -> pexp_ident ~loc {loc; txt = Lident x}) xs
