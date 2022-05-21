open Ppxlib
open Ast_builder.Default

type t =
  | Record of (longident * string) list
  | Tuple of string list
  | Constructor of longident * t option
  | Unit
let create_record ~prefix ls =
  Record (List.mapi (fun i l -> (l, prefix ^ string_of_int i)) ls)
let create_tuple ~prefix n =
  Tuple (List.init n (fun i -> prefix ^ string_of_int i))
(* let create_constructor ~prefix l a =
  let a' = match a with
    |
  Constructor (l, ) *)
let rec to_pat ~loc = function
  | Record xs ->
    ppat_record ~loc (List.map (fun (l, x) ->
        (Located.mk ~loc l, ppat_var ~loc (Located.mk ~loc x))
      ) xs) Closed
  | Tuple xs ->
    ppat_tuple ~loc (List.map (fun x ->
        ppat_var ~loc (Located.mk ~loc x)
      ) xs)
  | Constructor (l, a) ->
    ppat_construct ~loc (Located.mk ~loc l) (Option.map (to_pat ~loc) a)
  | Unit ->
    [%pat? ()]
let to_pats ~loc = function
  | Record xs ->
    List.map (fun (_, x) -> ppat_var ~loc (Located.mk ~loc x)) xs
  | Tuple xs ->
    List.map (fun x -> ppat_var ~loc (Located.mk ~loc x)) xs
  | Constructor _ | Unit ->
    failwith "TODO"
let to_exps ~loc = function
  | Record xs ->
    List.map (fun (_, x) -> pexp_ident ~loc {loc; txt = Lident x}) xs
  | Tuple xs ->
    List.map (fun x -> pexp_ident ~loc {loc; txt = Lident x}) xs
  | Constructor _ | Unit ->
    failwith "TODO"
let rec to_exp ~loc = function
  | Record xs ->
    pexp_record ~loc (List.map (fun (l, x) ->
        (Located.mk ~loc l, pexp_ident ~loc {loc; txt = Lident x})
      ) xs) None
  | Tuple xs ->
    pexp_tuple ~loc (List.map (fun x ->
        pexp_ident ~loc {loc; txt = Lident x}
      ) xs)
  | Constructor (l, a) ->
    pexp_construct ~loc (Located.mk ~loc l) (Option.map (to_exp ~loc) a)
  | Unit ->
    [%expr ()]
