open Ppxlib
open Ast_builder.Default

type t =
  | Record of (longident * t) list
  | Tuple of t list
  | Constructor of longident * t option
  | Unit
  | PolyConstructor of string * t option
  | Base of string
  | Inherit of longident * string
let create_record ~prefix ls =
  Record (List.mapi (fun i l -> (l, Base (prefix ^ string_of_int i))) ls)
let create_tuple ~prefix n =
  match n with
  | 0 -> Unit
  | 1 -> Base (prefix ^ "0")
  | n -> Tuple (List.init n (fun i -> Base (prefix ^ string_of_int i)))
let create_nested_tuple ~prefix n =
  match n with
  | 0 -> Unit
  | n ->
    List.init n (fun i -> Base (prefix ^ string_of_int i))
    |> List.rev
    |> (function
      | (last::others) -> List.fold_left (fun acc field ->
        Tuple [field; acc]
      ) last others
      | [] -> assert false
    )
let create_nested_variant ~len ~i base =
  let init =
    if i = len - 1 then
      base
    else
      Constructor (Longident.parse "Either.Left", Some base)
  in
  let rhs = List.fold_right (fun _ acc' ->
      Constructor (Longident.parse "Either.Right", Some acc')
    ) (List.init i (fun _ -> ())) init
  in
  rhs
(* let create_constructor ~prefix l a =
  let a' = match a with
    |
  Constructor (l, ) *)
let rec to_pat ~loc = function
  | Record xs ->
    ppat_record ~loc (List.map (fun (l, x) ->
        (Located.mk ~loc l, to_pat ~loc x)
      ) xs) Closed
  | Tuple xs ->
    ppat_tuple ~loc (List.map (to_pat ~loc) xs)
  | Constructor (l, a) ->
    ppat_construct ~loc (Located.mk ~loc l) (Option.map (to_pat ~loc) a)
  | Unit ->
    [%pat? ()]
  | PolyConstructor (l, a) ->
    ppat_variant ~loc l (Option.map (to_pat ~loc) a)
  | Base s ->
    ppat_var ~loc (Located.mk ~loc s)
  | Inherit (t, a) ->
    ppat_alias ~loc (ppat_type ~loc (Located.mk ~loc t)) (Located.mk ~loc a)
let rec to_exps ~loc = function
  | Record xs ->
    List.flatten (List.map (fun (_, x) -> to_exps ~loc x) xs)
  | Tuple xs ->
    List.flatten (List.map (to_exps ~loc) xs)
  | Unit ->
    []
  | Base s ->
    [pexp_ident ~loc {loc; txt = Lident s}]
  | Constructor _ | PolyConstructor _ ->
    failwith "to_exps: TODO"
  | Inherit (_, a) ->
    [pexp_ident ~loc {loc; txt = Lident a}]
let rec to_exp ~loc = function
  | Record xs ->
    pexp_record ~loc (List.map (fun (l, x) ->
        (Located.mk ~loc l, to_exp ~loc x)
      ) xs) None
  | Tuple xs ->
    pexp_tuple ~loc (List.map (to_exp ~loc) xs)
  | Constructor (l, a) ->
    pexp_construct ~loc (Located.mk ~loc l) (Option.map (to_exp ~loc) a)
  | Unit ->
    [%expr ()]
  | PolyConstructor (l, a) ->
    pexp_variant ~loc l (Option.map (to_exp ~loc) a)
  | Base s ->
    pexp_ident ~loc {loc; txt = Lident s}
  | Inherit (_, a) ->
    pexp_ident ~loc {loc; txt = Lident a}
