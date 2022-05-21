open Ppxlib
open Ast_builder.Default

module PatExp = PatExp

let reduce ~unit ~both = function
  | [] -> unit
  | [x] -> x
  | xs ->
    let xs = List.rev xs in
    match xs with
    | x :: xs ->
      List.fold_right both (List.rev xs) x (* omits hash_empty *)
    | [] -> assert false

module type ArgBase =
sig
  val name: string
  val typ: loc:location -> core_type -> core_type
  val unit: loc:location -> expression
end

module type Arg2 =
sig
  include ArgBase
  val both: loc:location -> expression -> expression -> expression
  val apply_iso: loc:location -> expression -> expression -> expression -> expression
end

module type ArgProduct =
sig
  include ArgBase
  val product: loc:location -> pe_create:(prefix:string -> PatExp.t) -> expression list -> expression
  val variant: loc:location -> ((prefix:string -> PatExp.t) * (prefix:string -> PatExp.t) * expression) list -> expression (* TODO: extract *)
end

module type Arg =
sig
  include ArgBase
  val record: loc:location -> longident list -> expression list -> expression
  val tuple: loc:location -> int -> expression list -> expression
  val variant: loc:location -> ((prefix:string -> PatExp.t) * (prefix:string -> PatExp.t) * expression) list -> expression
end

module MakeArg2 (Arg2: Arg2): Arg =
struct
  include Arg2

  let record ~loc ls es =
    let body =
      es
      |> reduce ~unit:(Arg2.unit ~loc) ~both:(Arg2.both ~loc)
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

  let tuple ~loc n es =
    let body =
      es
      |> reduce ~unit:(Arg2.unit ~loc) ~both:(Arg2.both ~loc)
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
    | [] | [_] -> assert false
    | [_; _] -> body (* avoid trivial iso *)
    | _ :: _ :: _ :: _ -> Arg2.apply_iso ~loc body f f'

  let variant ~loc:_ _ = failwith "TODO"
end

module MakeArgProduct (ArgProduct: ArgProduct): Arg =
struct
  include ArgProduct

  let record ~loc ls es =
    let pe_create ~prefix = PatExp.create_record ~prefix ls in
    ArgProduct.product ~loc ~pe_create es

  let tuple ~loc n es =
    let pe_create ~prefix = PatExp.create_tuple ~prefix n in
    ArgProduct.product ~loc ~pe_create es

  let variant = ArgProduct.variant
end


module type S =
sig
  val register: unit -> Deriving.t
end

module Make (Arg: Arg): S =
struct
  let rec expr ~loc ~quoter ct =
    let expr = expr ~quoter in
    match ct with
    | [%type: unit] ->
      Arg.unit ~loc
    | {ptyp_desc = Ptyp_constr ({txt = lid; loc}, args); _} ->
      let ident = pexp_ident ~loc {loc; txt = Ppx_deriving.mangle_lid (`Prefix Arg.name) lid} in
      let ident = Ppx_deriving.quote ~quoter ident in
      let apply_args =
        args
        |> List.map (fun ct ->
            (Nolabel, expr ~loc ct)
          )
      in
      pexp_apply ~loc ident apply_args
    | {ptyp_desc = Ptyp_tuple comps; _} ->
      expr_tuple ~loc ~quoter comps
    | {ptyp_desc = Ptyp_variant (rows, Closed, None); _} ->
      expr_poly_variant ~loc ~quoter rows
    | {ptyp_desc = Ptyp_var name; _} ->
      evar ~loc ("poly_" ^ name)
    | _ ->
      Location.raise_errorf ~loc "other"

  and expr_poly_variant ~loc ~quoter rows =
    rows
    |> List.map (fun {prf_desc; _} ->
        match prf_desc with
        | Rtag ({txt = label; loc}, true, []) ->
          ((fun ~prefix:_ -> PatExp.PolyConstructor (label, None)), (fun ~prefix:_ -> PatExp.Unit), Arg.unit ~loc)
        | Rtag ({txt = label; loc}, false, [ct]) ->
          ((fun ~prefix -> PatExp.PolyConstructor (label, Some (PatExp.Base prefix))), (fun ~prefix -> PatExp.Base prefix), expr ~loc ~quoter ct)
        | _ ->
          Location.raise_errorf ~loc "other variant"
      )
    |> Arg.variant ~loc

  and expr_variant ~loc ~quoter constrs =
    constrs
    |> List.map (fun {pcd_name = {txt = label; loc}; pcd_args; pcd_res; _} ->
        match pcd_res, pcd_args with
        | None, Pcstr_tuple [] ->
          ((fun ~prefix:_ -> PatExp.Constructor (Lident label, None)), (fun ~prefix:_ -> PatExp.Unit), Arg.unit ~loc)
        | None, Pcstr_tuple cts ->
          ((fun ~prefix -> PatExp.Constructor (Lident label, Some (PatExp.create_tuple ~prefix (List.length cts)))), PatExp.create_tuple (List.length cts), expr_tuple ~loc ~quoter cts)
        (* TODO: can do with record? *)
        (* | None, Pcstr_record lds ->
          let ls = lds
            |> List.map (fun {pld_name = {txt = label; _}; _} ->
            Lident label
          )
          in
          ((fun ~prefix -> PatExp.Constructor (Lident label, Some (PatExp.create_record ~prefix ls))), PatExp.create_record ls, expr_record ~loc ~quoter lds) *)
        | None, Pcstr_record lds ->
          let ls = lds
            |> List.map (fun {pld_name = {txt = label; _}; _} ->
            Lident label
          )
          in
          let cts = lds
            |> List.map (fun {pld_type; _} ->
            pld_type
          )
          in
          ((fun ~prefix -> PatExp.Constructor (Lident label, Some (PatExp.create_record ~prefix ls))), PatExp.create_tuple (List.length ls), expr_tuple ~loc ~quoter cts)
        | _ ->
          Location.raise_errorf ~loc "other variant"
      )
    |> Arg.variant ~loc

  and expr_record ~loc ~quoter (lds: label_declaration list) =
    Arg.record ~loc (lds
       |> List.map (fun {pld_name = {txt = label; _}; _} ->
        Lident label
      )) (lds
      |> List.map (fun {pld_type; _} ->
          expr ~loc ~quoter pld_type
        ))

  and expr_tuple ~loc ~quoter comps =
    Arg.tuple ~loc (List.length comps) (comps
      |> List.map (fun pld_type ->
          expr ~loc ~quoter pld_type
        ))

  let expr_declaration ~loc ~quoter td = match td with
    | {ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _} ->
      expr ~loc ~quoter ct
    | {ptype_kind = Ptype_abstract; _} ->
      Location.raise_errorf ~loc "Cannot derive accessors for abstract types"
    | {ptype_kind = Ptype_variant constrs; _} ->
      expr_variant ~loc ~quoter constrs
    | {ptype_kind = Ptype_open; _} ->
      Location.raise_errorf ~loc "Cannot derive accessors for open types"
    | {ptype_kind = Ptype_record fields; _} ->
      expr_record ~loc ~quoter fields

  let typ ~loc td =
    let ct = Ppx_deriving.core_type_of_type_decl td in
    Ppx_deriving.poly_arrow_of_type_decl
      (Arg.typ ~loc)
      td
      (Arg.typ ~loc ct)

  let generate_impl ~ctxt (_rec_flag, type_declarations) =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    type_declarations
    |> List.map (fun td ->
        let quoter = Ppx_deriving.create_quoter () in
        let expr = expr_declaration ~loc ~quoter td in
        let expr = Ppx_deriving.sanitize ~quoter expr in
        let expr = Ppx_deriving.poly_fun_of_type_decl td expr in
        let ct = typ ~loc td in
        let pat = ppat_var ~loc {loc; txt = Ppx_deriving.mangle_type_decl (`Prefix Arg.name) td} in
        let pat = ppat_constraint ~loc pat ct in
        Ast_helper.Vb.mk ~loc ~attrs:[Ppx_deriving.attr_warning [%expr "-39"]] pat expr
      )
    |> Ast_helper.Str.value ~loc Recursive
    |> fun v -> [v]

  let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
  let register () =
    Deriving.add Arg.name
      ~str_type_decl:impl_generator
end
