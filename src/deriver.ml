open Ppxlib
open Ast_builder.Default

include Deriver_intf

module Make (Arg: Intf.S): S =
struct
  let attr = Attribute.declare (Printf.sprintf "deriving.%s.%s" Arg.name Arg.name) Attribute.Context.core_type Ast_pattern.(single_expr_payload __) (fun expr -> expr)

  let unit ~loc = Arg.tuple ~loc []

  let rec expr ~loc ~quoter ct =
    match Attribute.get attr ct with
    | Some expr ->
      Expansion_helpers.Quoter.quote quoter expr
    | None ->
      match ct with
      | [%type: unit] ->
        unit ~loc
      | {ptyp_desc = Ptyp_constr ({txt = lid; loc}, args); _} ->
        let ident = pexp_ident ~loc {loc; txt = Expansion_helpers.mangle_lid (Prefix Arg.name) lid} in
        let ident = Expansion_helpers.Quoter.quote quoter ident in
        let apply_args = List.map (fun ct ->
            (Nolabel, expr ~loc ~quoter ct)
          ) args
        in
        pexp_apply ~loc ident apply_args
      | {ptyp_desc = Ptyp_tuple elems; _} ->
        expr_tuple ~loc ~quoter elems
      | {ptyp_desc = Ptyp_variant (rows, Closed, None); _} ->
        expr_poly_variant ~loc ~quoter rows
      | {ptyp_desc = Ptyp_var name; _} ->
        evar ~loc ("poly_" ^ name)
      | _ ->
        pexp_extension ~loc (Location.error_extensionf ~loc "unsupported core type")

  and expr_poly_variant ~loc ~quoter rows =
    let ces = List.map (fun {prf_desc; _} ->
        match prf_desc with
        | Rtag ({txt = label; loc}, true, []) ->
          ((fun ~prefix:_ -> PatExp.PolyConstructor (label, None)),
           (fun ~prefix:_ -> PatExp.Unit),
           unit ~loc,
           [])
        | Rtag ({txt = label; loc}, false, [ct]) ->
          let expr = expr ~loc ~quoter ct in
          ((fun ~prefix -> PatExp.PolyConstructor (label, Some (PatExp.Base prefix))),
           (fun ~prefix -> PatExp.Base prefix),
           expr,
           [expr])
        | Rinherit ({ptyp_desc = Ptyp_constr ({txt = lid; loc}, _); _} as ct) ->
          let expr = expr ~loc ~quoter ct in
          ((fun ~prefix -> PatExp.Inherit (lid, prefix)),
           (fun ~prefix -> PatExp.Inherit (lid, prefix)),
           expr,
           [expr])
        | _ ->
          Location.raise_errorf ~loc "unsupported row"
      ) rows
    in
    Arg.variant ~loc ces

  and expr_variant ~loc ~quoter constrs =
    let ces = List.map (fun {pcd_name = {txt = label; loc}; pcd_args; pcd_res; _} ->
        match pcd_res, pcd_args with
        | None, Pcstr_tuple [] ->
          ((fun ~prefix:_ -> PatExp.Constructor (Lident label, None)),
           (fun ~prefix:_ -> PatExp.Unit),
           unit ~loc,
           [])
        | None, Pcstr_tuple cts ->
          let n = List.length cts in
          ((fun ~prefix -> PatExp.Constructor (Lident label, Some (PatExp.create_tuple ~prefix n))),
           PatExp.create_tuple n,
           expr_tuple ~loc ~quoter cts,
           List.map (expr ~loc ~quoter) cts)
        | None, Pcstr_record lds ->
          let ls = List.map (fun {pld_name = {txt = label; _}; _} ->
              Lident label
            ) lds
          in
          let elems = List.map (fun {pld_type; _} ->
              pld_type
            ) lds
          in
          ((fun ~prefix -> PatExp.Constructor (Lident label, Some (PatExp.create_record ~prefix ls))),
           PatExp.create_tuple (List.length ls), (* TODO: can do with record? *)
           expr_tuple ~loc ~quoter elems, (* TODO: can do with record? *)
           List.map (expr ~loc ~quoter) elems)
        | _ ->
          Location.raise_errorf ~loc "unsupported constructor"
      ) constrs
    in
    Arg.variant ~loc ces

  and expr_record ~loc ~quoter (lds: label_declaration list) =
    let les = List.map (fun {pld_name = {txt = label; _}; pld_type; _} ->
        (Lident label, expr ~loc ~quoter pld_type)
      ) lds
    in
    Arg.record ~loc les

  and expr_tuple ~loc ~quoter elems =
    let es = List.map (expr ~loc ~quoter) elems in
    Arg.tuple ~loc es

  let expr_declaration ~loc ~quoter = function
    | {ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _} ->
      expr ~loc ~quoter ct
    | {ptype_kind = Ptype_abstract; _} ->
      pexp_extension ~loc (Location.error_extensionf ~loc "unsupported abstract type")
    | {ptype_kind = Ptype_variant constrs; _} ->
      expr_variant ~loc ~quoter constrs
    | {ptype_kind = Ptype_open; _} ->
      pexp_extension ~loc (Location.error_extensionf ~loc "unsupported open type")
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
    let vbs = List.map (fun td ->
        let quoter = Expansion_helpers.Quoter.create () in
        let expr = expr_declaration ~loc ~quoter td in
        let expr = Expansion_helpers.Quoter.sanitize quoter expr in
        let expr = Ppx_deriving.poly_fun_of_type_decl td expr in
        let ct = typ ~loc td in
        let pat = ppat_var ~loc {loc; txt = Expansion_helpers.mangle_type_decl (Prefix Arg.name) td} in
        let pat = ppat_constraint ~loc pat ct in
        Ast_helper.Vb.mk ~loc ~attrs:[Ppx_deriving.attr_warning [%expr "-39"]] pat expr
      ) type_declarations
    in
    [Ast_helper.Str.value ~loc Recursive vbs]

  let generate_intf ~ctxt (_rec_flag, type_declarations) =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    List.map (fun td ->
        let ct = typ ~loc td in
        let val_ = Ast_helper.Val.mk ~loc {loc; txt = Expansion_helpers.mangle_type_decl (Prefix Arg.name) td} ct in
        Ast_helper.Sig.value ~loc val_
      ) type_declarations

  let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
  let intf_generator = Deriving.Generator.V2.make_noarg generate_intf
  let extension ~loc ~path:_ ct =
    let quoter = Expansion_helpers.Quoter.create () in
    let expr = expr ~loc ~quoter ct in
    Expansion_helpers.Quoter.sanitize quoter expr

  let register () =
    Deriving.add Arg.name
      ~sig_type_decl:intf_generator
      ~str_type_decl:impl_generator
      ~extension
end
