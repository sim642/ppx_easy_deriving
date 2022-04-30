open Ppxlib
open Ast_builder.Default

module type Arg =
sig
  val name: string
  val typ: loc:location -> core_type -> core_type
  val unit: loc:location -> expression
  val both: loc:location -> expression -> expression -> expression
  val apply_iso: loc:location -> expression -> expression -> expression
end

module type S =
sig
  val register: unit -> Deriving.t
end

module Make (Arg: Arg): S =
struct
  let hash_reduce2 ~loc a b =
    Arg.both ~loc a b

  let hash_fold ~loc i =
    List.fold_left (hash_reduce2 ~loc) i

  let hash_fold' ~loc i xs =
    List.fold_right ((hash_reduce2 ~loc)) xs i

  let hash_empty ~loc = [%expr 0]

  let hash_reduce ~loc = function
    | [] -> hash_empty ~loc
    | [x] -> x
    | x :: xs -> hash_fold ~loc x xs (* omits hash_empty *)

  let hash_reduce' ~loc = function
    | [] -> hash_empty ~loc
    | [x] -> x
    | xs ->
      let xs = List.rev xs in
      match xs with
      | x :: xs ->
        hash_fold' ~loc x (List.rev xs) (* omits hash_empty *)
      | [] -> assert false

  let hash_variant ~loc i = eint ~loc i

  let rec expr ~loc ~quoter ct =
    let expr = expr ~quoter in
    match ct with
    (* | [%type: string] ->
      [%expr Hashtbl.hash]
    | [%type: char] ->
      [%expr Char.code]
    | [%type: bool] ->
      [%expr Bool.to_int]
    | [%type: int32] ->
      [%expr Int32.to_int]
    | [%type: int64] ->
      [%expr Int64.to_int]
    | [%type: int] ->
      [%expr fun (x: int) -> x]
    | [%type: unit] ->
      [%expr fun () -> [%e hash_empty ~loc]]
    | [%type: [%t? a] option] ->
      [%expr function
        (* like variants *)
        | None -> [%e hash_variant ~loc 0]
        | Some x -> [%e hash_reduce2 ~loc (hash_variant ~loc 1) [%expr [%e expr ~loc a] x]]
      ]
    | [%type: [%t? a] list] ->
      [%expr List.fold_left (fun a b -> [%e hash_reduce2 ~loc [%expr a] [%expr [%e expr ~loc a] b]]) [%e hash_empty ~loc]] *)
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
          let variant_i = Ppx_deriving.hash_variant label in
          let variant_const = hash_variant ~loc variant_i in
          case ~lhs:(ppat_variant ~loc label None)
            ~guard:None
            ~rhs:variant_const
        | Rtag ({txt = label; loc}, false, [ct]) ->
          let variant_i = Ppx_deriving.hash_variant label in
          let variant_const = hash_variant ~loc variant_i in
          let label_fun = expr ~loc ~quoter ct in
          case ~lhs:(ppat_variant ~loc label (Some [%pat? x]))
            ~guard:None
            ~rhs:(hash_reduce2 ~loc variant_const [%expr [%e label_fun] x])
        | _ ->
          Location.raise_errorf ~loc "other variant"
      )
    |> pexp_function ~loc

  and expr_variant ~loc ~quoter constrs =
    constrs
    |> List.mapi (fun variant_i {pcd_name = {txt = label; loc}; pcd_args; pcd_res; _} ->
        let variant_const = hash_variant ~loc variant_i in
        match pcd_res, pcd_args with
        | None, Pcstr_tuple [] ->
          case ~lhs:(ppat_construct ~loc {loc; txt = Lident label} None)
            ~guard:None
            ~rhs:variant_const
        | None, Pcstr_tuple cts ->
          let label_field ~loc prefix i =
            let name = prefix ^ string_of_int i in
            pexp_ident ~loc {loc; txt = Lident name}
          in
          let body =
            cts
            |> List.mapi (fun i comp_type ->
                (i, expr ~loc ~quoter comp_type)
              )
            |> List.map (fun (i, label_fun) ->
                [%expr [%e label_fun] [%e label_field ~loc "x" i]]
              )
            |> hash_fold ~loc variant_const
          in
          let pat prefix =
            cts
            |> List.mapi (fun i _ ->
                let name = prefix ^ string_of_int i in
                ppat_var ~loc {loc; txt = name}
              )
            |> ppat_tuple ~loc
            |> fun x -> ppat_construct ~loc {loc; txt = Lident label} (Some x)
          in
          case ~lhs:(pat "x")
            ~guard:None
            ~rhs:body
        | None, Pcstr_record lds ->
          let label_field ~loc record_expr label =
            pexp_field ~loc record_expr {loc; txt = Lident label}
          in
          let body x_expr =
            lds
            |> List.map (fun {pld_name = {txt = label; loc}; pld_type; _} ->
                (label, expr ~loc ~quoter pld_type)
              )
            |> List.map (fun (label, label_fun) ->
                [%expr [%e label_fun] [%e label_field ~loc x_expr label]]
              )
            |> hash_fold ~loc variant_const
          in
          let pat = ppat_construct ~loc {loc; txt = Lident label} (Some [%pat? x]) in
          case ~lhs:pat
            ~guard:None
            ~rhs:(body [%expr x])
        | _ ->
          Location.raise_errorf ~loc "other variant"
      )
    |> pexp_function ~loc

  and expr_record ~loc ~quoter lds =
    let label_field ~loc record_expr label =
      pexp_field ~loc record_expr {loc; txt = Lident label}
    in
    let body x_expr =
      lds
      |> List.map (fun {pld_name = {txt = label; loc}; pld_type; _} ->
          (label, expr ~loc ~quoter pld_type)
        )
      |> List.map (fun (label, label_fun) ->
          [%expr [%e label_fun] [%e label_field ~loc x_expr label]]
        )
      |> hash_reduce ~loc
    in
    [%expr fun x -> [%e body [%expr x]]]

  and expr_tuple ~loc ~quoter comps =
    let label_field ~loc prefix i =
      let name = prefix ^ string_of_int i in
      pexp_ident ~loc {loc; txt = Lident name}
    in
    let body =
      (* comps
      |> List.mapi (fun i comp_type ->
          (i, expr ~loc ~quoter comp_type)
        )
      |> List.map (fun (i, label_fun) ->
          [%expr [%e label_fun] [%e label_field ~loc "x" i]]
        )
      |> hash_reduce ~loc *)
      (* comps
      |> List.mapi (fun i _ ->
          label_field ~loc "x" i
        )
      |> List.rev
      |> List.fold_left (fun acc field ->
          [%expr ([%e field], [%e acc])]
        ) [%expr ()] *)
      comps
      |> List.map (fun comp_type ->
          expr ~loc ~quoter comp_type
        )
      |> hash_reduce' ~loc
    in
    let pat prefix =
      comps
      |> List.mapi (fun i _ ->
          let name = prefix ^ string_of_int i in
          ppat_var ~loc {loc; txt = name}
        )
      |> ppat_tuple ~loc
    in
    (* [%expr fun [%p pat "x"] -> [%e body]] *)
    let f =
      let body =
        comps
        |> List.mapi (fun i _ ->
            label_field ~loc "x" i
          )
        |> List.rev
        |> (function
          | (last::others) -> List.fold_left (fun acc field ->
            [%expr ([%e field], [%e acc])]
          ) last others
          | [] -> assert false
        )
      in
      [%expr fun [%p pat "x"] -> [%e body]]
    in
    Arg.apply_iso ~loc body f

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
