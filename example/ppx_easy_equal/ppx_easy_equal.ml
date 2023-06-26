open Ppxlib
open Ast_builder.Default
open Ppx_easy_deriving

(* $MDX part-begin=easy_equal *)
module EasyEqualArg: Product.Variant.S =
struct
  (** Name of deriver. *)
  let name = "easy_equal"

  (** Type of derived [easy_equal] function for type [t]. *)
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]

  (** [easy_equal] function body for a product type with list of given [easy_equal] functions for the elements. *)
  let product_body ~loc es pel per =
    let body =
      let esl = Pat_exp.to_exps ~loc pel in
      let esr = Pat_exp.to_exps ~loc per in
      Util.map3 (fun e l r ->
          (* Apply [easy_equal] function per element. *)
          [%expr [%e e] [%e l] [%e r]]
        ) es esl esr
    in
    (* Reduce per-element [easy_equal] results to a result for the entire product. *)
    Util.reduce ~unit:[%expr true] ~both:(fun acc x ->
        [%expr [%e acc] && [%e x]]
      ) body

  (** [easy_equal] function for a product type with list of given [easy_equal] functions for the elements.
      [pe_create] allows creating patterns and expressions of the actual product type. *)
  let product ~loc ~pe_create es =
    let pel = pe_create ~prefix:"l" in
    let per = pe_create ~prefix:"r" in
    let body = product_body ~loc es pel per in
    let pl = Pat_exp.to_pat ~loc pel in
    let pr = Pat_exp.to_pat ~loc per in
    [%expr fun [%p pl] [%p pr] -> [%e body]]

  (** [easy_equal] function for a (polymorphic) variant type with list of given [easy_equal] functions for the (polymorphic) variant constructors. *)
  let variant ~loc ces =
    let cases = List.map (fun (c, c2, _es, es2) ->
        let pel = c ~prefix:"l" in
        let per = c ~prefix:"r" in
        let pel2 = c2 ~prefix:"l" in
        let per2 = c2 ~prefix:"r" in
        let body = product_body ~loc es2 pel2 per2 in
        let pa = Pat_exp.to_pat ~loc pel in
        let pb = Pat_exp.to_pat ~loc per in
        (* Equality case for one constructor with given product argument. *)
        case ~lhs:[%pat? [%p pa], [%p pb]]
          ~guard:None
          ~rhs:body
      ) ces
    in
    let fallback =
      (* Inequality case as fallback. *)
      case ~lhs:[%pat? _, _]
        ~guard:None
        ~rhs:[%expr false]
    in
    let body = pexp_match ~loc [%expr l, r] (cases @ [fallback]) in
    [%expr fun l r -> [%e body]]
end

(* Create full deriver from product deriver and register it with ppxlib. *)
module EasyEqualDeriver = Deriver.Make (Product.Variant.Make (EasyEqualArg))
let _ = EasyEqualDeriver.register ()
(* $MDX part-end *)


(* $MDX part-begin=easy_equal2 *)
module EasyEqual2Arg: Simple.Variant.S =
struct
  (** Name of deriver. *)
  let name = "easy_equal2"

  (** Type of derived [easy_equal2] function for type [t]. *)
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]

  (** [easy_equal2] function for [unit] type. *)
  let unit ~loc = [%expr fun () () -> true]

  (** [easy_equal2] function for a pair of types with given [easy_equal2] functions [e1] and [e2]. *)
  let both ~loc e1 e2 = [%expr fun (l1, l2) (r1, r2) -> [%e e1] l1 r1 && [%e e2] l2 r2]

  (** [easy_equal2] function for the empty type. *)
  let empty ~loc = [%expr fun _ _ -> true]

  (** [easy_equal2] function for an either of types with given [easy_equal2] functions [e1] and [e2]. *)
  let either ~loc e1 e2 =
    [%expr fun l r ->
      match l, r with
      | Either.Left l, Either.Left r -> [%e e1] l r
      | Either.Right l, Either.Right r -> [%e e2] l r
      | _, _ -> false
    ]

  (** Apply isomorphism to given [easy_equal2] function.
      This allows {!both} and {!either} to be applied to tuples, records and (polymorphic) variants of arbitrary size. *)
  let apply_iso ~loc easy_equal2 ~f ~f':_ =
    [%expr fun l r -> [%e easy_equal2] ([%e f] l) ([%e f] r)]
end

(* Create full deriver from simple deriver and register it with ppxlib. *)
module EasyEqual2Deriver = Deriver.Make (Simple.Variant.Reduce (EasyEqual2Arg))
let _ = EasyEqual2Deriver.register ()
(* $MDX part-end *)
