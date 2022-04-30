module type Leq  = sig type t val leq : t -> t -> bool end
module Prod(L1:Leq)(L2:Leq)(L3:Leq) : Leq =
  struct
    type t = (L1.t * L2.t * L3.t)[@@deriving leq]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __2 () = L3.leq
          and __1 () = L2.leq
          and __0 () = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (a1, b1) ->
                     fun (a2, b2) ->
                       ((__0 ()) a1 a2) &&
                         ((fun (a1, b1) ->
                             fun (a2, b2) ->
                               ((__1 ()) a1 a2) && ((__2 ()) b1 b2)) b1 b2))
                    ((fun (x0, x1, x2) -> (x0, (x1, x2))) a)
                    ((fun (x0, x1, x2) -> (x0, (x1, x2))) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
