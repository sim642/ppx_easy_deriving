module type Lattice  =
  sig
    type t
    val leq : t -> t -> bool
    val join : t -> t -> t
    val bot : unit -> t
  end
module Direct(L1:Lattice) : Lattice =
  struct
    type t = L1.t[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __0 () = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0 ())
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __0 () = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0 ())
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __0 () = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0 ())
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple2(L1:Lattice)(L2:Lattice) : Lattice =
  struct
    type t = (L1.t * L2.t)[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __1 () = L2.leq
          and __0 () = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (a1, b1) ->
                     fun (a2, b2) -> ((__0 ()) a1 a2) && ((__1 ()) b1 b2))
                    ((fun (x0, x1) -> (x0, x1)) a)
                    ((fun (x0, x1) -> (x0, x1)) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __1 () = L2.join
          and __0 () = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (x0, x1) -> (x0, x1))
                    ((fun (a1, b1) ->
                        fun (a2, b2) -> (((__0 ()) a1 a2), ((__1 ()) b1 b2)))
                       ((fun (x0, x1) -> (x0, x1)) a)
                       ((fun (x0, x1) -> (x0, x1)) b)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __1 () = L2.bot
          and __0 () = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () ->
                (fun (x0, x1) -> (x0, x1))
                  ((fun () -> (((__0 ()) ()), ((__1 ()) ()))) ()))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple3(L1:Lattice)(L2:Lattice)(L3:Lattice) : Lattice =
  struct
    type t = (L1.t * L2.t * L3.t)[@@deriving lattice]
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
        let rec (join : t -> t -> t) =
          let __2 () = L3.join
          and __1 () = L2.join
          and __0 () = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (x0, (x1, x2)) -> (x0, x1, x2))
                    ((fun (a1, b1) ->
                        fun (a2, b2) ->
                          (((__0 ()) a1 a2),
                            ((fun (a1, b1) ->
                                fun (a2, b2) ->
                                  (((__1 ()) a1 a2), ((__2 ()) b1 b2))) b1 b2)))
                       ((fun (x0, x1, x2) -> (x0, (x1, x2))) a)
                       ((fun (x0, x1, x2) -> (x0, (x1, x2))) b)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __2 () = L3.bot
          and __1 () = L2.bot
          and __0 () = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () ->
                (fun (x0, (x1, x2)) -> (x0, x1, x2))
                  ((fun () ->
                      (((__0 ()) ()),
                        ((fun () -> (((__1 ()) ()), ((__2 ()) ()))) ()))) ()))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
