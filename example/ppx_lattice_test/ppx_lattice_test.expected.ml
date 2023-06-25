module type Lattice  =
  sig
    type t
    val leq : t -> t -> bool
    val join : t -> t -> t
    val bot : unit -> t
    val is_bot : t -> bool
    val relift : t -> t
  end
module type Lattice_derived  =
  sig
    type t[@@deriving lattice]
    include
      sig
        val leq : t -> t -> bool
        val join : t -> t -> t
        val bot : unit -> t
        val is_bot : t -> bool
        val relift : t -> t
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
module Lattice_derived_test1(L:Lattice) : Lattice_derived = L 
module Lattice_derived_test2(L:Lattice_derived) : Lattice = L 
module Unit : Lattice =
  struct
    type t = unit[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> fun () -> true)
          [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> fun () -> ())
          [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x -> ())
          [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> true)
          [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (relift : t -> t) =
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> ())
          [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
let _ =
  ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in fun () -> true)
  [@ocaml.warning "-A"])
let _ =
  ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in fun x -> ())
  [@ocaml.warning "-A"])
let _ =
  ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
      fun () -> fun () -> ())
  [@ocaml.warning "-A"])
let _ =
  ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
      fun () -> fun () -> true)
  [@ocaml.warning "-A"])
module Direct(L1:Lattice) : Lattice =
  struct
    type t = L1.t[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __0 = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (relift : t -> t) =
          let __0 = L1.relift in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple2(L1:Lattice)(L2:Lattice) : Lattice =
  struct
    type t = (L1.t * L2.t)[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __1 = L2.leq
          and __0 = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (l1, l2) -> fun (r1, r2) -> (__0 l1 r1) && (__1 l2 r2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (l1, l2) -> fun (r1, r2) -> ((__0 l1 r1), (__1 l2 r2)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x -> ((__0 x), (__1 x)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (x1, x2) -> (__0 x1) && (__1 x2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (relift : t -> t) =
          let __1 = L2.relift
          and __0 = L1.relift in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (x1, x2) -> ((__0 x1), (__1 x2)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple3(L1:Lattice)(L2:Lattice)(L3:Lattice) : Lattice =
  struct
    type t = (L1.t * L2.t * L3.t)[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __2 = L3.leq
          and __1 = L2.leq
          and __0 = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (l1, l2, l3) ->
                fun (r1, r2, r3) ->
                  (__0 l1 r1) && ((__1 l2 r2) && (__2 l3 r3)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __2 = L3.join
          and __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (l1, l2, l3) ->
                fun (r1, r2, r3) -> ((__0 l1 r1), (__1 l2 r2), (__2 l3 r3)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __2 = L3.bot
          and __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x -> ((__0 x), (__1 x), (__2 x)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __2 = L3.is_bot
          and __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (x1, x2, x3) -> (__0 x1) && ((__1 x2) && (__2 x3)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (relift : t -> t) =
          let __2 = L3.relift
          and __1 = L2.relift
          and __0 = L1.relift in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (x1, x2, x3) -> ((__0 x1), (__1 x2), (__2 x3)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record1(L1:Lattice) : Lattice =
  struct
    type t = {
      f1: L1.t }[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __0 = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = l1 } -> fun { f1 = r1 } -> __0 l1 r1)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = l1 } -> fun { f1 = r1 } -> { f1 = (__0 l1 r1) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x -> { f1 = (__0 x) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = x1 } -> __0 x1)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (relift : t -> t) =
          let __0 = L1.relift in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = x1 } -> { f1 = (__0 x1) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record2(L1:Lattice)(L2:Lattice) : Lattice =
  struct
    type t = {
      f1: L1.t ;
      f2: L2.t }[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __1 = L2.leq
          and __0 = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = l1; f2 = l2 } ->
                fun { f1 = r1; f2 = r2 } -> (__0 l1 r1) && (__1 l2 r2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = l1; f2 = l2 } ->
                fun { f1 = r1; f2 = r2 } ->
                  { f1 = (__0 l1 r1); f2 = (__1 l2 r2) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x -> { f1 = (__0 x); f2 = (__1 x) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = x1; f2 = x2 } -> (__0 x1) && (__1 x2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (relift : t -> t) =
          let __1 = L2.relift
          and __0 = L1.relift in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = x1; f2 = x2 } -> { f1 = (__0 x1); f2 = (__1 x2) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record3(L1:Lattice)(L2:Lattice)(L3:Lattice) : Lattice =
  struct
    type t = {
      f1: L1.t ;
      f2: L2.t ;
      f3: L3.t }[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __2 = L3.leq
          and __1 = L2.leq
          and __0 = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = l1; f2 = l2; f3 = l3 } ->
                fun { f1 = r1; f2 = r2; f3 = r3 } ->
                  (__0 l1 r1) && ((__1 l2 r2) && (__2 l3 r3)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __2 = L3.join
          and __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = l1; f2 = l2; f3 = l3 } ->
                fun { f1 = r1; f2 = r2; f3 = r3 } ->
                  { f1 = (__0 l1 r1); f2 = (__1 l2 r2); f3 = (__2 l3 r3) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __2 = L3.bot
          and __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x -> { f1 = (__0 x); f2 = (__1 x); f3 = (__2 x) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __2 = L3.is_bot
          and __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = x1; f2 = x2; f3 = x3 } ->
                (__0 x1) && ((__1 x2) && (__2 x3)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (relift : t -> t) =
          let __2 = L3.relift
          and __1 = L2.relift
          and __0 = L1.relift in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = x1; f2 = x2; f3 = x3 } ->
                { f1 = (__0 x1); f2 = (__1 x2); f3 = (__2 x3) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
