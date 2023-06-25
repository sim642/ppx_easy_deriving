module type Lattice  =
  sig
    type t
    val leq : t -> t -> bool
    val join : t -> t -> t
    val bot : unit -> t
    val is_bot : t -> bool
    val relift : t -> t
    val easy_equal : t -> t -> bool
    val easy_equal2 : t -> t -> bool
  end
module type Lattice_derived  =
  sig
    type t[@@deriving (lattice, easy_equal, easy_equal2)]
    include
      sig
        val leq : t -> t -> bool
        val join : t -> t -> t
        val bot : unit -> t
        val is_bot : t -> bool
        val relift : t -> t
        val easy_equal : t -> t -> bool
        val easy_equal2 : t -> t -> bool
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
module Lattice_derived_test1(L:Lattice) : Lattice_derived = L 
module Lattice_derived_test2(L:Lattice_derived) : Lattice = L 
module Unit : Lattice =
  struct
    type t = unit[@@deriving (lattice, easy_equal, easy_equal2)]
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
        let rec (easy_equal : t -> t -> bool) =
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> fun () -> true)
          [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> fun () -> true)
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
let _ =
  ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
      fun () -> fun () -> true)
  [@ocaml.warning "-A"])
let _ =
  ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
      fun () -> fun () -> true)
  [@ocaml.warning "-A"])
module Direct(L1:Lattice) : Lattice =
  struct
    type t = L1.t[@@deriving (lattice, easy_equal, easy_equal2)]
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
        let rec (easy_equal : t -> t -> bool) =
          let __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __0 = L1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple2(L1:Lattice)(L2:Lattice) : Lattice =
  struct
    type t = (L1.t * L2.t)[@@deriving (lattice, easy_equal, easy_equal2)]
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
        let rec (easy_equal : t -> t -> bool) =
          let __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a1, a2) -> fun (b1, b2) -> (__0 a1 b1) && (__1 a2 b2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __1 = L2.easy_equal2
          and __0 = L1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a1, b1) -> fun (a2, b2) -> (__0 a1 a2) && (__1 b1 b2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple3(L1:Lattice)(L2:Lattice)(L3:Lattice) : Lattice =
  struct
    type t = (L1.t * L2.t * L3.t)[@@deriving
                                   (lattice, easy_equal, easy_equal2)]
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
        let rec (easy_equal : t -> t -> bool) =
          let __2 = L3.easy_equal
          and __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a1, a2, a3) ->
                fun (b1, b2, b3) ->
                  (__0 a1 b1) && ((__1 a2 b2) && (__2 a3 b3)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __2 = L3.easy_equal2
          and __1 = L2.easy_equal2
          and __0 = L1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (a1, b1) ->
                     fun (a2, b2) ->
                       (__0 a1 a2) &&
                         ((fun (a1, b1) ->
                             fun (a2, b2) -> (__1 a1 a2) && (__2 b1 b2)) b1
                            b2)) ((fun (f1, f2, f3) -> (f1, (f2, f3))) a)
                    ((fun (f1, f2, f3) -> (f1, (f2, f3))) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record1(L1:Lattice) : Lattice =
  struct
    type t = {
      f1: L1.t }[@@deriving (lattice, easy_equal, easy_equal2)]
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
        let rec (easy_equal : t -> t -> bool) =
          let __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a1 } -> fun { f1 = b1 } -> __0 a1 b1)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __0 = L1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b -> __0 ((fun { f1 } -> f1) a) ((fun { f1 } -> f1) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record2(L1:Lattice)(L2:Lattice) : Lattice =
  struct
    type t = {
      f1: L1.t ;
      f2: L2.t }[@@deriving (lattice, easy_equal, easy_equal2)]
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
        let rec (easy_equal : t -> t -> bool) =
          let __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a1; f2 = a2 } ->
                fun { f1 = b1; f2 = b2 } -> (__0 a1 b1) && (__1 a2 b2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __1 = L2.easy_equal2
          and __0 = L1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (a1, b1) -> fun (a2, b2) -> (__0 a1 a2) && (__1 b1 b2))
                    ((fun { f1; f2 } -> (f1, f2)) a)
                    ((fun { f1; f2 } -> (f1, f2)) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record3(L1:Lattice)(L2:Lattice)(L3:Lattice) : Lattice =
  struct
    type t = {
      f1: L1.t ;
      f2: L2.t ;
      f3: L3.t }[@@deriving (lattice, easy_equal, easy_equal2)]
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
        let rec (easy_equal : t -> t -> bool) =
          let __2 = L3.easy_equal
          and __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a1; f2 = a2; f3 = a3 } ->
                fun { f1 = b1; f2 = b2; f3 = b3 } ->
                  (__0 a1 b1) && ((__1 a2 b2) && (__2 a3 b3)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __2 = L3.easy_equal2
          and __1 = L2.easy_equal2
          and __0 = L1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (a1, b1) ->
                     fun (a2, b2) ->
                       (__0 a1 a2) &&
                         ((fun (a1, b1) ->
                             fun (a2, b2) -> (__1 a1 a2) && (__2 b1 b2)) b1
                            b2)) ((fun { f1; f2; f3 } -> (f1, (f2, f3))) a)
                    ((fun { f1; f2; f3 } -> (f1, (f2, f3))) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Variant(L1:Lattice)(L2:Lattice)(L3:Lattice)(L4:Lattice)(L5:Lattice) =
  struct
    type t =
      | C1 
      | C2 of L1.t * L2.t 
      | C3 of {
      f1: L3.t ;
      f2: L4.t } 
      | C4 of L5.t [@@deriving (easy_equal, easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __9 = L5.easy_equal
          and __8 = L5.easy_equal
          and __7 = L4.easy_equal
          and __6 = L3.easy_equal
          and __5 = L4.easy_equal
          and __4 = L3.easy_equal
          and __3 = L2.easy_equal
          and __2 = L1.easy_equal
          and __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x ->
                fun y ->
                  match (x, y) with
                  | (C1, C1) -> true
                  | (C2 (a1, a2), C2 (b1, b2)) -> (__0 a1 b1) && (__1 a2 b2)
                  | (C3 { f1 = a1; f2 = a2 }, C3 { f1 = b1; f2 = b2 }) ->
                      (__4 a1 b1) && (__5 a2 b2)
                  | (C4 a1, C4 b1) -> __8 a1 b1
                  | (_, _) -> false)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __9 = L5.easy_equal2
          and __8 = L5.easy_equal2
          and __7 = L4.easy_equal2
          and __6 = L3.easy_equal2
          and __5 = L4.easy_equal2
          and __4 = L3.easy_equal2
          and __3 = L2.easy_equal2
          and __2 = L1.easy_equal2
          and __1 = L2.easy_equal2
          and __0 = L1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun a1 ->
                     fun a2 ->
                       match (a1, a2) with
                       | (Either.Left a1, Either.Left a2) ->
                           ((fun () -> fun () -> true)) a1 a2
                       | (Either.Right b1, Either.Right b2) ->
                           ((fun a1 ->
                               fun a2 ->
                                 match (a1, a2) with
                                 | (Either.Left a1, Either.Left a2) ->
                                     ((fun (a1, b1) ->
                                         fun (a2, b2) ->
                                           (__2 a1 a2) && (__3 b1 b2))) a1 a2
                                 | (Either.Right b1, Either.Right b2) ->
                                     ((fun a1 ->
                                         fun a2 ->
                                           match (a1, a2) with
                                           | (Either.Left a1, Either.Left a2)
                                               ->
                                               ((fun (a1, b1) ->
                                                   fun (a2, b2) ->
                                                     (__6 a1 a2) &&
                                                       (__7 b1 b2))) a1 a2
                                           | (Either.Right b1, Either.Right
                                              b2) -> __9 b1 b2
                                           | (_, _) -> false)) b1 b2
                                 | (_, _) -> false)) b1 b2
                       | (_, _) -> false)
                    ((function
                      | C1 -> Either.Left ()
                      | C2 (f1, f2) -> Either.Right (Either.Left (f1, f2))
                      | C3 { f1; f2 } ->
                          Either.Right (Either.Right (Either.Left (f1, f2)))
                      | C4 f1 ->
                          Either.Right (Either.Right (Either.Right f1))) a)
                    ((function
                      | C1 -> Either.Left ()
                      | C2 (f1, f2) -> Either.Right (Either.Left (f1, f2))
                      | C3 { f1; f2 } ->
                          Either.Right (Either.Right (Either.Left (f1, f2)))
                      | C4 f1 ->
                          Either.Right (Either.Right (Either.Right f1))) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
module PolyVariant(L1:Lattice)(L2:Lattice)(L5:Lattice) =
  struct
    type t = [ `C1  | `C2 of (L1.t * L2.t)  | `C4 of L5.t ][@@deriving
                                                             (easy_equal,
                                                               easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __2 = L5.easy_equal
          and __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x ->
                fun y ->
                  match (x, y) with
                  | (`C1, `C1) -> true
                  | (`C2 a, `C2 b) ->
                      ((fun (a1, a2) ->
                          fun (b1, b2) -> (__0 a1 b1) && (__1 a2 b2))) a b
                  | (`C4 a, `C4 b) -> __2 a b
                  | (_, _) -> false)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __2 = L5.easy_equal2
          and __1 = L2.easy_equal2
          and __0 = L1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun a1 ->
                     fun a2 ->
                       match (a1, a2) with
                       | (Either.Left a1, Either.Left a2) ->
                           ((fun () -> fun () -> true)) a1 a2
                       | (Either.Right b1, Either.Right b2) ->
                           ((fun a1 ->
                               fun a2 ->
                                 match (a1, a2) with
                                 | (Either.Left a1, Either.Left a2) ->
                                     ((fun (a1, b1) ->
                                         fun (a2, b2) ->
                                           (__0 a1 a2) && (__1 b1 b2))) a1 a2
                                 | (Either.Right b1, Either.Right b2) ->
                                     __2 b1 b2
                                 | (_, _) -> false)) b1 b2
                       | (_, _) -> false)
                    ((function
                      | `C1 -> Either.Left ()
                      | `C2 f -> Either.Right (Either.Left f)
                      | `C4 f -> Either.Right (Either.Right f)) a)
                    ((function
                      | `C1 -> Either.Left ()
                      | `C2 f -> Either.Right (Either.Left f)
                      | `C4 f -> Either.Right (Either.Right f)) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
module PolyVariantInherit(L1:Lattice)(L2:Lattice)(L5:Lattice) =
  struct
    module PV = (((PolyVariant)(L1))(L2))(L5)
    type u = PV.t[@@deriving (easy_equal, easy_equal2)]
    include
      struct
        let rec (easy_equal_u : u -> u -> bool) =
          let __0 = PV.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2_u : u -> u -> bool) =
          let __0 = PV.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type t = [ | PV.t | `C5  | u][@@deriving (easy_equal, easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __1 = easy_equal_u
          and __0 = PV.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x ->
                fun y ->
                  match (x, y) with
                  | ((#PV.t as a), (#PV.t as b)) -> __0 a b
                  | (`C5, `C5) -> true
                  | ((#u as a), (#u as b)) -> __1 a b
                  | (_, _) -> false)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __1 = easy_equal2_u
          and __0 = PV.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun a1 ->
                     fun a2 ->
                       match (a1, a2) with
                       | (Either.Left a1, Either.Left a2) -> __0 a1 a2
                       | (Either.Right b1, Either.Right b2) ->
                           ((fun a1 ->
                               fun a2 ->
                                 match (a1, a2) with
                                 | (Either.Left a1, Either.Left a2) ->
                                     ((fun () -> fun () -> true)) a1 a2
                                 | (Either.Right b1, Either.Right b2) ->
                                     __1 b1 b2
                                 | (_, _) -> false)) b1 b2
                       | (_, _) -> false)
                    ((function
                      | #PV.t as f -> Either.Left f
                      | `C5 -> Either.Right (Either.Left ())
                      | #u as f -> Either.Right (Either.Right f)) a)
                    ((function
                      | #PV.t as f -> Either.Left f
                      | `C5 -> Either.Right (Either.Left ())
                      | #u as f -> Either.Right (Either.Right f)) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
