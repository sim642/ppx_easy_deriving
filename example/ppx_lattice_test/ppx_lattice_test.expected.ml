module type Lattice  =
  sig
    type t
    val leq : t -> t -> bool
    val join : t -> t -> t
    val bot : unit -> t
    val is_bot : t -> bool
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
              fun a -> ())
          [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> true)
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
  ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in fun a -> ())
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
              fun (a0, a1) -> fun (b0, b1) -> (__0 a0 b0) && (__1 a1 b1))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a0, a1) -> fun (b0, b1) -> ((__0 a0 b0), (__1 a1 b1)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a -> ((__0 a), (__1 a)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a0, a1) -> (__0 a0) && (__1 a1))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal : t -> t -> bool) =
          let __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a0, a1) -> fun (b0, b1) -> (__0 a0 b0) && (__1 a1 b1))
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
              fun (a0, a1, a2) ->
                fun (b0, b1, b2) ->
                  (__0 a0 b0) && ((__1 a1 b1) && (__2 a2 b2)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __2 = L3.join
          and __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a0, a1, a2) ->
                fun (b0, b1, b2) -> ((__0 a0 b0), (__1 a1 b1), (__2 a2 b2)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __2 = L3.bot
          and __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a -> ((__0 a), (__1 a), (__2 a)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __2 = L3.is_bot
          and __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a0, a1, a2) -> (__0 a0) && ((__1 a1) && (__2 a2)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal : t -> t -> bool) =
          let __2 = L3.easy_equal
          and __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a0, a1, a2) ->
                fun (b0, b1, b2) ->
                  (__0 a0 b0) && ((__1 a1 b1) && (__2 a2 b2)))
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
                            b2)) ((fun (f0, f1, f2) -> (f0, (f1, f2))) a)
                    ((fun (f0, f1, f2) -> (f0, (f1, f2))) b))
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
              fun { f1 = a0 } -> fun { f1 = b0 } -> __0 a0 b0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0 } -> fun { f1 = b0 } -> { f1 = (__0 a0 b0) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a -> { f1 = (__0 a) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0 } -> __0 a0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal : t -> t -> bool) =
          let __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0 } -> fun { f1 = b0 } -> __0 a0 b0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __0 = L1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  __0 ((fun { f1 = f0 } -> f0) a) ((fun { f1 = f0 } -> f0) b))
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
              fun { f1 = a0; f2 = a1 } ->
                fun { f1 = b0; f2 = b1 } -> (__0 a0 b0) && (__1 a1 b1))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0; f2 = a1 } ->
                fun { f1 = b0; f2 = b1 } ->
                  { f1 = (__0 a0 b0); f2 = (__1 a1 b1) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a -> { f1 = (__0 a); f2 = (__1 a) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0; f2 = a1 } -> (__0 a0) && (__1 a1))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal : t -> t -> bool) =
          let __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0; f2 = a1 } ->
                fun { f1 = b0; f2 = b1 } -> (__0 a0 b0) && (__1 a1 b1))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __1 = L2.easy_equal2
          and __0 = L1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (a1, b1) -> fun (a2, b2) -> (__0 a1 a2) && (__1 b1 b2))
                    ((fun { f1 = f0; f2 = f1 } -> (f0, f1)) a)
                    ((fun { f1 = f0; f2 = f1 } -> (f0, f1)) b))
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
              fun { f1 = a0; f2 = a1; f3 = a2 } ->
                fun { f1 = b0; f2 = b1; f3 = b2 } ->
                  (__0 a0 b0) && ((__1 a1 b1) && (__2 a2 b2)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __2 = L3.join
          and __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0; f2 = a1; f3 = a2 } ->
                fun { f1 = b0; f2 = b1; f3 = b2 } ->
                  { f1 = (__0 a0 b0); f2 = (__1 a1 b1); f3 = (__2 a2 b2) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __2 = L3.bot
          and __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a -> { f1 = (__0 a); f2 = (__1 a); f3 = (__2 a) })
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __2 = L3.is_bot
          and __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0; f2 = a1; f3 = a2 } ->
                (__0 a0) && ((__1 a1) && (__2 a2)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal : t -> t -> bool) =
          let __2 = L3.easy_equal
          and __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0; f2 = a1; f3 = a2 } ->
                fun { f1 = b0; f2 = b1; f3 = b2 } ->
                  (__0 a0 b0) && ((__1 a1 b1) && (__2 a2 b2)))
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
                            b2))
                    ((fun { f1 = f0; f2 = f1; f3 = f2 } -> (f0, (f1, f2))) a)
                    ((fun { f1 = f0; f2 = f1; f3 = f2 } -> (f0, (f1, f2))) b))
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
                  | (C2 (a0, a1), C2 (b0, b1)) -> (__0 a0 b0) && (__1 a1 b1)
                  | (C3 { f1 = a0; f2 = a1 }, C3 { f1 = b0; f2 = b1 }) ->
                      (__4 a0 b0) && (__5 a1 b1)
                  | (C4 a0, C4 b0) -> __8 a0 b0
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
                      | C2 (a0, a1) -> Either.Right (Either.Left (a0, a1))
                      | C3 { f1 = a0; f2 = a1 } ->
                          Either.Right (Either.Right (Either.Left (a0, a1)))
                      | C4 a0 ->
                          Either.Right (Either.Right (Either.Right a0))) a)
                    ((function
                      | C1 -> Either.Left ()
                      | C2 (a0, a1) -> Either.Right (Either.Left (a0, a1))
                      | C3 { f1 = a0; f2 = a1 } ->
                          Either.Right (Either.Right (Either.Left (a0, a1)))
                      | C4 a0 ->
                          Either.Right (Either.Right (Either.Right a0))) b))
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
          let __5 = L5.easy_equal
          and __4 = L5.easy_equal
          and __3 = L2.easy_equal
          and __2 = L1.easy_equal
          and __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x ->
                fun y ->
                  match (x, y) with
                  | (`C1, `C1) -> true
                  | (`C2 a, `C2 b) ->
                      ((fun (a0, a1) ->
                          fun (b0, b1) -> (__0 a0 b0) && (__1 a1 b1))) a b
                  | (`C4 a, `C4 b) -> __4 a b
                  | (_, _) -> false)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __5 = L5.easy_equal2
          and __4 = L5.easy_equal2
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
                                     __5 b1 b2
                                 | (_, _) -> false)) b1 b2
                       | (_, _) -> false)
                    ((function
                      | `C1 -> Either.Left ()
                      | `C2 a -> Either.Right (Either.Left a)
                      | `C4 a -> Either.Right (Either.Right a)) a)
                    ((function
                      | `C1 -> Either.Left ()
                      | `C2 a -> Either.Right (Either.Left a)
                      | `C4 a -> Either.Right (Either.Right a)) b))
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
          let __3 = easy_equal_u
          and __2 = easy_equal_u
          and __1 = PV.easy_equal
          and __0 = PV.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun x ->
                fun y ->
                  match (x, y) with
                  | ((#PV.t as a), (#PV.t as b)) -> __0 a b
                  | (`C5, `C5) -> true
                  | ((#u as a), (#u as b)) -> __2 a b
                  | (_, _) -> false)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __3 = easy_equal2_u
          and __2 = easy_equal2_u
          and __1 = PV.easy_equal2
          and __0 = PV.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun a1 ->
                     fun a2 ->
                       match (a1, a2) with
                       | (Either.Left a1, Either.Left a2) -> __1 a1 a2
                       | (Either.Right b1, Either.Right b2) ->
                           ((fun a1 ->
                               fun a2 ->
                                 match (a1, a2) with
                                 | (Either.Left a1, Either.Left a2) ->
                                     ((fun () -> fun () -> true)) a1 a2
                                 | (Either.Right b1, Either.Right b2) ->
                                     __3 b1 b2
                                 | (_, _) -> false)) b1 b2
                       | (_, _) -> false)
                    ((function
                      | #PV.t as a -> Either.Left a
                      | `C5 -> Either.Right (Either.Left ())
                      | #u as a -> Either.Right (Either.Right a)) a)
                    ((function
                      | #PV.t as a -> Either.Left a
                      | `C5 -> Either.Right (Either.Left ())
                      | #u as a -> Either.Right (Either.Right a)) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
