module type Lattice  =
  sig
    type t
    val leq : t -> t -> bool
    val join : t -> t -> t
    val bot : unit -> t
    val is_bot : t -> bool
    val easy_equal : t -> t -> bool
  end
module Unit : Lattice =
  struct
    type t = unit[@@deriving (lattice, easy_equal)]
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
              fun () -> ())
          [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> true)
          [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal : t -> t -> bool) =
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> fun () -> true)
          [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Direct(L1:Lattice) : Lattice =
  struct
    type t = L1.t[@@deriving (lattice, easy_equal)]
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
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple2(L1:Lattice)(L2:Lattice) : Lattice =
  struct
    type t = (L1.t * L2.t)[@@deriving (lattice, easy_equal)]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __1 = L2.leq
          and __0 = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a1, b1) -> fun (a2, b2) -> (__0 a1 a2) && (__1 b1 b2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a1, b1) -> fun (a2, b2) -> ((__0 a1 a2), (__1 b1 b2)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> ((__0 ()), (__1 ())))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a, b) -> (__0 a) && (__1 b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal : t -> t -> bool) =
          let __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (a0, a1) -> fun (b0, b1) -> (__0 a0 b0) && (__1 a1 b1))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple3(L1:Lattice)(L2:Lattice)(L3:Lattice) : Lattice =
  struct
    type t = (L1.t * L2.t * L3.t)[@@deriving (lattice, easy_equal)]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __2 = L3.leq
          and __1 = L2.leq
          and __0 = L1.leq in
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
        let rec (join : t -> t -> t) =
          let __2 = L3.join
          and __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (f'0, (f'1, f'2)) -> (f'0, f'1, f'2))
                    ((fun (a1, b1) ->
                        fun (a2, b2) ->
                          ((__0 a1 a2),
                            ((fun (a1, b1) ->
                                fun (a2, b2) -> ((__1 a1 a2), (__2 b1 b2)))
                               b1 b2)))
                       ((fun (f0, f1, f2) -> (f0, (f1, f2))) a)
                       ((fun (f0, f1, f2) -> (f0, (f1, f2))) b)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __2 = L3.bot
          and __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () ->
                (fun (f'0, (f'1, f'2)) -> (f'0, f'1, f'2))
                  ((fun () ->
                      ((__0 ()), ((fun () -> ((__1 ()), (__2 ()))) ()))) ()))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __2 = L3.is_bot
          and __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                (fun (a, b) ->
                   (__0 a) && ((fun (a, b) -> (__1 a) && (__2 b)) b))
                  ((fun (f0, f1, f2) -> (f0, (f1, f2))) a))
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
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record1(L1:Lattice) : Lattice =
  struct
    type t = {
      f1: L1.t }[@@deriving (lattice, easy_equal)]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __0 = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  __0 ((fun { f1 = f0 } -> f0) a) ((fun { f1 = f0 } -> f0) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun f'0 -> { f1 = f'0 })
                    (__0 ((fun { f1 = f0 } -> f0) a)
                       ((fun { f1 = f0 } -> f0) b)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () -> (fun f'0 -> { f1 = f'0 }) (__0 ()))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a -> __0 ((fun { f1 = f0 } -> f0) a))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal : t -> t -> bool) =
          let __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0 } -> fun { f1 = b0 } -> __0 a0 b0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record2(L1:Lattice)(L2:Lattice) : Lattice =
  struct
    type t = {
      f1: L1.t ;
      f2: L2.t }[@@deriving (lattice, easy_equal)]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __1 = L2.leq
          and __0 = L1.leq in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (a1, b1) -> fun (a2, b2) -> (__0 a1 a2) && (__1 b1 b2))
                    ((fun { f1 = f0; f2 = f1 } -> (f0, f1)) a)
                    ((fun { f1 = f0; f2 = f1 } -> (f0, f1)) b))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (f'0, f'1) -> { f1 = f'0; f2 = f'1 })
                    ((fun (a1, b1) ->
                        fun (a2, b2) -> ((__0 a1 a2), (__1 b1 b2)))
                       ((fun { f1 = f0; f2 = f1 } -> (f0, f1)) a)
                       ((fun { f1 = f0; f2 = f1 } -> (f0, f1)) b)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () ->
                (fun (f'0, f'1) -> { f1 = f'0; f2 = f'1 })
                  ((fun () -> ((__0 ()), (__1 ()))) ()))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                (fun (a, b) -> (__0 a) && (__1 b))
                  ((fun { f1 = f0; f2 = f1 } -> (f0, f1)) a))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal : t -> t -> bool) =
          let __1 = L2.easy_equal
          and __0 = L1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = a0; f2 = a1 } ->
                fun { f1 = b0; f2 = b1 } -> (__0 a0 b0) && (__1 a1 b1))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record3(L1:Lattice)(L2:Lattice)(L3:Lattice) : Lattice =
  struct
    type t = {
      f1: L1.t ;
      f2: L2.t ;
      f3: L3.t }[@@deriving (lattice, easy_equal)]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let __2 = L3.leq
          and __1 = L2.leq
          and __0 = L1.leq in
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
        let rec (join : t -> t -> t) =
          let __2 = L3.join
          and __1 = L2.join
          and __0 = L1.join in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                fun b ->
                  (fun (f'0, (f'1, f'2)) -> { f1 = f'0; f2 = f'1; f3 = f'2 })
                    ((fun (a1, b1) ->
                        fun (a2, b2) ->
                          ((__0 a1 a2),
                            ((fun (a1, b1) ->
                                fun (a2, b2) -> ((__1 a1 a2), (__2 b1 b2)))
                               b1 b2)))
                       ((fun { f1 = f0; f2 = f1; f3 = f2 } -> (f0, (f1, f2)))
                          a)
                       ((fun { f1 = f0; f2 = f1; f3 = f2 } -> (f0, (f1, f2)))
                          b)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let __2 = L3.bot
          and __1 = L2.bot
          and __0 = L1.bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun () ->
                (fun (f'0, (f'1, f'2)) -> { f1 = f'0; f2 = f'1; f3 = f'2 })
                  ((fun () ->
                      ((__0 ()), ((fun () -> ((__1 ()), (__2 ()))) ()))) ()))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let __2 = L3.is_bot
          and __1 = L2.is_bot
          and __0 = L1.is_bot in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun a ->
                (fun (a, b) ->
                   (__0 a) && ((fun (a, b) -> (__1 a) && (__2 b)) b))
                  ((fun { f1 = f0; f2 = f1; f3 = f2 } -> (f0, (f1, f2))) a))
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
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
