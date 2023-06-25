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
        let rec (leq : t -> t -> bool) = fun () -> fun () -> true[@@ocaml.warning
                                                                   "-39"]
        let rec (join : t -> t -> t) = fun () -> fun () -> ()[@@ocaml.warning
                                                               "-39"]
        let rec (bot : unit -> t) = fun _ -> ()[@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) = fun () -> true[@@ocaml.warning "-39"]
        let rec (relift : t -> t) = fun () -> ()[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
let _ = fun () -> true
let _ = fun _ -> ()
let _ = fun () -> fun () -> ()
let _ = fun () -> fun () -> true
module Direct(L1:Lattice) : Lattice =
  struct
    type t = L1.t[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) = let rec __0 = L1.leq in __0[@@ocaml.warning
                                                                    "-39"]
        let rec (join : t -> t -> t) = let rec __0 = L1.join in __0[@@ocaml.warning
                                                                    "-39"]
        let rec (bot : unit -> t) = let rec __0 = L1.bot in __0[@@ocaml.warning
                                                                 "-39"]
        let rec (is_bot : t -> bool) = let rec __0 = L1.is_bot in __0
          [@@ocaml.warning "-39"]
        let rec (relift : t -> t) = let rec __0 = L1.relift in __0[@@ocaml.warning
                                                                    "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple2(L1:Lattice)(L2:Lattice) : Lattice =
  struct
    type t = (L1.t * L2.t)[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let rec __1 = L2.leq
          and __0 = L1.leq in
          fun (l1, l2) -> fun (r1, r2) -> (__0 l1 r1) && (__1 l2 r2)[@@ocaml.warning
                                                                    "-39"]
        let rec (join : t -> t -> t) =
          let rec __1 = L2.join
          and __0 = L1.join in
          fun (l1, l2) -> fun (r1, r2) -> ((__0 l1 r1), (__1 l2 r2))[@@ocaml.warning
                                                                    "-39"]
        let rec (bot : unit -> t) =
          let rec __1 = L2.bot
          and __0 = L1.bot in fun x -> ((__0 x), (__1 x))[@@ocaml.warning
                                                           "-39"]
        let rec (is_bot : t -> bool) =
          let rec __1 = L2.is_bot
          and __0 = L1.is_bot in fun (x1, x2) -> (__0 x1) && (__1 x2)
          [@@ocaml.warning "-39"]
        let rec (relift : t -> t) =
          let rec __1 = L2.relift
          and __0 = L1.relift in fun (x1, x2) -> ((__0 x1), (__1 x2))
          [@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple3(L1:Lattice)(L2:Lattice)(L3:Lattice) : Lattice =
  struct
    type t = (L1.t * L2.t * L3.t)[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let rec __2 = L3.leq
          and __1 = L2.leq
          and __0 = L1.leq in
          fun (l1, l2, l3) ->
            fun (r1, r2, r3) -> (__0 l1 r1) && ((__1 l2 r2) && (__2 l3 r3))
          [@@ocaml.warning "-39"]
        let rec (join : t -> t -> t) =
          let rec __2 = L3.join
          and __1 = L2.join
          and __0 = L1.join in
          fun (l1, l2, l3) ->
            fun (r1, r2, r3) -> ((__0 l1 r1), (__1 l2 r2), (__2 l3 r3))
          [@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let rec __2 = L3.bot
          and __1 = L2.bot
          and __0 = L1.bot in fun x -> ((__0 x), (__1 x), (__2 x))[@@ocaml.warning
                                                                    "-39"]
        let rec (is_bot : t -> bool) =
          let rec __2 = L3.is_bot
          and __1 = L2.is_bot
          and __0 = L1.is_bot in
          fun (x1, x2, x3) -> (__0 x1) && ((__1 x2) && (__2 x3))[@@ocaml.warning
                                                                  "-39"]
        let rec (relift : t -> t) =
          let rec __2 = L3.relift
          and __1 = L2.relift
          and __0 = L1.relift in
          fun (x1, x2, x3) -> ((__0 x1), (__1 x2), (__2 x3))[@@ocaml.warning
                                                              "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record1(L1:Lattice) : Lattice =
  struct
    type t = {
      f1: L1.t }[@@deriving lattice]
    include
      struct
        let rec (leq : t -> t -> bool) =
          let rec __0 = L1.leq in
          fun { f1 = l1 } -> fun { f1 = r1 } -> __0 l1 r1[@@ocaml.warning
                                                           "-39"]
        let rec (join : t -> t -> t) =
          let rec __0 = L1.join in
          fun { f1 = l1 } -> fun { f1 = r1 } -> { f1 = (__0 l1 r1) }[@@ocaml.warning
                                                                    "-39"]
        let rec (bot : unit -> t) =
          let rec __0 = L1.bot in fun x -> { f1 = (__0 x) }[@@ocaml.warning
                                                             "-39"]
        let rec (is_bot : t -> bool) =
          let rec __0 = L1.is_bot in fun { f1 = x1 } -> __0 x1[@@ocaml.warning
                                                                "-39"]
        let rec (relift : t -> t) =
          let rec __0 = L1.relift in fun { f1 = x1 } -> { f1 = (__0 x1) }
          [@@ocaml.warning "-39"]
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
          let rec __1 = L2.leq
          and __0 = L1.leq in
          fun { f1 = l1; f2 = l2 } ->
            fun { f1 = r1; f2 = r2 } -> (__0 l1 r1) && (__1 l2 r2)[@@ocaml.warning
                                                                    "-39"]
        let rec (join : t -> t -> t) =
          let rec __1 = L2.join
          and __0 = L1.join in
          fun { f1 = l1; f2 = l2 } ->
            fun { f1 = r1; f2 = r2 } ->
              { f1 = (__0 l1 r1); f2 = (__1 l2 r2) }[@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let rec __1 = L2.bot
          and __0 = L1.bot in fun x -> { f1 = (__0 x); f2 = (__1 x) }
          [@@ocaml.warning "-39"]
        let rec (is_bot : t -> bool) =
          let rec __1 = L2.is_bot
          and __0 = L1.is_bot in
          fun { f1 = x1; f2 = x2 } -> (__0 x1) && (__1 x2)[@@ocaml.warning
                                                            "-39"]
        let rec (relift : t -> t) =
          let rec __1 = L2.relift
          and __0 = L1.relift in
          fun { f1 = x1; f2 = x2 } -> { f1 = (__0 x1); f2 = (__1 x2) }
          [@@ocaml.warning "-39"]
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
          let rec __2 = L3.leq
          and __1 = L2.leq
          and __0 = L1.leq in
          fun { f1 = l1; f2 = l2; f3 = l3 } ->
            fun { f1 = r1; f2 = r2; f3 = r3 } ->
              (__0 l1 r1) && ((__1 l2 r2) && (__2 l3 r3))[@@ocaml.warning
                                                           "-39"]
        let rec (join : t -> t -> t) =
          let rec __2 = L3.join
          and __1 = L2.join
          and __0 = L1.join in
          fun { f1 = l1; f2 = l2; f3 = l3 } ->
            fun { f1 = r1; f2 = r2; f3 = r3 } ->
              { f1 = (__0 l1 r1); f2 = (__1 l2 r2); f3 = (__2 l3 r3) }
          [@@ocaml.warning "-39"]
        let rec (bot : unit -> t) =
          let rec __2 = L3.bot
          and __1 = L2.bot
          and __0 = L1.bot in
          fun x -> { f1 = (__0 x); f2 = (__1 x); f3 = (__2 x) }[@@ocaml.warning
                                                                 "-39"]
        let rec (is_bot : t -> bool) =
          let rec __2 = L3.is_bot
          and __1 = L2.is_bot
          and __0 = L1.is_bot in
          fun { f1 = x1; f2 = x2; f3 = x3 } ->
            (__0 x1) && ((__1 x2) && (__2 x3))[@@ocaml.warning "-39"]
        let rec (relift : t -> t) =
          let rec __2 = L3.relift
          and __1 = L2.relift
          and __0 = L1.relift in
          fun { f1 = x1; f2 = x2; f3 = x3 } ->
            { f1 = (__0 x1); f2 = (__1 x2); f3 = (__2 x3) }[@@ocaml.warning
                                                             "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
