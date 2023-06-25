module type Easy_equal  =
  sig type t val easy_equal : t -> t -> bool val easy_equal2 : t -> t -> bool
  end
module type Easy_equal_derived  =
  sig
    type t[@@deriving (easy_equal, easy_equal2)]
    include
      sig val easy_equal : t -> t -> bool val easy_equal2 : t -> t -> bool
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
module Easy_equal_derived_test1(E:Easy_equal) : Easy_equal_derived = E 
module Easy_equal_derived_test2(E:Easy_equal_derived) : Easy_equal = E 
module Unit : Easy_equal =
  struct
    type t = unit[@@deriving (easy_equal, easy_equal2)]
    include
      struct
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
  ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
      fun () -> fun () -> true)
  [@ocaml.warning "-A"])
let _ =
  ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
      fun () -> fun () -> true)
  [@ocaml.warning "-A"])
module Direct(E1:Easy_equal) : Easy_equal =
  struct
    type t = E1.t[@@deriving (easy_equal, easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __0 = E1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __0 = E1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Attr : Easy_equal =
  struct
    type t = ((int)[@easy_equal (=)][@easy_equal2 (=)])[@@deriving
                                                         (easy_equal,
                                                           easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __0 = (=) in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __0 = (=) in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in __0)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple2(E1:Easy_equal)(E2:Easy_equal) : Easy_equal =
  struct
    type t = (E1.t * E2.t)[@@deriving (easy_equal, easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __1 = E2.easy_equal
          and __0 = E1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (l1, l2) -> fun (r1, r2) -> (__0 l1 r1) && (__1 l2 r2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __1 = E2.easy_equal2
          and __0 = E1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (l1, l2) -> fun (r1, r2) -> (__0 l1 r1) && (__1 l2 r2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Tuple3(E1:Easy_equal)(E2:Easy_equal)(E3:Easy_equal) : Easy_equal =
  struct
    type t = (E1.t * E2.t * E3.t)[@@deriving (easy_equal, easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __2 = E3.easy_equal
          and __1 = E2.easy_equal
          and __0 = E1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun (l1, l2, l3) ->
                fun (r1, r2, r3) ->
                  (__0 l1 r1) && ((__1 l2 r2) && (__2 l3 r3)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __2 = E3.easy_equal2
          and __1 = E2.easy_equal2
          and __0 = E1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun l ->
                fun r ->
                  (fun (l1, l2) ->
                     fun (r1, r2) ->
                       (__0 l1 r1) &&
                         ((fun (l1, l2) ->
                             fun (r1, r2) -> (__1 l1 r1) && (__2 l2 r2)) l2
                            r2)) ((fun (f1, f2, f3) -> (f1, (f2, f3))) l)
                    ((fun (f1, f2, f3) -> (f1, (f2, f3))) r))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record1(E1:Easy_equal) : Easy_equal =
  struct
    type t = {
      f1: E1.t }[@@deriving (easy_equal, easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __0 = E1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = l1 } -> fun { f1 = r1 } -> __0 l1 r1)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __0 = E1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun l ->
                fun r -> __0 ((fun { f1 } -> f1) l) ((fun { f1 } -> f1) r))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record2(E1:Easy_equal)(E2:Easy_equal) : Easy_equal =
  struct
    type t = {
      f1: E1.t ;
      f2: E2.t }[@@deriving (easy_equal, easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __1 = E2.easy_equal
          and __0 = E1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = l1; f2 = l2 } ->
                fun { f1 = r1; f2 = r2 } -> (__0 l1 r1) && (__1 l2 r2))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __1 = E2.easy_equal2
          and __0 = E1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun l ->
                fun r ->
                  (fun (l1, l2) -> fun (r1, r2) -> (__0 l1 r1) && (__1 l2 r2))
                    ((fun { f1; f2 } -> (f1, f2)) l)
                    ((fun { f1; f2 } -> (f1, f2)) r))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Record3(E1:Easy_equal)(E2:Easy_equal)(E3:Easy_equal) : Easy_equal =
  struct
    type t = {
      f1: E1.t ;
      f2: E2.t ;
      f3: E3.t }[@@deriving (easy_equal, easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __2 = E3.easy_equal
          and __1 = E2.easy_equal
          and __0 = E1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun { f1 = l1; f2 = l2; f3 = l3 } ->
                fun { f1 = r1; f2 = r2; f3 = r3 } ->
                  (__0 l1 r1) && ((__1 l2 r2) && (__2 l3 r3)))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __2 = E3.easy_equal2
          and __1 = E2.easy_equal2
          and __0 = E1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun l ->
                fun r ->
                  (fun (l1, l2) ->
                     fun (r1, r2) ->
                       (__0 l1 r1) &&
                         ((fun (l1, l2) ->
                             fun (r1, r2) -> (__1 l1 r1) && (__2 l2 r2)) l2
                            r2)) ((fun { f1; f2; f3 } -> (f1, (f2, f3))) l)
                    ((fun { f1; f2; f3 } -> (f1, (f2, f3))) r))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end 
module Variant(E1:Easy_equal)(E2:Easy_equal)(E3:Easy_equal)(E4:Easy_equal)(E5:Easy_equal) =
  struct
    type t =
      | C1 
      | C2 of E1.t * E2.t 
      | C3 of {
      f1: E3.t ;
      f2: E4.t } 
      | C4 of E5.t [@@deriving (easy_equal, easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __9 = E5.easy_equal
          and __8 = E5.easy_equal
          and __7 = E4.easy_equal
          and __6 = E3.easy_equal
          and __5 = E4.easy_equal
          and __4 = E3.easy_equal
          and __3 = E2.easy_equal
          and __2 = E1.easy_equal
          and __1 = E2.easy_equal
          and __0 = E1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun l ->
                fun r ->
                  match (l, r) with
                  | (C1, C1) -> true
                  | (C2 (l1, l2), C2 (r1, r2)) -> (__0 l1 r1) && (__1 l2 r2)
                  | (C3 { f1 = l1; f2 = l2 }, C3 { f1 = r1; f2 = r2 }) ->
                      (__4 l1 r1) && (__5 l2 r2)
                  | (C4 l1, C4 r1) -> __8 l1 r1
                  | (_, _) -> false)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __9 = E5.easy_equal2
          and __8 = E5.easy_equal2
          and __7 = E4.easy_equal2
          and __6 = E3.easy_equal2
          and __5 = E4.easy_equal2
          and __4 = E3.easy_equal2
          and __3 = E2.easy_equal2
          and __2 = E1.easy_equal2
          and __1 = E2.easy_equal2
          and __0 = E1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun l ->
                fun r ->
                  (fun l ->
                     fun r ->
                       match (l, r) with
                       | (Either.Left l, Either.Left r) ->
                           ((fun () -> fun () -> true)) l r
                       | (Either.Right l, Either.Right r) ->
                           ((fun l ->
                               fun r ->
                                 match (l, r) with
                                 | (Either.Left l, Either.Left r) ->
                                     ((fun (l1, l2) ->
                                         fun (r1, r2) ->
                                           (__2 l1 r1) && (__3 l2 r2))) l r
                                 | (Either.Right l, Either.Right r) ->
                                     ((fun l ->
                                         fun r ->
                                           match (l, r) with
                                           | (Either.Left l, Either.Left r)
                                               ->
                                               ((fun (l1, l2) ->
                                                   fun (r1, r2) ->
                                                     (__6 l1 r1) &&
                                                       (__7 l2 r2))) l r
                                           | (Either.Right l, Either.Right r)
                                               -> __9 l r
                                           | (_, _) -> false)) l r
                                 | (_, _) -> false)) l r
                       | (_, _) -> false)
                    ((function
                      | C1 -> Either.Left ()
                      | C2 (f1, f2) -> Either.Right (Either.Left (f1, f2))
                      | C3 { f1; f2 } ->
                          Either.Right (Either.Right (Either.Left (f1, f2)))
                      | C4 f1 ->
                          Either.Right (Either.Right (Either.Right f1))) l)
                    ((function
                      | C1 -> Either.Left ()
                      | C2 (f1, f2) -> Either.Right (Either.Left (f1, f2))
                      | C3 { f1; f2 } ->
                          Either.Right (Either.Right (Either.Left (f1, f2)))
                      | C4 f1 ->
                          Either.Right (Either.Right (Either.Right f1))) r))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
module PolyVariant(E1:Easy_equal)(E2:Easy_equal)(E5:Easy_equal) =
  struct
    type t = [ `C1  | `C2 of (E1.t * E2.t)  | `C4 of E5.t ][@@deriving
                                                             (easy_equal,
                                                               easy_equal2)]
    include
      struct
        let rec (easy_equal : t -> t -> bool) =
          let __2 = E5.easy_equal
          and __1 = E2.easy_equal
          and __0 = E1.easy_equal in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun l ->
                fun r ->
                  match (l, r) with
                  | (`C1, `C1) -> true
                  | (`C2 l, `C2 r) ->
                      ((fun (l1, l2) ->
                          fun (r1, r2) -> (__0 l1 r1) && (__1 l2 r2))) l r
                  | (`C4 l, `C4 r) -> __2 l r
                  | (_, _) -> false)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __2 = E5.easy_equal2
          and __1 = E2.easy_equal2
          and __0 = E1.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun l ->
                fun r ->
                  (fun l ->
                     fun r ->
                       match (l, r) with
                       | (Either.Left l, Either.Left r) ->
                           ((fun () -> fun () -> true)) l r
                       | (Either.Right l, Either.Right r) ->
                           ((fun l ->
                               fun r ->
                                 match (l, r) with
                                 | (Either.Left l, Either.Left r) ->
                                     ((fun (l1, l2) ->
                                         fun (r1, r2) ->
                                           (__0 l1 r1) && (__1 l2 r2))) l r
                                 | (Either.Right l, Either.Right r) ->
                                     __2 l r
                                 | (_, _) -> false)) l r
                       | (_, _) -> false)
                    ((function
                      | `C1 -> Either.Left ()
                      | `C2 f -> Either.Right (Either.Left f)
                      | `C4 f -> Either.Right (Either.Right f)) l)
                    ((function
                      | `C1 -> Either.Left ()
                      | `C2 f -> Either.Right (Either.Left f)
                      | `C4 f -> Either.Right (Either.Right f)) r))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
module PolyVariantInherit(E1:Easy_equal)(E2:Easy_equal)(E5:Easy_equal) =
  struct
    module PV = (((PolyVariant)(E1))(E2))(E5)
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
              fun l ->
                fun r ->
                  match (l, r) with
                  | ((#PV.t as l), (#PV.t as r)) -> __0 l r
                  | (`C5, `C5) -> true
                  | ((#u as l), (#u as r)) -> __1 l r
                  | (_, _) -> false)
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
        let rec (easy_equal2 : t -> t -> bool) =
          let __1 = easy_equal2_u
          and __0 = PV.easy_equal2 in
          ((let open! ((Ppx_deriving_runtime)[@ocaml.warning "-A"]) in
              fun l ->
                fun r ->
                  (fun l ->
                     fun r ->
                       match (l, r) with
                       | (Either.Left l, Either.Left r) -> __0 l r
                       | (Either.Right l, Either.Right r) ->
                           ((fun l ->
                               fun r ->
                                 match (l, r) with
                                 | (Either.Left l, Either.Left r) ->
                                     ((fun () -> fun () -> true)) l r
                                 | (Either.Right l, Either.Right r) ->
                                     __1 l r
                                 | (_, _) -> false)) l r
                       | (_, _) -> false)
                    ((function
                      | #PV.t as f -> Either.Left f
                      | `C5 -> Either.Right (Either.Left ())
                      | #u as f -> Either.Right (Either.Right f)) l)
                    ((function
                      | #PV.t as f -> Either.Left f
                      | `C5 -> Either.Right (Either.Left ())
                      | #u as f -> Either.Right (Either.Right f)) r))
            [@ocaml.warning "-A"])[@@ocaml.warning "-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
