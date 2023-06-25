module type Easy_equal =
sig
  type t
  val easy_equal: t -> t -> bool
  val easy_equal2: t -> t -> bool
end

module type Easy_equal_derived =
sig
  type t [@@deriving easy_equal, easy_equal2]
end

module Easy_equal_derived_test1 (E: Easy_equal): Easy_equal_derived = E
module Easy_equal_derived_test2 (E: Easy_equal_derived): Easy_equal = E

module Unit: Easy_equal =
struct
  type t = unit [@@deriving easy_equal, easy_equal2]
end

let _ = [%easy_equal: unit]
let _ = [%easy_equal2: unit]

module Direct (E1: Easy_equal): Easy_equal =
struct
  type t = E1.t [@@deriving easy_equal, easy_equal2]
end

module Tuple2 (E1: Easy_equal) (E2: Easy_equal): Easy_equal =
struct
  type t = E1.t * E2.t [@@deriving easy_equal, easy_equal2]
end

module Tuple3 (E1: Easy_equal) (E2: Easy_equal) (E3: Easy_equal): Easy_equal =
struct
  type t = E1.t * E2.t * E3.t [@@deriving easy_equal, easy_equal2]
end

module Record1 (E1: Easy_equal): Easy_equal =
struct
  type t = {
    f1: E1.t;
  } [@@deriving easy_equal, easy_equal2]
end

module Record2 (E1: Easy_equal) (E2: Easy_equal): Easy_equal =
struct
  type t = {
    f1: E1.t;
    f2: E2.t;
  } [@@deriving easy_equal, easy_equal2]
end

module Record3 (E1: Easy_equal) (E2: Easy_equal) (E3: Easy_equal): Easy_equal =
struct
  type t = {
    f1: E1.t;
    f2: E2.t;
    f3: E3.t;
  } [@@deriving easy_equal, easy_equal2]
end

module Variant (E1: Easy_equal) (E2: Easy_equal) (E3: Easy_equal) (E4: Easy_equal) (E5: Easy_equal) =
struct
  type t =
    | C1
    | C2 of E1.t * E2.t
    | C3 of {f1: E3.t; f2: E4.t}
    | C4 of E5.t
  [@@deriving easy_equal, easy_equal2]
end

module PolyVariant (E1: Easy_equal) (E2: Easy_equal) (E5: Easy_equal) =
struct
  type t = [
    | `C1
    | `C2 of E1.t * E2.t
    | `C4 of E5.t
  ] [@@deriving easy_equal, easy_equal2]
end

module PolyVariantInherit (E1: Easy_equal) (E2: Easy_equal) (E5: Easy_equal) =
struct
  module PV = PolyVariant (E1) (E2) (E5)
  type u = PV.t [@@deriving easy_equal, easy_equal2]
  type t = [
    | PV.t
    | `C5
    | u (* actually same as PV.t, so this case should be last and unreachable *)
  ] [@@deriving easy_equal, easy_equal2]
end
