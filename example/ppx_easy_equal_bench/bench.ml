(* dune exec example/ppx_easy_equal_bench/bench.exe -- -a *)

open Benchmark
open Benchmark.Tree

let uncurry f (x, y) = f x y

module Int =
struct
  include Int
  let easy_equal = equal
  let ppx_type_directed_equal = equal
end

module String =
struct
  include String
  let easy_equal = equal
  let ppx_type_directed_equal = equal
end

module IntString =
struct
  type t = Int.t * String.t [@@deriving eq, easy_equal, ppx_type_directed_equal]
end

let () =
  register (
    "pair" @>>> [
        "snd" @> lazy (
            let args = ((1, "foo"), (1, "bar")) in
            throughputN 1 [
              ("ppx_deriving", uncurry IntString.equal, args);
              ("ppx_easy_deriving", uncurry IntString.easy_equal, args);
              ("ppx_type_directed_equal", uncurry IntString.ppx_type_directed_equal, args);
            ]
          );
        "fst" @> lazy (
            let args = ((1, "foo"), (2, "bar")) in
            throughputN 1 [
              ("ppx_deriving", uncurry IntString.equal, args);
              ("ppx_easy_deriving", uncurry IntString.easy_equal, args);
              ("ppx_type_directed_equal", uncurry IntString.ppx_type_directed_equal, args);
            ]
          );
      ]
  )


(* copied from ppx_type_directed_value examples *)

module M = struct
  type rec_6 =
    { f1 : String.t
    ; f2 : String.t
    ; f3 : String.t
    ; g1 : Int.t
    ; g2 : Int.t
    ; g3 : Int.t
    }
  [@@deriving eq, easy_equal, ppx_type_directed_equal]

  type rec_16 =
    { f1 : String.t
    ; f2 : String.t
    ; f3 : String.t
    ; f4 : String.t
    ; f5 : String.t
    ; f6 : String.t
    ; f7 : String.t
    ; f8 : String.t
    ; g1 : Int.t
    ; g2 : Int.t
    ; g3 : Int.t
    ; g4 : Int.t
    ; g5 : Int.t
    ; g6 : Int.t
    ; g7 : Int.t
    ; g8 : Int.t
    }
  [@@deriving eq, easy_equal, ppx_type_directed_equal]

  type rec_30 =
    { f1  : String.t
    ; f2  : String.t
    ; f3  : String.t
    ; f4  : String.t
    ; f5  : String.t
    ; f6  : String.t
    ; f7  : String.t
    ; f8  : String.t
    ; f9  : String.t
    ; f10 : String.t
    ; f11 : String.t
    ; f12 : String.t
    ; f13 : String.t
    ; f14 : String.t
    ; f15 : String.t
    ; g1  : Int.t
    ; g2  : Int.t
    ; g3  : Int.t
    ; g4  : Int.t
    ; g5  : Int.t
    ; g6  : Int.t
    ; g7  : Int.t
    ; g8  : Int.t
    ; g9  : Int.t
    ; g10 : Int.t
    ; g11 : Int.t
    ; g12 : Int.t
    ; g13 : Int.t
    ; g14 : Int.t
    ; g15 : Int.t
    }
  [@@deriving eq, easy_equal, ppx_type_directed_equal]

  type rec_60 =
    { f1  : String.t
    ; f2  : String.t
    ; f3  : String.t
    ; f4  : String.t
    ; f5  : String.t
    ; f6  : String.t
    ; f7  : String.t
    ; f8  : String.t
    ; f9  : String.t
    ; f10 : String.t
    ; f11 : String.t
    ; f12 : String.t
    ; f13 : String.t
    ; f14 : String.t
    ; f15 : String.t
    ; f16 : String.t
    ; f17 : String.t
    ; f18 : String.t
    ; f19 : String.t
    ; f20 : String.t
    ; f21 : String.t
    ; f22 : String.t
    ; f23 : String.t
    ; f24 : String.t
    ; f25 : String.t
    ; f26 : String.t
    ; f27 : String.t
    ; f28 : String.t
    ; f29 : String.t
    ; f30 : String.t
    ; g1  : Int.t
    ; g2  : Int.t
    ; g3  : Int.t
    ; g4  : Int.t
    ; g5  : Int.t
    ; g6  : Int.t
    ; g7  : Int.t
    ; g8  : Int.t
    ; g9  : Int.t
    ; g10 : Int.t
    ; g11 : Int.t
    ; g12 : Int.t
    ; g13 : Int.t
    ; g14 : Int.t
    ; g15 : Int.t
    ; g16 : Int.t
    ; g17 : Int.t
    ; g18 : Int.t
    ; g19 : Int.t
    ; g20 : Int.t
    ; g21 : Int.t
    ; g22 : Int.t
    ; g23 : Int.t
    ; g24 : Int.t
    ; g25 : Int.t
    ; g26 : Int.t
    ; g27 : Int.t
    ; g28 : Int.t
    ; g29 : Int.t
    ; g30 : Int.t
    }
  [@@deriving eq, easy_equal, ppx_type_directed_equal]
end

let rec_6 = { M.f1 = "hello"; f2 = "hello"; f3 = "hello"; g1 = 10; g2 = 10; g3 = 10 }

let rec_16 =
  { M.f1 = "hello"
  ; f2   = "hello"
  ; f3   = "hello"
  ; f4   = "hello"
  ; f5   = "hello"
  ; f6   = "hello"
  ; f7   = "hello"
  ; f8   = "hello"
  ; g1   = 10
  ; g2   = 10
  ; g3   = 10
  ; g4   = 10
  ; g5   = 10
  ; g6   = 10
  ; g7   = 10
  ; g8   = 10
  }
;;

let rec_30 =
  { M.f1 = "hello"
  ; f2   = "hello"
  ; f3   = "hello"
  ; f4   = "hello"
  ; f5   = "hello"
  ; f6   = "hello"
  ; f7   = "hello"
  ; f8   = "hello"
  ; f9   = "hello"
  ; f10  = "hello"
  ; f11  = "hello"
  ; f12  = "hello"
  ; f13  = "hello"
  ; f14  = "hello"
  ; f15  = "hello"
  ; g1   = 10
  ; g2   = 10
  ; g3   = 10
  ; g4   = 10
  ; g5   = 10
  ; g6   = 10
  ; g7   = 10
  ; g8   = 10
  ; g9   = 10
  ; g10  = 10
  ; g11  = 10
  ; g12  = 10
  ; g13  = 10
  ; g14  = 10
  ; g15  = 10
  }
;;

let rec_60 =
  { M.f1 = "hello"
  ; f2   = "hello"
  ; f3   = "hello"
  ; f4   = "hello"
  ; f5   = "hello"
  ; f6   = "hello"
  ; f7   = "hello"
  ; f8   = "hello"
  ; f9   = "hello"
  ; f10  = "hello"
  ; f11  = "hello"
  ; f12  = "hello"
  ; f13  = "hello"
  ; f14  = "hello"
  ; f15  = "hello"
  ; f16  = "hello"
  ; f17  = "hello"
  ; f18  = "hello"
  ; f19  = "hello"
  ; f20  = "hello"
  ; f21  = "hello"
  ; f22  = "hello"
  ; f23  = "hello"
  ; f24  = "hello"
  ; f25  = "hello"
  ; f26  = "hello"
  ; f27  = "hello"
  ; f28  = "hello"
  ; f29  = "hello"
  ; f30  = "hello"
  ; g1   = 10
  ; g2   = 10
  ; g3   = 10
  ; g4   = 10
  ; g5   = 10
  ; g6   = 10
  ; g7   = 10
  ; g8   = 10
  ; g9   = 10
  ; g10  = 10
  ; g11  = 10
  ; g12  = 10
  ; g13  = 10
  ; g14  = 10
  ; g15  = 10
  ; g16  = 10
  ; g17  = 10
  ; g18  = 10
  ; g19  = 10
  ; g20  = 10
  ; g21  = 10
  ; g22  = 10
  ; g23  = 10
  ; g24  = 10
  ; g25  = 10
  ; g26  = 10
  ; g27  = 10
  ; g28  = 10
  ; g29  = 10
  ; g30  = 10
  }

let () =
  register (
    "record" @>>> [
        "6" @> lazy (
            let args = (rec_6, { rec_6 with g3 = 100 }) in
            throughputN 1 [
              ("ppx_deriving", uncurry M.equal_rec_6, args);
              ("ppx_easy_deriving", uncurry M.easy_equal_rec_6, args);
              ("ppx_type_directed_equal", uncurry M.ppx_type_directed_equal_rec_6, args);
            ]
          );
        "16" @> lazy (
            let args = (rec_16, { rec_16 with g8 = 100 }) in
            throughputN 1 [
              ("ppx_deriving", uncurry M.equal_rec_16, args);
              ("ppx_easy_deriving", uncurry M.easy_equal_rec_16, args);
              ("ppx_type_directed_equal", uncurry M.ppx_type_directed_equal_rec_16, args);
            ]
          );
        "30" @> lazy (
            let args = (rec_30, { rec_30 with g15 = 100 }) in
            throughputN 1 [
              ("ppx_deriving", uncurry M.equal_rec_30, args);
              ("ppx_easy_deriving", uncurry M.easy_equal_rec_30, args);
              ("ppx_type_directed_equal", uncurry M.ppx_type_directed_equal_rec_30, args);
            ]
          );
        "60" @> lazy (
            let args = (rec_60, { rec_60 with g30 = 100 }) in
            throughputN 1 [
              ("ppx_deriving", uncurry M.equal_rec_60, args);
              ("ppx_easy_deriving", uncurry M.easy_equal_rec_60, args);
              ("ppx_type_directed_equal", uncurry M.ppx_type_directed_equal_rec_60, args);
            ]
          );
      ]
  )

let () = run_global ()
