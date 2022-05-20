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


let () = run_global ()
