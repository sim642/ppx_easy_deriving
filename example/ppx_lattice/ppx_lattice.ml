open Ppxlib
open Ppx_easy_deriving

module Arg: Arg =
struct
  let name = "leq"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]
  (* let unit ~loc = [%expr fun () () -> true] *)
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> [%e e1] a1 a2 && [%e e2] b1 b2]
  let apply_iso ~loc leq f _ =
    [%expr fun a b -> [%e leq] ([%e f] a) ([%e f] b)]
end

module Arg2: Arg =
struct
  let name = "join"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> [%t t]]
  (* let unit ~loc = [%expr fun () () -> ()] *)
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> ([%e e1] a1 a2, [%e e2] b1 b2)]
  let apply_iso ~loc join f f' =
    [%expr fun a b -> [%e f'] ([%e join] ([%e f] a) ([%e f] b))]
end

module Arg3: Arg =
struct
  let name = "bot"
  let typ ~loc t = [%type: unit -> [%t t]]
  (* let unit ~loc = [%expr fun () () -> ()] *)
  let both ~loc e1 e2 = [%expr fun () -> ([%e e1] (), [%e e2] ())]
  let apply_iso ~loc bot _ f' =
    [%expr fun () -> [%e f'] ([%e bot] ())]
end

module Deriver = Make (Arg)
module Deriver2 = Make (Arg2)
module Deriver3 = Make (Arg3)
let _ = Deriver.register ()
let _ = Deriver2.register ()
let _ = Deriver3.register ()
