open Ppxlib
open Ppx_easy_deriving

module Arg: Arg =
struct
  let name = "leq"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]
  let unit ~loc = [%expr fun () () -> true]
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> [%e e1] a1 a2 && [%e e2] b1 b2]
  let apply_iso ~loc leq f =
    [%expr fun a b -> [%e leq] ([%e f] a) ([%e f] b)]
end

module Deriver = Make (Arg)
let _ = Deriver.register ()
