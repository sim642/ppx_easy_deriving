open Ppxlib
open Ppx_easy_deriving

module EasyEqualArg: Arg =
struct
  let name = "easy_equal"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]
  let unit ~loc = [%expr fun () () -> true]
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> [%e e1] a1 a2 && [%e e2] b1 b2]
  let apply_iso ~loc leq f _ =
    [%expr fun a b -> [%e leq] ([%e f] a) ([%e f] b)]
end

module EasyEqualDeriver = Make (EasyEqualArg)
let _ = EasyEqualDeriver.register ()
