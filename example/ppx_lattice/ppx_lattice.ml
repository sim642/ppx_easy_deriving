open Ppxlib
open Ppx_easy_deriving

module LeqArg: Arg =
struct
  let name = "leq"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]
  let unit ~loc = [%expr fun () () -> true]
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> [%e e1] a1 a2 && [%e e2] b1 b2]
  let apply_iso ~loc leq f _ =
    [%expr fun a b -> [%e leq] ([%e f] a) ([%e f] b)]
end

module LeqDeriver = Make (LeqArg)
let leq_deriving = LeqDeriver.register ()


module JoinArg: Arg =
struct
  let name = "join"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> [%t t]]
  let unit ~loc = [%expr fun () () -> ()]
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> ([%e e1] a1 a2, [%e e2] b1 b2)]
  let apply_iso ~loc join f f' =
    [%expr fun a b -> [%e f'] ([%e join] ([%e f] a) ([%e f] b))]
end

module JoinDeriver = Make (JoinArg)
let join_deriving = JoinDeriver.register ()


module BotArg: Arg =
struct
  let name = "bot"
  let typ ~loc t = [%type: unit -> [%t t]]
  let unit ~loc = [%expr fun () -> ()]
  let both ~loc e1 e2 = [%expr fun () -> ([%e e1] (), [%e e2] ())]
  let apply_iso ~loc bot _ f' =
    [%expr fun () -> [%e f'] ([%e bot] ())]
end

module BotDeriver = Make (BotArg)
let bot_deriving = BotDeriver.register ()


let _ = Ppxlib.Deriving.add_alias "lattice" [
    bot_deriving;
    join_deriving;
    leq_deriving
  ]
