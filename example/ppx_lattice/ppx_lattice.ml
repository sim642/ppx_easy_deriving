open Ppxlib
open Ppx_easy_deriving

module LeqArg: Simple.Product.S =
struct
  let name = "leq"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> bool]
  let unit ~loc = [%expr fun () () -> true]
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> [%e e1] a1 a2 && [%e e2] b1 b2]
  let apply_iso ~loc leq f _ =
    [%expr fun a b -> [%e leq] ([%e f] a) ([%e f] b)]
end

module LeqDeriver = Make (Simple.Product.Reduce (LeqArg))
let leq_deriving = LeqDeriver.register ()


module JoinArg: Simple.Product.S =
struct
  let name = "join"
  let typ ~loc t = [%type: [%t t] -> [%t t] -> [%t t]]
  let unit ~loc = [%expr fun () () -> ()]
  let both ~loc e1 e2 = [%expr fun (a1, b1) (a2, b2) -> ([%e e1] a1 a2, [%e e2] b1 b2)]
  let apply_iso ~loc join f f' =
    [%expr fun a b -> [%e f'] ([%e join] ([%e f] a) ([%e f] b))]
end

module JoinDeriver = Make (Simple.Product.Reduce (JoinArg))
let join_deriving = JoinDeriver.register ()


module BotArg: Simple.Product.S =
struct
  let name = "bot"
  let typ ~loc t = [%type: unit -> [%t t]]
  let unit ~loc = [%expr fun () -> ()]
  let both ~loc e1 e2 = [%expr fun () -> ([%e e1] (), [%e e2] ())]
  let apply_iso ~loc bot _ f' =
    [%expr fun () -> [%e f'] ([%e bot] ())]
end

module BotDeriver = Make (Simple.Product.Reduce (BotArg))
let bot_deriving = BotDeriver.register ()


module IsBotArg: Simple.Product.S =
struct
  let name = "is_bot"
  let typ ~loc t = [%type: [%t t] -> bool]
  let unit ~loc = [%expr fun () -> true]
  let both ~loc e1 e2 = [%expr fun (a, b) -> [%e e1] a && [%e e2] b]
  let apply_iso ~loc is_bot f _ =
    [%expr fun a -> [%e is_bot] ([%e f] a)]
end

module IsBotDeriver = Make (Simple.Product.Reduce (IsBotArg))
let is_bot_deriving = IsBotDeriver.register ()


let _ = Ppxlib.Deriving.add_alias "lattice" [
    is_bot_deriving;
    bot_deriving;
    join_deriving;
    leq_deriving;
  ]
