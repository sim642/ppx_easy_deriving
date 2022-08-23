open Ppxlib
open Ppx_easy_deriving

module LeqArg: Product.Reduce2.S =
struct
  let name = "leq"
  let typ ~loc _ = [%type: bool]
  let unit ~loc = [%expr true]
  let both ~loc e1 e2 = [%expr [%e e1] && [%e e2]]
end

module LeqDeriver = Deriver (Product.Reduce2.Make (LeqArg))
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

module JoinDeriver = Deriver (Simple.Product.Reduce (JoinArg))
let join_deriving = JoinDeriver.register ()


module BotArg: Product.Create.S =
struct
  let name = "bot"
  let typ ~loc _ = [%type: unit]
end

module BotDeriver = Deriver (Product.Create.Make (BotArg))
let bot_deriving = BotDeriver.register ()


module IsBotArg: Product.Reduce1.S =
struct
  let name = "is_bot"
  let typ ~loc _ = [%type: bool]
  let unit ~loc = [%expr true]
  let both ~loc e1 e2 = [%expr [%e e1] && [%e e2]]
end

module IsBotDeriver = Deriver (Product.Reduce1.Make (IsBotArg))
let is_bot_deriving = IsBotDeriver.register ()


let _ = Ppxlib.Deriving.add_alias "lattice" [
    is_bot_deriving;
    bot_deriving;
    join_deriving;
    leq_deriving;
  ]
