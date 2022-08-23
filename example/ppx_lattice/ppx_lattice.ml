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


module JoinArg: Product.Map2.S =
struct
  let name = "join"
end

module JoinDeriver = Deriver (Product.Map2.Make (JoinArg))
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
