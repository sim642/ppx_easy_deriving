open Ppxlib
open Ppx_easy_deriving

module LeqArg: Product.Reduce.Conjunctive.S =
struct
  let name = "leq"
end

module LeqDeriver = Deriver (Product.Reduce2.Make (Product.Reduce.Conjunctive.Make (LeqArg)))
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


module IsBotArg: Product.Reduce.Conjunctive.S =
struct
  let name = "is_bot"
end

module IsBotDeriver = Deriver (Product.Reduce1.Make (Product.Reduce.Conjunctive.Make (IsBotArg)))
let is_bot_deriving = IsBotDeriver.register ()


module ReliftArg: Product.Map1.S =
struct
  let name = "relift"
end

module ReliftDeriver = Deriver (Product.Map1.Make (ReliftArg))
let relift_deriving = ReliftDeriver.register ()


let _ = Ppxlib.Deriving.add_alias "lattice" [
    relift_deriving;
    is_bot_deriving;
    bot_deriving;
    join_deriving;
    leq_deriving;
  ]
