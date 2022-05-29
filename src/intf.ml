open Ppxlib

module type Base =
sig
  val name: string
  val typ: loc:location -> core_type -> core_type
  val unit: loc:location -> expression (* TODO: separate *)
end

module type Tuple =
sig
  val tuple: loc:location -> expression list -> expression
end

module type Record =
sig
  val record: loc:location -> (longident * expression) list -> expression
end

module type Variant =
sig
  val variant: loc:location -> ((prefix:string -> PatExp.t) * (prefix:string ->   PatExp.t) * expression * expression list) list -> expression
end

module type S =
sig
  include Base
  include Tuple
  include Record
  include Variant
end
