(** Common representation of patterns and expressions. *)

open Ppxlib

type t =
  | Record of (longident * t) list
  | Tuple of t list
  | Constructor of longident * t option
  | Unit
  | PolyConstructor of string * t option
  | Base of string
  | Inherit of longident * string

val create_record: prefix:string -> longident list -> t
val create_tuple: prefix:string -> int -> t
val create_nested_tuple: prefix:string -> int -> t
val create_nested_variant: len:int -> i:int -> t -> t

val to_pat: loc:location -> t -> pattern
val to_exps: loc:location -> t -> expression list
val to_exp: loc:location -> t -> expression
