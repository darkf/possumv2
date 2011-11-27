module Possum

//[<CustomEquality>]
type expr = AtomNode of string
          | StringNode of string
          | IntegerNode of int
          | FunctionNode of string * int * (expr list -> expr)
          | NilNode

[<Class>]
type Consumable =
  new : expr list -> Consumable

  member consume : unit -> expr option
  member peek    : unit -> expr option

  member remaining : unit -> int

  member tell : unit -> int
  member seek : int -> int

val printConsumable : Consumable -> unit

val exprToString : expr -> string
val exprToInt    : expr -> int

val lookup : string -> expr option
val isFunction : expr option -> bool

val parseUntil : Consumable -> string -> expr list
val parseBody  : Consumable -> expr list
val parseOne   : Consumable -> expr list
val parseN     : Consumable -> int -> expr list
val parseExprs : Consumable -> Consumable

val evalOne : Consumable -> expr
val evalConsumable : Consumable -> unit

val gSym : System.Collections.Generic.Dictionary<string, expr>
val gSpecialForms : System.Collections.Generic.Dictionary<string, (Consumable -> expr)>

val initSym : unit -> unit