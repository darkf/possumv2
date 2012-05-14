module Consumable
open Types

[<Class>]
type Consumable =
  new : expr list -> Consumable

  member consume : unit -> expr option
  member peek    : unit -> expr option

  member remaining : unit -> int

  member tell : unit -> int
  member seek : int -> unit