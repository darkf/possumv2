module Consumable

[<Class>]
type Consumable =
  new : 'a list -> Consumable

  member consume : unit -> 'a option
  member peek    : unit -> 'a option

  member remaining : unit -> int

  member tell : unit -> int
  member seek : int -> unit