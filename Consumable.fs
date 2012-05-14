module Consumable
open Types

type Consumable(tokens : expr list) =
  let mutable index = 0

  member this.consume () =
    if index >= tokens.Length then
      None
    else
      let r = tokens.[index]
      index <- index + 1
      Some r

  member this.peek () =
    if index >= tokens.Length then
      None
    else
      Some tokens.[index]

  member this.remaining () = tokens.Length - index

  member this.tell () = index
  member this.seek x =
    if x < tokens.Length then
      index <- x