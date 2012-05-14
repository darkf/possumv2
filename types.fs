// possum v2.0.0
// copyright (c) 2011-2012 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module Types

//[<CustomEquality>]
type expr = AtomNode of string
          | StringNode of string
          | IntegerNode of int
          | FunctionNode of string * int * Environment * (expr list -> Environment -> expr)
          | SpecialFormNode of (Consumable -> Environment -> expr list) * (Consumable -> Environment -> expr)
          | StreamNode of System.IO.Stream
          | StructNode of ExprDict
          | PairNode of expr * expr
          | BoolNode of bool
          | NilNode

          with
          override x.ToString() =
            match x with
              | AtomNode s -> s
              | _ -> "fixme"

and ExprDict = System.Collections.Generic.Dictionary<string, expr>
and Environment = { sym : ExprDict; prev : Environment option; }

and Consumable(tokens : expr list) =
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

exception ParseError of string
exception BindingError of string * string
exception SemanticError of string

let rec exprToString e =
  match e with
    | AtomNode s -> s
    | StringNode s -> s
    | IntegerNode i -> string i
    | FunctionNode (name, _, _, _) -> sprintf "<fn '%s'>" name
    | SpecialFormNode (_, _) -> "<special form>"
    | BoolNode b -> if b then "true" else "false"
    | PairNode (a, b) -> sprintf "<pair %s -> %s>" (exprToString a) (exprToString b) // todo: format list
    | StreamNode s -> "<stream>"
    | StructNode s -> exprRepr e
    | NilNode -> "nil"
    //| _ -> sprintf "<<<error>>> -> %s" (string e)

and exprRepr e =
  match e with
    | AtomNode s -> sprintf "<atom '%s'>" s
    | StringNode s -> sprintf "<str '%s'>" s
    | IntegerNode i -> sprintf "<int %d>" i
    | FunctionNode (name, _, _, _) -> sprintf "<fn '%s'>" name
    | SpecialFormNode (_, _) -> "<special form>"
    | BoolNode b -> sprintf "<bool %s>" (if b then "true" else "false")
    | PairNode (a, b) -> sprintf "<pair %s -> %s>" (exprToString a) (exprToString b)
    | StreamNode s -> "<stream>"
    | StructNode st ->
      let mutable s = "<struct "
      let mutable j = []
      for field in st.Keys do
        let x = field + "=" + (exprToString st.[field])
        j <- x :: j
      s <- s + (j |> List.rev |> String.concat ", ") + ">"
      s
    | NilNode -> sprintf "<nil>"

let exprToInt (e : expr) : int =
  match e with
    |  IntegerNode i -> i
    | _ -> -1

let exprToBool (e : expr) : bool =
  match e with
    | BoolNode b -> b
    | _ -> false

let exprEquals (x : expr) (y : expr) : bool =
  match x, y with
    | IntegerNode a, IntegerNode b -> a = b
    | StringNode a, StringNode b -> a = b
    | BoolNode a, BoolNode b -> a = b
    | AtomNode a, AtomNode b -> a = b
    | NilNode, NilNode -> true
    | a, b ->
      printfn "!!! warning: equals called on (%A,%A) - returning false" a b
      false

let rec toPossumList (x : expr list) : expr =
  let rec iter (lst : expr list) =
    match lst with
      | x :: [] ->
        PairNode (x, NilNode)
      | x :: xs ->
        PairNode (x, (toPossumList xs))
      | [] ->
        NilNode
  iter x

let rec possumListAppend l m =
  match l with
    | NilNode -> m
    | PairNode (x, xs) -> PairNode (x, (possumListAppend xs m))
    | _ -> raise (SemanticError "possumListAppend")

let rec possumListReverse (lst : expr) : expr =
  match lst with
    | NilNode -> NilNode
    | PairNode (x, xs) -> possumListAppend (possumListReverse xs) (PairNode (x, NilNode))
    | _ -> raise (SemanticError "possumListReverse")