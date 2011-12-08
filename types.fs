// possum v2.0.0
// copyright (c) 2011 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module Types

//[<CustomEquality>]
type expr = AtomNode of string
          | StringNode of string
          | IntegerNode of int
          | FunctionNode of string * int * (expr list -> expr)
          | StreamNode of System.IO.Stream
          | PairNode of expr * expr
          | BoolNode of bool
          | NilNode

          with
          override x.ToString() =
            match x with
              | AtomNode s -> s
              | _ -> "fixme"

exception ParseError of string
exception BindingError of string * string
exception SemanticError of string

type ExprDict = System.Collections.Generic.Dictionary<string, expr>

let rec exprToString e =
  match e with
    | AtomNode s -> s
    | StringNode s -> s
    | IntegerNode i -> string i
    | FunctionNode (name, _, _) -> sprintf "<fn '%s'>" name
    | BoolNode b -> if b then "true" else "false"
    | PairNode (a, b) -> sprintf "<pair %s -> %s>" (exprToString a) (exprToString b) // todo: format list
    | StreamNode s -> "<stream>"
    | NilNode -> "nil"
    //| _ -> sprintf "<<<error>>> -> %s" (string e)

let exprRepr e =
  match e with
    | AtomNode s -> sprintf "<atom '%s'>" s
    | StringNode s -> sprintf "<str '%s'>" s
    | IntegerNode i -> sprintf "<int %d>" i
    | FunctionNode (name, _, _) -> sprintf "<fn '%s'>" name
    | BoolNode b -> sprintf "<bool %s>" (if b then "true" else "false")
    | PairNode (a, b) -> sprintf "<pair %s -> %s>" (exprToString a) (exprToString b)
    | StreamNode s -> "<stream>"
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