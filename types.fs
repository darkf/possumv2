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
    | StringNode s -> sprintf "<str '%s'>" s
    | IntegerNode i -> sprintf "<int %d>" i
    | FunctionNode (name, _, _) -> sprintf "<fn '%s'>" name
    | BoolNode b -> sprintf "<bool %s>" (if b then "true" else "false")
    | PairNode (a, b) -> sprintf "<pair %s -> %s>" (exprToString a) (exprToString b)
    | StreamNode s -> "<stream>"
    | NilNode -> sprintf "<nil>"
    //| _ -> sprintf "<<<error>>> -> %s" (string e)

let exprRepr e =
  match e with
    | AtomNode s -> sprintf "<atom '%s'>" s
    | _ -> exprToString e

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
