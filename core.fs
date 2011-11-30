module PossumCore

//[<CustomEquality>]
type expr = AtomNode of string
          | StringNode of string
          | IntegerNode of int
          | FunctionNode of string * int * (expr list -> expr)
          | StreamNode of System.IO.Stream
          | PairNode of expr * expr
          | BoolNode of bool
          | NilNode

exception ParseError of string
exception BindingError of string * string
exception SemanticError of string

type ExprDict = System.Collections.Generic.Dictionary<string, expr>