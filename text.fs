module PossumText

open System.Text.RegularExpressions
open Types

let regexMatch (args : expr list) : expr =
  // regex-match "d.*" "ducks"
  // returns a list of match groups
  match args with
    StringNode regex :: StringNode input :: [] ->
      let m = Regex.Match(input, regex)
      if not m.Success then
        NilNode
      else
        // ugly hack to create a possum list (cons in reverse order)
        let mutable x = NilNode
        for i = m.Groups.Count-1 downto 0 do
          x <- PairNode (StringNode m.Groups.[i].Value, x)
        x
    | _ -> raise (SemanticError "regex-match is string -> string")


let initModule (sym : ExprDict) =
  sym.["regex-match"] <- FunctionNode ("regex-match", 2, regexMatch)