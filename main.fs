// possum v2.0.0
// copyright (c) 2011 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module Main

open Possum
open Parser
open Types

let main =
  let args = System.Environment.GetCommandLineArgs ()
  if args.Length < 2 then
    // REPL mode
    initSym ()
    
    let mutable cont = true
    while cont do
      try
        printf ">> "
        let input = System.Console.ReadLine ()
        match input with
          | ":q" | ":quit" | ":exit" -> cont <- false
          | _ ->
            let r = evalConsumable (Consumable (parseString input))
            printfn "%s" (exprRepr r)
      with
        | BindingError (msg, _) -> printfn "BindingError: %s" msg
        | SemanticError msg ->     printfn "SemanticError: %s" msg
        | ParseError msg ->        printfn "ParseError: %s" msg
        | e ->                     printfn "Unhandled exception: %s" e.Message
  else
    let tokens = parseFile args.[1] //"defun f is end print \"hi\""
    let tc = Consumable tokens
    //printConsumable tc
    initSym ()
    
    let st = parseExprs tc
    try
      ignore (evalConsumable st)
    with
      | BindingError (msg, _) -> printfn "BindingError: %s" msg
      | SemanticError msg ->     printfn "SemanticError: %s" msg
      | ParseError msg ->        printfn "ParseError: %s" msg
      | e ->                     printfn "Unhandled exception: %s" e.Message

    ignore (System.Console.ReadKey ())