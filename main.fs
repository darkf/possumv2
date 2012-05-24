// possum v2.0.0
// copyright (c) 2011-2012 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module Main

open Possum
open Tokenize
open Parser
open Env
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
            let r = Consumable (tokenizeString input) |> evalConsumable globalEnv
            printfn "%s" (exprRepr r)
      with
        | BindingError (msg, _) -> printfn "BindingError: %s" msg
        | SemanticError msg ->     printfn "SemanticError: %s" msg
        | ParseError msg ->        printfn "ParseError: %s" msg
        | e ->                     printfn "Unhandled exception: %s" e.Message
  else
    let tokens = tokenizeFile args.[1]
    let tc = Consumable tokens
    initSym ()
    
    let st = parseExprs tc globalEnv
    try
      evalConsumable globalEnv st |> ignore
    with
      | BindingError (msg, _) -> printfn "BindingError: %s" msg
      | SemanticError msg ->     printfn "SemanticError: %s" msg
      | ParseError msg ->        printfn "ParseError: %s" msg
      | e ->                     printfn "Unhandled exception: %s" e.Message

    System.Console.ReadKey () |> ignore