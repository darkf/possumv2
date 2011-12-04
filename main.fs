// possum v2.0.0
// copyright (c) 2011 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module Main

open Possum
open Parser

let main =
  let args = System.Environment.GetCommandLineArgs ()
  if args.Length < 2 then
    printfn "usage: %s FILE" args.[0]
  else
    let tokens = parseFile args.[1] //"defun f is end print \"hi\""
    let tc = Consumable tokens
    //printConsumable tc
    initSym ()
    
    let st = parseExprs tc
    ignore (evalConsumable st)

  System.Console.ReadKey ()