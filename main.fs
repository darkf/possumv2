module Main

open Possum
open Parser

let main =
  let tokens = parseString "defun f is end print \"hi\""
  let tc = Consumable tokens

  initSym ()

  let st = parseExprs tc
  ignore (evalConsumable st)

  System.Console.ReadKey ()