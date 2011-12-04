// possum v2.0.0
// copyright (c) 2011 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module Parser

open Possum
open Types

let getNode acc =
  match acc with
    | "true" -> BoolNode true
    | "false" -> BoolNode false
    | "nil" -> NilNode
    | _ ->
      try
        IntegerNode (System.Int32.Parse acc)
      with
        _ ->
          AtomNode acc

let parseString (str : string) =
  let mutable acc = ""
  let mutable inString = false
  let mutable inAtom = false
  let mutable inEscape = false
  let mutable inComment = false
  let mutable xs = []

  for i in 0..str.Length-1 do
    match str.[i] with
      | '(' when str.[i+1] = '*' && not inComment ->
        // begin comment
        inComment <- true
      | ')' when str.[i-1] = '*' && inComment ->
        // end comment
        inComment <- false
      | _ when inComment -> ()
      // escape codes
      | '\\' when inString && not inEscape -> inEscape <- true
      | 'n' | 'r' | 't' | '"' | '\\' when inEscape ->
        acc <- acc + (match str.[i] with
                        | 'n' -> "\n"
                        | 'r' -> "\r"
                        | 't' -> "\t"
                        | '"' -> "\""
                        | '\\' -> "\\" | _ -> "")
        inEscape <- false
      // end string
      | '"' when inString = true ->
        xs <- xs @ [StringNode acc]
        inString <- false
        acc <- ""
      // begin string
      | '"' when not inString && not inAtom->
        inString <- true
      // whitespace
      | ' ' | '\t' | '\r' | '\n' when not inString ->
        if acc.Length > 0 then
          xs <- xs @ [getNode acc]
          acc <- ""
          inAtom <- false
      | c ->
        acc <- acc + (string c)
        if not inString then
          inAtom <- true

  if acc.Length > 0 then
    xs <- xs @ [getNode acc]

  xs

let parseFile (filename : string) =
  use sr = new System.IO.StreamReader(filename)
  let txt = sr.ReadToEnd ()
  parseString txt