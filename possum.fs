// possum v2.0.0
// copyright (c) 2011-2012 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module Possum

open System
open System.Collections.Generic
open Consumable
open Types
open Env


let gSym = new ExprDict()
let gSpecialForms = new Dictionary<string, (Consumable -> expr)>()
let envstack = new Stack<Environment>()
let globalEnv = {sym=gSym; prev=None}


let isFunction e =
  match e with
    Some (FunctionNode (_, _, _, _)) -> true
  | _ -> false

let printConsumable (tc : Consumable) =
  let mutable doContinue = true
  let now = tc.tell ()

  while doContinue do
    match tc.consume () with
        Some a -> printfn "%s" (exprToString a)
      | None ->
        doContinue <- false
  ignore (tc.seek now)


let rec evalOne (tc : Consumable) env =
  let t = tc.consume ()
  match t with
    Some (AtomNode s) ->
      if gSpecialForms.ContainsKey s then
        gSpecialForms.[s] tc
      else
        let r = lookup env s
        match r with
          Some (FunctionNode (name, arity, cls, fn)) ->
            // apply function
            fn [for x in 1..arity -> evalOne tc cls]
          | Some a -> a // variable value
          | None ->
            raise (BindingError ((sprintf "Unknown binding '%s'" s), s))

  | Some (StringNode _ as n) | Some (IntegerNode _ as n) | Some (BoolNode _ as n)
  | Some (NilNode as n)  | Some (StreamNode _ as n) -> n | Some (StructNode _ as n) | Some (PairNode (_, _) as n) -> n
  | Some (FunctionNode (_, _, _, _) as n) -> n
  | None -> raise (ParseError "None given to evalOne")

and evalConsumable env (tc : Consumable) : expr =
  let rec iter last =
    match tc.peek() with
    | None -> last
    | _ -> iter (evalOne tc env)

  iter NilNode

let _fnPlus (args : expr list) : expr =
  IntegerNode ((exprToInt args.[0]) + (exprToInt args.[1]))

let _fnMinus (args : expr list) : expr =
  IntegerNode ((exprToInt args.[0]) - (exprToInt args.[1]))

let _fnMul (args : expr list) : expr =
  IntegerNode ((exprToInt args.[0]) * (exprToInt args.[1]))

let initSym () =
  gSym.["print"] <- FunctionNode ("print", 1, globalEnv, (fun args -> printfn ": %s" (exprToString args.[0]); NilNode))
  gSym.["print-raw"] <- FunctionNode ("print-raw", 1, globalEnv, (fun args -> printf "%s" (exprToString args.[0]); NilNode))
  
  gSym.["+"] <- FunctionNode ("+", 2, globalEnv, _fnPlus)
  gSym.["-"] <- FunctionNode ("-", 2, globalEnv, _fnMinus)
  gSym.["*"] <- FunctionNode ("*", 2, globalEnv, _fnMul)
  (*gSym.["="] <- FunctionNode ("=", 2, fun args -> 
    let r = (exprEquals args.[0] args.[1])
    //printfn "eq? : %s" (if r = true then "true" else "false")
    BoolNode r)

  gSym.["cons"] <- FunctionNode ("cons", 2, fun args -> PairNode (args.[0], args.[1]))
  gSym.["car"] <- FunctionNode ("car", 1, fun args ->
    match args.[0] with
      | PairNode (a, _) -> a
      | _ -> raise (SemanticError "non-pair passed to car"))
  gSym.["cdr"] <- FunctionNode ("cdr", 1, fun args ->
    match args.[0] with
      | PairNode (_, b) -> b
      | _ -> raise (SemanticError "non-pair passed to cdr"))

  gSym.["pair?"] <- FunctionNode ("pair?", 1, fun args ->
    match args.[0] with
      | PairNode (_, _) -> BoolNode true
      | _ -> BoolNode false)

  gSym.["nil?"] <- FunctionNode ("nil?", 1, fun args ->
    match args.[0] with
      | NilNode -> BoolNode true
      | _ -> BoolNode false)

  gSym.["empty?"] <- FunctionNode ("empty?", 1, fun args->
    match args with
      | [StringNode s] -> BoolNode (s.Length = 0)
      | [PairNode (a,_)] -> BoolNode (exprEquals a NilNode)
      | _ -> raise (SemanticError "non-(string|pair) passed to empty?"))
      

  gSym.["concat"] <- FunctionNode ("concat", 2, fun args ->
    match args with
      | [a; b] -> StringNode ((exprToString a) + (exprToString b))
      | _ -> raise (SemanticError "wrong args to concat"))

  gSym.["list-reverse"] <- FunctionNode ("list-reverse", 1, fun args ->
    match args with
      | [PairNode (_,_) as a] -> possumListReverse a
      | _ -> raise (SemanticError "non-list passed to list-reverse"))

  gSym.["at"] <- FunctionNode ("at", 2, fun args ->
    match args with
      | [PairNode (_,_) as a; IntegerNode index] ->
        let rec iter l i =
          match l with
            | PairNode (a, NilNode) ->
              if i = index then a
              else NilNode
            | PairNode (a, b) ->
              if i = index then a
              else iter b (i + 1)
            | _ -> raise (SemanticError "non-pair in at")
        iter a 0
      | [StringNode s; IntegerNode index] ->
        if s.Length < index then NilNode
        else (StringNode (string s.[index]))
      | _ -> raise (SemanticError "non-string/list passed to at"))

  gSym.["with-set-at"] <- FunctionNode ("set-at", 3, fun args ->
    match args with
      | [PairNode (a, b) as p; IntegerNode index; value] ->
        let rec iter l i xs =
          match l with
            | PairNode (a, NilNode) ->
              a :: xs
            | PairNode (h, j) ->
              if i = index then
                iter j (i + 1) (value :: xs)
              else
                iter j (i + 1) (h :: xs)
        toPossumList (List.rev (iter p 0 []))
      | _ -> raise (SemanticError "wrong args to with-set-at"))

  gSym.["not"] <- FunctionNode ("not", 1, fun args ->
    match args with
      | [BoolNode b] -> BoolNode (not b)
      | _ -> raise (SemanticError "non-bool passed to not"))

  gSym.["str"] <- FunctionNode ("str", 1, fun args -> StringNode (exprToString args.[0]))
  gSym.["int"] <- FunctionNode ("int", 1, fun args ->
    match args with
      | [IntegerNode i as x]-> x
      | [StringNode s] -> IntegerNode (Int32.Parse s)
      | _ -> raise (SemanticError "wrong value passed to int"))
  gSym.["substring"] <- FunctionNode ("substring", 3, fun args ->
    match args with
      | [StringNode str; IntegerNode start; IntegerNode len] ->
        StringNode (str.Substring(start, len))
      | _ -> raise (SemanticError "substring takes string -> int -> int"))
  gSym.["string-length"] <- FunctionNode ("string-length", 1, fun args ->
    match args with
      | [StringNode str] ->
        IntegerNode str.Length
      | _ -> raise (SemanticError "non-string passed to string-length"))
  gSym.["string-split"] <- FunctionNode ("string-split", 2, fun args ->
    match args with
      | [StringNode str; StringNode delim] ->
        let s = str.Split([| delim |], StringSplitOptions.None)
        toPossumList (List.init s.Length (fun i -> StringNode s.[i]))
      | _ -> raise (SemanticError "non-string given to string-split"))
  *)

  gSym.["set-global-symbol"] <- FunctionNode ("set-global-symbol", 2, globalEnv, fun args ->
    match args with
      | [StringNode a; b] ->
        gSym.[a] <- b
        b
      | _ -> raise (SemanticError "invalid args to set-global-symbol"))

  gSym.["y"] <- IntegerNode 666
  
  (*gSym.["_printsym"] <- FunctionNode ("_printsym", 0, fun args ->
    //for key in gSym.Keys do
    //  printfn "  %s -> %s" key (exprToString gSym.[key])
    let rec iter i env =
      printfn "[%d]%s" i (if env = globalEnv then " (global)" else "")

      for key in env.sym.Keys do
        printfn "  %s -> %s" key (exprToString env.sym.[key])
      
      match env.prev with
        | Some a -> iter (i + 1) a
        | None -> ()
      
    iter 0 ENV)
    NilNode)*)

  //PossumStream.initModule gSym
  //PossumText.initModule gSym

  ()

