// possum v2.0.0
// copyright (c) 2011 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module Possum

open System
open Types

(*type 'a pslist = Cons of 'a * 'a pslist
               | Empty;;*)

type ExprDict = System.Collections.Generic.Dictionary<string, expr>
type Environment = { sym : ExprDict; prev : Environment option; }

type Consumable(toks : expr list) =
  let tokens = toks
  let mutable index = 0

  member this.consume () : expr option =
    if index >= tokens.Length then
      None
    else
      let r = tokens.[index]
      index <- index + 1
      Some r

  member this.peek () =
    if index >= tokens.Length then
      None
    else
      Some tokens.[index]

  member this.remaining () = tokens.Length - index

  member this.tell () = index
  member this.seek x =
    if x < toks.Length then
      index <- x
    0

let gSym = new ExprDict()
let gSpecialForms = new Collections.Generic.Dictionary<string, (Consumable -> expr)>()
let envstack = new Collections.Generic.Stack<Environment>()
let globalEnv = {sym=gSym; prev=None}

let lastEnv () =
  envstack.Peek ()

let pushNewEnv () =
  envstack.Push {sym=new ExprDict(); prev=Some (lastEnv ())}

let pushEnv env =
  envstack.Push env

let popEnv () =
  envstack.Pop ()

let lookup name =
  let rec iter env =
    if env.sym.ContainsKey name then
      Some env.sym.[name]
    else
      match env.prev with
        | Some a -> iter a
        | None -> None

  iter (lastEnv ())

let setSymFar name value =
  let rec iter env =
    if env.sym.ContainsKey name then
      env.sym.[name] <- value
    else
      // look into the upper scopes
      match env.prev with
        | Some a -> iter a
        | None -> ()

  iter (lastEnv ())

let setSymLocal name value =
  (lastEnv ()).sym.[name] <- value

let isFunction e =
  match e with
    Some (FunctionNode (_, _, _)) -> true
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

let rec parseUntil (tc : Consumable) (until : string) : expr list =
  let rec iter xs =
    let t = tc.peek ()
    match t with
      Some (AtomNode s) when s = until ->
          ignore (tc.consume ())
          xs
      | None -> xs
      | _ ->
          iter (xs @ (parseOne tc))
  iter []

and parseBody (tc : Consumable) : expr list =
  // parse until "end", basically
  parseUntil tc "end"

and parseOne (tc : Consumable) : expr list =
    let t = tc.consume ()
    match t with
      Some (AtomNode s) ->
        if gSpecialForms.ContainsKey s then
          match s with
          | "define" -> [AtomNode s] @ (parseN tc 2)
          | "defun" ->
            let name = (tc.consume ()).Value
            let args = parseUntil tc "is"
            let body = parseBody tc
            
            let fn (xargs : expr list) : expr =
              pushNewEnv ()
              for i in 0..args.Length-1 do
                match args.[i] with
                  | AtomNode s -> setSymLocal s xargs.[i]
                  | _ -> raise (ParseError "argument not an atom")

              let r = evalConsumable (Consumable body)
              ignore (popEnv ())
              r
            
            let f = FunctionNode (name.ToString(), args.Length, fn)
            gSym.[name.ToString()] <- f
            [f]
          | "cond" ->
            //printfn "parse cond"
            [AtomNode "cond"] @ (parseUntil tc "end") @ [AtomNode "end"]
          | _ ->
            raise (ParseError ("Special form isn't covered in parsing: " + s))
        else
          match lookup s with
            Some (FunctionNode (name, arity, fn)) ->
              //printfn "> consuming arguments for function %s: %d" name arity
              let args = parseN tc arity
              [AtomNode s] @ args
         
          | _ -> [AtomNode s]
    | Some (StringNode _ as n) | Some (IntegerNode _ as n) | Some (BoolNode _ as n) | Some (NilNode as n) -> [n]
    | None -> []
    | _ ->
      printfn "other"
      []

and parseN (tc : Consumable) (n : int) : expr list =
  let mutable out = []
  for i in [1..n] do
    out <- out @ (parseOne tc)
  out

and parseExprs (tc : Consumable) =
  let rec iter xs =
    let r = parseOne tc
    match r with
      [] -> xs
    | _ -> iter (xs @ r)

  Consumable (iter [])

and evalOne (tc : Consumable) =
  let t = tc.consume ()
  match t with
    Some (AtomNode s) ->
      if gSpecialForms.ContainsKey s then
        gSpecialForms.[s] tc
      else
        let r = lookup s
        match r with
          Some (FunctionNode (name, arity, fn)) ->
            // apply function
            let mutable xargs = []
            for i in 1..arity do
              xargs <- xargs @ [evalOne tc]
            fn xargs
          | Some a -> a // variable value
          | None ->
            raise (BindingError ((sprintf "Unknown binding '%s'" s), s))

  | Some (StringNode _ as n) | Some (IntegerNode _ as n) | Some (BoolNode _ as n) | Some (NilNode as n) -> n
  | Some (FunctionNode (_, _, _) as n) -> n
  | None -> raise (ParseError "None given to evalOne")
  //| _ -> printfn "other (_)"; NilNode

and evalConsumable (tc : Consumable) : expr =
  let rec iter last =
    match tc.peek() with
      | None -> last
      | _ -> iter (evalOne tc)

  iter NilNode

let _fnPlus (args : expr list) : expr =
  IntegerNode ((exprToInt args.[0]) + (exprToInt args.[1]))

let _fnMinus (args : expr list) : expr =
  IntegerNode ((exprToInt args.[0]) - (exprToInt args.[1]))

let _fnMul (args : expr list) : expr =
  IntegerNode ((exprToInt args.[0]) * (exprToInt args.[1]))

let _defineSF (tc : Consumable) : expr =
  match tc.consume () with
    | Some (AtomNode s) ->
      let value = evalOne tc
      //printfn "> defining '%s' to %s" s (exprToString value)
      setSymLocal s value
      value
    | _ -> raise (ParseError "non-atom given to define")

let _condSF (tc : Consumable) : expr =
  // cond
  //   or nil? x empty? x
  //     print "nil or empty"
  //   true
  //     print "not nil or empty"
  // end

  let rec iter () =
    match tc.peek() with
      | Some (AtomNode "end") ->
        ignore (tc.consume ())
        NilNode
      | _ ->
        let cond = evalOne tc
       
        if (exprToBool cond) = true then
          let r = evalOne tc
          ignore (parseUntil tc "end")
          r
        else
          ignore (parseOne tc) // get rid of path that didn't match
          iter ()
  iter ()

let initSym () =
  gSym.["print"] <- FunctionNode ("print", 1, (fun args -> printfn ": %s" (exprToString args.[0]); NilNode))
  gSym.["+"] <- FunctionNode ("+", 2, _fnPlus)
  gSym.["-"] <- FunctionNode ("-", 2, _fnMinus)
  gSym.["*"] <- FunctionNode ("*", 2, _fnMul)
  gSym.["="] <- FunctionNode ("=", 2, fun args -> 
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

  gSpecialForms.["define"] <- _defineSF
  gSpecialForms.["defun"] <- fun args -> NilNode // todo: shouldn't need this hack (just to fill the symtable)
  gSpecialForms.["cond"] <- _condSF

  PossumStream.initModule gSym

  pushEnv globalEnv
  ()

