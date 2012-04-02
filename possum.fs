// possum v2.0.0
// copyright (c) 2011-2012 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module Possum

open System
open System.Collections.Generic
open Types

type ExprDict = Dictionary<string, expr>
type Environment = { sym : ExprDict; prev : Environment option; }

type Consumable(tokens : expr list) =
  let mutable index = 0

  member this.consume () =
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
    if x < tokens.Length then
      index <- x

let gSym = new ExprDict()
let gSpecialForms = new Dictionary<string, (Consumable -> expr)>()
let envstack = new Stack<Environment>()
let globalEnv = {sym=gSym; prev=None}

let lastEnv () =
  envstack.Peek ()

let pushNewEnv () =
  envstack.Push {sym=new ExprDict(); prev=Some (lastEnv ())}

let pushEnv env =
  envstack.Push env

let popEnv () =
  envstack.Pop ()

let newEnv () =
  {sym=new ExprDict(); prev=Some (lastEnv ())}

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
        | None ->
          (* we're at the upper-most scope, and we can't find it. set it here *)
          env.sym.[name] <- value

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
          | "defun" ->  [AtomNode s] @ (parseUntil tc "end") @ [AtomNode "end"]
          | "defstruct" -> [AtomNode s] @ (parseUntil tc "end") @ [AtomNode "end"]
          | "getf" -> [AtomNode s] @ (parseN tc 2)
          | "setf" -> [AtomNode s] @ (parseN tc 2)
          | "->" -> [AtomNode s] @ (parseUntil tc "end") @ [AtomNode "end"]
          | "cond" ->   [AtomNode s] @ (parseUntil tc "end") @ [AtomNode "end"]
          | "begin" ->  [AtomNode s] @ (parseUntil tc "end") @ [AtomNode "end"]
          | "list" ->   [AtomNode s] @ (parseUntil tc "end") @ [AtomNode "end"]
          | "set" ->    [AtomNode s] @ (parseN tc 2)
          | "ref" ->    [AtomNode s; (tc.consume ()).Value]
          | "if" ->
            let cond = parseOne tc
            let then1 = parseOne tc
            match tc.peek () with
              | Some (AtomNode "else") ->
                ignore (tc.consume ());
                let then2 = parseOne tc
                [AtomNode "if"] @ cond @ then1 @ [AtomNode "else"] @ then2
              | _ ->
                [AtomNode "if"] @ cond @ then1
                
          | _ -> raise (ParseError ("Special form isn't covered in parsing: " + s))
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
  for i = 1 to n do
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

  | Some (StringNode _ as n) | Some (IntegerNode _ as n) | Some (BoolNode _ as n)
  | Some (NilNode as n)  | Some (StreamNode _ as n) -> n | Some (StructNode _ as n) | Some (PairNode (_, _) as n) -> n
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

let _setSF (tc : Consumable) =
  match tc.consume () with
    | Some (AtomNode s) ->
      let value = evalOne tc
      setSymFar s value
      value
    | _ -> raise (SemanticError "non-atom given to set")

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

let _defunSF (tc : Consumable) : expr =
  let name = (tc.consume ()).Value.ToString()
  let args = parseUntil tc "is"
  //pushNewEnv ()
  let body = parseBody tc

  // if we're defining this in the global scope, don't inherit anything, just push a new scope
  // otherwise, we're in a nested closure, let's define it locally and inherit the closing scope
  let env = (if (lastEnv ()) = globalEnv then
               newEnv ()
             else
               let o = lastEnv ()
               let e = {sym=ExprDict(); prev=Some globalEnv}
               // clone parent
               for key in o.sym.Keys do
                 e.sym.[key] <- o.sym.[key]
               e)
  
  let fn (xargs : expr list) : expr =
    pushEnv env
    for i = 0 to args.Length-1 do
      match args.[i] with
        | AtomNode s -> setSymLocal s xargs.[i]
        | _ -> raise (ParseError "argument not an atom")

    let r = evalConsumable (Consumable body)
    ignore (popEnv ())
    r
  
  let f = FunctionNode (name, args.Length, fn)

  // now the closure needs to know itself -- because when it inherits its parent,
  // this closure isn't yet defined, so we need to define it.
  // since env.sym is mutable, we can just do it now.
  env.sym.[name] <- f

  //ignore (popEnv ())
  setSymLocal name f
  //gSym.[name.ToString()] <- f
  f

let _defstructSF (tc : Consumable) : expr =
  match (tc.consume(), tc.consume()) with
    | (Some (AtomNode structName), Some (AtomNode "is")) -> 
      let fields = parseUntil tc "end" 
      let fn (args : expr list) : expr =
        if not (args.Length = fields.Length) then
          raise (SemanticError "must construct with all fields")

        let c = new ExprDict()

        for i = 0 to args.Length-1 do
          c.[exprToString fields.[i]] <- args.[i]

        StructNode c
      let f = FunctionNode ("<struct ctor>", fields.Length, fn)
      setSymLocal structName f
      NilNode
    | _ -> raise (ParseError "invalid defstruct syntax")

let _lambdaSF (tc : Consumable) : expr =
  let args = parseUntil tc "is"
  let body = parseBody tc

  // if we're defining this in the global scope, don't inherit anything, just push a new scope
  // otherwise, we're in a nested closure, let's define it locally and inherit the closing scope
  let env = (if (lastEnv ()) = globalEnv then
               newEnv ()
             else
               let o = lastEnv ()
               let e = {sym=ExprDict(); prev=Some globalEnv}
               // clone parent
               for key in o.sym.Keys do
                 e.sym.[key] <- o.sym.[key]
               e)
  
  let fn (xargs : expr list) : expr =
    pushEnv env
    for i = 0 to args.Length-1 do
      match args.[i] with
        | AtomNode s -> setSymLocal s xargs.[i]
        | _ -> raise (ParseError "argument not an atom")

    let r = evalConsumable (Consumable body)
    ignore (popEnv ())
    r

  FunctionNode ("<fn>", args.Length, fn)

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

  gSym.["empty?"] <- FunctionNode ("empty?", 1, fun args->
    match args with
      | [StringNode s] -> BoolNode (s.Length = 0)
      | [PairNode (a,_)] -> BoolNode (exprEquals a NilNode)
      | _ -> raise (SemanticError "non-(string|pair) passed to empty?"))
      

  gSpecialForms.["define"] <- _defineSF
  gSpecialForms.["defstruct"] <- _defstructSF
  gSpecialForms.["set"] <- _setSF
  gSpecialForms.["defun"] <- _defunSF
  gSpecialForms.["cond"] <- _condSF
  gSpecialForms.["begin"] <- fun tc ->
                               let rec iter r =
                                 match tc.peek () with
                                   | Some (AtomNode "end") -> ignore (tc.consume ()); r
                                   | _ -> iter (evalOne tc)
                               iter NilNode
  gSpecialForms.["list"] <- fun tc ->
                              let rec iter (xs : expr) =
                                match tc.consume () with
                                  | Some (AtomNode "end") -> xs
                                  | Some a -> iter (PairNode (a, xs))
                                  | None -> raise (ParseError "expected end, not EOF")
                              possumListReverse (iter NilNode)
  gSpecialForms.["if"] <- fun tc ->
                            let cond = exprToBool (evalOne tc)
                            let then1 = parseOne tc

                            match tc.peek () with
                              | Some (AtomNode "else") ->
                                ignore (tc.consume ()); // consume else
                                if cond then
                                  let r = evalConsumable (Consumable then1)
                                  ignore (parseOne tc) // consume else part
                                  r
                                else
                                  evalOne tc
                              | _ ->
                                if cond then evalConsumable (Consumable then1) else NilNode

  gSpecialForms.["ref"] <- fun tc ->
    match tc.consume () with
      | Some (AtomNode a as e) ->
        match (lookup a) with
          | Some (FunctionNode (_, _, _) as f) -> f
          | Some _ -> e
          | None -> e
      | Some a -> raise (SemanticError "cannot ref non-atoms")
      | None -> raise (ParseError "nothing to ref")

  gSpecialForms.["getf"] <- fun tc ->
    match (evalOne tc, tc.consume ()) with
      | (StructNode fields as st, Some (AtomNode field)) ->
        if fields.ContainsKey(field) then
          fields.[field]
        else
          raise (BindingError (sprintf "%s.%s" (exprToString st) field, ""))
      | _ -> raise (SemanticError "getf requires a struct and a field")


  gSpecialForms.["setf"] <- fun tc ->
    match (evalOne tc, tc.consume (), evalOne tc) with
      | (StructNode fields as st, Some (AtomNode field), value) ->
        if fields.ContainsKey(field) then
          fields.[field] <- value
          value
        else
          raise (BindingError (sprintf "%s.%s" (exprToString st) field, ""))
      | _ -> raise (SemanticError "setf requires a struct and a field")

  gSpecialForms.["->"] <- _lambdaSF

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

  gSym.["set-symbol"] <- FunctionNode ("set-symbol", 2, fun args ->
    match args with
      | [StringNode a; b] ->
        setSymLocal a b
        b
      | _ -> raise (SemanticError "invalid args to set-symbol"))

  gSym.["set-global-symbol"] <- FunctionNode ("set-global-symbol", 2, fun args ->
    match args with
      | [StringNode a; b] ->
        gSym.[a] <- b
        b
      | _ -> raise (SemanticError "invalid args to set-global-symbol"))
  
  gSym.["_printsym"] <- FunctionNode ("_printsym", 0, fun args ->
    //for key in gSym.Keys do
    //  printfn "  %s -> %s" key (exprToString gSym.[key])
    let rec iter i env =
      printfn "[%d]%s" i (if env = globalEnv then " (global)" else "")

      for key in env.sym.Keys do
        printfn "  %s -> %s" key (exprToString env.sym.[key])
      
      match env.prev with
        | Some a -> iter (i + 1) a
        | None -> ()
      
    iter 0 (lastEnv ())
    NilNode)

  PossumStream.initModule gSym
  PossumText.initModule gSym

  pushEnv globalEnv
  ()

