module Possum

open System

//[<CustomEquality>]
type expr = AtomNode of string
          | StringNode of string
          | IntegerNode of int
          | FunctionNode of string * int * (expr list -> expr)
          | BoolNode of bool
          | NilNode

          with
          override x.ToString () =
            match x with
              | AtomNode s -> s
              | _ -> x.ToString()

          (*interface IEquatable with
            override x.Equals y =
               match y with
                FunctionNode (name, arity, fn) -> name = x.name
                | _ -> x = y*)


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

let gSym = new Collections.Generic.Dictionary<string, expr>()
let gSpecialForms = new Collections.Generic.Dictionary<string, (Consumable -> expr)>()

let exprToString e =
  match e with
    (AtomNode s) -> sprintf "<atom '%s'>" s
  | (StringNode s) -> sprintf "<str '%s'>" s
  | (IntegerNode i) -> sprintf "<int %d>" i
  | (FunctionNode (name, _, _)) -> sprintf "<fn '%s'>" name
  | NilNode -> sprintf "<nil>"
  | _ -> "<<<error>>>"

let exprToInt (e : expr) : int =
  match e with
    (IntegerNode i) -> i
  | _ -> (-1)

let exprToBool (e : expr) : bool =
  match e with
    (BoolNode b) -> b
  | _ -> false

let exprEquals (x : expr) (y : expr) : bool =
  match x with
    | IntegerNode i ->
      match y with
        | IntegerNode i2 ->
          i = i2
        | _ ->
          printfn "can't ="
          false
    | _ ->
      printfn "can't ="
      false

let lookup name =
  if gSym.ContainsKey name then
    Some gSym.[name]
  else
    None

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
          //[AtomNode s] @ (parseN tc gSpecialForms.[s].arity)
          match s with
            "define" -> [AtomNode s] @ (parseN tc 2)
          | "defun" ->
            printfn "defun"
            let name = (tc.consume ()).Value
            //printfn "> defun: %s" (exprToString name)
            let args = parseUntil tc "is"
            let body = parseBody tc
            
            //gSym |> Seq.iter (fun (KeyValue (k,v)) ->
            //  printfn "%s -> %s" k (exprToString v))
            
            let fn (xargs : expr list) : expr =
              //printfn "- this is fn! (arg 0 = %s)" (exprToString xargs.[0])
              evalConsumable (Consumable body)
              NilNode
            
            let f = FunctionNode (name.ToString(), args.Length, fn)
            gSym.[name.ToString()] <- f
            //printfn "::: %s" (name.ToString ())
            [f]
          | "cond" ->
            printfn "parse cond"
            [AtomNode "cond"] @ (parseUntil tc "end") @ [AtomNode "end"]
        else
          match lookup s with
            Some (FunctionNode (name, arity, fn)) ->
              printfn "> consuming arguments for function %s: %d" name arity
              let args = parseN tc arity
              [AtomNode s] @ args
         
          | _ -> [AtomNode s]
    | Some (StringNode _ as n) | Some (IntegerNode _ as n) | Some (BoolNode _ as n) -> [n]
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
            //printfn "func (%s)" name
            // apply func
            let mutable xargs = []
            for i in [1..arity] do
              xargs <- xargs @ [evalOne tc]
            fn xargs
          | Some a ->
            //printfn "variable"
            a
          | None ->
            printfn "error: unknown binding '%s'" s
            // todo: error
            assert false
            NilNode

  | Some (StringNode _ as n) | Some (IntegerNode _ as n) | Some (BoolNode _ as n) -> n
  | Some node -> printfn "other: %s" (exprToString node); NilNode
  | None -> printfn "<<<none>>>"; NilNode
  | _ -> printfn "other (_)"; NilNode

and evalConsumable (tc : Consumable) =
  let rec iter last =
    while tc.remaining () > 0 do
      ignore (evalOne tc)

  iter NilNode

let _fnPlus (args : expr list) : expr =
  IntegerNode ((exprToInt args.[0]) + (exprToInt args.[1]))

let _defineSF (tc : Consumable) : expr =
  let name = tc.consume ()
  match name with
    Some (AtomNode s) ->
      let value = evalOne tc
      printfn "> defining '%s' to %s" s (exprToString value)
      gSym.[s] <- value
      value
  | _ -> assert false; NilNode // todo: figure out how to do error handling (exceptions or something)

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
          printfn "true!"
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
  gSym.["="] <- FunctionNode ("=", 2, fun args -> 
    let r = (exprEquals args.[0] args.[1])
    printfn "eq? : %s" (if r = true then "true" else "false")
    BoolNode r)

  gSpecialForms.["define"] <- _defineSF
  gSpecialForms.["defun"] <- fun args -> NilNode // todo: shouldn't need this hack (just to fill the symtable)
  gSpecialForms.["cond"] <- _condSF

