module Parser
open Tokenize
open Types
open Consumable
open Env

let rec parseUntil (tc : Consumable) env until =
  let rec iter xs =
    let t = tc.peek ()
    match t with
      Some (AtomNode s) when s = until ->
          ignore (tc.consume ())
          xs
      | None -> xs
      | _ ->
          iter (xs @ (parseOne tc env))
  iter []

and parseBody (tc : Consumable) env =
  // parse until "end", basically
  parseUntil tc env "end"

and parseOne (tc : Consumable) env =
    let t = tc.consume ()
    match t with
      Some (AtomNode s) ->
        (*
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
        else *)
          match lookup env s with
            Some (FunctionNode (_, arity, _, _)) ->
              //printfn "> consuming arguments for function %s: %d" name arity
              let args = parseN tc env arity
              [AtomNode s] @ args
         
          | _ -> [AtomNode s]
    | Some (StringNode _ as n) | Some (IntegerNode _ as n) | Some (BoolNode _ as n) | Some (NilNode as n) -> [n]
    | None -> []
    | _ ->
      printfn "other"
      []

and parseN (tc : Consumable) env n =
  List.fold (@) [] [for x in 1..n -> parseOne tc env]

and parseExprs (tc : Consumable) env =
  let rec iter xs =
    match parseOne tc env with
    | [] -> xs
    | r -> iter (xs @ r)

  Consumable (iter [])