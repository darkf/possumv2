module PossumStream
open Types

let fileOpen (args : expr list) : expr =
  match args.[0] with
    | StringNode path ->
      StreamNode (new System.IO.FileStream(path, System.IO.FileMode.OpenOrCreate))
    | _ -> raise (SemanticError "non-path passed to file-open")

let streamReadAll (args : expr list) : expr =
  match args.[0] with
    | StreamNode s ->
      StringNode ((new System.IO.StreamReader(s)).ReadToEnd ())
    | _ -> raise (SemanticError "non-stream passed to stream-read-all")

let streamClose (args : expr list) : expr =
  match args.[0] with
    | StreamNode s ->
      s.Close ()
      NilNode
    | _ -> raise (SemanticError "non-stream passed to stream-close")

let initModule (sym : ExprDict) =
  sym.["file-open"] <- FunctionNode ("file-open", 1, fileOpen)

  sym.["stream-read-all"] <- FunctionNode ("stream-read-all", 1, streamReadAll)
  sym.["stream-close"] <- FunctionNode ("stream-close", 1, streamClose)