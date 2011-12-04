// possum v2.0.0
// copyright (c) 2011 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module PossumStream
open System
open Types

let fileOpen (args : expr list) : expr =
  match args.[0] with
    | StringNode path ->
      StreamNode (new IO.FileStream(path, System.IO.FileMode.OpenOrCreate))
    | _ -> raise (SemanticError "non-path passed to file-open")

let socketOpen (args : expr list) : expr =
  match args.[0] with
    | StringNode host ->
      let port = exprToInt args.[1]
      StreamNode ((new Net.Sockets.TcpClient (host, port)).GetStream ())
    | _ -> raise (SemanticError "non-string passed to socket-open")

let streamReadInt32 (args : expr list) : expr =
  match args.[0] with
    | StreamNode s ->
      IntegerNode ((new IO.BinaryReader(s)).ReadInt32 ())
    | _ -> raise (SemanticError "non-stream passed to stream-read-int32")

let streamRead (args : expr list) : expr =
  match args.[0] with
    | StreamNode s ->
      let n = exprToInt args.[1]
      let buf : byte[] = Array.zeroCreate n
      match s.Read(buf, 0, n) with
        | 0 -> NilNode // EOF
        | _ -> StringNode (Text.Encoding.UTF8.GetString(buf, 0, n))
    | _ -> raise (SemanticError "non-stream passed to stream-read")

let streamReadAll (args : expr list) : expr =
  match args.[0] with
    | StreamNode s ->
      StringNode ((new IO.StreamReader(s)).ReadToEnd ())
    | _ -> raise (SemanticError "non-stream passed to stream-read-all")

let streamWrite (args : expr list) : expr =
  match args.[0] with
    | StreamNode s ->
      match args.[1] with
        | StringNode str ->
          let t = (new IO.StreamWriter(s))
          t.Write str
          t.Flush ()
          NilNode
        | _ -> raise (SemanticError "non-string passed to stream-write")
    | _ -> raise (SemanticError "non-stream passed to stream-write")

let streamClose args =
  match args with
    | StreamNode s :: [] ->
      s.Close ()
      NilNode
    | _ -> raise (SemanticError "non-stream passed to stream-close")

let initModule (sym : ExprDict) =
  sym.["file-open"] <- FunctionNode ("file-open", 1, fileOpen)

  sym.["socket-open"] <- FunctionNode ("socket-open", 2, socketOpen)

  sym.["stream-read"] <- FunctionNode ("stream-read", 2, streamRead)
  sym.["stream-read-all"] <- FunctionNode ("stream-read-all", 1, streamReadAll)
  sym.["stream-read-int32"] <- FunctionNode ("stream-read-int32", 1, streamReadInt32)

  sym.["stream-write"] <- FunctionNode ("stream-write", 2, streamWrite)

  sym.["rn"] <- FunctionNode ("rnrn", 0, fun args -> StringNode "\r\n")

  sym.["stream-close"] <- FunctionNode ("stream-close", 1, streamClose)