module Env
open System
open System.Collections.Generic
open Types

let newEnv p =
  {sym=new ExprDict(); prev=p}

let lookup first name =
  let rec iter env =
    if env.sym.ContainsKey name then
      Some env.sym.[name]
    else
      match env.prev with
        | Some a -> iter a
        | None -> None

  iter first

let setSymFar first name value =
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

  iter first

let setSymLocal env name value =
  env.sym.[name] <- value