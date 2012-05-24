// possum v2.0.0
// copyright (c) 2011-2012 darkf
// licensed under the terms of the MIT license
// see LICENSE for details

module Possum
open Env
open Types

//val evalOne : Consumable -> expr
val evalConsumable : Environment -> Consumable -> expr

val gSym : System.Collections.Generic.Dictionary<string, expr>
val globalEnv : Environment

val initSym : unit -> unit