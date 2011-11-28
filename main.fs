module Main

open Possum

let gTokens = [ //(AtomNode "print"); (StringNode "foo");
                //(AtomNode "print"); AtomNode("+"); IntegerNode(5); IntegerNode(3);
                //(AtomNode "define"); AtomNode("g"); AtomNode("+"); IntegerNode(5); AtomNode("+"); IntegerNode(2); IntegerNode(2);
                //AtomNode "print"; AtomNode "g";
                //AtomNode "defun"; AtomNode "foo"; AtomNode "x"; AtomNode "is"; AtomNode "print"; StringNode "whee from foo!"; AtomNode "print"; AtomNode "+"; IntegerNode 2; IntegerNode 5; AtomNode "end";
                //AtomNode "print"; AtomNode "foo"; IntegerNode 666;
                AtomNode "defun"; AtomNode "foo"; AtomNode "is";
                  AtomNode "define"; AtomNode "g"; AtomNode "+"; IntegerNode 5; IntegerNode 10;
                  AtomNode "cond";
                    AtomNode "="; IntegerNode 5; IntegerNode 5;
                      AtomNode "print"; StringNode "5 = 5!";
                    BoolNode true;
                      AtomNode "print"; StringNode "5 != 5";
                    AtomNode "end";
                  AtomNode "print"; StringNode "outer";
                AtomNode "end";

                AtomNode "foo";
                ]

let outputTokens () =
  for node in gTokens do
    printfn "%s" (exprToString node)

let main =
  let tc = Consumable gTokens

  initSym ()

  let st = parseExprs tc
  evalConsumable st

  System.Console.ReadKey ()