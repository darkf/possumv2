possum v2.0.0  
copyright (c) 2011-2012 darkf  
licensed under the terms of the MIT license  
see LICENSE for details

Possum is a scheme-inspired language that is designed to be concise and simple.  
It features fixed-arity functions, enabling us to lose the mess of parentheses.

It features networking (via sockets), text processing (regular expressions), and other things that make it generally useful.

Here's a canonical factorial function:

    defun fact n is
      if = n 0
        1
      else
        * n fact - n 1
    end

    print fact 10

You can write this on one line, two lines, or however many you want (although we recommend you indent your code nicely!)

Unfortunately, possum is at a very early stage in development, so expect bugs and things downright not working! (In other words, don't trust your life with it.)

Usage:

- Compile with `fsc.exe types.fs possum.fsi lib/stream.fs lib/text.fs possum.fs parser.fs main.fs -o possum.exe` (although a solution file for Visual Studio 2010 is included.)

- Run `possum.exe examples/hi.ps`
  or without arguments to enter REPL mode