Structures are a way to group data into one confined area, and create useful abstractions. They are simply data types with named fields, which can be constructed and manipulated.

Here's how we construct one:
    defstruct vector is
      x
      y
    end

where vector is a structure with two fields -- x and y.

We construct a vector by simply invoking its name, such as
    define v vector 10 20

which will create a vector with x=10, y=20.


This is all fine and dandy, but we need some way to access these fields. That's where the . operator comes in:

    print . v x (* prints 10 *)
    print . v y (* prints 20 *)

Right, now how do we manipulate these fields? With setf, of course.

    setf v x 15
    setf v y 42



Since possum is dynamically typed, we can use the concept of duck typing to work generically on types:

    defun + left right is
      +
        + . left x . right x
        + . left y . right y

to get the sum of their x and y components combined, as long as they have them.


---


Alternatively, we could use Racket's style of accessing struct members, such as:

    print vector-x v (* prints 10 *)
    print vector-y v (* prints 20 *)

and setting:

    set-vector-x! v 15
    set-vector-y! v 42

which allows for nifty things like re-defining accessors, which lets you do this:

    def set-vector-x! v x is
      if < x 0
        setf v x 0 (* minimum at 0 *)
      else
        setf v x x
    end

where setf is a built-in function which sets fields.