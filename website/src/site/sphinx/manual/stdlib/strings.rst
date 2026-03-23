.. _type-string:

Strings
=======

Datatypes and Constructors
--------------------------

The datatype for strings is ``String``.

String literals are enclosed in double quotes (``"``).  Line feed in a
string literal is written as ``\n``, carriage return as ``\r``.

Operators
---------

.. table:: Syntax
   :align: left
   :class: syntax

   ============   ================   =============   ===========
   Expression     Meaning            Associativity   Result type
   ============   ================   =============   ===========
   ``e1 == e2``   equality           left            Bool
   ``e1 != e2``   inequality         left            Bool
   ``e1 < e2``    less than          left            Bool
   ``e1 <= e2``   less or equal      left            Bool
   ``e1 > e2``    greater than       left            Bool
   ``e1 >= e2``   greater or equal   left            Bool
   ``e1 + e2``    concatenation      left            String
   ============   ================   =============   ===========


Functions
---------

toString
^^^^^^^^

This function converts any data into a printable string
representation.

::

  toString(5)
  // => "5"
  toString(True)
  // => "True"


substr
^^^^^^

Returns a substring of a given string ``str`` with length ``length``
starting from position ``start`` (inclusive).  The first character in
a string has position 0.

::

  substr("Hello world", 0, 5)
  // => "Hello"


strlen
^^^^^^

Returns the length of the given string ``str``.  The empty string
(``""``) has length 0.

::

  strlen("Hello")
  // => 5


println
^^^^^^^

Prints the given string ``s`` to standard output, followed by a
newline, meaning that the next output will not continue on the same
line.

::

  println("Hello")
  // => Unit
  // As a side effect, prints its argument to standard output


print
^^^^^

Prints the given string ``s`` to standard output.  Does not cause the
next output to begin on a new line.

::

  print("Hello")
  // => Unit
  // As a side effect, prints its argument to standard output
