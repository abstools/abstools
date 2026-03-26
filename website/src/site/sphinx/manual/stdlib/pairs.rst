.. _type-pair:

Pairs
=====

Datatypes and Constructors
--------------------------

The ``Pair<A, B>`` datatype holds a pair of values of types ``A`` and
``B``, respectively.  The constructor is called ``Pair`` as well.

::

  Pair<Int, String> pair = Pair(15, "Hello World");


Functions
---------

fst
^^^

The function ``fst`` returns the first value in a pair.

::

  fst(Pair(1, "Two"))
  // => 1


snd
^^^

The function ``snd`` returns the second value in a pair.

::

  snd(Pair(1, "Two"))
  // => "Two"
