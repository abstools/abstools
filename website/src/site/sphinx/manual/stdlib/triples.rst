.. _type-triple:

Triples
=======

Datatypes and Constructors
--------------------------

The ``Triple<A, B, C>`` datatype holds a triple of values of types
``A``, ``B`` and ``C``, respectively.  The constructor is called
``Triple`` as well.

::

  Triple<Int, String, Bool> triple = Triple(15, "Hello World", False);


Functions
---------

fstT
^^^^

The function ``fstT`` returns the first value in a triple.

::

  fstT(Triple(1, "Two", 3.0))
  // => 1


sndT
^^^^

The function ``sndT`` returns the second value in a triple.

::

  sndT(Triple(1, "Two", 3.0))
  // => "Two"


trdT
^^^^

The function ``trdT`` returns the third value in a triple.

::

  trdT(Triple(1, "Two", 3.0))
  // => 3.0
