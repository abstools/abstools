.. _type-optionals:

Optionals
=========

Datatypes and Constructors
--------------------------

The datatype ``Maybe<A>`` wraps a concrete value of type A.  The value
``Nothing`` denotes the absence of such a value.

::

  Maybe<Int> answer = Just(42);
  Maybe<String> question = Nothing;


Functions
---------

isJust
^^^^^^

The function ``isJust`` returns ``False`` if the ``Maybe`` value is
``Nothing``, ``True`` otherwise.

::

  isJust(Nothing)
  // => False
  isJust(Just(1))
  // => True


fromJust
^^^^^^^^

The function ``fromJust`` returns the wrapped value of a ``Maybe``.
It is an error to call ``fromJust`` on ``Nothing``.

::

  fromJust(Just(1))
  // => 1


fromJustDefault
^^^^^^^^^^^^^^^

The function ``fromJustDefault`` returns the wrapped value of the
first argument, or the second argument if the first argument is
``Nothing``.

::

  fromJustDefault(Nothing, False)
  // => False
