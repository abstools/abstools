.. _type-list:

Lists
=====

A list is a sequence of values of the same type.  Lists are
constructed via the ``list`` constructor function, e.g., ``list[1, 2,
3]`` creates a list of three integers.  An empty list is created via
``list[]`` or ``Nil``.

The time to access a value via ``nth`` is proportional to the length
of the list.  The first value of a list can be accessed in constant
time, using the ``head`` function.

The ``map``, ``fold`` and ``filter`` second-order functions described
in this section implement common functional programming patterns.  To
execute some statements for each element in a list, use a ``foreach``
loop (see :ref:`foreach-loop`).


Datatypes and Constructors
--------------------------

A list is defined either as the empty list (``Nil``) or as a value
``a`` followed by another list ``l`` (``Cons(a, l)``)::

  data List<A> = Nil | Cons(A head, List<A> tail);

Literal lists of arbitrary length can be written using a special
function ``list``.  In the following example, ``l1`` and ``l2``
contain the same elements::

  List<Int> l1 = list[1, 2, 3];
  List<Int> l2 = Cons(1, Cons(2, Cons(3, Nil)));


Functions
---------

head
^^^^

Returns the head of a list.

::

  head(list[1, 2, 3])
  // => 1


tail
^^^^

Returns the tail (rest) of a list.

::

  tail(list[1, 2, 3])
  // => list[2, 3]


length
^^^^^^

Returns the length of a list.  The length of ``Nil`` is 0.

::
  length(list[1, 2, 3])
  // => 3


isEmpty
^^^^^^^

Checks if a list is empty.  Returns ``True`` for ``Nil``, ``False``
otherwise.

::

  isEmpty(list[1, 2, 3])
  // => False
  isEmpty(list[])
  // => True


nth
^^^

Returns the ``n``-th element of a list.  Returns the head of ``l`` for
``n``=0, returns the last element of ``l`` when ``n`` is
``length(l)-1``.

It is an error if ``n`` is equal to or larger than ``length(l)``.

::

  nth(list[1, 2, 3], 2)
  // => 3


without
^^^^^^^

Returns a fresh list where all occurrences of ``a`` have been removed.

::

  without(list[1, 2, 3, 3, 5], 3)
  // => list[1, 2, 5]


concatenate
^^^^^^^^^^^

Returns a list containing all elements of list ``list1`` followed by
all elements of list ``list2``.

::

  concatenate(list[1, 2, 3], list[4, 5])
  // => list[1, 2, 3, 4, 5]


appendright
^^^^^^^^^^^

Returns a list containing all elements of list ``l`` followed by the
element ``p`` in the last position.

::

  appendright(list[1, 2], 3)
  // => list[1, 2, 3]


reverse
^^^^^^^

Returns a list containing all elements of ``l`` in reverse order.

::

  reverse(list[1, 2, 3])
  // => list[3, 2, 1]


copy
^^^^

Returns a list of length ``n`` containing ``p`` n times.

::

  copy(-1, 4)
  // => list[-1, -1, -1,  -1]


map
^^^

Applies a function to each element of a list, returning a list of
results in the same order.  The function ``fn`` must take an argument
of type ``A`` and return a value of type ``B``.

::

  map((Int x) => x + 1)(list[1, 2, 3])
  // => list[2, 3, 4]


filter
^^^^^^

Returns a list containing only the elements in the given list for
which the given predicate returns ``True``.  The function
``predicate`` must take an argument of type ``T`` and return a Boolean
value.

::

  filter((Int x) => x < 4)(list[1, 2, 3, 4, 5])
  // => list[1, 2, 3]


foldl
^^^^^

Accumulates a value starting with ``init`` and applying ``accumulate``
from left to right to current accumulator value and each element.  The
function ``accumulate`` must take two arguments: the first of type
``A`` (the type of the list) and the second of type ``B`` (the
accumulator and result type), and return a value of type ``B``.

::

  foldl((Int x, String res) => res + toString(x))(list[1, 2, 3], "")
  // => "123"


foldr
^^^^^

Accumulates a value starting with ``init`` and applying ``accumulate``
from right to left to each element and current accumulator value.  The
function ``accumulate`` must take two arguments: the first of type
``A`` (the type of the list) and the second of type ``B`` (the
accumulator and result type), and return a value of type ``B``.

::

  foldl((Int x, String res) => res + toString(x))(list[1, 2, 3], "")
  // => "321"


range
^^^^^

Returns a list of integers ranging from the first to the second
argument, inclusive.  If ``lower`` is equal to ``upper``, returns a
list of one element; if ``lower`` is larger than ``upper``, returns an
empty list.

::

  range(2, 5)
  // => list[2, 3, 4, 5]
