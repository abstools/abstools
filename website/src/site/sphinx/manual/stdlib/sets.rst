.. _type-set:

Sets
====

A set contains elements of the same type, without duplicates.  Sets
are constructed via the ``set`` constructor function, e.g., ``set[1,
2, 2, 3]`` creates a set of three integers 1, 2, 3.  The expression
``set[]`` produces the empty set.

To add an element to a set, use the function ``insertElement``, to
remove an element, use ``remove``.  To test for set membership, use
the function ``contains``.

The ``takeMaybe`` function can be used to iterate through a set as
follows::

  def Unit printAll<A>(Set<A> set) =
    case takeMaybe(set) {
      Nothing => println("Finished")
      | Just(e) => let (Unit dummy) = println("Element " + toString(e)) in printAll(remove(set, e))
    };

Alternatively, ``map`` and other functions that operate on lists, as
well as the ``foreach`` statement, can be used on sets by obtaining a
list of the set's elements via the ``elements`` function.

Datatypes and Constructors
--------------------------

The datatype for sets with elements of type ``A`` is ``Set<A>``.  The
``set`` constructor function is used to construct sets::

  set[1, 2, 3, 3, 3]
  // => set[1, 2, 3]


Functions
---------

contains
^^^^^^^^

Returns ``True`` if set ``ss`` contains element ``e``, ``False``
otherwise.

::

  contains(set[1, 2, 3], 2)
  // => True


emptySet
^^^^^^^^

Returns ``True`` if set ``xs`` is empty, ``False`` otherwise.

::

  emptySet(set[])
  // => True


size
^^^^

Returns the number of elements in set ``xs``.

::

  size(set[1, 2, 3])
  // => 3


elements
^^^^^^^^

Returns a list with all elements in set ``xs``.

::

  elements(set[1, 2, 3])
  // => list[1, 2, 3]


union
^^^^^

Returns a set containing all elements of sets ``set1`` and ``set2``.

::

  union(set[1, 2, 3], set[4, 3, 5])
  // => set[1, 2, 3, 4, 5]


intersection
^^^^^^^^^^^^

Returns a set containing all elements that are present in both sets
``set1`` and ``set2``.

::

  intersection(set[1, 2, 3], set[4, 3, 5])
  // => set[3]


difference
^^^^^^^^^^

Returns a set containing all elements of set ``set1`` not present in
set ``set2``.

::

  difference(set[1, 2, 3], set[4, 3, 5])
  // => set[1, 2]


isSubset
^^^^^^^^

Returns ``True`` if ``set`` contains all elements of ``maybe_subset``,
``False`` otherwise.

::

  isSubset(set[1], set[1, 2, 3])
  // => True


insertElement
^^^^^^^^^^^^^

Returns a set with all elements of set ``xs`` plus element ``e``.
Returns a set with the same elements as ``xs`` if ``xs`` already
contains ``e``.

::

  insertElement(set[2, 3], 1)
  // => set[1, 2, 3]


remove
^^^^^^

Returns a set with all elements of set ``xs`` except element ``e``.
Returns a set with the same elements as ``xs`` if ``xs`` did not
contain ``e``.

::

  remove(set[1, 2, 3], 1)
  // => set[2, 3]


take
^^^^

Returns one element from a non-empty set.  It is an error to call
``take`` on an empty set; consider using ``takeMaybe`` in that case.

::

  take(set[1, 2, 3])
  // => 1 // or 2 or 3


takeMaybe
^^^^^^^^^

Returns ``Just`` wrapping one element from a set, or ``Nothing`` for
an empty set.

::

  takeMaybe(set[1, 2, 3])
  // => Just(1) // or Just(2) or Just(3)
  takeMaybe(set[])
  // => Nothing
