.. _type-map:

Maps
====

Maps are dictionaries storing a *value* for each *key*.

Maps are constructed using by passing a list containing elements of
type ``Pair<A, B>`` to the ``map`` constructor function.  The keys of
the resulting map are of type ``A`` and values are of type ``B``.  The
expression ``map[]`` produces an empty map.

The following example produces a map with two entries ``1 -> "ABS"``
and ``3 -> "SACO"``::

  Map<Int, String> m = map[Pair(1, "ABS"), Pair(3, "SACO")];

.. note:: In case of duplicate keys, it is unspecified which value the
          map will contain for a given key.

The value associated with a key can be obtained using the ``lookup``
and ``lookupDefault`` functions.

A map can be iterated over via the functions ``keys``, ``values`` and
``entries``, which return the set of keys and the list of values and
entries of the map, respectively, and can be passed, for example, to
the ``foreach`` statement or the ``map`` function.


Datatypes and Constructors
--------------------------

The datatype for a map from type ``A`` to type ``B`` is is ``Map<A,
B>``.  The ``map`` constructor function is used to construct maps.

Functions
---------

emptyMap
^^^^^^^^

Returns ``True`` if the map is empty, ``False`` otherwise.

::

  emptyMap(map[Pair(1, 2)])
  // => False


removeKey
^^^^^^^^^

Returns a map with the first occurrence of ``key`` removed.

::

  removeKey(map[Pair(1, "One"), Pair(2, "Two")], 1)
  // => map[Pair(2, "Two")]


values
^^^^^^

Returns a list of all values within the map.

::

  values(map[Pair(1, "One"), Pair(2, "Two")])
  // => list["One", "Two"]


keys
^^^^

Returns a set of all keys of the map.

::

  keys(map[Pair(1, "One"), Pair(2, "Two")])
  // => set[1, 2]


entries
^^^^^^^

Returns a list of all entries (i.e., pairs of key and value) of the
map.

::

  entries(map[Pair(1, "One"), Pair(2, "Two")])
  // => list[Pair(1,"One"), Pair(2,"Two")]


lookup
^^^^^^

If value ``v`` is associated with a given key ``k``, return
``Just(v)``.  Otherwise, return ``Nothing``.

::

  lookup(map[Pair(1, "One"), Pair(2, "Two")], 1)
  // => Just("One")
  lookup(map[Pair(1, "One"), Pair(2, "Two")], 3)
  // => Nothing


lookupDefault
^^^^^^^^^^^^^

Returns the value associated with a given key ``k``.  If the map does
not contain an entry with key ``k``, return the default value ``d``.

::

  lookupDefault(map[Pair(1, "One"), Pair(2, "Two")], 3, "(not found)")
  // => "(not found)"

.. note:: If you need to know whether the map actually contains an
          entry for key ``k``, even if it maps to ``d``, consider
          using the function ``lookup`` instead.


lookupUnsafe
^^^^^^^^^^^^

Returns the value associated with key ``k``.  It is an error if the
map does not contain an entry with key ``k``.

::

  lookupUnsafe(map[Pair(1, "One"), Pair(2, "Two")], 1)
  // => "One"


lookupReverse
^^^^^^^^^^^^^

Given a value ``v``, if there is an entry ``Pair(k,v)`` in the map
return ``Just(k)``.  Otherwise, return ``Nothing``.

::

  lookupReverse(map[Pair(1, "One"), Pair(2, "Two")], "One")
  // => Just(1)


lookupReverseDefault
^^^^^^^^^^^^^^^^^^^^

Given a value ``v``, if there is an entry ``Pair(k,v)`` in the map
return ``k``.  Otherwise, return the default value ``d``.

::

  lookupReverseDefault(map[Pair(1, "One"), Pair(2, "Two")], "Three", -1)
  // => -1

.. note:: If you need to know whether the map actually contains an
          entry with value ``v``, even if its key is ``d``, consider
          using the function ``lookupReverse`` instead.


insert
^^^^^^

Returns a map with all entries of ``map`` plus an entry ``p``, which
is given as a pair (``Pair(key, value)``) and maps ``key`` to
``value``.  If ``map`` already contains an entry with the same key
``key``, it is not removed from the map but ``lookup`` will return the
new value ``value``.  (The function ``removeKey`` removes the first
entry for a given key and thus “undoes” the effect of calling
``insert``.)

::

  insert(map[Pair(1, "One"), Pair(2, "Two")], Pair(1, "OneTwo"))
  // => map[Pair(1,"OneTwo"), Pair(1,"One"), Pair(2,"Two")]


put
^^^

Returns a map with all entries of ``ms`` plus an entry mapping ``k``
to ``v``, minus the first entry already mapping ``k`` to a value.

::

  put(map[Pair(1, "One"), Pair(2, "Two")], 1, "OneTwo")
  // => map[Pair(1,"OneTwo"), Pair(2,"Two")]
