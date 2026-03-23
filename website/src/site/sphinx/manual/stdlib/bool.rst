.. _type-bool:

Boolean values
==============

Datatypes and Constructors
--------------------------

The literals for Boolean values are ``True`` and ``False``.  The name
of the datatype is ``Bool``.

Example::

  Bool value = True;


Operators
---------

The following operators apply to Boolean values:

.. table:: Syntax
   :align: left
   :class: syntax

   ============     ================   =============
   Expression       Meaning            Associativity
   ============     ================   =============
   ``e1 || e2``     logical or         left
   ``e1 && e2``     logical and        left
   ``e1 == e2``     equality           left
   ``e1 != e2``     inequality         left
   ``e1 < e2``      less than          left
   ``e1 <= e2``     less or equal      left
   ``e1 > e2``      greater than       left
   ``e1 >= e2``     greater or equal   left
   ``! e``          logical negation   right
   ============     ================   =============

The semantics of “less than” and similar operators for Booleans follow
the rules for algebraic datatypes, see :ref:`sec:operator-expressions`
(in short, ``False < True``).
