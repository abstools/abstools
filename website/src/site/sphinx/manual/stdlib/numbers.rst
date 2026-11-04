.. _type-numbers:

Numbers
=======

Datatypes and constructors
--------------------------

The numeric datatypes of ABS are ``Int`` (arbitrary-length integers),
``Rat`` (arbitrary-precision rational numbers) and ``Float`` (64-bit
floating point).  See :ref:`sec:builtin-types` for their syntax.

.. note:: Support for floating-point calculations is under
          development; calculations resulting in ``Inf`` or ``NaN``
          currently have unspecified runtime behavior.

Operators
---------

.. table:: Syntax
   :align: left
   :class: syntax

   =============   ================   =============   ====================
   Expression      Meaning            Associativity   Result type
   =============   ================   =============   ====================
   ``e1 == e2``    equality           left            ``Bool``
   ``e1 != e2``    inequality         left            ``Bool``
   ``e1 < e2``     less than          left            ``Bool``
   ``e1 \<= e2``   less or equal      left            ``Bool``
   ``e1 > e2``     greater than       left            ``Bool``
   ``e1 >= e2``    greater or equal   left            ``Bool``
   ``e1 + e2``     addition           left            number
   ``e1 - e2``     subtraction        left            number
   ``e1 * e2``     multiplication     left            number
   ``e1 / e2``     division           left            ``Rat`` or ``Float``
   ``e1 % e2``     modulo             left            number
   =============   ================   =============   ====================


Functions
---------

min, max
^^^^^^^^

These functions calculate the maximum and minimum of their two
arguments.  Since ABS datatypes are ordered, they can be applied to
arguments of all types.

::

  max(5, 3)
  // => 5

  min(2.5, 1.5)
  // => 1.5


abs
^^^

This function calculates the absolute (positive) value of its
argument.

::

  max(-2)
  // => 2


truncate
^^^^^^^^

Converts a rational number to an integer by truncating towards zero.

::

  truncate(7/5)
  // => 1


float
^^^^^

Converts an integer or rational number into a floating-point number.

::

  float(1)
  // => 1.0
  float(1/2)
  // => 0.5

.. note:: Very large integers and some rational numbers cannot be
          converted exactly into floating-point numbers.  More in
          general, ``rat(float(x)) == x`` might not be true.

rat
^^^

Converts a floating-point number into a rational number.

::

  rat(0.5)
  // => 1/2

.. note:: Conversion from floating-point to rational numbers is
          inexact and backend-specific.  In general, ``float(rat(x))
          == x`` might not be true.

floor
^^^^^

Returns the largest integer smaller or equal to the floating-point
argument.

::

  floor(5.3)
  // => 5


ceil
^^^^

Returns the smallest integer larger or equal to the floating-point
argument.

::

  ceil(5.3)
  // => 6


numerator
^^^^^^^^^

Returns the numerator of a rational number, or the number itself for
an integer.

::

  numerator(1/2)
  // => 1


denominator
^^^^^^^^^^^

Returns the denominator of a rational number, or ``1`` for an integer.

::

  denominator(1/2)
  // => 2


pow
^^^

This function calculates :math:`b^n`.

::

  pow(1/2, 4)
  // => 1/16


sqrt_newton
^^^^^^^^^^^

This function approximates :math:`\sqrt{x}`; it stops when two
subsequent estimates (as per Newton's algorithm) differ by less than
``epsilon``, its third argument.  The second argument is an initial
estimate of the square root.

::

  sqrt_newton(4, 2, 1/100)
  // => 2


exp_newton
^^^^^^^^^^

This function approximates :math:`e^x`; it stops when two subsequent
estimates (as per Newton's algorithm) differ by less than its second
argument ``epsilon``.

::

  exp_newton(1, 1/100)
  // => 163/60


sqrt
^^^^

This function returns :math:`\sqrt{x}`.  It is an error if its
argument is negative.

::

  sqrt(4.0)
  // => 2.0


log
^^^

This function returns :math:`ln(x)`, the natural logarithm of its argument.

::

  log(1.0)
  // => 0.0


exp
^^^

This function returns :math:`e^x`, Euler’s number :math:`e` raised to
the power of ``x``.

::

  exp(1.0)
  // => 2.7182818284590455


random
^^^^^^

Returns an integer between 0 (inclusive) and its argument (exclusive).

::

  random(5)
  // => 4
