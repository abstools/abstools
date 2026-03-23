.. _stdlib:predefined-exceptions:

Predefined exceptions in the Standard Library
=============================================

ABS provides pre-defined exceptions that are thrown in specific
circumstances.  See :ref:`sec:exception-types` for information about
exceptions.

NOTE: This list is subject to revision in future versions of ABS.  Not
all these exceptions are currently thrown by different backends in the
described situation.

``DivisionByZeroException``
    Raised in arithmetic expressions when the divisor (denominator) is
    equal to 0, as in +3/0+

``AssertionFailException``
    The assert keyword was called with +False+ as argument

``PatternMatchFailException``
    The pattern matching was not complete. In other words all c
    catch-all clause

``NullPointerException``
    A method was called on ``null``, or ``await`` or ``get`` were
    passed a null future.

``StackOverflowException``
    The calling stack has reached its limit (system error)

``HeapOverflowException``
    The memory heap is full (system error)

``KeyboardInterruptException``
    The user pressed a key sequence to interrupt the running ABS
    program

``ObjectDeadException``
    A method was called on a dead (crashed) object
