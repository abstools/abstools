**********
Statements
**********

This chapter specifies all ABS statements.

.. table:: Syntax
   :align: left
   :class: syntax

   ===============   ==============
   *Statement* ::=   *SkipStmt*
   \                 \| *VarDeclStmt*
   \                 \| *AssignStmt*
   \                 \| *ExpStmt*
   \                 \| *AssertStmt*
   \                 \| *AwaitStmt*
   \                 \| *SuspendStmt*
   \                 \| *ThrowStmt*
   \                 \| *ReturnStmt*
   \                 \| *Block*
   \                 \| *IfStmt*
   \                 \| *SwitchStmt*
   \                 \| *WhileStmt*
   \                 \| *ForeachStmt*
   \                 \| *TryCatchFinallyStmt*
   ===============   ==============


Skip
====

The skip statement is a statement that does nothing.

.. table:: Syntax
   :align: left
   :class: syntax

   ==============   ===========
   *SkipStmt* ::=   ``skip`` ``;``
   ==============   ===========

Example::

  skip;


Variable Declarations
=====================

A variable declaration statement is used to declare variables.
Variable declarations can occur at any point in a sequence of
statements; i.e., it is not necessary to declare variables only at the
beginning of methods or blocks.

Variables declared inside a block are in scope for the duration of the
block.  It is an error to declare a variable with the same name of
another variable in scope.  A local variable can have the same name as
an object field.

A variable declaration has an expression that defines the initial
value of the variable.  The initialization expression is mandatory
except for variables of reference types (interfaces and futures), in
which case the variable is initialized with ``null`` if the
initialization expression is omitted.

.. table:: Syntax
   :align: left
   :class: syntax

   =================   ==============
   *VarDeclStmt* ::=   *Type* *SimpleIdentifier* [ ``=`` *Exp* ] ``;``
   =================   ==============

Example::

  Bool b = True;

Note that the initialization expression does not need to be a pure expression.


Constant Declarations
---------------------

Variable and field declarations can carry a ``Final`` annotation.  The
effect of such an annotation is to forbid re-assignment to the
variable or field.

The following example will cause a compile-time error since we are
trying to assign a new value to ``constant_i``::

  {
      [Final] Int constant_i = 24;
      constant_i = 25;
  }


Assignment
==========

The assign statement assigns a value to a variable or a field.

Assignments to a field ``f`` can be written either ``this.f = e;`` or
``f = e;``.  In case a local variable ``f`` is in scope at the point
of the assignment statement, the ``this`` prefix has to be used to
assign to the field ``f``; assignment to ``f`` will change the value
of the local variable.

.. table:: Syntax
   :align: left
   :class: syntax

   ================   =====================
   *AssignStmt* ::=   [ ``this`` ``.`` ] *SimpleIdentifier* ``=`` *Exp* ``;``
   ================   =====================

Example::

  this.f = True;
  x = 5;

Note that the expression does not need to be pure.


Expressions as Statements
=========================

An expression statement is a statement that consists of a single
expression.  When an expression statement is executed, the expression
is evaluated and the resulting value is discarded.

Expression statements are used for their side effects, for example
issuing an asynchronous method call without waiting for its result.

.. table:: Syntax
   :align: left
   :class: syntax

   =============   =========
   *ExpStmt* ::=   *Exp* `;`
   =============   =========

.. note:: Creating an object without storing a reference (and hence
          never invoking a method on the new object) can be a
          meaningful operation, for example when the object has a
          ``run`` method and interacts with the rest of the system by
          calling methods on references passed in via the ``new``
          expression.

Example::

  server!operate();
  new Client(server);


Assertions
==========

An assert statement is a statement for asserting certain conditions.
If the expression evaluates to ``True``, executing an assertion is
equivalent to ``skip;``.  If the expression evaluates to ``False``, it
is equivalent to ``throw AssertionFailException;``.

.. table:: Syntax
   :align: left
   :class: syntax

   ================   =============
   *AssertStmt* ::=   ``assert`` *PureExp* ``;``
   ================   =============

Example::

  assert x != null;


.. _await-stmt:

Await (Statement)
=================

An await statement suspends the current task until the given guard
becomes active (evaluates to ``True``).  While the task is suspended,
other tasks within the same COG can be scheduled.

Guards can wait for a futures to become resolved, for a Boolean
condition over the object state to become true, or (in timed ABS) for
a certain duration to pass.

.. note:: When evaluating a guard results in an exception, that
          exception will be thrown when the task is scheduled next,
          and can be caught as normal (see
          :ref:`try-catch-finally-stmt`).  Note that a guard can be
          evaluated multiple times before its task is scheduled -- the
          most recent exception will be thrown even if the guard
          evaluated without an exception afterwards.

In general, each cog will continue running a task without preempting
it until the task is finished or it reaches a *scheduling point*.
Await statements are scheduling points, as are ``suspend`` statements
and assignment or expression statements containing an await expression
(see :ref:`await-expression`).

.. table:: Syntax
   :align: left
   :class: syntax

   ===============   ==============
   *AwaitStmt* ::=   ``await`` *Guard* ``;``
   *Guard* ::=       [ ``this`` ``.`` ] *SimpleIdentifier* ``?``
   \                 \| *PureExp*
   \                 \| *Guard* ``&`` *Guard*
   \                 \| ``duration`` ``(`` *PureExp* [ ``,`` *PureExp* ] ``)``
   ===============   ==============

Example::

  Fut<Bool> f = x!m();
  await f?; ①
  await this.x == True; ②
  await f? & this.y > 5; ③
  await duration(3, 5); ④

| ① A *claim guard* becomes active when the future is resolved
  (contains a value or an exception).
| ② A *field guard* is a Boolean expression over the object state.
| ③ A *guard conjunction* becomes active when both its components are
  active.
| ④ A *duration guard* becomes active after a certain amount of
  simulated time has passed.  See :ref:`sec:timed-abs` for more on
  timed models.


.. _suspend-stmt:

Unconditional Release: Suspend
==============================

The suspend statement causes the current task to be suspended.

.. table:: Syntax
   :align: left
   :class: syntax

   =================   ===========
   *SuspendStmt* ::=   ``suspend`` ``;``
   =================   ===========


.. note:: There is no guarantee that the cog will choose another task
          to run; the current task might be resumed immediately after
          suspending itself.

.. warning:: It is tempting to misuse the ``suspend`` statement for
             busy-waiting (``while (!condition) suspend;
             doTheThing();``).  Since ABS does not guarantee fair
             scheduling, it is highly recommended to await on the
             condition intead (``await condition; doTheThing();``).


Example::

  suspend;



Return
======

A return statement returns a value from a method.  A return statement
can only appear as the last statement in a method body.

For asynchronous method calls, executing the return statement will
cause the future to be resolved so that it contains a value.  Any
claim guards awaiting the future will become active.

Methods that have a ``Unit`` return type do not need an explicit
return statement.  The future will be resolved when the method
terminates.

.. table:: Syntax
   :align: left
   :class: syntax

   ================   ==============
   *ReturnStmt* ::=   ``return`` *Exp* ``;``
   ================   ==============

Example::

  return x;

.. note:: ABS methods are “single entry single exit”, i.e., do not
          allow exiting from multiple points via multiple ``return``
          statements.  This makes model analysis easier.


.. _throw-statement:

Throw
=====

The throw statement signals an exception (see
:ref:`sec:exception-types`).  It takes a single argument of type
``ABS.StdLib.Exception``, which is the exception value to throw.

.. table:: Syntax
   :align: left
   :class: syntax

   ===============   ==============
   *ThrowStmt* ::=   ``throw`` *PureExp* ``;``
   ===============   ==============

Example::

  throw AssertionFailException;

Note that the throw statement can only be used inside imperative code.
Functional code that cannot return a value in all cases should use the
``Maybe`` datatype::

  def Maybe<Int> f(Int x, Int y) = if (y < 0) then None else Just(x);

But also note that some built-in exceptions, like
``DivisionByZeroException`` and ``PatternMatchFailException`` can
originate from functional code.  See
:ref:`stdlib:predefined-exceptions` for a list of built-in exceptions.


Blocks of Statements
====================

A sequence of statements is called a *block*.  A block introduces a
scope for local variables.

.. table:: Syntax
   :align: left
   :class: syntax

   ===========   =========
   *Block* ::=   ``{`` { *Statement* } ``}``
   ===========   =========

Example::

  {
    Int a = 0; ①
    a = a + 1;
    n = a % 10; ②
  }

  { } ③

| ① The variable ``a`` is in scope until the end of the block.
| ② ``n`` has to exist in an outer block or as a class field.
| ③ An empty block is equivalent to ``skip;``.

.. note:: Semantically, a whole block is a single statement and can be
          written anywhere a single statement is valid.


Conditionals
============

ABS has the standard conditional statement.  The condition has to
evaluate to a Boolean value.

.. table:: Syntax
   :align: left
   :class: syntax

   ============   ===============
   *IfStmt* ::=   ``if`` ``(`` *PureExp* ``)`` *Stmt* [ ``else`` *Stmt* ]
   ============   ===============

Example::

  if (5 < x) {
    y = 6;
  } else {
    y = 7;
  }

  if (True)
    x = 5;


.. _switch-stmt:

Switch: Pattern Matching
========================

The switch statement, like the case expression (see
:ref:`case-expression`), consists of an expression and a series of
branches, each consisting of a pattern and a statement (which can be a
block).

When a switch statement is executed, its input expression is evaluated
and the value matched against the branches until a matching pattern is
found.  The statement in the right-hand side of that branch is then
executed.  Any variable bindings introduced by matching the pattern
are in effect while executing that statement.

If no pattern matches the expression, a ``PatternMatchFailException`` is
thrown.

For a description of the pattern syntax, see :ref:`case-expression`.

.. table:: Syntax
   :align: left
   :class: syntax

   ======================   ==================
   *SwitchStmt* ::=         ``switch`` ``(`` *PureExp* ``)`` ``{`` { *SwitchStmtBranch* } ``}``
   *SwitchStmtBranch* ::=   *Pattern* ``=>`` *Stmt*
   ======================   ==================

Example::

  Pair<Int, Int> p = Pair(2, 3);
  Int x = 0;
  switch (p) {
    Pair(2, y) => { x = y; skip; }
    _ => x = -1;
  }



The While Loop
==============

The while loop repeats its body while the condition evaluates to
``True``.  The condition is re-evaluated after each iteration of the
loop.

.. table:: Syntax
   :align: left
   :class: syntax

   ===============   ===========
   *WhileStmt* ::=   ``while`` ``(`` *PureExp* ``)`` *Stmt*
   ===============   ===========

Example::

   while (x < 5) {
     x = x + 1;
   }


.. _foreach-loop:

The Foreach Loop
================

The foreach loop repeatedly executes its body with a loop value
variable bound to each element of the given list, in sequence.  An
optional loop index variable is bound to the index of the element
bound to the value variable, starting with 0.

The rules for the loop variables follow the rules of local variable
declarations in blocks: the loop variables cannot shadow existing
variables, but can use the same name as an object field.  Loop
variables go out of scope after execution leaves the body of their
foreach loop.

.. table:: Syntax
   :align: left
   :class: syntax

   =================   ==============
   *ForeachStmt* ::=   ``foreach`` ``(`` *Identifier* [ ``,`` *Identifier*] ``in`` *PureExp* ``)`` *Stmt*
   =================   ==============


Example::

  foreach (v in list["a", "b", "c"]) {
    println(`v is bound to $v$`);
  }

  foreach (v, i in list["a", "b", "c"]) {
    println(`Element number $i$ has value $v$`);
  }

.. note:: Also see the second-order functions ``map``, ``filter``,
          ``foldl`` and ``foldr`` in Section :ref:`type-list` for common
          list processing idioms.


.. _try-catch-finally-stmt:

Handling Exceptions with Try-Catch-Finally
==========================================

Executing a statement can result in an exception, either explicitly
signaled using the ``throw`` keyword or implicitly, for example by
dividing by zero.  The try-catch-finally statement is used to handle
exceptions and resume normal execution afterwards.

.. table:: Syntax
   :align: left
   :class: syntax

   =========================   ===============
   *TryCatchFinallyStmt* ::=   ``try`` *Stmt*
   \                           ``catch`` ( ``{`` { *SwitchStmtBranch* } ``}`` | *SwitchStmtBranch* )
   \                           [ ``finally`` *Stmt* ]
   =========================   ===============

The statement protected by ``try`` (which can be a block) is executed
first.  If no exception is thrown, execution continues with the
optional ``finally`` statement, then with the next statement after the
try-catch-finally statement.

If during execution of the statement protected by ``try`` an exception
is thrown, it is matched one-by-one against the exception patterns
defined in the ``catch`` block.  The statement following the first
matching pattern will be executed, as in the switch statement (see
:ref:`switch-stmt`).  Execution continues with the optional
``finally`` statement, then with the statement following the
try-catch-finally statement.

If during execution of the statement protected by ``try`` an exception
is thrown that is not matched by any branch in the ``catch`` block,
the exception is *unhandled*.  In this case, first the optional
``finally`` statement is executed.  If the try-catch-finally was
protected by another try-catch-finally statement, the unhandled
exception is passed on to this surrounding try-catch-finally
statement.  Otherwise, the current process terminates and its future
is resolved by storing the unhandled exception.  Any ``get``
expression on this future will re-throw the exception (see
:ref:`get-expression`).  The object that ran the aborted process will
execute its recovery block with the unhandled exception as parameter
(see :ref:`sec:classes`).

Example::

  try {
      Rat z = 1/x; ①
  } catch {
      DivisionByZeroException => println("We divided by zero"); ②
  } finally {
      println("Leaving the protected area"); ③
  }

| ① If ``x`` is zero, this will throw an exception
| ② Division by zero is handled here; other exceptions will not be
  handled here
| ③ This statement is always executed

As a syntactic convenience, when matching only a single pattern, the
braces around the catch block can be omitted::

  try b = f.get; catch _ => b = False; ①

| ① A “catch-all” exception handler that sets ``b`` to a default value
  in case an unhandled exception was propagated via ``f``

.. note:: The ``finally`` block has the same restrictions as the class
          init and recovery blocks, i.e., it cannot contain processor
          release points (i.e., ``await`` or ``suspend``), blocking
          expressions (i.e., ``get``), or explicitly throw an
          exception via the ``throw`` statement.
