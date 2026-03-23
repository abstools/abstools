***********
Expressions
***********

This chapter specifies all ABS expressions.  Expressions are usually evaluated
for their value, although they can be used inside annotations as purely
syntactic constructs that do not need to follow standard evaluation rules.

ABS expressions can either be pure or have side effects. *Pure expressions*
only refer to entities in the functional and expression layer.  They can be
evaluated multiple times without influencing the execution of the imperative
and object oriented layers of ABS.  Pure expressions can be sub-expressions of
other expressions and can be used as function bodies.  *Side-effect
expressions*, on the other hand, refer to semantic entities “above” the
functional layer (objects and futures).  They are only legal “stand-alone”
(i.e., not as a sub-expression of another expression) and in certain places,
mostly in the right-hand side of an assignment.  This strict separation of
pure and side-effect expressions simplifies the language, for
example wrt. using expressions inside annotations and reasoning about
expressions in static analysis tools.


.. table:: Syntax
   :align: left
   :class: syntax

   =============   =================
   *Exp* ::=       *PureExp* | *EffExp*
   *PureExp* ::=   *SimpleIdentifier*
   \               \| ``this`` ``.`` *SimpleIdentifier*
   \               \| ``this``
   \               \| ``destiny``
   \               \| *Literal*
   \               \| *TemplateString*
   \               \| *LetExp*
   \               \| *DataConstrExp*
   \               \| *FnAppExp*
   \               \| *FnAppListExp*
   \               \| *ParFnAppExp*
   \               \| *WhenExp*
   \               \| *CaseExp*
   \               \| *OperatorExp*
   \               \| *TypeCheckExp*
   \               \| *TypeCastExp*
   \               \| ``(`` *PureExp* ``)``
   *EffExp* ::=    *NewExp*
   \               \| *SyncCall*
   \               \| *AsyncCall*
   \               \| *GetExp*
   \               \| *AwaitExp*
   =============   =================


Literals
========

Literals, as defined in :ref:`sec:literals`, are expressions.

Template Strings
================

Template strings are strings allowing embedded expressions.  In
contrast to normal string literals, template strings can contain
linebreaks and other special characters.

A template string starts and ends with a backtick (`````) character.
A template string can contain zero or more pure expressions enclosed
by dollar signs (``$``).  These expressions will be replaced by their
string representation (as with the function ``toString``) every time
the template string is evaluated.

The only characters that need to be escaped by a backslash (``\``) in
a template string are the backtick itself (`````) and the dollar sign
(``$``).  All other characters, including line breaks and the
backslash itself, do not need to be specially treated when writing a
template string.

.. table:: Syntax
   :align: left
   :class: syntax

   ====================   ===========================
   *TemplateString* ::=   ````` { ? *Valid String Character* ? }
   \                      { ``$`` *PureExp* ``$`` { ? *Valid String Character* ? } } `````
   ====================   ===========================

Example::

  `Hello $name$!
  The price is \$$price$.`

(Note that the template string spans two lines.)  The result of this
expression will be a string with two lines, and with the values of the
local variables `name` and `price` in place.

.. _sec:operator-expressions:

Operator Expressions
====================

ABS has a range of unary and binary operators working on pre-defined
datatypes.  All operators are pure (side effect-free).

.. table:: Syntax
   :align: left
   :class: syntax

   =================   ====================
   *OperatorExp* ::=   *UnaryExp* | *BinaryExp*
   *UnaryExp* ::=      *UnaryOp* *PureExp*
   *UnaryOp* ::=       ``!`` | ``-``
   *BinaryExp* ::=     *PureExp* *BinaryOp* *PureExp*
   *BinaryOp* ::=      ``||`` | ``&&`` | ``==`` | ``!=`` | ``<`` | ``\<=`` | ``>`` | ``>=`` | ``+`` | ``-`` | ``*`` | ``/`` | ``%``
   =================   ====================


The following table describes the meaning as well as the associativity
and the precedence of the different operators. The list is sorted from
low precedence to high precedence.

.. table:: ABS Operators

   =============   ================   =============   ==============   ===========
   Expression      Meaning            Associativity   Argument types   Result type
   =============   ================   =============   ==============   ===========
   ``e1 || e2``    logical or         left            ``Bool``         ``Bool``
   ``e1 && e2``    logical and        left            ``Bool``         ``Bool``
   ``e1 == e2``    equality           left            compatible       ``Bool``
   ``e1 != e2``    inequality         left            compatible       ``Bool``
   ``e1 < e2``     less than          left            compatible       ``Bool``
   ``e1 \<= e2``   less or equal      left            compatible       ``Bool``
   ``e1 > e2``     greater than       left            compatible       ``Bool``
   ``e1 >= e2``    greater or equal   left            compatible       ``Bool``
   ``e1 + e2``     concatenation      left            ``String``       ``String``
   ``e1 + e2``     addition           left            number           number
   ``e1 - e2``     subtraction        left            number           number
   ``e1 * e2``     multiplication     left            number           number
   ``e1 / e2``     division           left            number           ``Rat`` or ``Float``
   ``e1 % e2``     modulo             left            number           number
   ``!e``          logical negation   right           ``Bool``         ``Bool``
   ``-e``          integer negation   right           number           number
   =============   ================   =============   ==============   ===========


Semantics of Comparison Operators
---------------------------------

ABS has generic equality and less-than comparison between two values
of the same type.

Equality and inequality comparison is standard: by value for functional
datatypes and by reference for objects and futures.  I.e., two strings
``"Hello"`` compare as identical via ``==``, as do two sets containing identical
values.  Two references to objects or futures compare as identical via ``==`` if
they point to the same object or future.  The inequality operator ``!=``
evaluates to ``True`` for any two values that compare to ``False`` under ``==`` and
vice versa.

For the comparison operator ``<``, an ordering is defined in the following way.

- Numbers compare as usual.

- Strings compare lexicographically.

- Algebraic datatypes (including Boolean) compare first by constructor
  name, then by comparing constructor arguments left to right.

  Example::

    Cons(_, _) < Nil
    Cons(1, _) < Cons(2, _)

- Objects and futures are compared by identity, in an
  implementation-specific but stable way.  This means that for any two
  variables ``a`` and ``b`` that point to different objects, the value of
  ``a < b`` does not change as long as ``a`` and ``b`` are not
  re-assigned.footnote:[This ordering is not guaranteed to be stable
  between two invocations of a program.  If ABS ever develops object
  serialization, care must be taken to uphold any datatype invariants
  across program invocations, e.g., when reading back an ordered list
  of objects.]  The value ``null`` compares less than any other object
  reference.


Let
===

The expression ``let T v = p in b`` evaluates ``b``, with ``v`` bound
to the value of evaluating the expression ``p``.  The binding of ``v``
introduced by the let-expression can shadow a binding of ``v`` outside
of the ``let`` expression.

More than one binding can be established in one let-expression.
Bindings are evaluated sequentially, i.e., later bindings can use
earlier variables in their value expression.

.. table:: Syntax
   :align: left
   :class: syntax

   ============   =======================
   *LetExp* ::=   ``let`` *Type* *SimpleIdentifier* ``=`` *PureExp*
   \              { ``,`` *Type* *SimpleIdentifier* ``=`` *PureExp* }
   \              ``in`` *PureExp*
   ============   =======================

Example::

  let Int x = 2 + 2, Int y = x + 1 in y * 2 ①

| ① The value of this expression is 10 (``((2 + 2) + 1) * 2``)


Data Constructor Expressions
============================

Data Constructor Expressions are expressions that create data values
by applying arguments to data type constructors.  Data constructor
expressions look similar to function calls, but data constructors
always start with an upper-case letter.

For data type constructors without parameters, the parentheses are
optional.

.. table:: Syntax
   :align: left
   :class: syntax

   ===================   ===================
   *DataConstrExp* ::=   *TypeIdentifier* [ ``(`` [ *PureExp* { ``,`` *PureExp* } ] ``)`` ]
   ===================   ===================


Example::

  True
  Cons(True, Nil)
  Nil

Defining new data types and their constructors is described in
:ref:`sec:algebraic-data-types`.


Function Calls
==============

Function calls apply arguments to functions, producing a value.
Function call expressions look similar to data constructor
expressions, but function names always start with a lower-case letter.
The parentheses are mandatory in function calls.

.. table:: Syntax
   :align: left
   :class: syntax

   ==============   ================
   *FnAppExp* ::=   *Identifier* ``(`` [ *PureExp* { ``,`` *PureExp* } ] ``)``
   ==============   ================

Example::

  tail(Cons(True, Nil))
  head(list)


N-ary Function Calls
--------------------

Calls to n-ary Constructors (see :ref:`sec:n_ary-constructors`) are
written with brackets (``[]``) instead of parentheses (``()``).

.. table:: Syntax
   :align: left
   :class: syntax

   ==================   =================
   *FnAppListExp* ::=   *Identifier* ``[`` [ *PureExp* { ``,`` *PureExp* } ] ``]``
   ==================   =================

Example::

  set[1, 2, 3]
  map[Pair(1, True), Pair(2, False)]


Partially-Defined-Function Calls
================================

Calls to partially defined functions (see
:ref:`sec:partially-defined-functions`) are similar to function call
expressions, but have an additional prepended set of function
arguments.

.. table:: Syntax
   :align: left
   :class: syntax

   =======================   ============================
   *ParFnAppExp* ::=         *Identifier*
   \                         ``(`` *ParFnAppParam* { ``,`` *ParFnAppParam* } ``)``
   \                         ``(`` [ *PureExp* { ``,`` *PureExp* } ] ``)``
   *ParFnAppParam* ::=       *Identifier* | *AnonymousFunction*
   *AnonymousFunction* ::=   ``(`` [ *Type* *SimpleIdentifier* { ``,`` *Type* *SimpleIdentifier*  } ]  ``)``
   \                         ``=>`` *PureExp*
   =======================   ============================


Example::

  map(toString)(list[1, 2]) ①
  filter((Int i) => i > 0)(list[0, 1, 2]) ②

| ① evalutes to ``list["1", "2"]``
| ② evalutes to ``list[1, 2]``


Conditional Expressions
=======================

The value of the conditional expression ``when c then e1 else e2`` is either the
value of ``e1`` or the value of ``e2``, depending on the value of ``c``, which must
be of type ``Bool``.  Depending on the value of ``c``, either ``e1`` or ``e2`` is
evaluated, but not both.

.. table:: Syntax
   :align: left
   :class: syntax

   =============   ===================
   *WhenExp* ::=   ``when`` *PureExp* ``then`` *PureExp* ``else`` *PureExp*
   =============   ===================

Example::

  when 5 == 4 then "Hmm" else "ok"


.. _case-expression:

Case
====

ABS supports pattern matching via the Case Expression.  A case
expression consists of an input expression and one or more branches,
each consisting of a pattern and a right hand side expression.

The case expression evaluates its input expression and attempts to
match the resulting value against the branches until a matching
pattern is found.  The value of the case expression itself is the
value of the expression on the right-hand side of the first matching
pattern.

If no pattern matches the expression, a ``PatternMatchFailException`` is thrown.

There are four different kinds of patterns available in ABS:

* Variables (with different semantics depending on whether the
  variable is bound or not)
* Literal Patterns (e.g., ``5``)
* Data Constructor Patterns (e.g., ``Cons(Nil,x)``)
* Underscore Pattern (``_``)

.. table:: Syntax
   :align: left
   :class: syntax

   ===================   =============================
   *CaseExp* ::=          ``case`` *PureExp* ``{`` *CaseExpBranch* { ``|`` *CaseExpBranch* } ``}``
   *CaseExpBranch* ::=    *Pattern* ``=>`` *PureExp*
   *Pattern* ::=          ``_``
   \                      \| *SimpleIdentifier*
   \                      \| *Literal*
   \                      \| *ConstrPattern*
   *ConstrPattern* ::=    *TypeIdentifier* [ ``(`` [ *Pattern* { ``,`` *Pattern* }  ] ``)`` ]
   ===================   =============================


Variable Patterns
-----------------

Variable patterns are written as identifiers starting with a
lower-case letter.  If the identifier does not name a variable in the
current scope, the variable pattern matches any value and introduces a
binding of the given identifier to the matched value for the
right-hand side of the branch and the rest of the pattern itself.  If,
on the other hand, a variable with the given name is already in scope,
its value is compared to the value being matched against.

The variable being named by the variable pattern can be used in the
right-hand-side expression of the corresponding branch.  Typically,
pattern variables are used inside of data constructor patterns to
extract values from data constructors.  For example:


Example::

  let (Pair<Int, Int> a) = Pair(5, 5) in
    case a {
      Pair(x, x) => x ①
      | Pair(x, y) => y ②
    } ③

| ① This branch matches a pair with identical values.
| ② This branch matches every pair.  Since pairs with identical values
  are matched by the previous branch, `x` and `y` will be different.
| ③ The value of the whole expression is 5, produced by the first
  branch.

Example::

  let (x = 7) in
    case Pair(5, 5) {
      Pair(x, x) => x ①
      | Pair(x, y) => y ②
      | Pair(y, z) => z ③
    } ④

| ① This pattern does not match since `x` is bound to 7 and does not
  match 5.
| ② This pattern does not match either, for the same reason.
| ③ This pattern contains only unbound variable patterns and therefore
  matches.
| ④ The value of the whole expression is 5, produced by the third
  branch.



Literal Patterns
----------------

Literals can be used as patterns.  The pattern matches if the value of
the case expression is equal to the literal value.

Example::

  let (Pair<Int, Int> a) = Pair(5, 5) in
    case a {
      Pair(3, x) => x ①
      | Pair(x, y) => y ②
    } ③

| ① The pattern `3` does not match the value in the first position of
  the `Pair` constructor pattern.
| ② This pattern matches.
| ③ The value of the whole expression is 5, produced by the second
  branch.


Data Constructor Patterns
-------------------------

A data constructor pattern is written like a standard data constructor
expression.  Constructor arguments are again patterns.

Example::

  let (List<Int> l) = list[1, 2, 3] in
    case l {
      Nil => 0 ①
      | Cons(1, _) => 15 ②
      | Cons(_, Cons(y, _)) => y ③
    } ④

| ① This pattern matches the empty list.
| ② This pattern matches a list starting with the literal `1`.
| ③ This pattern matches a list of at least length 2, and binds the
  second element to `y`.
| ④ The value of the whole expression is 15, produced by the second
  branch.



The Wildcard Pattern
--------------------

The wildcard pattern, written with an underscore (``_``) matches any
value.

Example::

  let (List<Int> l) = list[1, 2, 3] in
    case l {
      Nil => True ①
      | _ => False ②
  }; ③

| ① This pattern matches the empty list.
| ② This pattern matches anything.
| ③ The value of the whole expression is `False`, produced by the
  second branch.

The wildcard pattern can be used as the last pattern in a case
expression to define a default case.


Typing of Case Expressions
--------------------------

A case expression is type-correct if and only if all its expressions
and all its branches are type-correct and the right-hand side of all
branches have a common supertype.  This common supertype is also the
type of the overall case expression.  A branch (a pattern and its
expression) is type-correct if its pattern and its right-hand side
expression are type-correct.  A pattern is type-correct if it can
match the corresponding case expression.



.. _typecheck-expression:

Type-Check Expressions
======================

Variables pointing to objects are typed by interface, which means that
the concrete class of the referenced object might support more methods
than can be called through the reference.  The type-check expression
checks if an object implements a given interface.

.. table:: Syntax
   :align: left
   :class: syntax

   ==================   ================
   *TypeCheckExp* ::=   *PureExp* ``implements`` *TypeIdentifier*
   ==================   ================

Example::

  interface I {}
  interface J {}
  class C implements I, J {}
  {
    I o = new C();
    if (o implements J) { ①
      println("o is a J");
    }
  }

| ① this evaluates to ``True``, since ``C`` implements both ``I`` and
  ``J`` even though they are distinct types.


.. _typecast-expression:

Type-Cast Expressions
=====================

Variables pointing to objects are typed by interface, which means that
the concrete class of the referenced object might support more methods
than can be called through the reference.  The type-cast expression
returns a reference of type ``I`` to the same object if it implements
the given interface ``I``, or ``null`` otherwise.

.. table:: Syntax
   :align: left
   :class: syntax

   =================   ==================
   *TypeCastExp* ::=   *PureExp* ``as`` *TypeIdentifier*
   =================   ==================

Example::

  interface I {}
  interface J {}
  class C implements I, J {}
  class D implements I {}
  {
    I o = new C();
    J j = o as J; ①
    I o2 = new D();
    J j2 = o2 as J; ②
  }

| ① ``j`` will be non-``null`` since ``C`` implements ``J``.
| ② ``j2`` will be ``null`` since ``D`` does not implement ``J``.


New
===

A ``new`` expression creates a new object from a class name and a list
of arguments.  In ABS, objects can be created in two different ways.
Either they are created in the current COG, using the ``new local``
expression, or they are created in a new COG by using the ``new``
expression (see :ref:`sec:concurrency-model` for more details about
cogs).

.. note:: This expression is a side-effect expression and cannot be
          used as a sub-expression.

.. table:: Syntax
   :align: left
   :class: syntax

   ============   ======================
   *NewExp* ::=   ``new`` [ ``local`` ] *TypeIdentifier* ``(`` [ *PureExp* {``,`` *PureExp* } ] ``)``
   ============   ======================

Example::

  new local Foo(5)
  new Bar()

Classes can declare an *init block* (see :ref:`sec:classes`), which is
executed for each new instance.  The semantics of the ``new``
expression guarantee that the init block is fully executed before the
new object begins receiving method calls.  Classes can also declare a
``run`` method, which is asynchronously invoked after the init block
and subject to the normal scheduling rules for processes.


Synchronous Method Calls
========================

A synchronous call consists of a target expression evaluating to an
interface type, a method name declared in that interface, and a list
of argument expressions.

.. note:: This expression is a side-effect expression and cannot be
          used as a sub-expression.

.. table:: Syntax
   :align: left
   :class: syntax

   ==============   ==================
   *SyncCall* ::=   *PureExp* ``.`` *SimpleIdentifier* ``(`` [ *PureExp* { ``,`` *PureExp* } ] ``)``
   ==============   ==================

Example::

  Bool b = x.m(5, 3);

The semantics of the synchronous method call differ depending on
whether the caller and callee are in the same cog.  A synchronous
method call between objects in the same cog has Java-like semantics,
i.e., the caller is suspended and the called method starts executing
immediately.  When the called method finishes, the caller process is
scheduled and resumes execution.

However, if caller and called object are in different cogs, a
synchronous method call is equivalent to an asynchronous method call
immediately followed by a ``get`` expression on the resulting future.
This means that the intuitive semantics of synchronous method calls
are preserved, but introduces the possibility of deadlocks in case the
callee tries to call back to an object of the caller cog.


.. _async-call-expression:

Asynchronous Method Calls
=========================

An asynchronous call consists of a target expression evaluating to an
interface type, a method name declared in that interface, and a list
of argument expressions.

.. note:: This expression is a side-effect expression and cannot be
          used as a sub-expression.

.. table:: Syntax
   :align: left
   :class: syntax

   ===============   ==============
   *AsyncCall* ::=   *PureExp* ``!`` *SimpleIdentifier* ``(`` [ *PureExp* { ``,`` *PureExp* } ] ``)``
   ===============   ==============

An asynchronous method call creates a new task in the COG that
contains the target.  This means that the caller task proceeds
independently and in parallel with the callee task, without waiting
for the result.  The result of evaluating an asynchronous method call
expression ``o!m(e)`` is a _future_ of type (``Fut<V>``), where ``V``
is the return type of the callee method ``m``.

This future is resolved (i.e., it gets a value) when the callee task
finishes.  It can be used to synchronize with the callee task and
obtain the result of the method call.

Example::

  Fut<Bool> f = x!m(5);


.. _get-expression:

Get
===

A get expression is used to obtain the value from a future.  The
current task is blocked until the future has been resolved, i.e.,
until either the return value is available or an exception has
occurred in the callee task.  No other task in the COG can be
activated while the current task is blocked by a get expression.

.. note:: This expression is a side-effect expression and cannot be
          used as a sub-expression.

.. table:: Syntax
   :align: left
   :class: syntax

   ============   ===========
   *GetExp* ::=   *PureExp* ``.`` ``get``
   ============   ===========

Example::

  Bool b = f.get;

If the future contains a normal return value, the value of the get
expression is that value.  If the future contains an exception thrown
by the callee process, evaluating the get expression will throw the
same exception.  The value thrown by a get expression can be caught by
try-catch as normal (see :ref:`try-catch-finally-stmt`).

The following example assigns the return value contained in ``f`` to the
variable ``b``.  In case of any error, ``b`` is assigned ``False``.

::

  try b = f.get; catch { _ => b = False; }


.. _await-expression:

Await (Expression)
==================

An await expression is a way to asynchronously call a method, wait for
the callee to finish, and optionally get the result in one expression.

.. note:: This expression is a side-effect expression and cannot be
          used as a sub-expression.

.. table:: Syntax
   :align: left
   :class: syntax

   ==============   ===============
   *AwaitExp* ::=   ``await`` *AsyncCall*
   ==============   ===============

Example::

  A x = await o!m();

The statement above is equivalent to these three statements::

  Fut<A> fx = o!m();
  await fx?;
  A x = fx.get;


Exception Propagation and the Await Expression
----------------------------------------------

As explained in Section :ref:`get-expression`, exceptions propagate from
callee to caller via the ``get`` expression.  An ``await`` statement
will proceed once the callee process has finished, but an exception in
the future will not be raised when executing the ``await`` statement.
To align the ``await`` expression with that behavior, an exception
will only be raised when the return value of a method call is used,
e.g., by assigning it to a variable. Hence the following line of code
will not raise an error even if the call to ``o!m()`` results in an
exception::

  await o!m();

Since the return value is ignored in the statement above, it is
equivalent to these two statements (note the absence of a ``get``
expression`)::

  Fut<A> fx = o!m();
  await fx?;


.. _destiny-expression:

Destiny (Expression)
====================

.. note:: This feature is only available in the Erlang backend.

The ``destiny`` expression returns the future of the asynchronous call
that is currently being executed.  It returns a value of type
``Destiny``.  For more information on how ``Destiny`` values can be
used, see :ref:`type-destiny`.

.. table:: Syntax
   :align: left
   :class: syntax

   ================   ===========
   _DestinyExp_ ::=   ``destiny``
   ================   ===========


Example::

  class C {
      Destiny myMethod() {
          return destiny;
      }

      Unit run() {
          Fut<Destiny> f = this!myMethod();
          await f?;
          Destiny g = f.get;
          assert(f == g); ①
      }
  }

| ① the expression will evaluate to ``True`` since the returned
  ``destiny`` value is the same as the future of the asynchronous call
  to ``myMethod``.

If ``destiny`` is evaluated as part of a synchronous call S then it evaluates to
the future of the task that is executing S:

Example::

  class C {
      Unit syn(Destiny caller) {
          assert(destiny == caller);
      }

      Unit asyn() {
          this.syn(destiny);
      }

      Unit run() {
          this!asyn();
      }
  }

.. note:: There is also a ``destinyOf`` built-in function that can
          retrieve the future of the asynchronous call that is
          currently being executed by a ``Process``, see
          :ref:`sec:process-attributes`.
