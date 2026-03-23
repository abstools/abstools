*****
Types
*****

ABS has a static, nominal type system.  Local variables, object
fields, function parameters and method parameters are statically
typed.  A type name can refer to a algebraic data type, an interface,
a type synonym.  There are a number of pre-defined data types which
are documented in Chapter :ref:`sec:standard-library`.

A *type name* is a sequence of letters, digits and  underscores (``_``) starting
with an uppercase letter.  In case of a parametric data type, the type name
continues with a left angle (``<``), a list of type names separated by commas
and ends with a right angle (``>``).

.. table:: Syntax
   :align: left
   :class: syntax

   ==========   ===============
   *Type* ::=   *TypeIdentifier* [ ``<`` *Type* { ``,`` *Type* } ``>`` ]
   ==========   ===============

New types are defined as either *interfaces* or *algebraic data
types*.  Algebraic data types can be *parametric*, which is useful for
defining “container-like” data types.  Only algebraic data types can
be parameterized, i.e., ABS does not currently offer class templates.

Note that classes are not types in ABS; object references are typed by
an interface that the object’s class implements.  (Classes that do not
implement any interfaces can be instantiated, and can interact with
the rest of the system via their ``run`` method and constructor
arguments.)

Example::

   String ①
   A_s1mple_type ②
   Map<Int, List<String>> ③

| ① The type name is `String`.  The string type is defined in the standard library.
| ② This is a type name containing underscores and digits.
| ③ This type name denotes a map from integers to lists of strings.


.. _sec:builtin-types:

Built-in Types
==============

ABS offers the following built-in datatypes:

.. table::
   :align: left

   =================   =============================   ===========================   =======================
   Name                Description                     Example                       Documentation
   =================   =============================   ===========================   =======================
   ``Unit``            The empty (void) type           ``Unit``                      :ref:`type-unit`
   ``Bool``            Boolean values                  ``True``, ``False``           :ref:`type-bool`
   ``Int``             Integers of arbitrary size      ``0``, ``-15``                :ref:`type-numbers`
   ``Rat``             Rational numbers                ``1/5``, ``22/58775``         :ref:`type-numbers`
   ``Float``           Floating-point numbers          ``0.5``, ``1.0e-15``          :ref:`type-numbers`
   ``String``          Strings                         ``"Hello world\n"``           :ref:`type-string`
   ``Fut<A>``          Futures                         --                            :ref:`type-future`
   ``Destiny``         Supertype of all futures        --                            :ref:`type-destiny`
   ``Exception``       Exceptions                      ``DivisionByZeroException``   :ref:`sec:exception-types`
   ``List<A>``         Lists of values of type ``A``   ``list[1, 2, 3]``             :ref:`type-list`
   ``Set<A>``          Sets of values of type ``A``    ``set[True, False]``          :ref:`type-set`
   ``Map<A,B>``        Maps from type ``A`` to ``B``   ``map[Pair(1, True)]``        :ref:`type-map`
   ``Pair<A,B>``       Pairs of values                 ``Pair(1, True)``             :ref:`type-pair`
   ``Triple<A,B,C>``   Triples of values               ``Triple(1, "hi", True)``     :ref:`type-triple`
   ``Maybe<A>``        Optional values                 ``Just(True)``, ``Nothing``   :ref:`type-optionals`
   =================   =============================   ===========================   =======================


``Int`` type is a subtype of ``Rat``; this means that ``Int`` values
are assignable to places of type ``Rat``.  Rational values can be
converted to integers via the ``truncate`` function.  ``Float`` is a
distinct numeric type.

.. note::

   Support for floating-point calculations is under development;
   calculations resulting in ``Inf`` or ``NaN`` currently have unspecified
   runtime behavior.

The future type ``Fut<A>`` is a special built-in type that denotes
that an ABS value of type ``A`` will become available in the future.
The value that a future holds and will return can be of any concrete
type.

Example::

   Fut<String> ①
   Fut<List<Rat>> ②

| ① This future will contain a value of type ``String``
| ② This future will contain a list of rational numbers

.. _sec:algebraic-data-types:

Algebraic Data Types
====================

Algebraic Data Types in ABS are used to implement user-defined,
immutable data values.  Because values of algebraic data types are
immutable, they can be safely passed on to other objects and make it
easy to reason about program correctness.

.. table:: Syntax
   :align: left
   :class: syntax

   ==================   ========================
   *DataTypeDecl* ::=   ``data`` *SimpleTypeIdentifier* [ *TypeParams* ]
   \                    [ ``=`` *DataConstr* { ``|`` *DataConstr* } ] ``;``
   *TypeParams* ::=     ``<`` *SimpleTypeIdentifier* { ``,`` *SimpleTypeIdentifier* } ``>``
   *DataConstr* ::=     *SimpleTypeIdentifier* [ ``(`` *Type* [ *SimpleIdentifier* ]
   \                    { ``,`` *Type* [ *SimpleIdentifier* ] } ``)`` ]
   ==================   ========================

Data Type *Constructors* enumerate the possible values of a data type.
Constructors can have zero or more arguments.  Each argument can have
an optional accessor function (see :ref:`sec:accessor-functions`).


Example::

  data IntList = NoInt | ConsInt(Int head, IntList tail); ①
  data Bool = True | False; ②
  data NotInstantiable; ③

| ① The data type `IntList` has two constructors: `NoInt` and
  `ConsInt`.  The second constructor defines two accessor functions.
| ② This is the definition of the built-in data type `Bool`.
| ③ This type does not have constructors and therefore cannot be
  instantiated.

.. _sec:accessor-functions:

Accessor Functions
------------------

Data constructor arguments can optionally have a name, which needs to
be a valid identifier.  If a name is given, it defines a function
that, when passed a value expressed with the given constructor, return
the argument.

The name of an accessor function must be unique in the module it is
defined in, it is an error to have a function definition with the same
name as an accessor function.  Two constructors for the same data type
can have an accessor function with the same name if they return the
same type.

Example::

  data Test = Test1(String s, Int i) ①
            | Test2(String s, Bool b);

  data Person = Person(String name, Int age);
  {
    Person john = Person("John", 34);
    Int age = age(john); ②
  }

| ① Both constructors of ``Test`` define an accessor function ``s``,
  which is ok since ``s`` returns a string in all cases.
| ② The call to `age` returns 34.


Parametric Data Types
---------------------

Algebraic data types can carry *type parameters*.  Data types with
type parameters are called *parametric data types*.

Parametric Data Types are useful to define “container” data types,
such as lists, sets or maps. Parametric data types are declared like
normal data types but have an additional type parameter section inside
broken brackets (``<`` ``>``) after the data type name.

Example::

  data List<A> = Nil | Cons(A, List<A>);

When using a parametric data type, concrete types are given for the type parameters.

Example::

  List<Int> l = Cons(1, Cons(2, Nil));

.. _sec:n_ary-constructors:

N-ary Constructors
------------------

Literal values of recursive data types like lists and sets can be
arbitrarily long, and nested constructor expressions can become
unwieldy.  ABS provides a special syntax for n-ary constructors, which
are transformed into constructor expressions via a user-supplied
function.

Example::

  def Set<A> set<A>(List<A> l) = ①
      case l {
         Nil => EmptySet
         | Cons(x,xs) => insertElement(set(xs), x)
      };

  {
    Set<Int> s1 = set(Cons(1, Cons(2, Cons(3, Nil)))); ②
    Set<Int> s = set[1, 2, 3]; ③
  }

| ① The parametric function `set` is defined to take a list of
  elements and return a set.
| ② `set` is called with a literal list constructed as normal.
| ③ `set` is called with the special n-ary constructor syntax.  The
  two calls return the same value.

The constructor function usually has the same name as the type it is
constructing.  For example, a value of type `Set` is constructed via
the function `set`.


Fully Abstract Data Types
-------------------------

Using the module system it is possible to define abstract data types.
For an abstract data type, only the functions that operate on them are
known to the client, but not its constructors.  This can be
implemented in ABS by putting such a data type in its own module and
only exporting the data type and its functions, without exporting the
constructors.


Interface Types
===============

Interfaces in ABS describe the functionality of objects.  Thus,
Interfaces in ABS are similar to interfaces in Java.  Unlike Java,
object references are typed by interfaces and not by their class.  See
:ref:`typecheck-expression` and :ref:`typecast-expression` for how to obtain
a reference to the same object via a different interface.

The syntax of interfaces is given in :ref:`sec:interfaces`.


.. _sec:exception-types:

Exceptions
==========

In higher-level programming languages, exceptions are generally used to signal
an *erroneous* or *abnormal* runtime behavior of the program, that should be
treated (handled) separately compared to normal values.

Exceptions are declared with the keyword ``exception``, followed by
the name of an exception and an optional list of parameters.  The
semantics are the same as for defining data constructors; naming a
parameter will create an accessor function (see
:ref:`sec:accessor-functions`).

.. table:: Syntax
   :align: left
   :class: syntax

   ===================   ========================
   *ExceptionDecl* ::=   ``exception`` *SimpleTypeIdentifier*
   \                     [ ( *Type* [ *SimpleIdentifier* ] { ``,`` *Type* [ *SimpleIdentifier* ] } ) ] ``;``
   ===================   ========================


Like any other constructor or datatype name, exceptions always start with an
upper-case letter.

Exceptions are of type ``ABS.StdLib.Exception``, which is pre-defined in the
standard library.  It is possible to store exception values in variables of
type ``Exception``.

Example::

  exception MyException;
  exception MyOtherException(String param, Int); // no accessor for second param

In ABS, exceptions are first-class values; the user can construct
exception-values, assign them to variables, pass them in expressions, etc.  An
exception can be thrown via the ``throw`` statement (see :ref:`throw-statement`)
and be caught in a ``catch`` block (see :ref:`try-catch-finally-stmt`).
Additionally, the object itself can recover its invariant after an uncaught
exception in a process via its recovery block (see :ref:`sec:classes`).



Type Synonyms
=============

A *Type Synonym* is an alternative type name for a type.  Type synonyms are
introduced with the keyword ``type``.  Parametric type synonyms are not
currently supported.

.. table:: Syntax
   :align: left
   :class: syntax

   =================   ==================
   *TypeSynDecl* ::=   ``type`` *SimpleTypeIdentifier* ``=`` *Type* ``;``
   =================   ==================

Example::

  type Filename = String;
  type Filenames = Set<Filename>;
  type Servername = String;
  type Packet = String;
  type File = List<Packet>;
  type Catalog = List<Pair<Servername,Filenames>>;


Location Types
==============

A *Location Type* is an interface type annotated with a location.  The
location is seen relative to the current context, and can be one of
the following:

.. table::
   :align: left

   =============   =====================
   Name            Description
   =============   =====================
   ``Far``         The object is on a different cog
   ``Near``        The object is on the same cog
   ``Somewhere``   The object is either ``Near`` or ``Far``
   ``Infer``       Have the type-checker infer the location
   =============   =====================

When location type checking is active, the following example is
erroneous, and will result in a type error:

Example::

  interface I { }
  class C {
      [Far] I m([Near] I o) {
          return o; ①
      }
      [Somewhere] I m2([Near] I o) {
          return o; ②
      }
  }

| ① Here, ``o`` is of type ``[Near] I``, which cannot be converted to
  the return type ``[Far] I``.
| ② Here, ``o``’s type ``[Near] I`` can be converted to ``[Somewhere]
  I``.
