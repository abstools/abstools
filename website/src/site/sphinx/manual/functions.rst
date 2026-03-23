********************
Function Definitions
********************

Functions take a list of arguments and evaluate the expression in
their body, producing a return value.  ABS functions are always pure.
This means the body of a function can use all pure expressions but no
expressions with side effects.


Functions can be *parametric*, which means that they can take and return
parametric datatypes.  This means that a function ``head`` defined over a
parametric list datatype can return the first element of a list, regardless of
its type.  Parametric functions are defined like normal functions but have an
additional type parameter section inside angle brackets (``<`` ``>``) after the
function name.

.. table:: Syntax
   :align: left
   :class: syntax

   ===================   ===========================
   *FunctionDecl*  ::=   ``def`` *Type* *SimpleIdentifier*
   \                     [ ``<`` *SimpleTypeIdentifier* { ``,`` *SimpleTypeIdentifier* } ``>`` ]
   \                     ``(`` [ *Type* *SimpleIdentifier* { ``,`` *Type* *SimpleIdentifier*  } ]  ``)``
   \                     ``=`` *PureExp* ``;``
   ===================   ===========================


Example::

  def Rat abs(Rat x) = if x > 0 then x else -x; ①

  def Int length<A>(List<A> list) = ②
  case list {
    Nil => 0
    | Cons(_, ls) => 1 + length(ls)
  };

  def A head<A>(List<A> list) = ③
    case list { Cons(x, _) => x };

| ① The `abs` function returns the absolute value of its argument.
| ② This parametric function takes lists with arbitrary values and
  returns an Integer.
| ③ This parametric function returns the same type that is contained
  in the list.  (Note that `head` is a partial function which is not
  defined for empty lists.)


.. _sec:partially-defined-functions:

Partial Function Definitions
============================

For reasons of simplicity and analyzability, ABS does not offer
higher-order functions.  On the other hand, many common patterns of
functional programming are extremely useful, for example the
well-known ``map``, ``filter`` and ``fold`` higher-order functions.
For this reason, ABS supports *partial function definitions*.

Partial function definitions are function definitions taking an
additional set of parameters.  These additional parameters can be
either names of normal functions, or anonymous functions (see
:ref:`sec:anonymous-functions`).  Partial function definitions define a
set of functions which only differ in function applications but share
overall structure.  Put another way, partial function definitions
define second-order functions -- functions that take first-order
functions as arguments.  Partially defined functions can be used
inside functional code, but cannot be passed as parameters to other
partial functions.

A partially defined function is called the same way as a normal
function, with a separate, non-empty argument list containing the
functional arguments.  For recursion inside the body of a partially
defined function, omit the function parameter list.

.. table:: Syntax
   :align: left
   :class: syntax

   ==========================   ===================
   *PartialFunctionDecl*  ::=   ``def`` *Type* *SimpleIdentifier*
   \                            [ ``<`` *SimpleTypeIdentifier* { ``,`` *SimpleTypeIdentifier* } ``>`` ]
   \                            ``(`` *SimpleIdentifier* { ``,`` *SimpleIdentifier*  } ``)``
   \                            ``(`` [ *Type* *SimpleIdentifier* { ``,`` *Type* *SimpleIdentifier*  } ]  ``)``
   \                            ``=`` *PureExp* ``;``
   ==========================   ===================

Example::

  // Apply a function fn A -> B to a value A
  def B apply<A, B>(fn)(A value) = fn(value);

  def Int double(Int x) = x * 2;

  {
    Int doubled = apply(double)(2); ①
  }

| ① ``doubled`` will have the value four.

Example::

  def List<B> map<A, B>(f)(List<A> list) = case list { ①
      Nil => Nil
      | Cons(x, xs) => Cons(f(x), map(xs)) ②
  };

  def Int double(Int x) = x * 2;

  {
    // doubled = [2, 4, 6]
    List<Int> doubled = map(double)(list[1, 2, 3]);
  }

| ① This definition of `map` is contained in the standard library.
| ② Note the recursive call to `map` omits the function parameter
  list.

.. note:: For each call of a partial function, a normal function
          definition is generated at compile time by replacing the
          functional parameters syntactically by the functions passed
          in the additional parameter list.  This is done before type
          checking and after delta and trait flattening -- any type
          mismatch and similar errors are caught afterwards during
          type checking.  If multiple statements call a partially
          defined function with the same function-name arguments, only
          one expansion is generated.


.. _sec:anonymous-functions:

Anonymous Functions
===================

To reduce the need to declare a function with a new function name
explicitly every time a partially defined function is called, ABS uses
anonymous functions.  Anonymous functions are only allowed in the
first arguments list calls to partially defined functions.

.. table:: Syntax
   :align: left
   :class: syntax

   ========================   =================
   *AnonymousFunction*  ::=   ``(`` [ *Type* *SimpleIdentifier* { ``,`` *Type* *SimpleIdentifier*  } ]  ``)``
   \                          ``=>`` *PureExp*
   ========================   =================

An anonymous function specifies a number of parameters and an expression that
may refer to the declared parameters.

The following example is equivalent to the previous example, but does not
define the ``double`` function explicitly::

  {
    List<Int> list = list[1, 2, 3];
    list = map((Int y) => y * 2)(list); ①
  }

| ① ``list`` will have the value ``list[2, 4, 6]``

Anonymous functions can refer to variables and fields accessible in
the context of the partial function call::

  {
    Int factor = 5;
    List<Int> list = list[1, 2, 3];
    list = map((Int y) => y * factor)(list); ①
  }

| ① ``list`` will have the value ``list[5, 10, 15]``


.. note:: Anonymous functions are inlined into the expansion of the
          partial function definition.  Errors caused by wrong typing
          are caught after the expansion during the type checking of
          core ABS, but the expanded function definition has an
          annotation referring to the statement that caused the
          expansion, hence error reporting will be accurate wrt. the
          original source code.


Built-In Functions
==================

Some special "functions" cannot be defined with pure expressions, for
example the function ``println``.  The definition of such functions is
done via a special function body written as ``builtin``, with optional
pure expression arguments ``builtin(a, "b", 3)``.  Such builtin
functions have to be defined separately in the code generator for each
backend where the model is compiled.

Built-in functions in the standard library include:

- The functions ``sqrt``, ``log``, ``exp`` that work on floating-point
  numbers
- Functions that convert between different numerical types:
  ``truncate``, ``float``, ``rat``, ``floor``, ``ceil``,
  ``numerator``, ``denominator``
- String functions: ``substr``, ``strlen``, ``toString``, ``println``
- Clock access: ``currentms``, ``ms_since_model_start``
- Functions that return process attributes (see Section
  :ref:`sec:process-attributes`)
- The ``random`` function


Embedded SQLite Database Queries
================================

The Erlang and Java backends can read from a relational database and
convert the result into ABS lists.  Currently only `SQLite
<https://sqlite.org>`__ databases are supported.

A SQLite database is queried by writing a function with a ``builtin``
body with three or more arguments: a literal ``sqlite3``, the name of
the database file, the query as a SQL string, and zero or more
arguments to the query.  The return value is a list of ABS values.
The return type of such a function can be:

- ``List<Int>``, when the query returns rows with a single SQLite
  ``INTEGER`` value;
- ``List<Float>``, when the query returns rows with a single SQLite
  ``INTEGER`` or ``REAL`` value;
- ``List<Rat>``, when the query returns rows with a single SQLite
  ``INTEGER`` or ``REAL`` value;
- ``List<Bool>``, when the query returns rows with a single SQLite
  ``INTEGER`` value, where ``0`` corresponds to ABS ``False``;
- ``List<String>``, when the query returns rows with a single SQLite
  ``TEXT`` value;
- A list of an algebraic datatype, with a single constructor taking
  only the above datatypes, such that the constructor can be invoked
  for each row returned by the query.

Query parameters are written as ``?`` in the SQL query string.  Each
of these parameters must be supplied with a value.  ABS query
parameters are converted into SQL values (see
https://www.sqlite.org/datatype3.html) as follows:

- ``Int`` values are converted into ``INTEGER``;
- ``Float`` values are converted into ``REAL``;
- ``Rat`` values are converted into floating-point numbers before
  passing them to the query function;
- ``Bool`` values are converted into ``0`` and ``1`` (note that SQLite
  treats all non-zero values as true; consider directly using a query
  parameter of type ``Int`` instead);
- ``String`` values are converted into ``STRING``.

Example: creating a database from the command line::

  $ sqlite3 /tmp/test.sqlite3
  CREATE TABLE IF NOT EXISTS test_table (
    int_value INTEGER,
    float_value REAL,
    string_value TEXT,
    bool_value BOOLEAN
  );
  INSERT INTO test_table(int_value, float_value, string_value, bool_value)
       VALUES (15, 13.53, "hello", 0);
  INSERT INTO test_table(int_value, float_value, string_value, bool_value)
       VALUES (30, 42.5, "world", 1);
  .quit

With the above database, the following ABS model can be run:

Example: reading from the database::

  module Test;

  def List<String> fstring() = builtin(sqlite3, ①
      "/tmp/test.sqlite3",
      "SELECT string_value FROM test_table");

  def List<Rat> frat() = builtin(sqlite3, ②
      "/tmp/test.sqlite3",
      "SELECT float_value FROM test_table");

  data RowResult = RowResult(Int, Bool, Float, Rat, String);

  def List<RowResult> ftuple() = builtin(sqlite3, ③
      "/tmp/test.sqlite3",
      `SELECT int_value, bool_value, float_value, float_value, string_value
         FROM test_table`);

  def List<RowResult> ftuple_with_params(String str, Rat rat) = builtin(sqlite3,
      "/tmp/test.sqlite3",
      `SELECT int_value, bool_value, float_value, float_value, string_value
         FROM test_table
        WHERE string_value = ? ④
          AND float_value = ?`,
      str, ⑤
      rat);

  {
      foreach (v, i in frat()) {
          println(`$i$'th rational value is $v$`);
      }
      foreach (v, i in fstring()) {
          println(`$i$'th string value is $v$`);
      }
      foreach (v, i in ftuple()) {
          println(`$i$'th tuple is $v$`);
      }
      foreach (v, i in ftuple_with_params("world", 85/2)) {
          println(`$i$'th tuple is $v$`);
      }
  }

| ① A `builtin` function with first argument `sqlite3` takes two or
  more additional arguments: the name of the database, a SQL query
  string, and the query parameters.
| ② SQL `REAL` values are converted to ABS rational values
| ③ SQL query results with more than one column are converted into ABS
  user-defined datatypes. The data constructor of the result datatype
  has to take the same number of arguments as the `SELECT` returns,
  and the datatypes must be compatible.
| ④ Query parameters in SQLite are written as plain `?` in the query
  string.
| ⑤ For each parameter in the SQL query, a value must be supplied.

For the Erlang backend, when the database file (the second argument to
the ``builtin`` expression) does not contain a path, the model will
look for it in the ``priv`` directory of the compiled model.  That
directory is the value of the erlang function
``code:priv_dir(absmodel)``, typically
``gen/erl/absmodel/_build/default/lib/absmodel/priv/``.  For the Java
backend, the path to the database file is resolved relative to the
current path of the process running the model.
