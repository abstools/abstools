.. _type-future:

The Future Type
===============

Futures are placeholders for return values of asynchronous methods
calls.

Future values are produced by asynchronous method calls (see
:ref:`async-call-expression`).  The current process can suspend itself
until a future is resolved, i.e., until the return value of the
asynchronous method call is available (see :ref:`await-stmt`).  The get
expression returns the value of a future (see :ref:`get-expression`).  In
case the future is not yet resolved, the get expression blocks the
current cog.

Example::

  Fut<Int> f = o!add(2, 3); ①
  await f?; ②
  Int result = f.get; ③

| ① This statement defines a future variable ``f`` to hold the integer
  result of the method call to ``add``.
| ② The ``await`` statement suspends the current process until ``f``
  is resolved.
| ③ The ``get`` expression returns the value computed by the ``add``
  call.

Futures are first-class values that can be stored and passed around.
In case only the return value of the method call is needed and not the
future itself, a shorthand can be used that combines the above three
statements::

  Int result = await o!add(2, 3); ①

| ① This statement invokes ``add``, suspends the current process until
  the result is available, then stores it in ``result``.

.. _type-destiny:

The Destiny Type
----------------

.. note:: This feature is currently only available in the Erlang
          backend.

The ``Destiny`` type is the type of the ``destiny`` expression, see
:ref:`destiny-expression`.  Thus, its values are also futures::

  Unit myMethod() {
      Destiny x = destiny;
  }

The ``Destiny`` type is a supertype of all future types.  For example,
values of any future type ``Fut<T>`` can implicitly be cast to the
``Destiny`` type::

  Destiny f = this!myMethod();

The ``Destiny`` type supports all boolean comparisons that ``Fut<T>``
types support. Thus, one can use it to compare futures even across
different future types to identify asynchronous method calls::

  Destiny f = this!myMethod();
  Fut<Int> g = this!myMethod();
  assert(f == f && f != g);

The Get-Expression of a ``Destiny`` value is typed by the internal
type ``Bottom`` that has no values.  Thus, it is permitted to
synchronize on a future typed by ``Destiny`` but one can not extract
its result::

  Int myMethod() {
      Destiny f = destiny;
      f.get; ①
      await f?; ②
      Int x = f.get; ③
      return 42;
  }

| ① This is permitted but results in a deadlock, as the method is now
  synchronizing on its own completion.
| ② Applying ``await`` to a ``Destiny`` value is also permitted but in
  this case it has the same deadlock problem.
| ③ This will result in a compile-time error.
