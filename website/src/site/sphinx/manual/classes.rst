.. _sec:classes:

*******
Classes
*******

Classes in ABS describe stateful behavior.  All objects in ABS have
exactly one class; objects are created via the ``new`` expression.

Classes implement one or more interfaces.  If a class implements more
than one interface, :ref:`typecheck-expression` and
:ref:`typecast-expression` are used to obtain a reference to the same
object typed with a different interface.  If no explicit
``implements`` clause is given, the class will extend the standard
library interface ``ABS.StdLib.Object`` (see :ref:`type-object`).  The
``ABS.StdLib.Object`` interface does not specify any methods.

.. note:: Classes typically explicitly implement one or more
          interfaces so that methods can be called on them, but the
          ``run`` method makes it meaningful to have objects without
          public methods; such objects cannot accept method calls but
          can send method calls to other objects.

NOTE: For ease of reasoning and analysis, ABS methods differ only by
name.  It is an error for a class to implement two interfaces that
both contain a method with the same name.

Classes have *fields* that define the state of objects of that class.
All fields are private and can only be accessed from methods defined
in the class.  Fields are defined in two ways:

- *Field declarations* in the body of the class define fields that get
  their initial value from their init expression.
- *Class parameters* define fields that get their initial value from
  the ``new`` expression.

Classes have an optional *init block*, which is executed for each new
object before any other code.  The init block cannot contain processor
release points (i.e., ``await`` or ``suspend``), blocking expressions
(i.e., ``get``, ``duration``, using resources), or explicitly throw an
exception via the ``throw`` statement.  It is mainly used for
complicated field initializations and can be omitted.

Classes have an optional *run method* (see :ref:`sec:active-classes`).

Classes have an optional *recovery block*.  In case an uncaught
exception occurs in a method, the exception is matched against the
patterns given in the recovery block, and the associated statement(s)
are executed.  If the exception does not match any pattern in the
recovery block, or if the recovery block itself raises an exception,
the object is killed.  Code in the recovery block has the same
restrictions as in the init block (i.e., no processor release points,
blocking expressions and ``throw`` statements).

.. table:: Syntax
   :align: left
   :class: syntax

   ========================   ===========================
   *ClassDecl* ::=            ``class`` *SimpleTypeIdentifier* [ ``(`` *ClassParameters* ``)`` ]
   \                          [ ``implements`` *InterfaceList* ]
   \                          ``{`` [ *FieldDeclList* ] [ *Block* ] [*RecoveryBlock*] { *TraitUse* }
   \                          { *MethDecl* } ``}``
   *ClassParameters* ::=      *Type* *SimpleIdentifier* { ``,`` *Type* *SimpleIdentifier* }
   *InterfaceList* ::=        *TypeIdentifier* { ``,`` *TypeIdentifier* }
   *TraitUse* ::=             ``uses`` *TraitExpr* ``;``
   *FieldDeclList* ::=        { *Type* *SimpleIdentifier* [ ``=`` *PureExp* ] ``;`` }
   *RecoveryBlock* ::=        ``recover`` ``{`` { *Pattern* ``=>`` *Stmt* } ``}``
   *MethDecl* ::=             *Type* *SimpleIdentifier*
   \                          ``(`` [ *Type* *SimpleIdentifier* { ``,`` *Type* *SimpleIdentifier* } ] ``)`` *Block*
   ========================   ===========================


A class definition contains zero or more method definitions.  Each
method has a name, return type and zero or more parameters.  All
methods declared in an interface that is implemented by the class or
one of their super-interfaces must be defined in the class body or in
one of the used traits (see :ref:`sec:traits`).  A class is free to
define methods not declared in an interface; such methods are private
to the class and cannot be called from outside the class.

NOTE: ABS currently does not support method overloading.  Each method
must have a unique name since methods are not disambiguated by their
parameter lists.

Example::

  class DataBase(Map<Filename,File> db) implements DB {
      File getFile(Filename fId) {
          return lookup(db, fId);
      }

      Int getLength(Filename fId){
          return length(lookup(db, fId));
      }

      Unit storeFile(Filename fId, File file) {
          db = insert(Pair(fId,file), db);
      }

      Filenames listFiles() {
          return keys(db);
      }
  }

  class Node(DB db, Peer admin, Filename file) implements Peer {
      Catalog catalog;
      List<Server> myNeighbors;
      // implementation...
  }


.. _sec:active-classes:

Active Classes
==============

A class can be active or passive. Active classes start an activity on
their own upon creation. Passive classes only react to incoming method
calls. A class is active if and only if it has a run method::

  Unit run() {
      // active behavior ...
  }

The run method is asynchronously called after object initialization.
Note that the run method does not need to terminate, but should
periodically suspend itself so that the object can execute other
methods.


Constant Fields
===============

Similar to variable declarations, field declarations and class
parameters can carry a ``Final`` annotation.  the effect of such an
annotation is to forbid re-assignment to such a field.

The following example will lead to compile-time errors since we are
trying to assign new values to two fields declared as ``Final``::

  class Sample ([Final] Int constant_i) {
      [Final] Int constant_j = 24;
      Unit m() {
          constant_i = 25; ①
          constant_j = 24; ②
      }
  }

| ① compile-time error since the class parameter was declared
  ``Final``.
| ② compile-time error since the field was declared ``Final``.

Read-only Methods
=================

For any method definition annotated with ``[Readonly]``, the compiler
will statically check that its body does not contain assignments to
fields.

If an interface declares a method to be read-only, its definition has
to be annotated with ``[Readonly]`` as well.

The following example shows a class declaration with a readonly method
that will lead to a compile error::

  class Wrong {
      Int field = 12;
      [Readonly] Unit fails() {
          field = field + 1; ①
      }
  }

| ① compile-time error since the method was declared ``Readonly`` but
  modifies a field.


Atomic Methods
==============

For any method definition annotated with ``[Atomic]``, the compiler
will statically check that its body does not contain suspension points
(``suspend`` and ``await`` statements) and blocking ``get``
expressions.  Such methods can be called inside init blocks and in
``finally`` clauses; all other methods cannot be called in these
places.

If an interface declares a method to be atomic, its implementation has
to be annotated with ``[Atomic]`` as well.

The following example shows a call to an atomic method from an init
block.  Removing the ``Atomic`` annotation from method ``m`` would
lead to a compile-time error::

  class Sample {
      Int field = 12;

      {
          field = this.m(); ①
      }

      [Atomic] Int m() {
          return 24;
      }
  }

| ① this method call inside the init block is allowed since the method
  is declared ``Atomic``.
