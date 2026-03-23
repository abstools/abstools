.. _sec:modules:

*******
Modules
*******

All Core ABS definitions (classes, interfaces, functions data types,
type aliases) are contained in modules.  All definitions are visible
in their own module only, except when the module exports the name and
it is imported in another module or referenced by its qualified name.

NOTE: The ``export`` clause in a module definition exports *names*,
not definitions as such.  This means that if a module defines a class
and an interface with the same name, both definitions will be
accessible if that name is contained in the ``export`` clause.


Defining a Module
=================

.. table:: Syntax
   :align: left
   :class: syntax

   ====================   =============================
   *ModuleDecl* ::=       ``module`` *TypeIdentifier* ``;`` { *ExportImport* } { *Decl* } [ *Block* ]
   *ExportImport* ::=     *Export* {vbar} *Import*
   *Export* ::=           ``export`` *IdentifierList* [ ``from`` *TypeIdentifier* ] ``;``
   \                      \| ``export`` ``*`` [ ``from`` *TypeIdentifier* ] ``;``
   *Import* ::=           ``import`` *IdentifierList* [ ``from`` *TypeIdentifier* ] ``;``
   \                      \| ``import`` ``*`` ``from`` *TypeIdentifier* ``;``
   *IdentifierList* ::=   *AnyIdentifier* { ``,`` *AnyIdentifier* }
   *AnyIdentifier* ::=    *Identifier* {vbar} *TypeIdentifier*
   Decl* ::=              *FunctionDecl*
   \                      \| *PartialFunctionDecl*
   \                      \| *TypeSynDecl*
   \                      \| *DataTypeDecl*
   \                      \| *ExceptionDecl*
   \                      \| *InterfaceDecl*
   \                      \| *ClassDecl*
   \                      \| *TraitDecl*
   ====================   =============================


A module name is a type name and must always start with an upper case
letter.

Every module starts with a declaration of the form::

  module MyModule;

This declaration starts a new module named ``MyModule``.  All
declarations after this line until the next module declaration belong
to the module ``MyModule``.

.. note:: The module ``ABS.StdLib`` contains the standard library and
          is imported by every module by default.  If a module
          contains an explicit ``import x, y from ABS.StdLib;`` import
          clause, only ``x`` and ``y`` will be imported, otherwise all
          names exported from the standard library will be imported in
          that module.


Exporting Identifiers
=====================

By default, modules do not export any names.  In order to make names
defined within a module available to other modules, the names have to
be *exported*.  For example, to export a data type and a data
constructor, one can write something like this::

  module Drinks;
  export Drink, pourMilk, pourWater;
  data Drink = Milk | Water;
  def Drink pourMilk() = Milk;
  def Drink pourWater() = Water;

Note that in this example, the data constructors are not exported, and
other modules can only create values of type ``Drink`` by calling the
exported constructor functions ``pourMilk`` and ``pourWater``.  By
only exporting the data type without any of its constructors, one can
realize *abstract data types* in ABS.

A special export clause ``export *;`` exports all names that are
defined in the module.  Note that imported names are *not* re-exported
by ``export *;`` or ``export Name;`` (but can be re-exported via
``export * from OtherModule;`` and ``export Name from OtherModule;``
clauses)::

  module Test;
  export *;
  import * from OtherModule;
  export * from OtherModule;


Importing Identifiers
=====================

In order to use exported names of a module in another module, the
names have to be imported.  In a module definition, a list of import
clauses follows the list of export clauses.  After being imported,
these names are accessible in the current module.

Names can be accessible either qualified (with package prefix) or
unqualified, depending on how they are imported.

The following example makes the ``Drink`` data type of the module
``Drinks`` accessible as ``Drinks.Drink``::

  module Bar;
  import Drinks.Drink; ①
  import pourMilk from Drinks; ②

| ① The name ``Drink`` is accessible in module ``Bar`` as
  ``Drinks.Drink``.
| ② The name ``pourMilk`` is accessible in module ``Bar`` both as
  ``Drinks.pourMilk`` and ``pourMilk``.


The ``import * from Module;`` statement makes all names that are
exported from module ``Module`` accessible without module qualifier in
the current module::

  module Bar;
  import * from Drinks; ①

| ① All names from ``Drinks`` are accessible in ``Bar`` with and
  without the ``Drinks.`` prefix.


Re-exporting Imported Names
---------------------------

It is possible to re-export names that are imported from another
module. For example, this code::

  module Bar;
  import * from Drinks;
  export * from Drinks;

re-exports from ``Bar`` all names that are exported by module
``Drinks``.  Another module that writes ``import * from Bar;`` will
have the names from ``Drinks`` accessible as well.

To re-export only selected names, include only these names in the
export list::

  module Bar;
  import * from Drinks;
  export Drink from Drinks; ①

Here, only ``Drink`` is exported from ``Bar``.

Note that only names that have been imported can be re-exported.  For
example::

  module Bar;
  import Drink from Drinks;
  export * from Drinks;

only re-exports ``Drink``, as this is the only name imported from
module ``Drinks``.
