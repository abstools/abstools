.. _sec:traits:

******
Traits
******

ABS does not support inheritance for code reuse.  Method
implementations that are common between classes can be defined once
and used inside these classes by using *traits*.  A trait is a set of
method definitions that can be added to a class via the ``uses``
clause in the class definition.

Traits are applied to classes at compile-time and do not change the
interface(s) of a class.  Classes and their methods are type-checked
once all traits are applied.

Traits can re-use and modify other traits via *trait operations*.  A
trait can add methods to another trait, modify methods in another
trait, or remove methods from another trait.

.. table:: Syntax
   :align: left
   :class: syntax

   ===============   =================
   *TraitDecl* ::=   ``trait`` *TraitName* = *TraitExpr*
   *TraitExpr* ::=   *MethodSet* { *TraitOper* }
   *MethodSet* ::=   *TraitName* {vbar} *MethDecl* {vbar} ``{`` { *MethDecl* }  ``}``
   *TraitName* ::=   *SimpleIdentifier*
   *TraitOper* ::=   ``adds`` *MethodSet*
   \                 ``modifies`` *MethodSet*
   \                 ``removes`` ( *MethSig* {vbar} ``{`` { *MethSig* } ``}`` )
   ===============   =================


A trait is defined at the module level.

The effect of applying a trait ``T`` to a class (using ``uses T``
inside the class body) is to add the methods in that trait to the
class definition.

Trait Operations
================

The trait operation ``adds`` adds all the elements of its *MethodSet*
to the trait.  It is an error if any method of the *MethodSet* is
already present in the trait, but any error will only be raised
*after* applying all operations, during type checking.

Example::

  trait T = { Unit x() { skip; } } ①
  trait T2 = { Unit y() { skip; } } adds T ②

| ① ``T`` contains the method ``x``.
| ② ``T2`` contains the methods ``y`` and ``x``.


The trait operation ``modifies`` changes all the methods in the trait
to the new implementation described in this *MethodSet*.  If a method
with the same name is not present in the trait, the method is added
instead.

The methods in the ``modifies`` operation may contain ``original()``
calls which will call the version of the method before the operation
application.  In case an overriden method is not present,
``original()`` calls will lead to a compile-time error.

Example::

  trait T = { Bool m() { return False; } }
  trait T2 = { Bool m() { Bool orig = original(); return !orig; } }
  class C {
      uses T modifies T2; ①
  }

| ① Calling ``C.m()`` will return ``True``.


The operation ``removes`` removes the method(s) with the provided signatures
from the trait.  If a method with the same name is not present in the class
(or set of methods), an error will be raised during compilation.

Example::

  trait T = { Bool m(){ return False; } }
  class C {
      uses T removes { Bool m(); }; ①
  }

| ① Class ``C`` does not contain any methods.
