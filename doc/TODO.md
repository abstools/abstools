# Errors in BNFC grammar

- EQualCall
- EqualVar
- Init Block is mandatory inside class declaration
- No support for annotations

# Errors in Translation

- Interface Method names clashes with non-interface (class) method names
- Only support 1 module definition per file
- The imported modules must be scanned for Interfaces and collected so as to generate the necessary types (with some 1st-phase export collection per module)
- Right now, we use lazy IO, switch to strict IO with `evaluate`


# The way it is laid out, when the instances are generated,
the methods should take the object as the last argument (easier
for partial application)

# Comments about the ABS language for Translation

- A written method in a class that does not belong/implement any interface, cannot be called from outside.
So then what is the point of this class? => This method can only be called locally inside another same-class method.

- Method calls must explicitly be called by: object.method() or this.method(). Otherwise they point to a pure function() call.

- The type of int is unrestricted like Haskell's Integer? Then translated to Integer

# The advantages of having a Haskell backend

- Mostly (except ABS's letnonrec to Haskell's letrec) 1-to-1 translation of the functional core (datatypes + pure functions)
- 1-to-1 correspondence of OO interface inheritance and Haskell's typeclasses
- ABS and Haskell has the exact same module system
- Type Inference becaus of Milner type system
- Add support for lambda functions and thus higher-order functions?

# ABS/Haskells incompatibilities

- letrec vs letnonrec

# TODO enhance syntax

- Make typing optional (because of Haskell's type inference)
- Add support for polymorphic type synonyms <https://envisage.ifi.uio.no:8080/redmine/issues/108>

# TODO for the Haskell backend

- For now x = x + 1 different thant this.x = this.x + 1 . We should implement variable lookup in AG
- Write documentation inside the source modules
- Generate haddock
- Retaining the ABS comments and generating Haskell comments


# TODO enhance ABS language

- add lambdas
- add function arrow-type (->)
- thus add higher-order functions
- maybe eta reduction
- add let-polymorphism (need to change the translation of ABS let from lambdas to Haskell's letrec with name sanitation)

let-polymorphism examples:

let f x = x in (f "a", f True)

let i = id in (i "a", i True)
