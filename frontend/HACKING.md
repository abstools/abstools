# Developer Guide

This file contains some hints about how to modify various parts of the
language implementation.  Contributions welcome!

## Adding new subcommands to the `absc` frontend

Adding new classes containing a `public static void main(String[]
args)` method should be avoided.  Instead, implement new analysis
tools etc. as subcommands: see the class
`org.abs_models.frontend.typechecker.CheckSPLCommand` for an example.

When calling `java absfrontend.jar`, the main method invoked is
contained in the class `org.abs_models.Absc`.  Register your
subcommand in the `subcommands` array of the `@Command` annotation to
that class, and put your code into a `public Void call()` method in
your class; it will be invoked when the user calls `absc
thenewsubcommand ...`.  Again, see
`org.abs_models.frontend.typechecker.CheckSPLCommand` for an example.

## Adding new language elements

1. Modify parser and AST

  - modify `ABS.ast`: introduce new language elements
  - modify `abs.g4`: decide on surface syntax
  - add to `CreateJastAddASTListener.java`: create AST elements defined in `ABS.ast`

2. Add type checking

  - modify type checker: adapt `typeCheck()`, `getType()`, make
    variable definitions visible to rest of program

3. Adapt backends

  This applies to new language elements that are not rewritten into
  more basic forms

  - modify each backend to generate the required code

4. Rewrite into more basic forms

  - Consider explicit rewriting instead of using JastAdd equations

  - Introduce boolean flag to disable rewriting
  - Only rewrite in core part of the language

    In deltas, traits: type information not available, other semantic
    constraints not met, ... -- we cannot safely mutate the AST there
    via equations!

  - Adapt pretty-printer, including `--nosugar` directive

  - Never re-insert AST fragments, always copy

  - Consider invalidating caches afterwards

5. Write test cases

6. Update manual


## Type Checking

### Type Checking the SPL

The main entry point for SPL checking is the class `org.abs_models.frontend.typechecker.CheckSPLCommand`.

The SPL type checker is invoked by calling `typecheckPL()` on the
`Model` AST node.  This top level method is defined by a JastAdd
attribute in `ProductLineTypeAnalysis.jadd`, which delegates to
`ProductLineTypeAnalysisHelper.typeCheckPL(..);`

FIXME:
> The SPL type checker should be called before flattening a product,
> but after running the old SPL sanity checks (such as checking for
> duplicate definitions, unused deltas etc.). Currently, the old
> checks are part of the product-specific type checker so they are run
> after flattening.


The helper method does the following:

- Check the SPL for strong unambiguity. This enables us to use any one
  valid delta order to assemble the SPL products.

- Build a trie data structure whose nodes represent all program
  variations by applying deltas to the core program _type
  abstraction_.

#### The Program Type Abstraction

Implemented in `ProgramTypeAbstraction.java`.

A program type abstraction (TA) records the program elements of core ABS as simple names (strings):
- Classes
  - Fields
  - Methods
- Functions (TODO)
- ADTs (TODO)
- etc. (TODO)

A delta can be applied to a TA `ta` by calling `ta.applyDelta(DeltaDecl d)`. This modifies the TA as described by the delta (by adding/removing/modifying elements). In case the delta cannot be applied (due to unfulfilled requirements -- e.g. adding a method to a class that does not exists), a type error is added to the TA's `SemanticConditionList`.

In addition, a TA keeps track of the SPL products that it represents. A TA will typically represent either no product or one particular product of the SPL. However, there can be several identical products in an SPL, which are thus represented by the same TA. The products are stored in `Set<ImplicitProduct> products`.

#### The Delta Trie

The _Product Family Generation Trie (PFGT)_ is built by `ProductLineTypeAnalysisHelper.buildPFGT(..)`. Essentially, for each valid feature combination defined by the feature model (i.e. the products of the SPL -- represented as `ImplicitProduct` AST nodes), a type abstraction is built and stored in a trie data structure. 
