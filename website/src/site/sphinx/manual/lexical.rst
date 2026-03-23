*****************
Lexical Structure
*****************

This section describes the lexical structure of the ABS language.  We
use the following EBNF conventions for specifying the syntax of ABS
elements.

.. list-table::
   :widths: auto

   * - ``Typewriter text``

     - Terminal symbols (occurring in program source code)

   * - *CursiveText*

     - Non-terminals (production rule names)

   * - ::=

     - Separator between left hand side and right hand side of a rule

   * - |

     - Variant; either of the element(s) separated by the bar

   * - [ ... ]

     - Optionals; the enclosed elements can be omitted

   * - { ... }

     - Repetition; zero or more occurrences of the enclosed elements

   * - ? ... ?

     - Special group (elements in the group are specified informally)

   * - [: ... :]

     - A character class, as in extended regular expression syntax

   * - ( ... )

     - Grouping


Line Terminators and White Spaces
=================================

Line terminators and white spaces are defined as in Java.

.. table:: Syntax
   :align: left
   :class: syntax

   ====================   ==================
   *LineTerminator* ::=   ``\n`` | ``\r`` | ``\r\n``
   *WhiteSpace* ::=        *LineTerminator* | ``␣`` | ``\t`` | *Comment*
   ====================   ==================

..
   use U+2423 or U+2420 for the space symbol




Comments
========

ABS supports the two common Java-style styles of comments: end-of-line
comments and block comments.

.. table:: Syntax
   :align: left
   :class: syntax

   ==================   ====================
   *Comment* ::=        *LineComment* | *BlockComment*
   *LineComment* ::=    ``//`` { ? characters except *LineTerminator* ? } *LineTerminator*
   *BlockComment* ::=   ``/*`` { ? characters except ``*/`` ? } ``*/``
   ==================   ====================

An end-of-line comment starts with two slashes, e.g., ``// text``. All
text that follows ``//`` until the end of the line is treated as a
comment.

A block comment is enclosed in ``/* ... */``, e.g., ``/* this is a
comment */``.  Block comments can span multiple lines and do not nest.

Example::

   // this is a comment
   module A; // this is also a comment

   /* this
   is a multiline
   comment. */


Identifiers
===========

Identifiers consist of a letter followed by a sequence of letters,
numbers and underscores (``_``).

ABS distinguishes identifiers and type identifiers.  Identifiers start
with a lower-case character, type identifiers start with an upper-case
character.  Identifiers name local variables, fields, methods and
functions.  Type identifiers name interfaces, classes, types, type
constructors, deltas and products.

Identifiers can be *qualified* with a module name (see
:ref:`sec:modules`) or *simple* (without module name prefix).

.. table:: Syntax
   :align: left
   :class: syntax

   =============================   ==========================
   *SimpleIdentifier* ::=          [: lower :] { [: alpha :] | [: digit :] | ``_`` }
   *SimpleTypeIdentifier* ::=      [: upper :] { [: alpha :] | [: digit :] | ``_`` }
   *QualifiedIdentifier* ::=       { *SimpleTypeIdentifier* ``.`` } *SimpleIdentifier*
   *QualifiedTypeIdentifier* ::=   { *SimpleTypeIdentifier* ``.`` } *SimpleTypeIdentifier*
   *Identifier* ::=                *SimpleIdentifier* | *QualifiedIdentifier*
   *TypeIdentifier* ::=            *SimpleTypeIdentifier* | *QualifiedTypeIdentifier*
   =============================   ==========================


Keywords
========

The following words are keywords in the ABS language and are invalid as identifiers.

..
   // TODO check and update this list


``adds``, ``after``, ``assert``, ``await``, ``builtin``, ``case``,
``catch``, ``class``, ``core``, ``data``, ``def``, ``delta``, ``die``,
``else``, ``exception``, ``export``, ``extends``, ``features``,
``finally``, ``from``, ``get``, ``hasField``, ``hasInterface``,
``hasMethod``, ``if``, ``implements``, ``import``, ``in``,
``interface``, ``let``, ``local``, ``modifies``, ``module``, ``new``,
``null``, ``original``, ``product``, ``productline``, ``recover``,
``removes``, ``return``, ``skip``, ``suspend``, ``this``, ``throw``,
``trait``, ``try``, ``type``, ``uses``, ``when``, ``while``,

.. _sec:literals:

Literals
========

A literal is a textual representation of a value. ABS supports integer
literals, floating-point literals, string literals, and the null
literal.  Rational numbers are written using the division operator
``/``, e.g., ``1/4`` for one quarter.

Strings are enclosed in double quotes (``"``).  Line feed in a string
is written as ``\n``, carriage return as ``\r``, tab as ``\t``.

The null literal is written as ``null``.

.. table:: Syntax
   :align: left
   :class: syntax

   ===================   ========================
   *Literal* ::=         *IntLiteral* | *FloatLiteral* | *StringLiteral* |
   *IntLiteral* ::=      ``0`` | ( ( ``1`` | ... | ``9`` ) { [: digit :] } )
   *FloatLiteral* ::=    [ *IntLiteral* ] ``.`` [: digit :] { [: digit :] } [ ( ``e`` | ``E`` ) [ ``-`` | ``+`` ] *IntLiteral* ]
   *StringLiteral* ::=   ``"`` { ? *Valid String Character* ? } ``"``
   ===================   ========================

Valid string characters are defined as in the Java language.


Annotations
===========

Annotations consist of a syntactically valid pure expression,
optionally preceded by a type identifier (the “tag”) and a colon
(``:``).  They can be put in front of statements and definitions.
Annotations are enclosed in square brackets (``[]``).

There can be more than one annotation in one place.  When annotating a
place with more than one annotation, writing the annotations in
separate pairs of brackets or separated by commas in one pair of
brackets produces the same effect.

.. table:: Syntax
   :align: left
   :class: syntax

   ========================   ===================
   *AnnotationFragment* ::=   [ *TypeIdentifier* ``:`` ] *PureExp*
   *Annotation* ::=           { ``[`` *Annotationfragment* { ``,`` *AnnotationFragment* } ``]`` }
   ========================   ===================


Annotations are used to write auxiliary information that can be used by
various tools.  Unknown (user-defined) annotations are ignored by the
tool chain.  Pre-defined annotations are usually type-checked.

Example::

   [Near] class Example { ... }

This is an example of annotations with a tag::

   [Cost: 15, Deadline: Duration(20)] o!m();

The same annotations, written in separate brackets::

   [Cost: 15] [Deadline: Duration(20)] o!m();

Annotations are associated with the following language construct.  In
the examples above, the first annotation pertains to the definition of
the class ``Example``, the second annotation is attached to the
asynchronous method call ``o!m()``.

In general, it is not an error to have more than one annotation with
the same tag in the same place.  However, some pre-defined annotations
might forbid this.
