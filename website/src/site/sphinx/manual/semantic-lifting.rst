.. _sec:semantic-lifting:

****************
Semantic lifting
****************

ABS implements *semantic lifting*, i.e., obtaining a semantic
representation of aspects of the model and the runtime state in
:term:`RDF` form.

Semantically-lifted program state can be queried from the command
line, a SPARQL endpoint implemented in the :ref:`Model API
<sec:model-api>`, or within the model itself.

Semantic lifting is supported in the Java backend.


The lifting ontology
====================

The following namespaces are always defined:

``abs:``

   The ABS language ontology, containing definitions for ABS
   interfaces, classes, fields, datatypes and constructors.

``prog:``

   The program ontology, containing all classes, interfaces, members,
   datatypes and datatype constructors of the model.

``run:``

   the runtime ontology, containing objects and object state.

Additionally, when an ABS model is compiled with an argument
``--domain-ontology domain.ttl`` to the compiler, the content of the
file named by that argument (``domain.ttl`` in the example) is added
to the lifted program state and is visible to all SPARQL queries.

Representation of ABS language constructs
-----------------------------------------

This section shows concretely how ABS model and state information is
lifted.

.. note:: The lifting ontology might change as we gain experience
          applying semantic lifting to concrete case studies.

.. list-table:: RDF representations for ABS language constructs
   :header-rows: 1
   :widths: auto

   * - ABS construct

     - RDF representation

   * - interface

     - resource of type ``abs:interface``

   * - class

     - resource of type ``abs:class``

   * - datatype

     - resource of type ``abs:datatype``

   * - data constructor

     - resource of type ``abs:dataconstructor``

   * - future

     - resource of type ``prog:ABS.StdLib.Fut``

   * - string

     - RDF string

   * - number

     - RDF int or float number

   * - Boolean

     - RDF boolean

   * - null value

     - RDF string ``"null"``


We consider the following small ABS model::

  module Test;

  interface I {}

  class C (Maybe<Int> i) implements I {}

  {
    I o = new C(Just(1));
  }

All datatypes are represented as resources of type ``abs:datatype``,
their dataconstructors are represented via ``abs:dataconstructor``::

  prog:ABS.StdLib.1985362663
        rdf:type            abs:datatype;
        rdfs:label          "ABS.StdLib.Maybe";
        abs:hasConstructor  prog:ABS.StdLib.Just , prog:ABS.StdLib.Nothing .

  prog:ABS.StdLib.Just  rdf:type  abs:dataconstructor;
        rdfs:label  "ABS.StdLib.Just" .

All interfaces are represented as resources of type ``abs:interface``,
with a label containing the fully-qualified name of the interface::

  prog:Test.823775087  a  abs:interface;
        rdfs:label   "Test.I";
        abs:extends  prog:ABS.StdLib.2078396010 .  # ABS.StdLib.Object

All classes are represented as resources of type ``abs:class``, again
with a label with the class name::

  prog:Test.1041552272  a  abs:class;
        rdfs:label      "Test.C";
        abs:hasField    prog:Test.i;
        abs:implements  prog:Test.823775087 .

  prog:Test.i  a      abs:field;
        rdfs:label  "Test.i" .

All the information above is static and calculated at compile-time.
At runtime, the object created in the main block is lifted as
follows::

  run:obj1719860023  rdf:type prog:Test.1041552272;
        abs:in       run:cog1652764753;
        prog:Test.i  [ rdf:type   prog:ABS.StdLib.Just;
                       prog:arg0  1
                     ] .

  run:cog1652764753  rdf:type  abs:cog .

Numeric and string data values are represented directly, user-defined
datatypes are lifted as anonymous resources referencing the
constructor and its arguments.

Note that the lifted representation of an object also references the
cog that contains the object.  This information can be used to, for
example, find all objects running on the same cog.



Linking objects to domain concepts
----------------------------------

An ABS class can be linked to a domain concept via a ``DomainClass`` annotation::

   [DomainClass: "domain:Housing"]
   class House { } ①

   [DomainClass: when i > 0 then ":containsPositive" else ":containsNonPositive"] ②
   class C(Int i) { }

| ① All lifted instances ``x`` of ``House`` will have an additional
  triple ``x rdf:type domain:Housing``.

| ② The ``DomainClass`` annotation is an expression that must return a
  string and can use all fields of the object to calculate its result.


Accessing lifted state externally
=================================

When an ABS model is started with the ``--printRDF`` argument, the
complete semantic representation is printed to the terminal in
:term:`TRTL` format after the model finishes.

When an ABS model is started with the ``--sparqlQuery`` argument
followed by a valid SPARQL query, that query is run after the model
finishes and its result is printed in TRTL format.

When an ABS model is running with the :ref:`Model API <sec:model-api>`
active, it provides a SPARQL endpoint under the ``/sparql`` URL.  The
endpoint accepts SPARQL queries as specified in `Section 2.1
<https://www.w3.org/TR/sparql11-protocol/#query-operation>`__ of the
`SPARQL 1.1 Protocol <https://www.w3.org/TR/sparql11-protocol/>`__.
The sparql endpoint returns results in `JSON format
<https://www.w3.org/TR/sparql11-results-json/>`__ by default.



Accessing lifted state within the model
=======================================

A SPARQL query that can be executed at runtime from within the model
is defined by writing a function with a ``builtin`` body with two
arguments: a literal ``sparql`` followed by the query as a SPARQL
string.

The result of a SPARQL query is converted into an ABS list of values.

Currently only the first SPARQL variable listed in the ``SELECT``
clause is used to construct the ABS return value.  Valid return types
of ``builtin`` SPARQL query functions are:

- ``List<Int>`` when the first SPARQL variable is a RDF literal that
  can be converted to an integer;

- ``List<Float>`` when the first SPARQL variable is a RDF literal that
  can be converted to a float;

- ``List<Rat>`` ditto;

- ``List<Bool>`` when the first SPARQL variable is a RDF literal that
  can be converted to a Boolean;

- ``List<String>`` when the first SPARQL variable is a RDF literal;

- ``List<I>`` when ``I`` names an interface, and the first SPARQL
  variable is a RDF resource that names an ABS object whose class
  implements that interface.

.. note:: It is an error if the resources found by a query where ABS
          expects objects of type ``I`` do not represent ABS objects
          that implement ``I``, but currently such objects are
          silently dropped when creating the result.

::

  module Test;

  interface I {}

  class C(Int i) implements I {}

  def List<String> all_integer_field_values() = builtin(sparql, ①
      `SELECT ?i
       WHERE { ?obj a/rdfs:label "Test.C" . ②
               ?obj prog:Test.i ?i . }`); ③

  def List<I> all_I_instances() = builtin(sparql,
      `SELECT ?o WHERE {
         ?o a/abs:implements/rdfs:label "BackendTest.I" . ④
       }`);


  {
      I o1 = new C(5);
      I o2 = new C(4);
      I o3 = new C(2);
      List<String> result = all_integer_field_values(); ⑤
      List<I> all_I = all_I_instances(); ⑥
  }

| ① A ``builtin`` function with first argument ``sparql`` takes an
  additional argument, a SPARQL query string.

| ② The lifted representation of an ABS class has the ABS qualified
  name as an ``rdfs:label`` property, so we use a property path
  `a/rdfs:label` to find class instances.

| ③ The ``prog:`` namespace contains, among others, all class and
  attribute definitions.  The ``prog`` ontology uses fully qualified
  ABS names.

| ④ This query uses SPARQL *property paths*
  `<https://www.w3.org/TR/sparql11-property-paths/>`__ to succinctly
  find resources of a class that implements an interface with a label
  ``"BackendTest.I"``.

| ⑤ This query returns a list containing "2", "4", "5" in some
  permutation.  Since the function returns ``List<String>``, the RDF
  literals are converted to ABS strings.

| ⑥ This query returns the list of all objects implementing the
  interface ``I``.  Note that ABS objects are subject to garbage
  collection, so objects that are not referenced might have vanished by
  the time the query executes.

Parameterized queries
---------------------

It is possible to pass parameters to SPARQL queries.  Currently
parameters of type ``Bool``, ``Int``, ``Rat`` (converted to floating
point), ``Float``, ``String`` and ``Object`` (object references) are
supported.

A SPARQL query can contain one or more placeholders, written as ``?``.
The query function must supply a value for each of these placeholders::

  module Test;

  class C(String s, Int i) {
  }

    def List<String> query_via_int(Int i) = builtin(sparql,
        `SELECT ?s WHERE {
           ?o a/rdfs:label "Test.C" ;
              prog:Test.s ?s ;
              prog:Test.i ? . ①
         }`, i); ②

    def List<String> query_via_object(I o) = builtin(sparql,
          `SELECT ?s WHERE {
             ? a/abs:implements/rdfs:label "BackendTest.I" ; ③
               prog:BackendTest.s ?s .
           }`, o);


  {
      Object o = new C("hello", 15);
      Object o2 = new C("world", 1);
      List<String> world_string = query_via_int(1); ④
      println(`query_via_int(1): $world_string$`);
      List<String> hello_string = query_via_object(o); ⑤
      println(`query_via_object(o): $hello_string$`);
  }

| ① The SPARQL query contains a placeholder ``?``.

| ② The value of the function parameter ``i`` replaces the placeholder
  before the query is executed.

| ③ The parameter ``o`` is converted into an RDF resource IRI and
  inserted as subject of the two search triples.

| ④ The query will return a one-element list containing ``"world"``.

| ⑤ The query will return a one-element list containing ``"hello"``.

.. note:: The following caveats apply to SPARQL queries with
          parameters:

          - Querying for integer numbers beyond the range of the Java
            ``long`` type results in a runtime exception rather than
            silently wrong query results

          - Queries are susceptible to SPARQL injection; see the
            section "SPARQL Injection Notes" at the bottom of the
            `documentation
            <https://jena.apache.org/documentation/query/parameterized-sparql-strings.html>`_
            of the underlying Jena library.

