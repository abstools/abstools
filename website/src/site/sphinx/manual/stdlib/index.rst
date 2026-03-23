.. _sec:standard-library:

********************
The Standard Library
********************

This chapter documents the ABS standard library.  All definitions,
except where noted otherwise, are contained in the module
``ABS.StdLib``.

The module ``ABS.StdLib`` is included by default in every module.
Therefore, no ``import`` statement is necessary to use these
definitions.  Note that if a module explicitly imports some
identifiers from ``ABS.StdLib``, only those identifiers will be
available.

.. toctree::
   :maxdepth: 2

   bool
   numbers
   strings
   unit
   object
   futures
   exceptions
   lists
   sets
   maps
   pairs
   triples
   optionals
   others
