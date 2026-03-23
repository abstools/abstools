.. _stdlib-others:

Others
======

This subsection lists definitions of the standard library that do not
fit in any other sections.

Functions
---------

ms_since_model_start
^^^^^^^^^^^^^^^^^^^^

The function ``ms_since_model_start`` returns a non-negative integer
containing the number of milliseconds elapsed since the model was
started.  It is useful mainly for benchmarking.

::

  ms_since_model_start()
  // => 15
