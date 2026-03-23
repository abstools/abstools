
.. _sec:schedulers:

***********************
User-Defined Schedulers
***********************

User-defined schedulers are an experimental feature of Timed ABS.
They are currently available on the Erlang backend.  This section
describes the current state of the implementation.

All identifiers introduced in this section reside in the
``ABS.Scheduler`` module, which is not imported by default.

See `<https://doi.org/10.1007/s11334-012-0184-5>`__ for a discussion
of ABS user-defined schedulers and process attributes.


Defining Schedulers
===================

A scheduler is a function that takes a list of processes and chooses
one of them.  Schedulers are defined as ordinary ABS functions, taking
an argument of type ``List<Process>`` as their first argument and
returning a result of type ``Process``.

The following example schedulers illustrate the concept.  This
deterministic scheduler returns the first process of the list::

  def Process defaultscheduler(List<Process> queue) = head(queue);

A random scheduler chooses a process non-deterministically::

  def Process randomscheduler(List<Process> queue) = nth(queue, random(length(queue)));



Using Schedulers
================

A scheduler is not called by code within the ABS model.  Instead, the
scheduler function is called by the runtime system to decide which
process to schedule.  It is guaranteed that the process list is
non-empty, and the ABS type system guarantees that the scheduler
returns one of the processes in the list it gets passed.

Schedulers apply to *cogs* since cogs are responsible for scheduling
one of their processes when idle.  Since cogs are created via ``new``
expressions, a scheduler can be given at that point via an annotation.
Classes can have a *default scheduler* that is given as an annotation
to the class definition; any cog created when instantiating this class
will have that scheduler by default (unless overridden by an
annotation at the ``new`` expression).

The following example shows how to annotate a class to schedule
processes via the ``randomscheduler`` function by default.  The first
argument to the scheduler (i.e., the list of processes) must have the
name ``queue``.

::

  [Scheduler: defaultscheduler(queue)] class C implements I {
      ...
  }

All instances of ``C`` created via ``new`` will use
``defaultscheduler`` to schedule tasks in the created cog.

The following example shows how to create an instance of ``C`` with a
different scheduler::

  [Scheduler: randomscheduler(queue)] I instance = new C();


Using Object State in Schedulers
================================

Schedulers can have more than one argument.  The first argument must
be the list of processes, values for subsequent arguments are taken
from value of an object field with the same name when calling the
scheduler.

The object whose fields are used is the first object in a cog, i.e.,
the object created via the ``new`` expression that created the cog.
Subseqent objects created in that cog via ``new local`` cannot
influence the scheduler.

::

  module Test;
  import * from ABS.StdLib.Scheduler;

  class C {
      Bool flag = False; ①
  }

  def Process some_scheduler(List<Process> queue, Bool value) = head(queue); ②

  {
      [Scheduler: some_scheduler(queue, flag)] new C(); ③
  }

| ① Here, class ``C`` defines a field called ``flag``
| ② The scheduler ``some_scheduler`` has a second argument of type
  ``Bool``
| ③ The current value of field ``flag`` will be used as the second
  argument to ``some_scheduler``


.. _sec:process-attributes:

Processes and Process Attributes
================================

A *process* is an ABS abstract datatype; i.e., there is no constructor
available to create a ``Process`` value inside ABS.  Processes are
created by the runtime and handed to schedulers.

Processes have *attributes* which can be used by schedulers to choose
the next process to run.  For example, a scheduler could always prefer
processes that run a certain method.  The following attributes are
available:

.. table:: Process Attributes

   =================   ============   ==================
   Name                Type           Meaning
   =================   ============   ==================
   ``method``          ``String``     Name of the method executed by the process
   ``arrival``         ``Time``       Time when method call was issued
   ``proc_deadline``   ``Duration``   The (current) deadline value of the process (``InfDuration`` if none)
   ``destinyOf``       ``Destiny``    The future of the asynchronous call that is currently being executed by the process
   =================   ============   ==================

Using process attributes, an Earliest Deadline First (EDF) scheduler
can be defined in ABS as follows::

  def Process earliest_deadline_scheduler(List<Process> queue) =
    foldl((Process a, Process b) =>
        if durationLessThan(proc_deadline(a), proc_deadline(b)) // a < b
        then a else b)
      (queue, head(queue));

.. note:: Other process attributes in the real-time scheduling field
          include ``cost``, ``start``, and ``crit``, which are planned
          to be implemented.  The attributes ``value``, ``finish`` are
          of questionable usefulness but might be implemented still.

