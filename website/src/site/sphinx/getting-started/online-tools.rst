
Use On-Line Tools
=================

The simplest way to use the ABS tools is on-line, in the
collaboratory.  This means you only need a modern browser to start
experimenting with ABS.

The Collaboratory is a browser-based IDE for ABS that offers an
online, zero install version of the ABS toolchain.  Use it to
experiment with the language in a risk-free environment.

Load our ready-made examples or run your own ABS models. The editor
has many features of an IDE, including showing you an outline of the
code, and of course reporting syntax- and type errors.

The collaboratory also supports many of the analysis tools for ABS,
like COSTA and SACO.

A collaboratory instance is hosted online at
`<`http://ei.abs-models.org:8082/clients/web/>`__.  Please use this
instance responsibly, and consider running the collaboratory on your
local machine.

Running the Collaboratory Locally
---------------------------------

To run the collaboratory on a local machine, first install docker from
`<https://www.docker.com>`__.  Then, run the following command in a
terminal window:

.. code-block:: sh

   docker run -d --rm -p 8080:80 --name collaboratory abslang/collaboratory:latest

When the command has finished, connect a browser to
`<http://localhost:8080>`__ and start using ABS!

To stop the collaboratory, use the following command:

.. code-block:: sh

   docker stop collaboratory



