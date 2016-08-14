The ABS grammar
===============

This directory contains Karl Palmskog's formalization of Core ABS in
[ott](http://www.cl.cam.ac.uk/~pes20/ott/).  It was created during the
[HATS project](http://hats-project.eu/).  The coq support files in the `coq/`
subdirectory are copied unchanged from the ott 0.25 distribution; see
`coq/LICENSE` for copyright information.

Preparation
-----------

1. Install [opam](https://opam.ocaml.org), see
   https://opam.ocaml.org/doc/Install.html for instructions.

2. Install ott via opam:

        opam init
        opam install ott

3. If you want to re-run the coq proof, install [coq](https://coq.inria.fr/)
   version 8.3 from https://coq.inria.fr/distrib/8.3pl5/files/ (newer version
   will not work).

Creating the pdf
----------------

Running `make pdf` after installing ott will create the files `core-abs.pdf`
and `core-abs-coq.pdf`.

Creating the coq proof
----------------------

Running `make coq` after installing ott and coq will create the file
`core_abs.vo`.
