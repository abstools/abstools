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

3. If you want to re-run the coq proof, install [coq](https://coq.inria.fr/).

Creating the pdf
----------------

Running `make pdf` after installing ott will create the files `core-abs.pdf`
and `core-abs-coq.pdf`.

Creating the coq proof
----------------------

Running `make coq` after installing ott and coq will create the file
`core_abs.v` containing Coq versions of definitions in `core-abs-simplified.ott`.

To use the generated files you will need Ott for Coq.
Install via opam:

    opam repo add coq-released https://coq.inria.fr/opam/released
    opam install coq-ott

You can then typecheck `core_abs.v` with

    coqc core_abs.v
    
Note: this will emit several warnings if you are on a newer version than Coq 8.12.
