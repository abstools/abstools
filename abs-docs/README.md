The ABS manual
==============

Run `make` inside this directory build the ABS manual.  (You need to have
maven installed.)  The generated files are in the `target/` subdirectory.
Currently we generate the manual in html, docbook, pdf and epub3 formats.

See http://asciidoctor.org/docs/asciidoc-syntax-quick-reference/ and
http://asciidoctor.org/docs/asciidoc-writers-guide/ for documentation on the
AsciiDoc syntax.

The subdirectory `ReferenceManual/` contains the old Latex-based manual.  It is
starting to become outdated but parts are still valuable and/or more
exhaustive than the new manual.  Transfer of these parts into the new manual
is ongoing.

The subdirectory `Ott/` contains a formal grammar and type system of ABS.
