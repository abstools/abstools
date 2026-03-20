# Website

This directory contains the sources for the ABS website, including the
nreference manual.

# References

[reStructuredText Markup Specification](https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html)

[Sphinx — Sphinx documentation](https://www.sphinx-doc.org/en/master/)

# Compiling

If Python, Sphinx are not installed: in the `abstools/` root
directory, run `./gradlew sphinx`.

If Python, Sphinx are installed: in this directory, run `sphinx-build
-M html src/site/sphinx build`

# Document structure

See <https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html#sections>

> Normally, there are no heading levels assigned to certain characters
> as the structure is determined from the succession of
> headings. However, this convention is used in Python Developer’s
> Guide for documenting which you may follow:

- `#` with overline, for parts
- `*` with overline, for chapters
- `=` for sections
- `-` for subsections
- `^` for subsubsections
- `"` for paragraphs
