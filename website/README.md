# Website

This directory contains the sources for the ABS website, including the
language reference manual.  The site is built using
[Sphinx](https://www.sphinx-doc.org/), and deployed both inside the
docker collaboratory container and at https://abs-models.org.

# TODOs

- [X] Set up basic build infrastructure
- [ ] Implement links to examples (local vs online collaboratory)
- [X] Convert Overview
- [X] Convert Getting Started
- [ ] Convert Documentation/Examples
- [ ] Convert Documentation/Tutorials
- [X] Convert ABS Language Manual
- [X] Convert Publications
- [X] Convert Contact
- [X] Convert Acknowledgments
- [ ] Implement bibliography



# References

[reStructuredText Markup Specification](https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html)

[Sphinx — Sphinx documentation](https://www.sphinx-doc.org/en/master/)

# Compiling

If Python, Sphinx are not installed: in the `abstools/` root
directory, run `./gradlew sphinx`.

If Python, Sphinx are installed: in this directory, run `sphinx-build
-M html src/site/sphinx build`

When using [uv](https://docs.astral.sh/uv/), run this command:

    uvx --from sphinx sphinx-build -M html src/site/sphinx build

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
