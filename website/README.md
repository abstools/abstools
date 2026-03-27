# Website

This directory contains the sources for the ABS website, including the
language reference manual.  The site is built using
[Sphinx](https://www.sphinx-doc.org/), and deployed both inside the
docker collaboratory container and at https://abs-models.org.

# TODOs

- [X] Set up basic build infrastructure
- [X] Implement links to examples (local vs online collaboratory)
- [X] Convert Overview
- [X] Convert Getting Started
- [X] Convert Documentation/Examples
- [ ] Convert Documentation/Tutorials
- [X] Convert ABS Language Manual
- [X] Convert Publications
- [X] Convert Contact
- [X] Convert Acknowledgments
- [ ] Implement bibliography


# References

[reStructuredText Markup Specification](https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html)

[Sphinx — Sphinx documentation](https://www.sphinx-doc.org/en/master/)

[PlantUML sequence diagrams](https://plantuml.com/sequence-diagram)

# Compiling

- Using gradle: in the `abstools/` root directory, run `./gradlew
  sphinx`.

- Using [uv](https://docs.astral.sh/uv/): in this directory, run `uvx
  --from sphinx --with sphinxcontrib-plantuml sphinx-build -M html
  src/site/sphinx build`

- Using Python: Install sphinx, sphinxcontrib-plantuml, then in this
  directory run `sphinx-build -M html src/site/sphinx build`

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

# Collaboratory links

To build for local collaboratory: `SPHINX_COLLABORATORY=true
sphinx-build -M html src/site/sphinx build`

```rst
:eifilelink:`/collaboratory/examples/Deadlock/BOL/uglyChain.abs`
:eifilelink:`uglyChain </collaboratory/examples/Deadlock/BOL/uglyChain.abs>`
:eifilelink:`/collaboratory/examples/Deadlock/BOL/uglyChain.abs|app=dsa`
:eifilelink:`uglyChain </collaboratory/examples/Deadlock/BOL/uglyChain.abs|app=dsa>`
```

```md
{eifilelink}`/collaboratory/examples/Deadlock/BOL/uglyChain.abs`
{eifilelink}`uglyChain </collaboratory/examples/Deadlock/BOL/uglyChain.abs>`
{eifilelink}`/collaboratory/examples/Deadlock/BOL/uglyChain.abs|app=dsa`
{eifilelink}`uglyChain </collaboratory/examples/Deadlock/BOL/uglyChain.abs|app=dsa>`
```
