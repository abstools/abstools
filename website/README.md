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
- [X] Implement glossary
- [ ] Implement index
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

## Deploying

To deploy into a website repository checked out below `../../abs/`,
clear that directory then run this command:

```sh
uvx --from sphinx --with sphinxcontrib-plantuml sphinx-build src/site/sphinx ../../abs/abstools.github.io
```

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

# Style checking with vale

The file `.vale.ini` in this directory contains
[vale](https://vale.sh) rules that can be used to check the language
of the website.  Vale's suggestions should not be seen as mandatory,
but can help to write our documentation with decent style.

Run `vale sync` to download the style files specified by `.vale.ini`,
and see the "Integrations" section of [the vale
documentation](https://vale.sh/docs) for setting it up in your
preferred editing environment.
