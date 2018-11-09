# Release Checklist

- Run unit tests, check for fresh failing tests (compared to last
  version)

- Create docker (`make docker`)
  - check if all tools are installed (selection box non-empty)
  - check that "Hello World" ABS program can start with Erlang simulator
  - check that "Hello World" ABS program doesn't crash SACO / CostABS

- Create vagrant (`vagrant up`)
  - check if all tools in collaboratory are installed
  - check that "Hello World" ABS program can start with Erlang simulator
  - check that "Hello World" ABS program doesn't crash SACO / CostABS

- Release commit should include only an updated `CHANGELOG.md`, with a
  fresh `## [Unreleased]` section containing the four subsections `###
  Added`, `### Changed`, `### Removed`, `### Fixed`.  The first line
  of the commit message should be `Release version x.y.z`, followed by
  the changelog.

- Add release tag `version_x.y.z` with the same message.

- push release commit (`git push`) and tag (`git push --tags`)

- Check that CircleCI built and uploaded the docker images; otherwise
  build and upload via
  - `docker build -t abslang/collaboratory:<version> .`
  - `docker push abslang/collaboratory:<version>`

- Send mail to `abs-announce@abs-models.org`, `abs-dev@abs-models.org`

# Version numbering

ABS version numbers are of the form `x.y.z` (major.minor.patch).  This
is not quite semantic versioning (https://semver.org): we increase the
patch number for feature and bug fix releases (i.e., existing models
continue running), increase the minor number for
backwards-incompatible changes in the language (i.e., existing models
need to be adapted in straightforward ways) and increase the major
version number for major redesigns of the language (i.e., existing
models need to be re-implemented).

## Release commit and tag message format

The release commit contains only an updated `CHANGELOG.md`, with the commit message the same as the contents of `CHANGELOG.md` for the current release.

This commit is tagged with `version_x.y.z`, with the tag message
identical to the commit message.
