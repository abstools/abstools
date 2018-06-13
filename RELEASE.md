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

- Increment version number in `frontend/build.xml` and commit with
  summary message (see below)

- Add release tag with same summary message

- push release commit (`git push`) and tag (`git push --tags`)

- Build and upload docker image:
  - `docker build -t abslang/collaboratory:<version> .`
  - `docker push abslang/collaboratory:<version>`

- Send mail to `abs-announce@abs-models.org`, `abs-dev@abs-models.org`

# Version numbering

ABS version numbers are of the form `x.y.z` (major.minor.patch).  This
is not semantic versioning (https://semver.org) since we increase the
patch number for feature and bug fix releases, increase the minor
number for backwards-incompatible changes in the language and reserve
the major version number for a redesign of the language.

## Release commit and tag message format

A new release is tagged with `version_x.y.z`, with the tag message
identical to the commit message.

The release commit contains only an increase to the version number in
`frontend/build.xml`.  The commit message contains the new version
number, followed by a paragraph listing incompatible changes (if any)
and then a bullet list describing noteworthy changes.  The same
message can be sent out as an announcement email.

Example commit message:

> Release version 1.3.5
>
> This is a backward-compatible release.  There are no
> backward-incompatible changes, existing models should run as normal.
>
> - Feature 1 ...
>
> - Feature 2 ...

