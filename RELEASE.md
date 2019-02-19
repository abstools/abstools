# Release Checklist

- Run unit tests, check for fresh failing tests (compared to last
  version)

- Manually compile and run a small model on macOS, Linux, Windows

- Create docker (`make docker ; make run-collaboratory`)
  - check if all tools are installed in collaboratory (selection box non-empty)
  - check that collaboratory can start "Hello World" ABS program with Erlang simulator
  - check that "Hello World" ABS program doesn't crash SACO / CostABS in collaboratory
  - check that absc container works / was created: `docker run --rm abslang/absc:latest -h` should produce the same output as `absc -h` on the local machine

- Create vagrant (`vagrant up`) (NOTE: currently broken and probably unused)
  - check if all tools in collaboratory are installed
  - check that "Hello World" ABS program can start with Erlang simulator
  - check that "Hello World" ABS program doesn't crash SACO / CostABS

- Update `CHANGELOG.md`
  - Rename section `[Unreleased]` to `[x.y.z] - yyyy-mm-dd`
  - Add and update hyperlinks at the end of the file: update link
    definition for `[Unreleased]`to compare with the latest version,
    add line `[x.y.z]` comparing latest and previous versions
  - Add fresh section as follows:

```md
## [Unreleased]

### Added

### Changed

### Removed

### Fixed

```

- Prepare release commit

  - The release commit should include only the updated `CHANGELOG.md`

  - The first line of the commit message should be `Release version
   x.y.z`, followed by the contents of the change log for the current version

- Add release tag `vx.y.z` with the same message as the commit message.

- push release commit (`git push`) and tag (`git push --tags`)

- Send mail to `abs-announce@abs-models.org`, `abs-dev@abs-models.org`

- Check that CircleCI built and uploaded the docker images; otherwise
  build and upload (in the main `abstools` directory):
  - `docker build -t abslang/collaboratory:x.y.z .`
  - `docker push abslang/collaboratory:x.y.z`
  - `docker build -t abslang/collaboratory:latest .`
  - `docker push abslang/collaboratory:latest`
  - `docker build -t abslang/absc:x.y.z -f frontend/Dockerfile .`
  - `docker push abslang/absc:x.y.z`
  - `docker build -t abslang/absc:latest -f frontend/Dockerfile .`
  - `docker push abslang/absc:latest`

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

This commit is tagged with `vx.y.z`, with the tag message
identical to the commit message.
