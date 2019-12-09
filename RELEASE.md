# Release Checklist

If possible, perform the release using Java 8, since jars generated with later
versions (e.g., Java 11) cannot be used with earlier versions.

## Pre-release checks

- Run unit tests, check for fresh failing tests (compared to last
  version)

- Run unit tests for erlang backend with pessimal gc (replace body of `gc:is_collection_needed/1` with `true`)

- Manually compile and run a small model on macOS, Linux, Windows

- Create docker (`make docker ; make run-collaboratory`)
  - check if all tools are installed in collaboratory (selection box non-empty)
  - check that collaboratory can start "Hello World" ABS program with Erlang simulator
  - check that "Hello World" ABS program doesn't crash SACO / CostABS in collaboratory
  - check that absc container works / was created: `docker run --rm abslang/absc:latest -h` should produce the same output as `absc -h` on the local machine

- Check that the manual looks ok

  - `open abs-docs/build/asciidoc/html5/index.html`

## Release steps

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

  - The release commit should include only the updated `CHANGELOG.md`.

  - The first line of the commit message should be `Release version
   x.y.z`, followed by the contents of the change log for the current version,
    with the headline markers `#` replaced by stars `*` (since the
    # symbol denotes comments in a github commit message).

- Add release tag `vx.y.z` with the same message as the commit message.

- run `./gradlew clean ; ./gradlew assemble` to update version information for
  compiler, manual
  
  - check the output of `absc -V`; it should output the new version number
  - check the header of `abs-docs/build/docs/asciidoc/index.html`, it should
    contain the new version number
  - build with maven (`cd abs-docs ; mvn install`) and check that
    `abs-docs/target/html/index.html` looks correct
    (http://docs.abs-models.org still uses this build system)

- push release commit (`git push`)

- update [https://abs-models.org/manual/]: copy the content of
    `abs-docs/build/asciidoc/html5/` into the `static/manual/` subdirectory of
    the repository at [https://github.com/abstools/abs-models.org], then
    redeploy the website

- finalize release on github (automating these steps would involve handling
  github API keys, so we keep it manual.)

  - push release tag (`git push --tags`) -- this will start the CircleCI
    docker build, which will download the new version of the website created
    in the previous step

  - upload `absfrontend.jar`: go to
    [https://github.com/abstools/abstools/releases/tag/vx.y.z].  Click "Edit
    Tag", drag `frontend/dist/absfrontend.jar` into the area that says "Attach
    binaries by dropping them here or selecting them." and wait until the
    upload is complete.  Click the green "Finalize Release" button.

## Post-release steps

- Send mail to `abs-announce@abs-models.org`, `abs-dev@abs-models.org`

- Build and upload the docker images:
  - `docker build -t abslang/collaboratory:x.y.z -f docker/collaboratory.Dockerfile .`
  - `docker build -t abslang/collaboratory:latest -f docker/collaboratory.Dockerfile .`
  - `docker build -t abslang/absc:x.y.z -f docker/absc.Dockerfile .`
  - `docker build -t abslang/absc:latest -f docker/absc.Dockerfile .`
  - `docker push abslang/collaboratory:x.y.z`
  - `docker push abslang/collaboratory:latest`
  - `docker push abslang/absc:x.y.z`
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
