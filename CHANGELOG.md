# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).  This project does not quite adhere to [Semantic Versioning](https://semver.org/spec/v2.0.0.html) since it does not declare a public API, as mandated in the linked specification.  The patch version number increases when existing models continue running with the new version; the minor version number increases when existing models can be adapted in a straightforward way; the major version number increases when existing models need to be partially or totally rewritten.

## [Unreleased]

### Added

- The Erlang backend now supports model-specific visualization based on the Model API.  Use `absc -erlang -http-index-file ./index.html
  -http-static-dir ./my_js_libs *.abs` to add a custom `index.html` file and static resources (Javascript files, images, ...) to a model; connect a browser to the running model on the Model API port to see it rendered. Static files are available from the Model API (and from index.html) via `/static/filename`.

- The Model API now allows advancing the clock via external stimuli -- a model started with the model api (`-p 8080`) and with a clock limit (`-l x`) will stop and wait at t=`x`.  A call `/clock/advance?by=y` increases the limit by `y`, thereby waking up blocked processes.  `/clock/now` can be used to obtain the current clock value.

- The Model API now supports input parameters of type `Map<String, X>` (where `X` is a supported input type).

- Added expressions `x implements I` and `x as I` to check for and convert an object reference to another interface.  If the underlying class of `x` does not implement `I`, return `False` and `null`, respectively.

- The `let` expression can now implement multiple bindings.  `let (T1 x1) = v1, (T2 x2) = v2 in b` is a shorthand for `let (T1 x1) = v1 in let (T2 x2) = v2 in b`.  No changes to any backends and analysis tools needed since the AST was not changed -- the parser generates the same AST for the two expressions above.

- Add a `Dockerfile` to run `absc` inside a container.  To build the image, use `cd frontend ; docker build -t absc .`.  To use the container to compile an abs model `file.abs` in the current directory, use `docker run --rm -v "$PWD":/usr/src -w /usr/src absc -erlang file.abs`.  To get a command-line inside the container for the current directory (e.g., to run a model via `gen/erl/run` inside the container), use `docker run -it --rm -v "$PWD":/usr/src -w /usr/src --entrypoint /bin/sh absc`.

- ABS snippets embedded in org-mode documents via org-babel now accept lists as input.

- When complaining about duplicate variable declarations, show the location of the original declaration.

- Added an `.editorconfig` file specifying indentation etc. coding conventions.

- Added [SpotBugs](https://spotbugs.github.io) to the build process.

- Added unit tests for the Model API.

- Added Continuous Integration builds via CircleCI

### Changed

- The Model API now only accepts connections from localhost.

- Switched to the [gradle](https://gradle.org) build system for the ABS compiler.  The directory structure inside `frontend/` was extensively reorganized to conform with maven / gradle directory layout conventions.

- Modules that explicitly import something from the `ABS.StdLib` standard library will not implicitly import everything from `ABS.StdLib` anymore.  The behavior of modules not having such an import clause is unchanged, i.e., they still contain an impolicit `import * from ABS.StdLib` clause.

- In a module definition, `import` and `export` clauses can now be written in any order.

- The parentheses (`()`) around the variable type and name in `let` expressions are now optional.  I.e., instead of `let (Int x) = 4 in ...` write  `let Int x = 4 in ...`.  The old syntax is undocumented but kept for backward compatilibity.

- Avoid crash in `DC.decrementResources`: when trying to decrement by more than available, only decrement to the maximum possible.  `decrementResources` now returns the actual amount by which the resource was decreased.  This changes the signature of three methods in the `ABS.DC.DeploymentComponent` interface in a backwards-incompatible way.

- When pretty-printing to a file, the resulting file will use Unix line endings.

- An empty case expression (`Unit x = case 5 {};`) is now a syntax error instead of a type error.

### Removed

- BBEdit editor support moved to github.com/abstools/bbedit.

- Removed the old Creol-style negation operator (`~`) which was kept in the parser for backward compatibility.

### Fixed

- The unit tests now execute in parallel, speeding up testing on multi-processor machines.

- The Model API now accepts JSON integers for arguments of type `Float`.

- The unit tests now run better on Windows.

- Spurious type errors in second-order function calls under certain circumstances.

- `durationLessThan` will not claim that infinite durations are smaller than finite ones.

## [1.5.6] - 2018-06-13

### Removed

- The eclipse plugin was removed: it was tightly coupled to internals of the compiler and suffered from severe bit-rot.  A new, XText-based plugin is under development at https://github.com/mohddanish211/MasterThesis

### Fixed

- The prolog backend now generates output usable by SACO/CostABS again (broken since version 1.5.3)

## [1.5.5] - 2018-05-31

### Added

- Added the numeric type `ABS.StdLib.Float`, defined as 64-bit floating point values using the appropriate type of the backend (e.g., Erlang `float`, Java `double`).  This type can be used instead of `ABS.StdLib.Rat` for speed and “bug-compatibility” with preexisting floating-point code.

- The functions `sqrt`, `log`, `exp`, operating on floating-point values, have been added to the standard library.

- `Float` is now a supported input type for the Model API.


- The Model API can now start on a random free port; use -1 as port to enable this.  No matter how the port is specified, the port number is printed to stderr when the Model API starts.

- ABS now has some support in Emacs Org mode.  It is possible to run ABS fragments in org-babel and add their output to the org document.

### Changed

- The default visualization of deployment component CPU loads has been rewritten using the d3.js library.

## [1.5.4] - 2018-04-09

### Added

- The Model API now accept lists of supported types as input parameters.

### Changed

- When calling a method `m` via `await o!m()`, errors inside `m` will not be re-thrown caller-side anymore.  When accessing the return value (i.e., `x = await o!m()`) any errors inside `m` will be re-thrown caller-side as before.

### Fixed

- The toolchain now compiles on Java 8 or later.

- An interaction between the CloudProvider class and the Model API was fixed. Models using the CloudProvider class will not experience spurious time advances any more when controlled through the Model API.

- Simulating models using the Erlang backend on Windows has been fixed.

- Many other bug fixes, fewer unit test failures.

## [1.5.3] - 2018-01-22

### Added

- ABS now has second-order functions (unfolded at compile-time) and anonymous function syntax (valid only as arguments to second-order functions), making it possible to use many idioms from functional programming while keeping the language semantics as-is and not breaking any analysis tools.  Thanks to Björn Petersen and Eduard Kamburjan.

- Add `map`, `filter`, `foldl`, `foldr` to the standard library.

- Add `entries` function for maps, returning the list of (key, value) pairs contained in the map.

- Compiled models can now output the compiler version which was used to compile them.  Use `gen/erl/run -v` for the erlang backend.

- Added `/quit` to the Model API, which immediately shuts down a running model (to be used mainly for unit testing).

- Case statements have been implemented in the old Java backend.

### Changed

- Pretty-print list literals using the `list[...]` syntax instead of as nested `Cons` constructor expressions.

### Fixed

- Remove some superfluous output from pretty-printing modules.

- The number of deprecation warnings issued by erlang while compiling the toolchain has been reduced.

## [1.5.2] - 2017-11-13

### Added

- Implement parameters for exceptions.

- Implement deadlines in the Erlang backend

- Implement user-defined schedulers in the Erlang backend

- Add a function `elements` that returns a list of the elements of a given set.

- For multicore machines: add a runtime parameter to models compiled with the erlang backend to restrict the number of schedulers (threads) to use.

### Changed

- Syntactic convenience: allow more than one annotation in a pair of brackets (`[]`), separated by commas.  Annotations can still be written next to each other in a pair of brackets each.

## [1.5.1] - 2017-07-24

### Added

- A new `foreach` loop was added that iterates through a list.

### Changed

- Minimum Erlang version is now 19 (version 20 is the latest release).

- Methods marked `[Atomic]` can now asynchronously call methods, as long as they do not interact with the resulting future via `await` or `get`.

### Fixed

- The size of the `absfrontend.jar' package is reduced by 40%.

- Documentation fix: ABS supported annotating variables and fields as `[Final]`. Document this.

- Repair delta parameter application (bug #159)

## [1.5.0] - 2017-07-13

### Added

- Remove silent truncation of `Rat` to `Int`.  Models relying on automatic truncation of rationals will fail during compilation with type errors.

- A new `takeMaybe` function for easier iteration through sets

- More Productline and Delta type checking

- Better output from `absc -help`, including instructions on how to note the exact version of the toolchain used.

### Changed

### Removed

### Fixed

- Re-using an imported identifier in a definition (e.g., `import foo from M; def foo = ...`) now leads to a compile-time error.  Such redefinitions used to be silently ignored, leading to subtle errors.

- The compiler frontend can now handle very long literal lists without stack overflow.

## [1.4.2] - 2017-04-12

### Added

- The Model API, a way of interacting with a running model via HTTP requests, is now documented.  More ABS datatypes can now be output as JSON instead of via converting them to strings.  The old version of the model API is still available by prepending `v1/` to the URI path of all requests.

- Software product lines are now type-checked.

- We now support identically-named accessor functions for different constructors of the same datatype, provided that the named members of each constructor have the same type.

- Exceptions have been implemented.

- Deprecation warnings have been added to the frontend.

- The Emacs mode now handles multi-file models properly by finding the definitions of modules imported in the current file in abs files in the same directory.

- A function `isSubset` has been added.

- The functions `numerator`, `denominator` have been added for rational numbers.

- A simple code coverage information generation facility has been added for the Erlang backend.  See the description of the `-cover` parameter in the output of `absc -help`.  Coverage can be used e.g. with Emacs [cov-mode](https://github.com/AdamNiederer/cov).

### Changed

- When no branch matches in a case expression or case statement, a `PatternMatchFailException` is thrown.  (This used to be undefined behaviour.)

- Implicit truncation of rational numbers is now deprecated.

- Directly using the constructors `EmptySet', `Insert', `EmptyMap', `InsertAssoc' is now deprecated.  When using sets and maps, the functions in the standard library (`lookup', `contains', `take', etc.) should be used.

- ABS lists are now implemented directly as Erlang lists in the Erlang backend.

- The cog scheduler now uses semi-random scheduling of its tasks, avoiding livelock in rare circumstances.

- Backend-specific messages (i.e., output not generated by the model via `println`) are now printed to stderr.

### Fixed

- Simulation speed has been increased greatly by not running the garbage collector every 100ms.  When a collection occurs it will take more time, but overall execution time will decrease.  Many models will run to completion without garbage collection.

- A rare occurrence of premature time advance has been fixed.

- An issue with time advancing when using the model api has been fixed.

## [1.4.1] - 2016-11-22

## [1.4.0] - 2016-09-30



[Unreleased]: https://github.com/abstools/abstools/compare/version_1.5.6...HEAD
[1.5.6]: https://github.com/abstools/abstools/compare/version_1.5.5...version_1.5.6
[1.5.5]: https://github.com/abstools/abstools/compare/version_1.5.4...version_1.5.5
[1.5.4]: https://github.com/abstools/abstools/compare/version_1.5.3...version_1.5.4
[1.5.3]: https://github.com/abstools/abstools/compare/version_1.5.2...version_1.5.3
[1.5.2]: https://github.com/abstools/abstools/compare/version_1.5.1...version_1.5.2
[1.5.1]: https://github.com/abstools/abstools/compare/version_1.5.0...version_1.5.1
[1.5.0]: https://github.com/abstools/abstools/compare/version_1.4.2...version_1.5.0
[1.4.2]: https://github.com/abstools/abstools/compare/version_1.4.1...version_1.4.2
[1.4.1]: https://github.com/abstools/abstools/compare/version_1.4.0...version_1.4.1
