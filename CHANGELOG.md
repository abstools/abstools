# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).  This project does not quite adhere to [Semantic Versioning](https://semver.org/spec/v2.0.0.html) since it does not declare a public API, as mandated in the linked specification.  The patch version number increases when existing models continue running with the new version; the minor version number increases when existing models can be adapted in a straightforward way; the major version number increases when existing models need to be partially or totally rewritten.


## [Unreleased]

### Added

### Changed

- Refactor the Java backend to use Java-native datatypes directly
  instead of wrapping them inside subclasses of `ABSDataType`:
  - Instead of `ABSString`, use `java.lang.String`
  - Instead of `ABSBool`, use `java.lang.Boolean`
  - Instead of `ABSFloat`, use `java.lang.Double`
  - Instead of `ABSRational`, use `org.apfloat.Aprational`
  - Instead of `ABSInteger`, use `org.apfloat.Apint`
  - Implement ABS user-defined datatypes as Java `record`s

- Refactor the Java backend to use Java 21 pattern matching and record
  patterns to implement the ABS `switch` statement and `case`
  expression instead of creating temporary matcher and binding
  objects.  This should significantly reduce memory use when
  processing large datastructures.

### Removed

### Fixed

Various bug fixes and minor refactorings.

## [1.10.0] - 2024-11-07

### Added

- Added a new expression `destiny` that evaluates the future of the currently-running task.  Since a synchronous method call does not get a new future but has a different return type, `destiny` is typed with the new type `Destiny`, a supertype of all futures.

- Added a new process attribute `destinyOf`, useful in user-defined schedulers, that returns the future of a given process.

- In the Erlang and Java backends, it is now possible to define functions that read from a provided SQLite database: `def List<Int> f(Int x) = builtin(sqlite3, "db.sqlite3", "SELECT i FROM t where i > ?", x);` will return a list of numbers greater than `x` in table `t` of database file `db.sqlite3`.  See the manual for more details.

- The Java backend now supports Timed ABS, as documented in Chapter 13 of the manual.

- The Java backend now supports deployment and resource modeling, as documented in Chapter 15 of the manual.

- The Java backend now supports the Model API, as documented in Chapter 12 of the manual.

- The scripts `absc` and `absc.bat` to invoke the compiler are now located in `frontend/bin/`.  For backwards compatibility, the old script locations still work.

- The java backend can now optionally generate a jar file that can run the compiled model stand-alone:
  - Use the `-o` option to create a jar file, e.g., `absc -j model.abs -o model.jar` (by default, no jar file is generated).
  - To run the generated jar file, use `java -jar model.jar`.
  - Regardless of the presence of this option, the Java backend will always generate Java source and class files in the `gen/` subdirectory (or the directory specified via `-d`).

### Changed

- The toolchain now requires Java 21, Erlang >= 26 and a C compiler.

- The Java backend now requires the full JDK (Java Development Kit) to be installed when compiling ABS models; previously it was possible to run the tool chain after installing only the JRE (Java Runtime Environment).  (Note that the JDK was always required to compile the toolchain itself.)

- The Java backend now compiles generated files with the same version of the Java compiler that the backend itself was compiled with; previously, the Java backend was hardcoded to use Java 1.5 as compilation target.

- The syntax for command-line options was changed for models generated via the Java backend: instead of, e.g., `--randomseed=1234` use `--randomseed 1234`.

- Minor incompatible change: the built-in function `toString` now encloses String arguments for user-defined datatypes in quotes.  I.e., for a datatype `data Person = Person(String, Int);`, the expression `toString(Person("Joe", 20))` produces `Person("Joe",20)` instead of `Person(Joe,20)`.  For string values, `toString` produces the string without quotes as before.

### Fixed

- Removed the one-minute timeout for the Model API (calls to the Model API used to abort with a 5xx error code after one minute, no matter whether the associated task was still running and would have completed successfully).

## [1.9.3] - 2021-05-18

### Changed

- The toolchain now requires Java 11 or later.

- The long command-line parameter names for recording and replaying traces have been changed from `--dump-trace` and `--replay-trace` to `--record` and `--replay`.  The short parameters stay the same (`-t` and `-r`, respectively).

### Fixed

- Performance improvement in the Erlang backend: a cog with multiple processes awaiting on Boolean conditions now evaluates all guards in-process instead of delegating to the waiting processes.  This results in better performance -- observed an improvement factor of 3-9 and utilization of a single core (as opposed to  running on all cores of a multi-core machine) in microbenchmarks involving a single cog.

## [1.9.2] - 2021-03-02

### Added

- A first version of location type checking has been added.  This feature is currently not enabled by default.  Use the parameter `--loctypecheck` to enable location type checking.  Location types express whether a given object resides on the same cog as the current object; see Section 3.6 in the manual for initial documentation.

### Fixed

- A deadlock in the erlang backend has been fixed. https://github.com/abstools/abstools/issues/276

## [1.9.1] - 2020-11-27

### Added

- The `foreach` loop now supports an optional second variable of type `Int` that is bound to the index of the current element, starting at zero.  The syntax is `foreach (elem, index in list) { ... }`.

- The second argument to the `duration` statement and `await duration` guard is now optional; `duration(x)` has the same behavior as `duration(x, x)`.

- The new function `ms_since_model_start` returns an integer containing the elapsed time in milliseonds since the model was started.

- The Model API in the erlang backend can now export its endpoints with a link prefix via the `--url-prefix` command-line option.

### Changed

- Function calls in the erlang backend are significantly faster via removal of the check for pending garbage collection at the beginning of each function body.  (Note that garbage collection, i.e., removing unreferenced futures and objects, happens rarely, so the increased waiting time when entering gc caused by this change is not paid often.)

- The import/export combination `import A from OtherModule; export A;` now issues a compile-time warning instead of an error.  This keeps older models running while announcing the upcoming change.  (The same construct is an error in the xtext branch already.)

- In the Erlang backend, wall-clock time for running a model is now reported in milliseconds instead  of microseconds.  (To see the elapsed time, start the model with parameter `-v`.)

### Fixed

- Fixed a potential hang in the erlang backend.

## [1.9.0] - 2020-03-25

### Added

- Methods can now be annotated `[Readonly]`.  Attempting to assign values to object fields in such a method will lead to a compile-time error.

- The `case` pattern matching expresssion now uses bars (`|`) to separate case branches.  The older syntax (`;` to terminate each branch) is still supported by the parser and will not lead to compile-time warnings.

### Changed

- Incompatible change: deltas now share a namespace with modules.  Defining a delta with the same name as a module results in a compile-time error.

- Incompatible change: the plain export clause `export Name;` will only export `Name` if `Name` is defined in the current module; use `export Name from OtherModule;` to re-export `Name` imported from `OtherModule`.  This mirrors the behavior of `export *;` vs. `export * from OtherModule;`.

- Future incompatible change: the pattern matching statement now uses the keyword `switch` instead of `case`.  The pattern matching expression uses `case` as before.  Using the old syntax emits a compile-time warning.

- Future incompatible change: The conditional expression now uses the keyword `when` instead of `if`.  The conditional statement uses `if` as before.  Using the old syntax emits a compile-time warning.

- Method implementations can now be annotated `[Atomic]` when the corresponding interface declaration is not annotated atomic.

### Fixed

- Multiple `uses` clauses in a delta now cause a compilation failure.  The parser used to accept deltas with more than one `uses` clause, but all clauses except the first were silently ignored.

- It is now possible to use qualified names in `adds` module modifier clauses in deltas.

- The error message when using a simple name in a modifier clause in a delta without a `uses` clause has been clarified.

## [1.8.2] - 2019-12-09

### Added

- All interfaces without `extends` clause now extend the interface `ABS.StdLib.Object`.  This means that it is always possible to write `Object o = x;` with  `x` being a reference to an object of any class.

- All classes without `implements` clause now implement the interface `ABS.StdLib.Object`.  This means that for all object references `x`, `x implements Object` will evaluate to true and `x as Object` will not evaluate to `null`.

- Field values can now be passed to custom scheduling functions in the erlang backend.  See Section 14.3 of the language manual for details.

- The erlang backend can now record and replay traces of scheduling decisions; see the output of `gen/erl/run -h`, specifically the `--dump-trace` (`-t`) and `--replay-trace` (`-r`) options.  The trace of the currently running model is also available via the Model API: a model started with `-p 8080` will make its trace available at `https://localhost:8080/trace`.

- The `abslang/absc` docker image now includes the analysis tools `apet`, `cofloco`, `costabs`, `maypar`, `pubs` and `syco`, which only support the linux platform but can now be run inside the container on other platforms.

- The erlang backend now supports a new runtime parameter `-v`, `--verbose` for more output when starting a model.  A model started with `-v` will print the module name, the port of the model api (if any), and a summary of elapsed time upon termination.

### Changed

- Incompatible change: A partial function must have a non-empty list of function parameters.  This change comes with no loss of functionality since a partial function without functional parameters is strictly the same function as a non-partial function with identical (non-function) argument list and body.

- Resource consumption (via `Cost` annotations) in the erlang backend is now more deterministic: once the deployment component gives part of the requested resources to a cog, it will not give resources of the same type to another cog until the first request has been fulfilled.

- Models in the erlang backend do not print the module and model api port anymore unless started with the `-v` or `--verbose` parameter.

- The short-form parameter for printing information about the compiler version in the erlang backend changed from `-v` to `-V`.  The long-form parameter (`--version`) is unchanged.

### Removed

- The Maude backend is now deprecated, and the unit tests have been deactivated.  (This change was briefly discussed during the 2019 ABS workshop, with no dissenting opinions.)

### Fixed

- Fixed a cause of spurious clock advances under rare circumstances (~1.5% chance of occurrence when calling empty methods on one cog only, see test case `TimeTests.no_time_advance1`).

- The build command `./gradlew build`, which runs unit tests, now terminates with a non-zero exit code when a unit test fails.  (To build without running unit tests, use `./gradlew assemble`.)

## [1.8.1] - 2019-03-28

### Added

- The collaboratory (available as a container via the docker hub) now includes the abs website, including the language manual and the tutorials.

### Fixed

- Upgraded the collaboratory container from php 5.6 (unsupported) to php 7.3

- Fixed the SACO and MHP tools in the collaboratory by introducing a backward-compatibility shim class `abs.backend.prolog.PrologBackend`.

## [1.8.0] - 2019-03-26

### Changed

- Incompatible change: we now follow the [POSIX](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html) and [GNU](https://www.gnu.org/prep/standards/html_node/Command_002dLine-Interfaces.html) recommendations for command line interfaces.  This breaks existing scripts until they are adapted for the new syntax.  (This change comes with extensive refactoring and simplifying of our frontend code, making future extensions of the `absc` command much more straightforward.  We hope the one-time pain is worth it.)

- Incompatible change: all options related to software product line checking have been moved to their own subcommand, see the output for `absc checkspl -h`.  The new syntax clarifies that these calculations are not meant to be done at the same time as compiling a model.

- On the erlang backend, `await f?;` now unconditionally suspends the process instead of continuing when the future `f` is ready.  This change makes the semantics more uniform; specifically, the number and location of suspension points of a given process does not depend on whether another process has finished.  (In other words, whether a process would suspend or not was a race condition with another process before this change.)

## [1.7.0] - 2019-02-19

### Added

- Added template strings to the language.  Template strings are multi-line strings delimited by single backtick (<tt>\`</tt>) characters.  Pure expressions embedded in template strings (delimited by `$...$`) are evaluated and their values spliced into the template string.
  - It was decided not to use Javascript notation (`${...}`) for embedded expressions -- otherwise the lexer would need to be stateful in order to distinguish a closing brace `}` inside and outside a template string.  This also keeps us open for a transition to other, less powerful parsing frameworks like Xtext.

### Changed

- Incompatible change: The function `toString`, when passed a string, returns the string unchanged instead of surrounding it with quotes (`"`) and escaping embedded quotes (`"` → `\"`).
  - Note that escaping quotes inside strings was only implemented in the erlang backend; the other backends returned the string unchanged but surrounded by quotes.

- Release tags are now in the format `vx.y.z` instead of `version_x.y.z` to improve compatibility with inflexible tools.

### Removed

- Removed the Emacs mode from the repository.  Emacs editing support should be installed via its package manager -- see https://github.com/abstools/abs-mode for instructions.

### Fixed

- Starting models on windows via `gen\erl\run.bat` works again.

- Models on windows now accept command-line parameters -- see the output of `gen\erl\run.bat -h`.

## [1.6.0] - 2019-01-29

### Added

- The Erlang backend now supports model-specific visualization based on the Model API.  Use `absc -erlang -http-index-file ./index.html -http-static-dir ./my_js_libs *.abs` to add a custom `index.html` file and static resources (Javascript files, images, ...) to a model; connect a browser to the running model on the Model API port to see it rendered. Static files are available from the Model API (and from index.html) via `/static/filename`.

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

- The command-line option `-nostdlib` was removed.  This never produced models that were runnable, and tools which don't handle the standard library  can ignore any modules starting with `ABS.`.  To make sure that a given module does not inadvertently use the standard library, import an identifier (e.g., `import Unit from ABS.StdLib`) and do not use that imported identifier inside the module.

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


[Unreleased]: https://github.com/abstools/abstools/compare/v1.10.0...HEAD
[1.10.0]: https://github.com/abstools/abstools/compare/v1.9.3...v1.10.0
[1.9.3]: https://github.com/abstools/abstools/compare/v1.9.2...v1.9.3
[1.9.2]: https://github.com/abstools/abstools/compare/v1.9.1...v1.9.2
[1.9.1]: https://github.com/abstools/abstools/compare/v1.9.0...v1.9.1
[1.9.0]: https://github.com/abstools/abstools/compare/v1.8.2...v1.9.0
[1.8.2]: https://github.com/abstools/abstools/compare/v1.8.1...v1.8.2
[1.8.1]: https://github.com/abstools/abstools/compare/v_1.8.0...v1.8.1
[1.8.0]: https://github.com/abstools/abstools/compare/v_1.7.0...v1.8.0
[1.7.0]: https://github.com/abstools/abstools/compare/version_1.6.0...v1.7.0
[1.6.0]: https://github.com/abstools/abstools/compare/version_1.5.6...version_1.6.0
[1.5.6]: https://github.com/abstools/abstools/compare/version_1.5.5...version_1.5.6
[1.5.5]: https://github.com/abstools/abstools/compare/version_1.5.4...version_1.5.5
[1.5.4]: https://github.com/abstools/abstools/compare/version_1.5.3...version_1.5.4
[1.5.3]: https://github.com/abstools/abstools/compare/version_1.5.2...version_1.5.3
[1.5.2]: https://github.com/abstools/abstools/compare/version_1.5.1...version_1.5.2
[1.5.1]: https://github.com/abstools/abstools/compare/version_1.5.0...version_1.5.1
[1.5.0]: https://github.com/abstools/abstools/compare/version_1.4.2...version_1.5.0
[1.4.2]: https://github.com/abstools/abstools/compare/version_1.4.1...version_1.4.2
[1.4.1]: https://github.com/abstools/abstools/compare/version_1.4.0...version_1.4.1
