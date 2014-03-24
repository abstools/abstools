# abs2haskell tool



## Requirements for building the compiler

The compiler itself is written
in haskell and distributed as a normal Haskell package.

Therefore you need a recent version of the standard Haskell compiler, called GHC
and the package manager of Haskell, called `Cabal`. I assume your OS distribution
can provide you with these:

- GHC (I recommend version >=7.6)
- Cabal (>=1.4)
- The `cabal-install` program. The compiler depends on other community
packages/libraries. This program will automatically fetch
and install any library dependencies.

## Building and installing the compiler

If you have the above installed then simply run inside the `abs2haskell/` directory:

~~~
make
~~~

## Running the compiler to generate Haskell code

After install the compiler, you should
have the program `abs2haskell` under your `PATH`.

Examples of running:

~~~
abs2haskell examples/BenchMaps.abs 

# An ABS program may have multiple main blocks in different modules. 
# So you have to specify in which module is the main block you want to build with

abs2haskell --main-is=examples/BenchMaps.abs examples/BenchMaps.abs 

abs2haskell examples/   # will compile all ABS files under examples directory

~~~


The compiler will generate ".hs" files for each compiled ABS module.
No other runtime system libraries and dependencies will be generated.



## Compiling the generated Haskell code to machine code (so you can run it)

~~~
ghc --make -threaded examples/BenchMaps.hs # put the generated haskell file that has the main block here
~~~

## Running the final program

~~~
./examples/BenchMaps -O # means run it on 1 core with default optimizations
./examples/BenchMaps -O +RTS -N1 # the same as the above
./examples/BenchMaps -O +RTS -N2 # run it on 2 cores
./examples/BenchMaps -O +RTS -N4 # run it on 4 cores
./examples/BenchMaps -O +RTS -NK # run it on K cores
~~~

## (Optional) Building the Grammar

You will need

- The [bnf converter](http://bnfc.digitalgrammars.com/)
- The `alex` Haskell lexer (do `cabal install alex`)
- The `happy` Haskell parser (do `cabal install happy`)

The grammar lies under `src/ABS.cf`.
After your modifications, run:

~~~
make grammar
~~~

to generate the Haskell source files.
Then you do `make`, so the abs2haskell compiler can pickup
any changes of the grammar.
