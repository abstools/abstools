PREFIX=/usr/local/bin

install:
	cabal update
# dependency of hse, also install it on global-level so it can be in the PATH
	cabal install happy-1.19.4 --bindir=${PREFIX}
	cabal install --only-dependencies
	cabal install --bindir=${PREFIX}

test:
	cabal install --only-dependencies --enable-tests
	cabal test

grammar: dist/grammar_test/TestABS
dist/grammar_test/TestABS: src/ABS.cf # run when grammar changes
	cd src; bnfc -haskell ABS.cf # generates haskell-source parser, lexer, and helper code
	cd src; mv DocABS.txt ../doc # move the generated grammar documentation
	cd src; happy -gca ParABS.y; alex -g LexABS.x # generates Haskell parse-example to parse ABS code
	@mkdir -p dist/grammar_test/
	cd src; ghc --make TestABS.hs -o ../dist/grammar_test/TestABS # compiles the Haskell parse-ABS-example

doc: 
	pandoc -t html -s README.md -o README.html	
	pandoc -t html -s doc/TODO.md -o doc/TODO.html
	pandoc -f t2t -t html -s doc/DocABS.txt -o doc/GrammarDocumentation.html

clean:
	-rm -rf dist/
	-rm -f src/ParABS.y src/LexABS.x src/TestABS.* # cleanup bnfc intermediate code
	-rm -f src/*.hi src/*.o examples/*.hi examples/*.o # remove any compiled example
	-rm -f doc/*.html

.PHONY: install test grammar clean doc
