all:
	cabal --enable-tests install

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.hi *.o *.bak
	-rm -f DocABS.ps

grammar:
	# run when grammar changes
	cd src; bnfc -haskell ABS.cf; happy -gca ParABS.y; alex -g LexABS.x
	# latex DocABS.tex ; dvips DocABS.dvi -o DocABS.ps
	mkdir -p dist/grammar_test/
	cd src; ghc --make TestABS.hs -o ../dist/grammar_test/TestABS
