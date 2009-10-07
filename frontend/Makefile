# Targets to compile and test the Antlr-based ABS parser.
# GNU make required for (future versions of) target `tests'

LIB	= lib/antlr-3.2.jar
CP	= .
JAVAC	= javac -cp $(LIB):$(CP)
ANTLR	= java -cp $(LIB) org.antlr.Tool
PARSER	= java -cp $(LIB):$(CP) parser.ABSParser

# TESTS	= $(wildcard tests/*.abs)

.PHONY : clean clean_java clean_antlr sense tests

# Fix this target with to use variable $(TESTS) and change parser
# to run on all input files, not just one 
tests: absparser
	$(PARSER) tests/trivial.abs

absparser: parser/ABSLexer.class parser/ABSParser.class

parser/%.class : parser/%.java
	$(JAVAC) $^

parser/ABSLexer.java parser/ABSParser.java: parser/ABS.g
	$(ANTLR) -o . $<

clean: clean_java clean_antlr

clean_java:
	@echo "Removing class files"
	@cd parser; rm -f *.class

clean_antlr:
	@echo "Removing automatically generated Antlr files"
	@rm -f *.tokens
	@cd parser; rm -f ABSParser.java ABSLexer.java *.tokens

sense:
	@fortune
