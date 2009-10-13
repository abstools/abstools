# Targets to compile and test the Antlr-based ABS parser.
# GNU make required for (future versions of) target `tests'

SOURCE   = src/
LIB	 = lib/antlr-3.2.jar
COMPILED = bin/
GENERATED_SRC = gen/
JAVAC	 = javac -sourcepath $(SOURCE):$(GENERATED_SRC) -cp $(LIB):$(COMPILED) -d $(COMPILED)
ANTLR	 = java -cp $(LIB) org.antlr.Tool
PARSER	 = java -cp $(LIB):$(COMPILED) abs.frontend.parser.ABSParser


# TESTS	= $(wildcard tests/*.abs)

.PHONY : clean clean_java clean_antlr sense tests

# Fix this target with to use variable $(TESTS) and change parser
# to run on all input files, not just one 
tests: absparser
#	$(PARSER) tests/skeleton.abs
	$(PARSER) tests/trivial.abs

absparser: $(COMPILED)abs/frontend/parser/ABSLexer.class $(COMPILED)abs/frontend/parser/ABSParser.class

$(COMPILED)abs/frontend/parser/%.class : $(GENERATED_SRC)abs/frontend/parser/%.java
	mkdir -p $(COMPILED)
	$(JAVAC) $^

$(GENERATED_SRC)abs/frontend/parser/ABSLexer.java \
$(GENERATED_SRC)abs/frontend/parser/ABSParser.java: $(SOURCE)abs/frontend/parser/ABS.g
	$(ANTLR)  -fo $(GENERATED_SRC)abs/frontend/parser  $<

clean: clean_java clean_antlr

clean_java:
	@echo "Removing class files"
	@rm -rf $(GENERATED_SRC)
	@rm -rf $(COMPILED)

clean_antlr:
	@echo "Removing automatically generated Antlr files"
	@rm -rf $(GENERATED_SRC)
	@rm -rf $(COMPILED)

sense:
	@fortune
