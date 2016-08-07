# compilation is in two steps: 
# 1. generate Java source from ABS: 
#      source: all ABS files in src/
#      dest:   gen/java
# 2. generate class files from (provided and generated) Java source
#      source: gen/java
#      dest:   gen/bin
#
# gen is for dynamic, gens for generated static code

BASEDIR=../..
CP=$(BASEDIR)/bin:$(BASEDIR)/lib/org.sat4j.core.jar:$(BASEDIR)/lib/org.sat4j.pb.jar:$(BASEDIR)/lib/sat4j-pb.jar:$(BASEDIR)/lib/org.sat4j.maxsat.jar:$(BASEDIR)/lib/sat4j-maxsat.jar:$(BASEDIR)/lib/ecj-3.6.2.jar:$(BASEDIR)/lib/choco-solver-2.1.1.jar

.PHONY: clean cleans gen gens


all: clean gensrc_dyn gen
static: cleans gensrc_static gens

gensrc_dyn:
	java -cp $(CP) abs.backend.java.JavaBackend -sourceonly -dynamic -d gen/java src/example.abs

gensrc_static:
	java -cp $(CP) abs.backend.java.JavaBackend -sourceonly -d gens/java src/example.abs

gen:
	javac -cp $(CP):gen/java:src gen/java/M/Main.java -d gen/bin

gens:
	javac -cp $(CP):gens/java src/*.java -d gens/bin

run:
	java -cp $(CP):gen/bin M.Main

clean:
	rm -rf gen
	mkdir -p gen/java gen/bin
cleans:
	rm -rf gens
	mkdir -p gens/java gens/bin
