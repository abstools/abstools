BASEDIR=../..
CP=$(BASEDIR)/bin:$(BASEDIR)/lib/beaver-rt-src.jar:$(BASEDIR)/lib/beaver-rt.jar:$(BASEDIR)/lib/org.sat4j.core.jar:$(BASEDIR)/lib/org.sat4j.pb.jar:$(BASEDIR)/lib/sat4j-pb.jar:$(BASEDIR)/lib/org.sat4j.maxsat.jar:$(BASEDIR)/lib/sat4j-maxsat.jar:$(BASEDIR)/lib/ecj-3.6.2.jar:$(BASEDIR)/lib/choco-solver-2.1.1.jar



all: clean gensrc_dyn gen
static: clean gensrc_static gen

gensrc_dyn:
	java -cp $(CP) abs.backend.java.JavaBackend -sourceonly -dynamic -d gen/java lib/meta.abs src/*.abs

gensrc_static:
	java -cp $(CP) abs.backend.java.JavaBackend -sourceonly -d gen/java lib/meta.abs src/*.abs

gen:
	javac -cp $(CP):lib/java lib/java/*.java -d gen


clean:
	rm -rf gen
	mkdir -p gen/java
