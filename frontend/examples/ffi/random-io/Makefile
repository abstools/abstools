DIST=../../../dist

gen: clean
	java -cp $(DIST)/absfrontend.jar org.abs_models.backend.java.JavaBackend -d gen/java abs/*.abs $@
	javac -cp $(DIST)/absfrontend.jar:gen/java java/*.java -d java

run:
	java -cp $(DIST)/absfrontend.jar:gen/java:java Test.Main $@

rund:
	java -Dabs.fli.debug=true -cp $(DIST)/absfrontend.jar:gen/java:java Test.Main $@

clean:
	rm -rf gen
	mkdir -p gen/java
	find java -name "*.class" -exec rm -v '{}' \;
