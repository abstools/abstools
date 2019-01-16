DIST=../../dist

gen: clean
	java -cp $(DIST)/absfrontend.jar JavaBackend -d gen/java abs/*.abs -product=Test  $@
	javac -cp $(DIST)/absfrontend.jar:gen/java java/*.java -d java

run:
	java -cp $(DIST)/absfrontend.jar:gen/java:java SpeedTest.Main $@

rund:
	java -Dabs.fli.debug=true -cp $(DIST)/absfrontend.jar:gen/java:java SpeedTest.Main $@

unit:
	rm -f abs/TestRunner.abs
	java -cp $(DIST)/absfrontend.jar ABSTestRunnerCompiler abs/*.abs -product=Test -o abs/TestRunner.abs
	
runt:
	java -cp $(DIST)/absfrontend.jar:gen/java:java AbsUnit.TestRunner.Main $@

clean:
	rm -rf gen
	mkdir -p gen/java
	find java -name "*.class" -exec rm -v '{}' \;
