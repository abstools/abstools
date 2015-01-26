all: compile

compile:
	bin/rebar compile
	@printf '#!/usr/bin/env escript\n%%%%! -pa ebin\nmain(Arg)->runtime:run(Arg).' > run
	@chmod u+x  run
	@printf '#!/bin/bash\nerl -pa ebin -s runtime start -extra \"$$@\" \n' > start
	@chmod u+x  start	

run: compile
	./run

start: compile
	./start

clean:
	@rm -R ebin/* || true
	@rm run || true
	@rm runM || true
