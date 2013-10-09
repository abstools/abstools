compile: always  run start

always : 
	@erl -pa ebin -noinput -eval "case make:all() of up_to_date -> halt(0); _ -> halt(1) end."

run :
	@printf '#!/usr/bin/env escript\n%%%%! -pa ebin\nmain(Arg)->runtime:start(Arg).' > run
	@chmod u+x  run

start :
	@printf '#!/bin/bash\nif [[ -n $$1 ]];then\nerl -pa ebin -s runtime startA $$1\n else\n erl -pa ebin \nfi\n' > start
	@chmod u+x  start	

clean :
	@rm -R ebin/* ||true
	@rm run || true
	@rm runM || true