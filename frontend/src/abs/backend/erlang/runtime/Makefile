compile: always  run start

always : 
	@erl -pa ebin -noinput -eval "case make:all() of up_to_date -> halt(0); _ -> halt(1) end."

run :
	@printf '#!/usr/bin/env escript\n%%%%!  -sname master -rsh ssh -pa ebin\nmain(Arg)->runtime:run(Arg).' > run
	@chmod u+x  run

start :
	@printf '#!/bin/bash\nerl -pa ebin -s runtime start -sname master -rsh ssh  -extra \"$$@\" \n' > start
	@chmod u+x  start	

clean :
	@rm -R ebin/* ||true
	@rm run || true
	@rm runM || true