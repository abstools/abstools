compile:
	erl -pa ebin -make
	
ebin/runtime.beam : 
	erl -pa ebin -make
	
run	: ebin/runtime.beam
	erl -pa ebin
	
runM	: ebin/runtime.beam
	erl -pa ebin -s runtime start m_M
	

clean :
	rm -R ebin/* 