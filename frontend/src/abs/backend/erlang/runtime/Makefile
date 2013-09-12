compile:
	erl -pa ebin -make
	
ebin/runtime.beam : 
	erl -pa ebin -make
	
run	: ebin/runtime.beam
	erl -pa ebin

clean :
	rm -R ebin/* 