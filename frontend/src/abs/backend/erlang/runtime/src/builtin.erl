-module(builtin).
-export([substr/3,currentms/0,lowlevelDeadline/0,random/1,getProductLine/0,strlen/1,toString/1,truncate/1]).


lowlevelDeadline() ->
	-1.
currentms()->
	{MS,S,MuS}=erlang:now(),	
	(MS*1000000 + S)*1000 + MuS div 1000.

substr(S,Start,Len) ->
	lists:sublist(S, Start+1, Len).

random(N)->
	random:uniform(N)-1.

strlen(S)->
	length(S).

toString(I) when is_integer(I) ->
	integer_to_list(I);
toString(F) when is_float(F)->
	float_to_list(F).

truncate(N)->
	trunc(N).



getProductLine()->
	exit("Not Implemented").