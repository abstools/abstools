-module(runtime).

-export([start/0,start/1,run/1]).

-define(CMDLINE_SPEC,[
					  {debug,$d,"debug",{boolean,false},"Prints debug status output"},
					  {main_module,undefined,undefined,string,"Name of Module containing MainBlock"}
					 ]).


start()->
	case init:get_plain_arguments() of
		[]->
			no_start_argument;
		Args->
			start(Args)
   end.	

start(Args) ->
	parse(Args,"start").		

run(Args) ->
	parse(Args,"run").	

parse(Args,Exec)->
	case getopt:parse_and_check(?CMDLINE_SPEC,Args) of
		{ok,{Parsed,[]}} ->
			start_mod(Parsed);
		_ ->
		  getopt:usage(?CMDLINE_SPEC,Exec)
	end.

start_mod(Arguments)  ->
    M=proplists:get_value(main_module,Arguments),
	io:format("Start ~s~n",[M]),
	Module=list_to_atom("m_"++re:replace(M,"[.]","_",[{return,list},global])),	
	eventstream:start_link(),
	case proplists:get_value(debug,Arguments) of 
		true ->	
			eventstream:add_handler(console_logger,[]);
		false ->
			ok
	end,
	eventstream:add_handler(cog_monitor,[self()]),
    Cog=cog:start(),
    R=cog:add_and_notify(Cog,main_task,[Module]),
    RetVal=task:join(R),
	cog_monitor:waitfor(),
	timer:sleep(1),
	eventstream:stop(),
	RetVal.



