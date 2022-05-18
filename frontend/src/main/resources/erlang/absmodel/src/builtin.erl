%%This file is licensed under the terms of the Modified BSD License.
-module(builtin).
%%All builtin functions
%%Must correspond to the set in GenerateErlang.jadd:FnApp.ERLANG_BUILTINS
-include_lib("../include/abs_types.hrl").
-export([currentms/1,ms_since_model_start/1,getProductLine/1,lowlevelDeadline/1,print/2,println/2,strlen/2,substr/4,thisDC/1,toString/2]).
-export([random/2,truncate/2,numerator/2, denominator/2]).
-export([float/2, rat/2, floor/2, ceil/2, sqrt/2, log/2, exp/2]).

-export([method/2, destinyOf/2, arrival/2, proc_deadline/2]).


lowlevelDeadline(_Cog) ->
    TaskInfo = get(task_info),
    case TaskInfo#task_info.proc_deadline of
        dataInfDuration -> -1;
        {dataDuration, Amount} ->
            Now=clock:now(),
            {dataTime, CreateTime} = TaskInfo#task_info.creation,
            %% negative result means infinite deadline so we clamp with 0 -
            %% this is arguably wrong but was demanded by the Java backend :(
            _Deadline=rationals:max(rationals:sub(Amount, rationals:sub(Now, CreateTime)), 0)
    end.

currentms(_Cog)->
    clock:now().

ms_since_model_start(_Cog) ->
    clock:time_since_model_start().

substr(_Cog, B, Start, Len) ->
    iolist_to_binary(substr_bin(B, Start, Len, [])).

substr_bin(_, 0, 0, Acc) ->
    lists:reverse(Acc);
substr_bin(<<C/utf8, Rest/binary>>, 0, Len, Acc) ->
    substr_bin(Rest, 0, Len - 1, [C | Acc]);
substr_bin(<<_/utf8, Rest/binary>>, Start, Len, []) ->
    substr_bin(Rest, Start - 1, Len, []).


%% https://medium.com/@jlouis666/erlang-string-handling-7588daad8f05
strlen(_Cog, B) ->
    strlen_bin(B, 0).
strlen_bin(<<>>, K) -> K;
strlen_bin(<<_/utf8, Rest/binary>>, K) -> strlen_bin(Rest, K+1).

random(_Cog,N)->
    rand:uniform(N)-1.


constructorname_to_string(A) ->
    String = atom_to_list(A),
    %% Constructor names start with "data".  Sometimes we get passed other
    %% symbols; pass these through unmolested.
    Realname = case string:str(String, "data") == 1 of
                   true -> lists:nthtail(4, String);
                   false -> String
               end,
    list_to_binary(Realname).


abslistish_to_iolist(Cog, Cons, Emp, {Cons, H, Emp}) ->
    toString(Cog, H);
abslistish_to_iolist(Cog, Cons, Emp, {Cons, H, T={Cons, _, _}}) ->
    [toString(Cog, H), ", ", abslistish_to_iolist(Cog, Cons, Emp, T)];
abslistish_to_iolist(Cog, Cons, _Emp, {Cons, H, T}) ->
    [toString(Cog, H), ", ", toString(Cog, T)].


toString(_Cog, true) -> <<"True"/utf8>>;
toString(_Cog, false) -> <<"False"/utf8>>;
toString(_Cog,I) when is_float(I) ->
    list_to_binary(mochinum:digits(I));
toString(_Cog,I) when is_integer(I) ->
    integer_to_binary(I);
toString(_Cog,{N,D}) when is_integer(N),is_integer(D)->
    {N1, D1} = rationals:proper({N, D}),
    case D1 of
        1 -> integer_to_binary(N1);
        _ -> iolist_to_binary([integer_to_binary(N1), <<"/"/utf8>>, integer_to_binary(D1)])
    end;
toString(_Cog,S) when is_binary(S) -> S;
toString(_Cog, null) -> <<"null"/utf8>>;
toString(_Cog,A) when is_atom(A) -> constructorname_to_string(A);
toString(Cog,P) when is_pid(P) ->
    Status=future:poll(P),
    case Status of
        true -> Value=future:get_after_await(P, Cog),
                iolist_to_binary([pid_to_list(P), ":", toString(Cog, Value)]);
        false -> iolist_to_binary([pid_to_list(P), ":empty"])
    end;
toString(_Cog,O=#object{cog=Cog,oid=Oid}) ->
    C=object:get_class_from_ref(O),
    ClassName=case C of
                  none -> <<"<no class - main module>">>;
                  _ -> re:replace(string:substr(atom_to_list(C), 7), "_", ".", [{return, list}])
              end,
    iolist_to_binary([ClassName,
                      ":", pid_to_list(Cog#cog.ref), "-", integer_to_binary(Oid)]);
toString(_Cog, L) when is_list(L) ->
    iolist_to_binary(["list[",
                      lists:join(", ", lists:map(fun(I) -> toString(_Cog, I) end, L)),
                      "]"]);
toString(_Cog, T) when is_tuple(T) ->
    [C|A] = tuple_to_list(T),
    case C of
        dataInsert ->
            iolist_to_binary(["set[", abslistish_to_iolist(_Cog, dataInsert, dataEmptySet, T), "]"]);
        dataInsertAssoc ->
            iolist_to_binary(["map[", abslistish_to_iolist(_Cog, dataInsertAssoc, dataEmptyMap, T), "]"]);
        _ -> iolist_to_binary([constructorname_to_string(C),
                               "(", lists:join(",", [toString(_Cog,X) || X <- A]),
                               ")"])
    end.


truncate(_Cog,{N,D})->
    N div D;
truncate(_Cog,N)->
    N.

numerator(_Cog, {N, _D}) ->
    N;
numerator(_Cog, A) when is_integer(A) ->
    A.

denominator(_Cog, {_N, D}) ->
    D;
denominator(_Cog, A) when is_integer(A) ->
    1.

float(_Cog, {N, D}) ->
    N / D;
float(_Cog, A) when is_integer(A) ->
    float(A).

rat(_Cog, F) ->
    %% this is slightly ugly.
    Rest = lists:dropwhile(fun(E) -> E /= $. end, mochinum:digits(F)),
    case Rest of
        ".0" ->
            trunc(F);
        _ ->
            Length = length(Rest) - 1,
            Factor = mochinum:int_pow(10, Length),
            rationals:new(trunc(F * Factor), Factor)
    end.

floor(_Cog, F) ->
    erlang:floor(F).

ceil(_Cog, F) ->
    erlang:ceil(F).

sqrt(_Cog, F) ->
    math:sqrt(F).

log(_Cog, F) ->
    math:log(F).

exp(_Cog, F) ->
    math:exp(F).

println(_Cog,S)->
    io:format("~s~n",[S]).

print(_Cog,S)->
    io:format("~s",[S]).

getProductLine(_Cog)->
    exit("Not Implemented").

thisDC(#cog{dcobj=DC}) ->
    DC.

%% ABS.Scheduler functions
method(_Cog, #task_info{method=Method}) ->
    Method.
destinyOf(_Cog, #task_info{destiny=Future}) ->
    Future.
arrival(_Cog, #task_info{arrival=Arrival}) ->
    Arrival.
proc_deadline(_Cog, #task_info{proc_deadline=dataInfDuration}) ->
    dataInfDuration;
proc_deadline(_Cog, #task_info{
                       proc_deadline={ dataDuration, OriginalDeadline},
                       creation={dataTime, CreationTime}}) ->
    Time = clock:now(),
    Elapsed = rationals:sub(Time, CreationTime),
    NewDeadline = rationals:sub(OriginalDeadline, Elapsed),
    { dataDuration, NewDeadline }.
