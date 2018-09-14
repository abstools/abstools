%%This file is licensed under the terms of the Modified BSD License.
-module(active_object_task).
-behaviour(task).
-export([init/4,start/1]).
-include_lib("abs_types.hrl").

%% Active object task -> executes run method

init(_Cog,_Future,Object,Params)->
    object:new_object_task(Object,self(),Params),
    Object.

start(O)->
    C=object:get_class_from_ref(O),
    try
        apply(C, m_run,[O,[]])
    catch
        _:Reason ->
            object:die(O,Reason)
    end.



