%%This file is licensed under the terms of the Modified BSD License.
-module(init_task).
-behaviour(task).
-export([init/4,start/1]).
-include_lib("abs_types.hrl").
%% Inits an object
%% Used if it is spawned on an new COG


init(Cog,_Future,CalleeObj,Args)->
    {CalleeObj,Args}.


start({O=#object{cog=Cog},Args})->
    C=object:get_class_from_ref(O),
    cog:activate_object(Cog, O),
    C:init(O,Args).


