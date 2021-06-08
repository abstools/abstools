%%This file is licensed under the terms of the Modified BSD License.
-module(main_app).

-behaviour(application).
-export([start/2, stop/1]).

-include_lib("../include/absmodulename.hrl").

normalize_url_prefix(none) ->
    "/";
normalize_url_prefix(Urlprefix) ->
    case string:equal(Urlprefix, "/") of
        true -> "/";
        false ->
            %% Ensure that the prefix starts and ends with a slash.
            Prefix1 = [ "/" | string:trim(Urlprefix, leading, "/")],
            lists:append([string:trim(Prefix1, trailing, "/"), "/"])
    end.

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(absmodel, port),
    {ok, RawPrefix} = application:get_env(absmodel, url_prefix),
    Urlprefix=normalize_url_prefix(RawPrefix),
    {ok, Module} = application:get_env(absmodel, module),
    {ok, Verbose} = application:get_env(absmodel, verbose),
    {ok, Debug} = application:get_env(absmodel, debug),
    {ok, Clocklimit} = application:get_env(absmodel, clocklimit),
    {ok, Trace} = application:get_env(absmodel, replay),
    Dispatch = cowboy_router:compile([{'_',
                                       [{Urlprefix, cowboy_static, {priv_file, absmodel, "index.html"}},
                                        {[Urlprefix, "static/[...]"], cowboy_static, {priv_dir, absmodel, "static"}},
                                        {[Urlprefix, "v1/:request/[...]"], modelapi_v1, []},
                                        {[Urlprefix, "v2/:request/[...]"], modelapi_v2, []},
                                        {[Urlprefix, ":request/[...]"], modelapi_v2, []}]}]),
    %% In case we need a random port, see example at bottom of
    %% https://ninenines.eu/docs/en/cowboy/2.0/manual/cowboy.start_clear/
    case cowboy:start_clear(http, [{port, Port}, {ip, loopback}],
                            #{env => #{dispatch => Dispatch}
                             , idle_timeout => infinity }) of
        {ok, _} ->
            case Verbose of
                0 -> ok;
                _ -> io:format(standard_error, "Starting server on port ~w, abort with Ctrl-C~n", [ranch:get_port(http)])
            end;
        _ -> io:format(standard_error, "Failed to start model API on port ~w (is another model already running?)~nAborting~n", [Port]),
             halt(1)
    end,
    runtime:start_link([Module, Verbose, Debug, Clocklimit, true, Trace]).

stop(_State) ->
    ok.
