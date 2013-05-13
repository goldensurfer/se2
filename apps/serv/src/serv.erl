%%%-------------------------------------------------------------------
%%% @author 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%% @copyright (C) 2013, 190033 peregud pavel
%%% @doc
%%%
%%% Provides API for serv application.
%%%
%%% @end
%%% Created :  5 Mar 2013 by 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%%-------------------------------------------------------------------
-module(serv).

%% API
-export([start/0, stop/0]).
-export([start_normal/1, start_championship/2]).

%%%===================================================================
%%% API
%%%===================================================================

start_normal(Port) when is_list(Port) ->
    start_normal(list_to_integer(Port));
start_normal(Port) when is_integer(Port)->
    application:load(serv),
    application:set_env(serv, client_port, Port),
    start().

start_championship(Port, N) when is_list(Port) ->
    start_championship(list_to_integer(Port), N);
start_championship(Port, N) when is_list(N) ->
    start_championship(Port, list_to_integer(N));
start_championship(Port, N) ->
    application:load(serv),
    application:set_env(serv, client_port, Port),
    application:set_env(serv, mode, championship),
    application:set_env(serv, invites, N),
    start().

start() ->
    ok = ensure(ranch, permanent),
    ok = ensure(gproc, permanent),
    ok = ensure(inets, permanent),
    ok = ensure(lager, permanent),
    ok = application:start(serv, permanent).

stop() ->
    application:stop(serv),
    application:stop(lager),
    application:stop(ranch),
    application:stop(lager),
    application:stop(gproc).

%%%===================================================================
%%% Internal functions
%%%===================================================================

ensure(App) ->
    ensure(App, transient).

ensure(App, Method) ->
    case application:start(App, Method) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok;
	Error ->
	    io:fwrite(user, "Start of application ~p has failed with reason ~p", [App, Error]),
	    timer:sleep(500),
	    Error
    end.
