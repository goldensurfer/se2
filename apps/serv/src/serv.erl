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

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    ok = ensure(ranch, permanent),
    ok = ensure(gproc, permanent),
    ok = ensure(inets, permanent),
    ok = ensure(erlsom, permanent),
    ok = ensure(lager, permanent),
    ok = application:start(serv, permanent).

stop() ->
    application:stop(serv),
    application:stop(lager),
    application:stop(erlsom),
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
