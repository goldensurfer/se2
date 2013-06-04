%%%-------------------------------------------------------------------
%%% @author Paul Peregud <>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 30 Apr 2013 by Paul Peregud <>
%%%-------------------------------------------------------------------
-module(gma).

%% API
-include_lib("serv/include/se2.hrl").

-export([start_all/3, start_all/4, start/0]).

start_all(Host, Port, Id) ->
    start_all(Host, Port, Id, ?MAGIC).

start_all(Host, Port, Id, Game) ->
    ok = ensure(gproc, permanent),
    ok = ensure(inets, permanent),
    ok = ensure(lager, permanent),
    lager:set_loglevel(lager_console_backend, info),
    ok = application:start(gma, permanent),
    supervisor:start_child(gma_sup, {ttt_socket, {gm_client, start_link, [Host, Port, Id, Game]},
				     permanent, 5000, worker, [gm_client]
				    }).
    %% gm_client:start_link(Host, Port, Id, Game).

start() ->
    ok = ensure(gproc, permanent),
    ok = ensure(inets, permanent),
    ok = ensure(lager, permanent),
    ok = application:start(gma, permanent).

stop() ->
    application:stop(gma),
    application:stop(serv),
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
