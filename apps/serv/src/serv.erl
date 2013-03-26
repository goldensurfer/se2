%%%-------------------------------------------------------------------
%%% @author 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%% @copyright (C) 2013, 190033 peregud pavel
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2013 by 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%%-------------------------------------------------------------------
-module(serv).

%% API
-export([start/0]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    ok = application:start(ranch),
    ok = application:start(gproc),
    ok = application:start(inets),
    ok = application:start(erlsom),
    ok = application:start(lager),
    ok = application:start(serv, permanent).

%%%===================================================================
%%% Internal functions
%%%===================================================================
