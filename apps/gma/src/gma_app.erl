%%%-------------------------------------------------------------------
%%% @author 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%% @copyright (C) 2013, 190033 peregud pavel
%%% @doc
%%% 
%%% @end
%%% Created :  30 Apr 2013 by 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%%-------------------------------------------------------------------
-module(gma_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% note that all requrements from app.src file should be started 
%% before calling application:start(serv).
start(_StartType, _StartArgs) ->
    gma_sup:start_link().

stop(_State) ->
    gma_sup:stop().

