%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@kari.lan>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2013 by Paul Peregud <pawel@kari.lan>
%%%-------------------------------------------------------------------
-module(games_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {M, F, A} = {ttt, start_link, []},
    supervisor:start_link({local, ?MODULE}, ?MODULE, [{M, F, A}]).

add_child(GameId, Players) ->
    supervisor:start_child(?MODULE, [GameId, Players]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([{M, F, A}]) ->
    {ok,{{simple_one_for_one,10,10},
         [{undefined, {M, F, A}, transient, 3000, worker, [M]}]
        }}.
