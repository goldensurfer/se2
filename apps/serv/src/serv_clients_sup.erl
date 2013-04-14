%%%-------------------------------------------------------------------
%%% @author 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%% @copyright (C) 2013, 190033 peregud pavel
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2013 by 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%%-------------------------------------------------------------------
-module(serv_clients_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {M, F, A} = {client, start_link, []},
    supervisor:start_link({local, ?MODULE}, ?MODULE, [{M, F, A}]).

add_child(Ref, Args) ->
    supervisor:start_child(Ref, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([{M, F, A}]) ->
    {ok,{{simple_one_for_one,10,10},
         [{undefined, {M, F, A}, transient, 3000, worker, [M]}]
        }}.
