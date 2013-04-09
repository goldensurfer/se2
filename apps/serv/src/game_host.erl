%%%-------------------------------------------------------------------
%%% @author 190033 peregud pavel <peregudp@p20310.mini.pw.edu.pl>
%%% @copyright (C) 2013, 190033 peregud pavel
%%% @doc
%%%
%%% @end
%%% Created :  9 Apr 2013 by 190033 peregud pavel <peregudp@p20310.mini.pw.edu.pl>
%%%-------------------------------------------------------------------
-module(game_host).

-behaviour(gen_server).

%% API
-export([start_link/0, check_game/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check_game(Game) ->
    gen_server:cast(?SERVER, {check, Game}).

%%%===================================================================
%%% callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    {stop, {odd_call, Request}, State}.

handle_cast({check, Game}, State) ->
    case gproc:lookup_local_property({registered_for_game, Game}) of
	[_|_] = List when length(List) > 2 ->
	    create_game(Game, List);
	[] ->
	    ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {stop, {odd_cast, _Msg}, State}.

handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_game(Game, ListOfPlayers) ->
    erlang:error(not_impl).
