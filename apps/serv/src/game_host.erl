%%%-------------------------------------------------------------------
%%% @author 190033 peregud pavel <peregudp@p20310.mini.pw.edu.pl>
%%% @copyright (C) 2013, 190033 peregud pavel
%%% @doc
%%%
%%% This module handles game's lifecycle from Servers POV.
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

-include_lib("serv/include/logging.hrl").

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

handle_cast({check, GameType}, State) ->
    List = gproc:lookup_local_properties({registered_for_game, GameType}),
    GM = gproc:lookup_local_properties({gm_for_game, GameType}),
    ?INFO("checking players and gms:~n~p", [{List, GM}]),
    case {List, GM} of
	{[_|_] = List, [{GMPid, {_Id, PlayersMin, _PlayersMax}} | _]} 
	  when length(List) >= PlayersMin ->
	    create_game(GMPid, GameType, lists:sublist(List, PlayersMin));
	_ ->
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

create_id() ->
    [ crypto:rand_uniform($a, $z) || _ <- lists:seq(1, 8) ].

create_game(GMPid, GameType, ListOfPlayers) ->
    GameId = create_id(),
    {ok, GamePid} = room_sup:add_child(GameId, GameType, GMPid, ListOfPlayers),
    [ client:join_game(Pid, GameType, GamePid, GameId) || 
	{Pid, _Nick} <- ListOfPlayers ].

is_champ() ->
    championship =:= application:get_env(serv, mode).

invites() ->
    application:get_env(serv, invites).
