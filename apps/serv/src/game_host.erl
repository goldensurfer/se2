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
-export([start_link/0, check_game/1, game_ended/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include_lib("serv/include/logging.hrl").
-include_lib("serv/include/se2.hrl").

-type player() :: {pid(), nick()}.
-record(game, {
	  room :: pid(),
	  id :: game_id(),
	  players :: [player()]
	 }).
-type game() :: #game{}.
-type mode() :: normal | championship.
-record(state, {
	  mode = normal :: mode(),
	  pending_players = [] :: list(player()),
	  active_games = [] :: list(game()),
	  pending_games = [] = list(game()),
	  results = ets:new(res, [])
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check_game(Game) ->
    gen_server:cast(?SERVER, {check, Game}).

game_ended(RoomPid, GameId, WL) ->
    gen_server:cast(?SERVER, {game_ended, RoomPid, GameId, WL}).

%%%===================================================================
%%% callbacks
%%%===================================================================

init([]) ->
    {ok, Mode} = application:get_env(serv, mode),
    {ok, #state{mode = Mode}}.

handle_call(Request, _From, State) ->
    {stop, {odd_call, Request}, State}.

handle_cast({check, GameType}, State=#state{mode = normal}) ->
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
handle_cast({check, GameType}, State=#state{mode = championship, 
					    pending_games = []}) ->
    List = gproc:lookup_local_properties({registered_for_game, GameType}),
    {ok, Required} = invites(),
    Actual = length(List),
    GM = gproc:lookup_local_properties({gm_for_game, GameType}),
    ?INFO("checking players and gms:~n~p", [{List, GM}]),
    case {GM, Actual >= Required} of
	{[{GMPid, {_Id, PlayersMin, _PlayersMax}} | _], true} ->
	    Games = create_championship(GMPid, GameType, List),
	    gen_server:cast(self(), start_round),
	    {noreply, State#state{pending = Games}};
	_ ->
	    {noreply, State}
    end;

handle_cast(start_round, State) ->
    case Pending of
	[] -> {noreply, State};
	

handle_cast({game_ended, RoomPid, GameId, WL}, State = #state{mode = normal}) ->
    {noreply, State};

handle_cast({game_ended, RoomPid, GameId, WL}, State = #state{mode = championship}) ->
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

create_championship(GMPid, GameType, List) ->
    [ #game{id = create_id(), 
	    players = [#player{pid = PidA, nick = NickA}, 
		       #player{pid = PidA, nick = NickA}]} 
      || {PidA, NickA} <- ListOfPlayers, 
	 {PidB, NickB} <- ListOfPlayers, 
	 NickA < NickB ].

create_game(GMPid, GameType, ListOfPlayers) ->
    GameId = create_id(),
    {ok, GamePid} = room_sup:add_child(GameId, GameType, GMPid, ListOfPlayers),
    [ client:join_game(Pid, GameType, GamePid, GameId) || 
	{Pid, _Nick} <- ListOfPlayers ].

create_id() ->
    [ crypto:rand_uniform($a, $z) || _ <- lists:seq(1, 8) ].

is_champ() ->
    championship =:= application:get_env(serv, mode).

invites() ->
    application:get_env(serv, invites).
