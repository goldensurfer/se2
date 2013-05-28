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
-export([start_link/0, list/0, check_game/1, game_ended/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include_lib("serv/include/logging.hrl").
-include_lib("serv/include/se2.hrl").

-type mode() :: normal | championship.
-record(state, {
	  mode = normal :: mode(),
	  pending_players = [] :: list(player()),
	  active_games = [] :: list(game()),
	  pending_games = [] :: list(game()),
	  finished_games = [] :: list(game()),
	  results = ets:new(res, [])
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

list() ->
    gen_server:call(?SERVER, list).

check_game(Game) ->
    gen_server:cast(?SERVER, {check, Game}).

game_ended(RoomPid, GameId, WL) ->
    gen_server:cast(?SERVER, {game_ended, RoomPid, GameId, WL}).


%%%===================================================================
%%% callbacks
%%%===================================================================

init([]) ->
    {ok, Mode} = application:get_env(serv, mode),
    true = undefined =/= Mode,
    {ok, #state{mode = Mode}}.

handle_call(list, _From, State) ->
    Reply = [{pending_games, ?s.pending_games},
	     {active_games, ?s.active_games},
	     {pending_players, ?s.pending_players},
	     {finished_games, ?s.finished_games}
	    ],
    {reply, {ok, Reply}, State};
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
    Required = invites(),
    Actual = length(List),
    GM = gproc:lookup_local_properties({gm_for_game, GameType}),
    ?INFO("checking players and gms:~n~p~nrequired: ~p, actual: ~p", [{List, GM}, Required, Actual]),
    case {GM, Actual >= Required} of
	{[{_GMPid, {_Id, _, _}} | _], true} ->
	    PG = create_championship(GameType, List),
	    self() ! start_round,
	    {noreply, State#state{pending_games = PG, pending_players = List}};
	_ ->
	    {noreply, State}
    end;

handle_cast({check, _GameType}, State) ->
    ?WARNING("someone connected during the championship!"),
    {noreply, State};

handle_cast({game_ended, _RoomPid, _GameId, _WL}, State = #state{mode = normal}) ->
    {noreply, State};

handle_cast({game_ended, _RoomPid, GameId, _WL}, State = #state{mode = championship}) ->
    case lists:keytake(GameId, #game.id, State#state.active_games) of
	false ->
	    {stop, {error, unknown_running_game}, State};
	{value, Room, AG1} ->
	    PP = Room#game.players ++ ?s.pending_players,
	    self() ! start_round,
	    {noreply, ?s{finished_games = [Room | ?s.finished_games],
			 pending_players = PP, 
			 active_games = AG1
			 }}
    end;
handle_cast(_Msg, State) ->
    {stop, {odd_cast, _Msg}, State}.

handle_info(start_round, State=#state{pending_games=[], active_games=[]}) ->
    Players0 = collect_players(?s.finished_games),
    Players1 = count_victories(Players0, ?s.finished_games),
    Players = lists:keysort(2, Players1),
    ?ALERT("Championship has ended!!!~n~p~n~p", [Players, ?s.finished_games]),
    {noreply, ?s{mode = normal}};
handle_info(start_round, State) ->
    ?NOTICE("active_games:~n~p~npending games: ~n~p~npending players: ~n~p", 
	    [State#state.active_games, State#state.pending_games, State#state.pending_players]),
    PG = ?s.pending_games,
    PP = ?s.pending_players,
    {AG1, PG1, PP1} = choose_games({PG, PP}),
    AG1withPids = start_round0(AG1),
    ?NOTICE("started games: ~p", [AG1withPids]),
    NAG = AG1withPids ++ ?s.active_games,
    {noreply, ?s{active_games = NAG, pending_games = PG1, pending_players = PP1}};
handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_championship(GameType, List) ->
    [  begin
	   erlang:monitor(process, Pid),
	   client:join_championship(Pid, GameType)
       end || {Pid, _Nick} <- List ],
    Games = [ #game{id = create_id(), 
		    game_type = GameType,
		    players = [{PidA, NickA}, {PidB, NickB}]} 
	      || {PidA, NickA} <- List, 
		 {PidB, NickB} <- List, 
		 NickA < NickB ],
    Games.

create_game(GMPid, GameType, ListOfPlayers) ->
    GameId = create_id(),
    create_game(GMPid, GameId, GameType, ListOfPlayers).

create_game(GMPid, GameId, GameType, ListOfPlayers) ->
    {ok, GamePid} = room_sup:add_child(GameId, GameType, GMPid, ListOfPlayers),
    [ client:join_game(Pid, GameType, GamePid, GameId) || 
	{Pid, _Nick} <- ListOfPlayers ],
    {ok, GamePid}.

start_round0(Games) ->
    [ begin 
	  case gproc:lookup_local_properties({gm_for_game, GT}) of
	      [{GM, _}] ->
		  {ok, Room} = create_ch_game(GM, GameId, GT, LOP),
		  Game#game{room = Room}
	  end
      end || Game = #game{players = LOP, game_type = GT, id = GameId} <-Games].

create_ch_game(GMPid, GameId, GameType, ListOfPlayers) ->
    {ok, GamePid} = room_sup:add_child(GameId, GameType, GMPid, ListOfPlayers),
    [ client:join_ch_game(Pid, GameType, GamePid, GameId) || 
	{Pid, _Nick} <- ListOfPlayers ],
    {ok, GamePid}.

create_id() ->
    [ crypto:rand_uniform($a, $z) || _ <- lists:seq(1, 8) ].

invites() ->
    case application:get_env(serv, invites) of
	{ok, I} when is_integer(I) -> I;
	{ok, I} when is_list(I) -> list_to_integer(I)
    end.

choose_games({PG, PP}) ->
    ?DBG("choose_games0", []),
    F = fun(H = #game{}, {AAG, APG, APP}) -> 
		[{PidA, NickA}, {PidB, NickB}] = H#game.players,
		?DBG("keytake ~p", [{NickA, 2, APP}]),
		case lists:keytake(NickA, 2, APP) of
		    false ->
			?DBG("H: ~p, false 1", [H]),
			{AAG, [H | APG], APP};
		    {value, {PidA, NickA}, APP1} ->
			case lists:keytake(NickB, 2, APP1) of
			    false ->
				?DBG("H: ~p, false 2", [H]),
				{AAG, [H | APG], APP};
			    {value, {PidB, NickB}, APP2} ->
				{[H | AAG], APG, APP2}
			end
		end
	end,
    lists:foldl(F, {[], [], PP}, PG).

collect_players(Games) ->
    Pairs = [ [Nick1, Nick2] || #game{players=[{Nick1, _}, {Nick2, _}]} <- Games ],
    lists:usort(lists:flatten(Pairs)).

count_victories(Players0, Games) ->
    Players1 = [ {Nick, 0} || Nick <- Players0 ],
    D = dict:from_list(Players1),
    F = fun(Game, Dict) ->
		dict:update_counter(Game#game.winner, 1, Dict)
	end,
    D1 = lists:foldl(F, D, Games),
    dict:to_list(D1).
    
