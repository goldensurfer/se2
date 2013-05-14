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
-export([start_link/0, check_game/1, game_ended/3, start_round/2]).

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

start_round(GMPid, GameType) ->
    gen_server:cast(?SERVER, {start_round, GMPid, GameType}).

%%%===================================================================
%%% callbacks
%%%===================================================================

init([]) ->
    {ok, Mode} = application:get_env(serv, mode),
    true = undefined =/= Mode,
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
    Required = invites(),
    Actual = length(List),
    GM = gproc:lookup_local_properties({gm_for_game, GameType}),
    ?INFO("checking players and gms:~n~p~nrequired: ~p, actual: ~p", [{List, GM}, Required, Actual]),
    case {GM, Actual >= Required} of
	{[{GMPid, {_Id, _, _}} | _], true} ->
	    PLs = [ N || {_, N} <- List ],
	    ?NOTICE("1 creating championship for players: ~p", [PLs]),
	    {Games, Players} = create_championship(GMPid, GameType, List),
	    ?NOTICE("2 creating championship for players: ~p", [PLs]),
	    start_round(GMPid, GameType),
	    ?NOTICE("3 creating championship for players: ~p", [PLs]),
	    {noreply, State#state{pending_games = Games, pending_players = Players}};
	_ ->
	    {noreply, State}
    end;

handle_cast({check, GameType}, State) ->
    ?WARNING("someone connected during the championship!"),
    {noreply, State};

handle_cast({start_round, GMPid, GameType}, State) ->
    ?DBG("pending games: ~p~npending players: ~p", [State#state.pending_games, State#state.pending_players]),
    case State#state.pending_games of
	[] -> 
	    {noreply, State};
	_ -> 
	    State1 = start_round0(GMPid, GameType, State),
	    {noreply, State1}
    end;

handle_cast({game_ended, _RoomPid, _GameId, _WL}, State = #state{mode = normal}) ->
    {noreply, State};

handle_cast({game_ended, _RoomPid, _GameId, _WL}, State = #state{mode = championship}) ->
    case lists:keytake(GameId, #room.id, State#state.active_games) of
	false ->
	    {stop, {error, unknown_running_game}, State};
	{value, Room, RG1} ->
	    PP = Room#game.players ++ State#state.pending_players,
asdasdasdasda
	    start_round0(),
	    {noreply, State#state{pending_players = PP, active_games = RG1}}
    end;
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

create_championship(_GMPid, GameType, List) ->
    [  begin
	   erlang:monitor(process, Pid),
	   client:join_championship(Pid, GameType)
       end || {Pid, _Nick} <- List ],
    Games = [ #game{id = create_id(), 
		    players = [{PidA, NickA}, {PidB, NickB}]} 
	      || {PidA, NickA} <- List, 
		 {PidB, NickB} <- List, 
		 NickA < NickB ],
    {Games, List}.

create_game(GMPid, GameType, ListOfPlayers) ->
    GameId = create_id(),
    create_game(GMPid, GameId, GameType, ListOfPlayers).

create_game(GMPid, GameId, GameType, ListOfPlayers) ->
    {ok, GamePid} = room_sup:add_child(GameId, GameType, GMPid, ListOfPlayers),
    [ client:join_game(Pid, GameType, GamePid, GameId) || 
	{Pid, _Nick} <- ListOfPlayers ],
    {ok, GamePid}.

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

start_round0(GMPid, GameType, State = #state{active_games = RG,
					     pending_players = PP,
					     pending_games = PG}) ->
    ?DBG("start_round0", []),
    F = fun(H = #game{}, {ARG, APG, APP}) -> 
		[{PidA, NickA}, {PidB, NickB}] = H#game.players,
		?DBG("keytake ~p", [{NickA, 2, APP}]),
		case lists:keytake(NickA, 2, APP) of
		    false ->
			?DBG("H: ~p, false 1", [H]),
			{ARG, [H | APG], APP};
		    {value, {PidA, NickA}, APP1} ->
			case lists:keytake(NickB, 2, APP1) of
			    false ->
				?DBG("H: ~p, false 2", [H]),
				{ARG, [H | APG], APP};
			    {value, {PidB, NickB}, APP2} ->
				?DBG("H: ~p, creating!!!!", [H]),
				GameId = H#game.id,
				{ok, RoomPid} = create_ch_game(GMPid, GameId, GameType, H#game.players),
				H1 = H#game{room = RoomPid},
				{[H1 | ARG], APG, APP2}
			end
		end
	end,
    {RG1, PG1, PP1} = lists:foldl(F, {RG, [], PP}, PG),
    State#state{active_games = RG1, pending_players = PP1, pending_games = PG1}.
