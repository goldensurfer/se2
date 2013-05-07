%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@kari.lan>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% Implements tic-tac-toe game logic and stateful server
%%%
%%% @end
%%% Created : 29 Apr 2013 by Paul Peregud <pawel@kari.lan>
%%%-------------------------------------------------------------------
-module(ttt).

-behaviour(gen_server).

%% API
-export([start_link/3, move/3, check_conditions/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-type who() ::  xs | os.

-record(state, {
	  cl :: pid(),
	  id :: binary(),
	  players :: [binary()],
	  board = ets:new(board_state, []) :: ets:tid(),
	  next = xs :: who(),
	  range = {0, 19} :: {non_neg_integer(), non_neg_integer()}
	 }).

-define(s, State#state).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Cl, GameId, Players) ->
    gen_server:start_link(?MODULE, [Cl, GameId, Players], []).

move(Pid, X, Y) ->
    gen_server:cast(Pid, {move, X, Y}).

check_conditions([A, A]) ->
    false;
check_conditions([_, _]) ->
    true;
check_conditions(_) ->
    false.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Cl, GameId, Nicks]) ->
    self() ! do_begin,
    Players = lists:zip([xs, os], Nicks),
    {ok, #state{cl = Cl, id = GameId, players = Players}}.

handle_call(_Request, _From, State) ->
    {stop, {odd_call, _Request}, State}.

handle_cast({move, X, Y}, State = #state{next = Who}) ->
    case check_range(X, Y, ?s.range) of
	true ->
	    case ets:insert_new(?s.board, {{X, Y}, Who}) of
		true ->
		    case check_victory(?s.range, ?s.board) of
			true ->
			    Winner = player(other(Who), State),
			    WL = {Winner, player(Who, State)},
			    gm_client:game_over(?s.id, WL, {X, Y}),
			    ?DBG("game over: ~p won via 5 in line", [Winner]),
			    {stop, normal, State};
			false ->
			    gm_client:next_player(?s.id, other(Who), {X, Y}),
			    {noreply, State}
		    end;
		false ->
		    WL = {player(other(Who), State), player(Who, State)},
		    gm_client:game_over(?s.id, WL, {X, Y}),
		    {stop, normal, State}
	    end;
	false ->
	    WL = {player(other(Who), State), player(Who, State)},
	    gm_client:game_over(?s.id, WL, {X, Y}),
	    {stop, normal, State}
    end;
handle_cast(_Msg, State) ->
    {stop, {odd_cast, _Msg}, State}.

handle_info(do_begin, State) ->
    gm_client:next_player(?s.id, player(?s.next, State), undefined),
    {noreply, State};
handle_info(_Info, State) ->
    {stop, {odd_info, _Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

player(Key, #state{players = Players}) ->
    {Key, Nick} = lists:keyfind(Key, 1, Players),
    Nick.
    
other(xs) ->
    os;
other(os) ->
    xs.

check_range(X, Y, {Min, Max}) ->
    interval(Min, X, Max) andalso interval(Min, Y, Max).

interval(Min, X, Max) ->
    Min =< X andalso X =< Max. 
    
check_victory({_Min, _Max}, _Tid) ->
    false.

