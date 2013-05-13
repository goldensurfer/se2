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

-include_lib("serv/include/logging.hrl").

-define(SERVER, ?MODULE). 

-type who() ::  xs | os.
-type x() ::  integer().
-type y() ::  x().

-record(state, {
	  cl :: pid(),
	  id :: binary(),
	  players :: [binary()],
	  board = ets:new(board_state, []) :: ets:tid(),
	  next = xs :: who(),
	  range = {0, 19} :: {non_neg_integer(), non_neg_integer()},
	  history = [] :: list({who(), x(), y()})
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

handle_cast({move, X0, Y0}, State0 = #state{next = Who}) ->
    ?DBG("move ~p,~p by ~p", [X0, Y0, player(Who, State0)]),
    X = list_to_integer(X0),
    Y = list_to_integer(Y0),
    Move = {Who, {X, Y}},
    State = add_history(Move, State0),
    {Board, History} = get_history(State),
    validate_board(?s.board),
    case check_range(X, Y, ?s.range) of
	true ->
	    case ets:insert_new(?s.board, {{X, Y}, Who}) of
		true ->
		    case check_victory(Who, {X, Y}, ?s.range, ?s.board) of	
			true ->
			    Winner = player(other(Who), State),
			    WL = {Winner, player(Who, State)},
			    gm_client:game_over(?s.id, WL, {X, Y}),
			    ?DBG("game over: ~p won via 5 in line~n~p~n~p", 
				 [Winner, History, Board]),
			    {stop, normal, State};
			false ->
			    Other = other(Who),
			    OtherNick = player(Other, State),
			    ?DBG("next: ~p", [OtherNick]),
			    gm_client:next_player(?s.id, OtherNick, {X, Y}),
			    {noreply, State#state{next = Other}}
		    end;
		false ->
		    Pos = ets:lookup(?s.board, {X, Y}),
		    Winner = player(other(Who), State),
		    Loser = player(Who, State),
		    WL = {Winner, Loser},
		    ?DBG("game over: ~p won via move ~p to occupied position by ~p where ~p~n~p~n~p", 
			 [Winner, {{X, Y}, Who}, Loser, Pos, History, Board]),
		    gm_client:game_over(?s.id, WL, {X, Y}),
		    {stop, normal, State}
	    end;
	false ->
	    Winner = player(other(Who), State),
	    Loser = player(Who, State),
	    WL = {Winner, Loser},
	    ?DBG("game over: ~p won via move outside the boundaries by ~p~n~p~n~p", 
		 [Winner, Loser, History, Board]),
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

add_history({_Who, {_X, _Y}} = Move, 
	    State = #state{history = History}) ->
    State#state{history = [Move | History]}.

get_history(#state{board = Tid, history = History}) ->
    All = ets:tab2list(Tid),
    {lists:sort(All), lists:reverse(History)}.

player(Key, #state{players = Players}) ->
    {Key, Nick} = lists:keyfind(Key, 1, Players),
    Nick.

validate_board(Tid) ->
    F = fun({{X, Y}, _}) when is_integer(X), is_integer(Y) -> true;
	   (_) -> false
	end,
    true = lists:all(F, ets:tab2list(Tid)).
    
other(xs) ->
    os;
other(os) ->
    xs.

check_range(X, Y, {Min, Max}) 
  when is_integer(X), is_integer(Y) ->
    interval(Min, X, Max) andalso interval(Min, Y, Max).

interval(Min, X, Max) ->
    Min =< X andalso X =< Max. 
    
check_victory(Who, {X, Y}, Range, Tid) ->
    walk(Who, {X-5, Y-5}, {1, 1}, 0, Range, Tid) orelse
	walk(Who, {X, Y-5}, {0, 1}, 0, Range, Tid) orelse
	walk(Who, {X-5, Y}, {1, 0}, 0, Range, Tid).

walk(_, _, _, 5, _, _) ->
    true;
walk(Who, {X, Y}, {DX, DY} = D, _, {Min, _} = R, Tid) 
  when X < Min; Y < Min ->
    walk(Who, {X+DX, Y+DY}, D, 0, R, Tid);
walk(Who, {X, Y}, {DX, DY} = D, Score, {_, Max} = R, Tid) 
  when X =< Max, Y =< Max ->
    case who(X, Y, Tid) of
	Who ->
	    walk(Who, {X+DX, Y+DY}, D, Score+1, R, Tid);
	_->
	    walk(Who, {X+DX, Y+DY}, D, 0, R, Tid)
    end;
walk(_, _, _, _, _, _) ->
    false.

who(X, Y, Tid) 
  when is_integer(X), is_integer(Y) ->
    case ets:lookup(Tid, {X, Y}) of
	[{_, Color}] ->
	    Color;
	[] ->
	    '_'
    end.
