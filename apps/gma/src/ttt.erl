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
-export([start_link/3, check_conditions/1]).

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
	  range = {1, 20} :: {non_neg_integer(), non_neg_integer()}
	 }).

-define(s, State#state).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Cl, GameId, Players) ->
    gen_server:start_link(?MODULE, [Cl, GameId, Players], []).

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

handle_call({move, Who, X, Y} = Move, _From, State = #state{next = Who}) ->
    case check_range(X, Y, ?s.range) of
	ok ->
	    case ets:insert_new(?s.board, {{X, Y}, Who}) of
		true ->
		    case check_victory(?s.range, ?s.board) of
			true ->
			    {stop, normal, {game_over, player(Who, State), Move}, State};
			false ->
			    {reply, {next_player, other(Who), {Who, X, Y}}, State}
		    end;
		false ->
		    {stop, normal, {game_over, player(other(Who), State), Move}, State}
	    end;
	error ->
	    {stop, normal, {game_over, player(other(Who), State), Move}, State}
    end;
handle_call({move, Who, _X, _Y} = Move, _From, State) ->
    {stop, normal, {game_over, player(other(Who), State), Move}, State};
handle_call(_Request, _From, State) ->
    {stop, {odd_call, _Request}, State}.

handle_cast(_Msg, State) ->
    {stop, {odd_cast, _Msg}, State}.

handle_info(do_begin, State) ->
    gm_client:next_player(?s.id, player(?s.next, State)),
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

check_range(_X, _Y, _) ->
    error(non_impl).
    
check_victory(_, _) ->
    error(non_impl).
    
