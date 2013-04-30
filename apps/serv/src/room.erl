%%%-------------------------------------------------------------------
%%% @author Paul Peregud <>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 30 Apr 2013 by Paul Peregud <>
%%%-------------------------------------------------------------------
-module(room).

-behaviour(gen_server).

%% API
-export([start_link/4, join/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("serv/include/logging.hrl").

-define(SERVER, ?MODULE). 

-type tag() :: any().

-record(state, {
	  timer,
	  id :: any(),
	  type :: any(),
	  gm :: pid(),
	  players = [] :: [{pid(), binary()}],
	  target = [] :: [{pid(), binary()}],
	  captured = [] :: [{pid(), tag()}],
	  playing = false
	 }).
-define(s, State#state).

%%%===================================================================
%%% API
%%%===================================================================

start_link(GameId, GameType, GMPid, Players) ->
    gen_server:start_link(?MODULE, [GameId, GameType, GMPid, Players], []).

join(GamePid, Nick) ->
    gen_server:call(GamePid, {join, {self(), Nick}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([GameId, GameType, GMPid, Players]) ->
    ?INFO("starting room", []),
    {ok, TRef} = timer:send_after(1000, players_too_slow),
    {ok, #state{timer = TRef, id = GameId, type = GameType,
		gm = GMPid, target = Players}}.
handle_call({join, {Pid, Nick}}, From, 
	    State = #state{playing = false, target = Target0}) ->
    Players = [{Pid, Nick} | ?s.players],
    Target = tl(Target0),
    Captured = [From | ?s.captured],
    case Target of
	[] ->
	    ?INFO("starting game!", []),
	    [ gen_server:reply(Client, true) || Client <- Captured ],
	    timer:cancel(?s.timer),
	    Nicks = [ Nick || {_, Nick} <- Players ],
	    gm:begin_game(?s.gm, ?s.id, Nicks),
	    {noreply, State#state{playing = true, captured = [], 
				  target = [], players = Players}};
	_ ->
	    ?INFO("need more..", []),
	    {noreply, State#state{captured = Captured, 
				  target = Target, players = Players}}
    end;
handle_call(Request, _From, State) ->
    {stop, {odd_call, Request}, State}.

handle_cast(_Msg, State) ->
    {stop, {odd_cast, _Msg}, State}.

handle_info(players_too_slow, State = #state{playing = false}) ->
    ?INFO("players are too slow, shutting room down", []),
    [ gen_server:reply(Client, {error, someone_was_too_slow}) || Client <- ?s.captured ],
    {stop, normal, State};
handle_info(players_too_slow, State) ->
    {noreply, State};
handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
