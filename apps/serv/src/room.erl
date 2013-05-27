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
-export([start_link/4, join/2, publish/2, to_gm/2, end_game/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("serv/include/logging.hrl").
-include_lib("serv/include/se2.hrl").

-define(SERVER, ?MODULE). 

-type tag() :: any().

-record(state, {
	  timer,
	  id :: game_id(),
	  type :: binary(),
	  gm :: pid(),
	  gm_ref :: reference(),
	  players = [] :: [player()],
	  target = [] :: [player()],
	  captured = [] :: [{pid(), tag()}],
	  playing = false
	 }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(GameId, GameType, GMPid, Players) ->
    gen_server:start_link(?MODULE, [GameId, GameType, GMPid, Players], []).

join(Pid, Nick) ->
    gen_server:call(Pid, {join, {self(), Nick}}).

publish(Pid, Msg) ->
    gen_server:call(Pid, {publish, Msg}).

to_gm(Pid, Msg) ->
    gen_server:call(Pid, {to_gm, Msg}).

end_game(Pid, WL) ->
    gen_server:cast(Pid, {game_ended, WL}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([GameId, GameType, GMPid, Players]) ->
    ?INFO("starting room for ~p", [Players]),
    {ok, TRef} = timer:send_after(1000, players_too_slow),
    {ok, #state{timer = TRef, id = GameId, type = GameType,
		gm = GMPid, target = Players,
		gm_ref = monitor(process, GMPid)
	       }}.
handle_call({join, {Pid, Nick}}, From, 
	    State = #state{playing = false, target = Target0}) ->
    _Ref = monitor(process, Pid),
    Players = [{Pid, Nick} | ?s.players],
    Target = tl(Target0),
    Captured = [From | ?s.captured],
    case Target of
	[] ->
	    ?NOTICE("starting game! ~p", [?s.id]),
	    [ gen_server:reply(Client, true) || Client <- Captured ],
	    timer:cancel(?s.timer),
	    Nicks = [ ANick || {_, ANick} <- Players ],
	    gproc:reg({n, l, {room, ?s.id}}),
	    gm:begin_game(?s.gm, ?s.id, Nicks),
	    {noreply, ?s{playing = true, captured = [], 
			 target = [], players = Players}};
	_ ->
	    ?INFO("need more..", []),
	    {noreply, ?s{captured = Captured, 
			 target = Target, players = Players}}
    end;
handle_call({publish, Msg}, _From, State) ->
    [ client:send(Pid, Msg) || {Pid, _} <- ?s.players ],
    {reply, ok, State};
handle_call({to_gm, Msg}, _From, State) ->
    gm:send(?s.gm, Msg),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    {stop, {odd_call, Request}, State}.

handle_cast({game_ended, {Winner, Loser} = WL}, State) ->
    ?NOTICE("game ~p ended. ~p has beat ~p", [?s.id, Winner, Loser]),
    [ client:game_ended(Pid) || {Pid, _} <- ?s.players ],
    game_host:game_ended(self(), ?s.id, WL),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {stop, {odd_cast, _Msg}, State}.

handle_info(players_too_slow, State = #state{playing = false}) ->
    ?WARNING("players are too slow, shutting room down", []),
    [ gen_server:reply(Client, {error, someone_was_too_slow}) || Client <- ?s.captured ],
    game_host:game_ended(?s.id, nc),
    {stop, normal, State};
handle_info(players_too_slow, State) ->
    {noreply, State};
handle_info({'DOWN', MonRef, _Type, _Object, Info}, 
	    State = #state{gm_ref = MonRef}) ->
    MsgT = "Game Master has crashed with msg: ~p",
    Msg = sxml:error(io_lib:fwrite(MsgT, [Info])),
    [ client:send(Pid, Msg) || {Pid, _} <- ?s.players ],
    {stop, {gm_crash, Info}, State};
handle_info({'DOWN', _MonRef, _Type, Pid, Reason} = Info, State) ->
    case lists:keyfind(Pid, 1, ?s.players) of
	{Pid, Nick} ->
	    MsgT = "Player ~p has crashed with msg: ~p",
	    Msg = sxml:error(io_lib:fwrite(MsgT, [Nick, Info])),
	    [ client:send(APid, Msg) || {APid, _} <- ?s.players ],
	    {stop, {player_crashed, Reason}, State};
	false ->
	    {stop, {odd_info, Info}, State}
    end;
handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
