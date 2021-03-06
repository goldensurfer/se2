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
-export([start_link/4, join/2, publish/2, to_gm/2, move/4, end_game/2]).

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
	  playing = false,
	  move_to,
	  last_pl
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
    Pid ! {game_ended, WL}.

move(Pid, TheId, MoveEl, Who) ->
    %% room:to_gm(Pid, sxml:move(TheId, MoveEl)),
    gen_server:call(Pid, {move, TheId, MoveEl, Who}).


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
handle_call({move, TheId, MoveEl, {Pid, Nick}}, _From, State) ->
    OldRef = State#state.move_to,
    OldRef /= undefined andalso
	erlang:cancel_timer(OldRef),
    Msg = sxml:move(TheId, MoveEl),
    gm:send(?s.gm, Msg),
    Ref = erlang:start_timer(timer:seconds(20), self(), move_timeout),
    {reply, ok, State#state{move_to = Ref, last_pl = {Pid, Nick}}};
handle_call({join, {Pid, Nick}}, From, 
	    State = #state{playing = false, target = Target0}) ->
    _Ref = monitor(process, Pid),
    Players = [{Pid, Nick} | ?s.players],
    Target = tl(Target0),
    Captured = [From | ?s.captured],
    case Target of
	[] ->
	    ?NOTICE("starting game! ~p. Players: ~p", [?s.id, Players]),
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

handle_cast(_Msg, State) ->
    {stop, {odd_cast, _Msg}, State}.

handle_info(players_too_slow, State = #state{playing = false}) ->
    ?WARNING("players are too slow, shutting room down", []),
    [ gen_server:reply(Client, {error, someone_was_too_slow}) || Client <- ?s.captured ],
    game_host:game_ended(self(), ?s.id, undefined),
    {stop, normal, State};
handle_info(players_too_slow, State) ->
    {noreply, State};
handle_info({'DOWN', MonRef, _Type, _Object, Info}, 
	    State = #state{gm_ref = MonRef}) ->
    MsgT = "Game Master has crashed with msg: ~p",
    Msg = sxml:error(io_lib:fwrite(MsgT, [Info])),
    [ client:send(Pid, Msg) || {Pid, _} <- ?s.players ],
    {stop, {gm_crash, Info}, State};
handle_info({game_ended, {Winner, Loser} = WL}, State) ->
    ?NOTICE("game ~p ended. ~p has beat ~p", [?s.id, Winner, Loser]),
    [ client:game_ended(Pid) || {Pid, _} <- ?s.players ],
    game_host:game_ended(self(), ?s.id, WL),
    {stop, normal, State};
handle_info({'DOWN', _MonRef, _Type, Pid, Reason} = Info, State) ->
    case lists:keyfind(Pid, 1, ?s.players) of
	{Pid, Loser} ->
	    MsgT = "Player ~p has crashed with reason: ~p",
	    ?WARNING(MsgT, [Loser, Reason]),
	    [{_, Winner}] = ?s.players -- [{Pid, Loser}],
	    handle_info({game_ended, {Winner, Loser}}, State);
	false ->
	    {stop, {odd_down, Info}, State}
    end;
handle_info({timeout, Ref, move_timeout}, State = #state{move_to = Ref1, last_pl = Pl}) when Ref =:= Ref1 ->
    [{LoserPid, Loser}] = ?s.players -- [Pl],
    {_, Winner} = Pl,
    GameOver = sxml:game_over(?s.id, {Winner, Loser}, undefined),
    [ client:send(Pid, GameOver) || {Pid, _} <- ?s.players ],
    erlang:exit(LoserPid, move_timeout),
    ?NOTICE("Player ~p won because player ~p took more then 20 seconds to make a move", [Winner, Loser]),
    handle_info({game_ended, {Winner, Loser}}, State);
handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
