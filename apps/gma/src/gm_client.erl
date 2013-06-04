%%%-------------------------------------------------------------------
%%% @author 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%% @copyright (C) 2013, 190033 peregud pavel
%%% @doc
%%%
%%% This module is a GM client. It handles it's TCP connection, 
%%% xml parsing and commands. Uses gen_server behavior but it is not
%%% part of supervision tree.
%%%
%%% Needs major refactoring.
%%%
%%% @end
%%% Created : 16 Apr 2013 by 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%%-------------------------------------------------------------------
-module(gm_client).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_link/3, start_link/4]).
-export([next_player/3, game_over/3]).

%% test API
%% -export([sendGS/0, sendLogin/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(sxml, [gse/2, gav/2]).

-define(SERVER, ?MODULE). 

-record(state, {
	  socket,
	  buffer = []
	 }).

-type state() :: #state{}.
-type msg() :: binary().

-include_lib("serv/include/se2.hrl").
-include_lib("serv/include/logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Port, Id) ->
    start_link(Host, Port, Id, ?MAGIC).

start_link(Host, Port, Id, Game) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port, Id, Game], []).

next_player(GameId, Next, GS) ->
    gen_server:call(?SERVER, {next_player, GameId, Next, GS}).

game_over(GameId, Winner, GS) ->
    gen_server:call(?SERVER, {game_over, GameId, Winner, GS}).

%% sendGS() ->
%%     GI = {gameId, [{id, "123"}], []},
%%     NP = {nextPlayer, [{nick, "TestPlayer"}], []},
%%     gen_server:cast(?SERVER, msg({message, [{type, "gameState"}], [GI, NP]})).

%% sendLogin() ->
%%     Msg = sxml:msg({message, [{type, playerLogin}], [{playerLogin, [{nick, "Jaedong"}, {gameType, ?MAGIC}], []}]}),
%%     gen_server:cast(?SERVER, Msg).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port, Id, Game]) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary]),
    gen_tcp:send(Socket, sxml:gm_login(Id, Game)),
    {ok, #state{socket = Socket}}.

handle_call({next_player, Id, Player, GS}, _From, State) ->
    GameId = {gameId, [{id, Id}], []},
    NextPlayer = {nextPlayer, [{nick, Player}], []},
    GameState = case GS of 
		    undefined ->
			{gameState, [], []};
		    {X, Y} ->
			Tac = {tac, [{x, X}, {y, Y}], []},
			{gameState, [], [Tac]}
		end,
    Msg = {message, [{type, gameState}], [GameId, NextPlayer, GameState]},
    gen_tcp:send(?s.socket, sxml:msg(Msg)),
    {reply, ok, State};
handle_call({game_over, Id, {Winner, Loser}, GS}, _From, State) ->
    ?DBG("game over: ~p", [{Id, {Winner, Loser}, GS}]),
    gen_tcp:send(?s.socket, sxml:game_over(Id, {Winner, Loser}, GS)),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    gen_tcp:send(?s.socket, Msg),
    {noreply, State}.

handle_info({tcp, S, DataBin}, State) ->
    Data0 = binary_to_list(DataBin),
    %% ?DBG("got data: ~p", [Data0]),
    Data = ?s.buffer++Data0,
    try xmerl_scan:string(Data, [{quiet, true}]) of
	{Element, Tail} ->
	    case handle_xml(Element, State) of
		{ok, State1} ->
		    handle_info({tcp, S, <<"">>}, 
				rec(State1#state{buffer = Tail}));
		{ok, State1, Msg} ->
		    gen_tcp:send(?s.socket, Msg),
		    handle_info({tcp, S, <<"">>}, 
				rec(State1#state{buffer = Tail}));
		{stop, Reason, Msg} ->
		    gen_tcp:send(?s.socket, Msg),
		    gen_tcp:close(?s.socket),
		    {stop, Reason, State}
	    end
    catch
	ErrType:ErrMsg ->
	    %% ?DBG("parsing xml: ~p", [{ErrType, ErrMsg}]),
	    {noreply, rec(?s{buffer = Data})}
    end;
handle_info({tcp_closed, _}, State) ->
    {stop, server_has_closed_connection, State};
handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

msg(Msg) ->
    sxml:msg(Msg).

%% this function handles Game Master's specific part of XML protocol. 
-spec handle_xml(#xmlElement{}, state()) ->
			{ok, NewState::state()} |
			{ok, NewState::state(), Response::msg()} |
			{stop, Error::atom(), ErrorMsg::msg()}.
handle_xml(E, State) ->
    %% ?DBG("~p", [{E, State}]),
    Type = gav(type, E),
    case Type of
	"error" ->
	    ?WARNING("got error, finishing."),
	    {stop, normal, State};
	"ping" ->
	    {ok, State, sxml:pong()};
	"beginGame" ->
	    E1 = gse(gameId, E),
	    Id = gav(id, E1),
	    Players = sxml:get_sub_elements(player, E),
	    Nicks = [ gav(nick, E2) || E2 <- Players ],
	    begin_game(Id, Nicks, State);
	"loginResponse" ->
	    E1 = gse(response, E),
	    Accept = gav(accept, E1),
	    case Accept of
		"yes" ->
		    finish_login(State);
		"no" ->
		    E2 = gse(error, E),
		    ErrId = gav(id, E2),
		    {stop, {login_rejected_by_server, ErrId}}
	    end;
	"move" ->
	    E1 = gse(gameId, E), 
	    E2 = gse(move, E),
	    E3 = gse(tic, E2),
	    X = gav(x, E3),
	    Y = gav(y, E3),
	    GameId = gav(id, E1),
	    Pid = validate_game_id(GameId),
	    ttt:move(Pid, X, Y),
	    {ok, State};
	X ->
	    ErrMsg = io_lib:fwrite("unknown message type: ~p", [X]),
	    ?ERROR(ErrMsg, []),
	    {stop, unknown_xml_message_type, ErrMsg}
    end.

validate_game_id(GameId) ->
    case gproc:where({n, l, {game, GameId}}) of
	Pid when is_pid(Pid) ->
	    Pid;
	Else ->
	    ?ERROR("validate_game_id: lookup returned ~p", [Else]),
	    T = "game with gameId = ~p does not exist",
	    Msg = io_lib:fwrite(T, [GameId]),
	    erlang:error({stop, wrong_game_id, sxml:error(Msg)})
    end.


%%%===================================================================
%%% Commands
%%%===================================================================
begin_game(GameId, Players, State) ->
    Self = self(),
    case ttt:check_conditions(Players) of
	true ->
	    Key = {n, l, {game, GameId}},
	    case gproc:reg_or_locate(Key) of
		{Self, _} ->
		    ?NOTICE("Starting game ~p for ~p", [GameId, Players]),
		    {ok, GamePid} = games_sup:add_child(self(), GameId, Players),
		    gproc:give_away(Key, GamePid),
		    {ok, State};
		_ ->
		    Msg = io_lib:fwrite("Game with id ~p already exists!", [GameId]),
		    {stop, game_already_exists, sxml:error(Msg)}
	    end;
	false ->
	    Msg = io_lib:fwrite("wrong number of players: ~p", [length(Players)]),
	    {stop, game_conditions_unsatisfied, sxml:error(Msg)}
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================
finish_login(State) ->
    ?NOTICE("Logged in!", []),
    {ok, State}.

rec(State) ->
    inet:setopts(?s.socket, [{active, true}]),
    State.

    
