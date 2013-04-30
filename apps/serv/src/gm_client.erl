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

%% test API
-export([sendGS/0, sendLogin/0]).

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

-define(s, State#state).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port, Id, Game], []).

sendGS() ->
    GI = {gameId, [{id, "123"}], []},
    NP = {nextPlayer, [{nick, "TestPlayer"}], []},
    gen_server:cast(?SERVER, msg({message, [{type, "gameState"}], [GI, NP]})).

sendLogin() ->
    Msg = sxml:msg({message, [{type, playerLogin}], [{playerLogin, [{nick, "Jaedong"}, {gameType, ?MAGIC}], []}]}),
    gen_server:cast(?SERVER, Msg).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port, Id, Game]) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary]),
    gen_tcp:send(Socket, sxml:gm_login(Id, Game)),
    {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    gen_tcp:send(State#state.socket, Msg),
    {noreply, State}.

handle_info({tcp, _Socket, DataBin}, State) ->
    Data0 = binary_to_list(DataBin),
    ?D("got data: ~p", [Data0]),
    Data = State#state.buffer++Data0,
    try xmerl_scan:string(Data) of
	{Element, Tail} ->
	    case handle_xml(Element, State) of
		{ok, State1} ->
		    {noreply, rec(State1#state{buffer = Tail})};
		{ok, State1, Msg} ->
		    gen_tcp:send(?s.socket, Msg),
		    {noreply, rec(State1#state{buffer = Tail})};
		{stop, Reason, Msg} ->
		    gen_tcp:send(State#state.socket, Msg),
		    gen_tcp:close(State#state.socket),
		    {stop, Reason, State}
	    end
    catch
	ErrType:ErrMsg ->
	    ?D("parsing xml: ~p", [{ErrType, ErrMsg}]),
	    {noreply, rec(State#state{buffer = Data})}
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
    ?D("~p", [{E, State}]),
    case gav(type, E) of
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
	    error(not_impl);
	X ->
	    ErrMsg = io_lib:fwrite("unknown message type: ~p", [X]),
	    ?D(ErrMsg, []),
	    ?ERROR(ErrMsg, []),
	    {stop, unknown_xml_message_type, ErrMsg}
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
		    {ok, GamePid} = games_sup:add_child(GameId, Players),
		    Self = gproc:give_away(Key, GamePid),
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
%%% Commands
%%%===================================================================
finish_login(State) ->
    {ok, State}.

rec(State) ->
    ok = inet:setopts(State#state.socket, [{active, true}]),
    State.
