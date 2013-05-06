%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@kari.lan>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% This module handles Game Masters's TCP connection, xml parsing 
%%% and commands. Uses gen_server behavior and is a part 
%%% of supervision tree.
%%%
%%% Needs major refactoring.
%%%
%%% @end
%%% Created : 14 Apr 2013 by Paul Peregud <pawel@kari.lan>
%%%-------------------------------------------------------------------
-module(gm).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_link/4, begin_game/3]).

-export([t/0, t1/0, t2/0, t3/0, t4/0, t5/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(sxml, [gse/2, gav/2]).

-define(SERVER, ?MODULE). 
-define(s, State#state).

-record(state, {
	  socket,
	  transport,
	  buffer = [],
	  state :: gs()
	 }).

-type gs() :: undefined | error | registered.
-type state() :: #state{}.
-type msg() :: binary().

-include_lib("serv/include/se2.hrl").
-include_lib("serv/include/logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

begin_game(Pid, Id, Nicks) ->
    gen_server:cast(Pid, {begin_game, self(), Id, Nicks}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ListenerPid, Socket, Transport, _Opts = []]) ->
    self() ! {accept_ack, ListenerPid},
    {ok, #state{
	    socket = Socket, 
	    transport = Transport
	   }}.

handle_call(Request, _From, State) ->
    {stop, {odd_call, Request}, State}.

handle_cast({begin_game, RoomPid, Id, Nicks}, State = #state{state = registered}) ->
    GameId = {gameId, [{id, Id}], []},
    Players = [ {player, [{nick, Nick}], []} || Nick <- Nicks ],
    Msg = {message, [{type,beginGame}], [GameId | Players]},
    gen_tcp:send(?s.socket, sxml:msg(Msg)),
    {noreply, State};
handle_cast(Msg, State) ->
    {stop, {odd_cast, Msg}, State}.

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
		{stop, Reason, State = #state{}} ->
		    gen_tcp:close(State#state.socket),
		    {stop, Reason, State};
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
handle_info({accept_ack, ListenerPid}, State) ->
    ranch:accept_ack(ListenerPid),
    {noreply, rec(State)};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

rec(State) ->
    ok = inet:setopts(State#state.socket, [{active, true}]),
    State.

icl(Msg) ->
    {stop, incomplete_xml, sxml:error(Msg)}.

%% this function handles Game Master's specific part of XML protocol. 
-spec handle_xml(#xmlElement{}, state()) ->
			{ok, NewState::state()} |
			{ok, NewState::state(), Response::msg()} |
			{stop, Error::atom(), ErrorMsg::msg()} |
			{stop, Reason::atom(), NewState::state()}. %% this one is for graceful termination
handle_xml(E, State) ->
    ?D("~p", [{E, State}]),
    Type = gav(type, E),
    case Type of
	"error" ->
	    {stop, received_error, State};
	"ping" ->
	    {ok, State, sxml:pong()};
	"move" ->
	    E1 = gse(gameId, E), 
	    _E2 = gse(move, E),
	    _TheId = gav(id, E1),
	    ?D("msg type: ~p, id: ~p", [Type, _TheId]),
	    {ok, State};
	"playerLeftGame" ->
	    E1 = gse(player, E), 
	    E2 = gse(gameId, E),
	    Nick = gav(nick, E1), 
	    Id = gav(id, E2),
	    ?D("msg type: ~p, nick: ~p, id: ~p", [Type, Nick, Id]),
	    {ok, State};
	"serverShutdown" ->
	    ?D("msg type: ~p", [Type]),
	    {ok, State};
	"gameState" ->
	    GameId = gse(gameId, E),
	    case gse({nextPlayer, gameOver}, E) of
		{E1, false} ->
		    case gav(nick, E1) of
			false ->
			    icl("nick attribute missing");
			Nick ->
			    ?D("msg type: ~p/nextPlayer, gameId: ~p, nick: ~p", [Type, GameId, Nick]),
			    {ok, State}
		    end;
		{_, E2} ->
		    Nick = gav(nick, E2), 
		    Res = gav(result, E2),
		    case Res of
			Res when Res /= "winner", Res /= "loser" ->
			    icl("result attribute: allowed values are loser or winner");
			Res ->
			    ?D("msg type: ~p/gameOver, gameId: ~p, nick: ~p, result", [Type, GameId, Nick, Res]),
			    {ok, State}
		    end
	    end;
	"gameMasterLogin" ->
	    E1 = gse(gameMasterLogin, E),
	    Nick = gav(id, E1), 
	    GameType = gav(gameType, E1), 
	    PlayersMin0 = gav(playersMin, E1), 
	    PlayersMax0 = gav(playersMax, E1),
	    PlayersMin = list_to_integer(PlayersMin0),
	    PlayersMax = list_to_integer(PlayersMax0),
	    login(Nick, GameType, PlayersMin, PlayersMax, State);
	X ->
	    ErrMsg = io_lib:fwrite("unknown message type: ~p", [X]),
	    ?D(ErrMsg, []),
	    ?ERROR(ErrMsg, []),
	    {stop, unknown_xml_message_type, ErrMsg}
    end.

%%%===================================================================
%%% Commands
%%%===================================================================

login(Id, ?MAGIC = GameType, PlayersMin, PlayersMax, State = #state{state = undefined}) ->
    case gp:reg(n, {gm, GameType}, true) of
	true ->
	    gproc:mreg(p, l, [{{gm_for_game, GameType}, {Id, PlayersMin, PlayersMax}}]),
	    {ok, ?s{state = registered}, sxml:login_response()};
	false ->
    	    {stop, already_registered, sxml:login_response(gm_already_registered)}
    end;
login(_, OtherGame, _, _, #state{state = undefined}) ->
    {stop, improper_game_type, sxml:login_response(improper_game_type)};
login(_, _, _, _, _) ->
    {stop, already_registered, sxml:login_response(already_registered)}.

%%%===================================================================
%%% Tests
%%%===================================================================

xml_test() ->
    Cases0 = [t(), t1(), t2(), t3(), t4(), t5()],
    Cases1 = [binary_to_list(X) || X <- Cases0],
    Answers = [ok, ok, err, ok, ok, ok],
    Nos = lists:seq(1, length(Cases1)),
    Cases = lists:zip3(Nos, Cases1, Answers),
    F = fun({No, Arg, Res}) ->
		R1 = try xmerl_scan:string(Arg) of
			 {_, _} ->
			     ok
		     catch
			 ErrType:ErrMsg ->
			     err
		     end,
		{No, Arg, Res, Res} 
		    = {No, Arg, Res, R1}
	end,
    lists:map(F, Cases).
		
t() ->
    <<"<message type=\"playerLogin\">
<playerLogin nick=\"Jaedong\" gameType=\"Starcraft\"/>
</message>">>.

t1() ->
    <<"<message type=\"playerLogin\">
<playerLogin nick=\"Jaedong\" gameType=\"Starcraft\"/>
</message>
<message type">>.

t2() ->
    <<"<message type=\"playerLogin\">
<playerLogin nick=\"Jaedong\" game">>.

t3() ->
    <<"<message type=\"playerLogin\">
<playerLogin nick=\"Jaedong\" gameType=\"Starcraft\"/>
</message>
<message type=\"playerLogin\">
<playerLogin nick=\"Jaedong\" gameType=\"Starcraft\"/>
</message>">>.


t4() ->
    <<"<message type=\"playerLogin\">
<playerLogin nick=\"Jaedong\" gameType=\"Starcraft\"/>
</message><message type">>.

t5() ->
    <<"<message type=\"playerLogin\">
<playerLogin nick=\"Jaedong\" gameType=\"Starcraft\"/>
</message><message type=\"playerLogin\">
<playerLogin nick=\"Jaedong\" gameType=\"Starcraft\"/>
</message>">>.
