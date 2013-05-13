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
-export([start_link/4, begin_game/3, send/2]).

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

send(Pid, Msg) ->
    gen_server:cast(Pid, {send, Msg}).

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

handle_cast({send, Msg}, State) ->
    gen_tcp:send(?s.socket, Msg),
    {noreply, State};
handle_cast({begin_game, _RoomPid, Id, Nicks}, State = #state{state = registered}) ->
    Msg = sxml:begin_game(Id, Nicks),
    gen_tcp:send(?s.socket, Msg),
    {noreply, State};
handle_cast(Msg, State) ->
    {stop, {odd_cast, Msg}, State}.

handle_info({tcp, _Socket, DataBin}, State) ->
    Data0 = binary_to_list(DataBin),
    ?DBG("got data: ~p", [Data0]),
    Data = State#state.buffer++Data0,
    try xmerl_scan:string(Data) of
	{Element, Tail} ->
	    try handle_xml(Element, State) of
		{ok, State1} ->
		    {noreply, rec(State1#state{buffer = Tail})};
		{ok, State1, Msg} ->
		    gen_tcp:send(?s.socket, Msg),
		    {noreply, rec(State1#state{buffer = Tail})};
		{stop, Reason, State = #state{}} ->
		    gen_tcp:close(State#state.socket),
		    ?ERROR("stopping: ~p", [Reason]),
		    {stop, Reason, State};
		{stop, Reason, Msg} ->
		    gen_tcp:send(State#state.socket, Msg),
		    gen_tcp:close(State#state.socket),
		    ?ERROR("stopping: ~p~nmsg: ~p", [Reason, Msg]),
		    {stop, Reason, State}
	    catch 
		error:{stop, Reason, State = #state{}} ->
		    gen_tcp:close(State#state.socket),
		    ?ERROR("stopping: ~p", [Reason]),
		    {stop, Reason, State};
		error:{stop, Reason, Msg} ->
		    gen_tcp:send(State#state.socket, Msg),
		    gen_tcp:close(State#state.socket),
		    ?ERROR("stopping: ~p~nmsg: ~p", [Reason, Msg]),
		    {stop, Reason, State}
	    end
    catch
	ErrType:ErrMsg ->
	    ?ERROR("parsing xml failed:~n~p", [{ErrType, ErrMsg}]),
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
    %% handle closed socket by receiving message
    _ = inet:setopts(State#state.socket, [{active, true}]),
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
    ?DBG("Got xml:~n~p", [{E, State}]),
    Type = gav(type, E),
    case Type of
	"error" ->
	    {stop, received_error, State};
	"ping" ->
	    {ok, State, sxml:pong()};
	"playerLeftGame" ->
	    E1 = gse(player, E), 
	    E2 = gse(gameId, E),
	    Nick = gav(nick, E1), 
	    Id = gav(id, E2),
	    ?WARNING("msg type: ~p, nick: ~p, id: ~p", [Type, Nick, Id]),
	    {ok, State};
	"serverShutdown" ->
	    ?CRITICAL("msg type: ~p", [Type]),
	    {ok, State};
	"gameState" ->
	    GameIdTag = gse(gameId, E),
	    GS = gse(gameState, E),
	    GameId = gav(id, GameIdTag),
	    Pid = validate_room_id(GameId),
	    case gse({nextPlayer, gameOver}, E) of
		{E1, false} ->
		    Nick = gav(nick, E1),
		    ?DBG("msg type: ~p/nextPlayer, gameId: ~p, nick: ~p", [Type, GameId, Nick]),
		    room:publish(Pid, sxml:next_player(GameId, Nick, GS)),
		    {ok, State};
		{_, E2} ->
		    Players = sxml:gsen(player, E2, 2),
		    ?INFO("msg type: ~p/gameOver, gameId: ~p, ~nplayers: ~p~nGS:~p", 
			  [Type, GameId, Players, GS]),
		    WL = get_winner_loser(Players),
		    room:publish(Pid, sxml:game_over(GameId, WL, GS)),
		    {ok, State}
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
	    ?ERROR(ErrMsg, []),
	    {stop, unknown_xml_message_type, ErrMsg}
    end.

validate_room_id(GameId) ->
    case gproc:where({n, l, {room, GameId}}) of 
	Pid when is_pid(Pid) ->
	    Pid;
	_ ->
	    T = "game with gameId = ~p does not exist",
	    Msg = io_lib:fwrite(T, [GameId]),
	    erlang:error({stop, wrong_room_id, sxml:error(Msg)})
    end.

get_winner_loser(Players) ->
    {get_pl("winner", Players), get_pl("loser", Players)}.

get_pl(Result, Players) ->
    Filter = fun(Pl) -> Result =:= sxml:get_attr_value(result, Pl) end,
    L = lists:filter(Filter, Players),
    ?DBG("L is ~p", [L]),
    case L of
	[Pl] ->
	    Res = sxml:get_attr_value(nick, Pl),
	    T = "~p has no nick specified",
	    Msg = io_lib:fwrite(T, [Result]),
	    Res =/= false orelse
		begin
		    ?DBG("~p", [Msg]),
		    erlang:error({stop, incomplete_xml, sxml:error(Msg)})
		end,
	    Res;
	[] ->
	    T = "please specify the ~p",
	    Msg = io_lib:fwrite(T, [Result]),
	    erlang:error({stop, incomplete_xml, sxml:error(Msg)})
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
login(_, _OtherGame, _, _, #state{state = undefined}) ->
    Msg = sxml:login_response(improper_game_type),
    {stop, {improper_game_type, _OtherGame}, Msg};
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
			 _ErrType:_ErrMsg ->
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
