%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@kari.lan>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% This module handles player's TCP connection, xml parsing 
%%% and commands. Uses gen_server behavior and is a part 
%%% of supervision tree.
%%%
%%% Needs major refactoring.
%%%
%%% @end
%%% Created : 14 Apr 2013 by Paul Peregud <pawel@kari.lan>
%%%-------------------------------------------------------------------
-module(client).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_link/4]).

-export([t/0, t1/0, t2/0, t3/0, t4/0, t5/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(s, State#state).

-record(state, {
	  socket,
	  transport,
	  buffer = [],
	  pl_state :: gs()
	 }).

-type gs() :: undefined | error | registered | tournament | playing.
-type state() :: #state{}.
-type msg() :: binary().

-include_lib("serv/include/logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

sendLR() ->
    R = {response, [{accept, "yes"}], []},
    Msg = {message, [{type, "loginResponse"}], [R]},
    gen_server:cast(?MODULE, msg(Msg)).

sendBG() ->
    GI = {gameId, [{id, "123"}], []},
    N = {player, [{nick, "Jaedong"}], []},
    Msg = {message, [{type, "beginGame"}], [GI, N]},
    gen_server:cast(?MODULE, msg(Msg)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ListenerPid, Socket, Transport, _Opts = []]) ->
    gproc:add_local_property({?MODULE}, true),
    self() ! {accept_ack, ListenerPid},
    {ok, #state{
	    socket = Socket, 
	    transport = Transport
	   }}.

handle_call(Request, _From, State) ->
    {stop, {odd_call, Request}, State}.

handle_cast(Msg, State) ->
    gen_tcp:send(State#state.socket, Msg),
    {noreply, State}.
%% handle_cast(Msg, State) ->
%%     {stop, {odd_cast, Msg}, State}.

handle_info({tcp, _Socket, DataBin}, State) ->
    Data0 = binary_to_list(DataBin),
    ?D("got data: ~p", [Data0]),
    Data = State#state.buffer++Data0,
    try xmerl_scan:string(Data) of
	{Element, Tail} ->
	    try handle_xml(Element, State) of
		{ok, State1} ->
		    {noreply, rec(State1#state{buffer = Tail})};
		{ok, State1, Msg} ->
		    gen_tcp:send(?s.socket, Msg),
		    {noreply, rec(State1#state{buffer = Tail})}
	    catch 
		error:{stop, Reason, State = #state{}} ->
		    gen_tcp:close(State#state.socket),
		    {stop, Reason, State};
		error:{stop, Reason, Msg} ->
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

msg(Msg) ->
    sxml:msg(Msg).

icl(Msg) ->
    {stop, incomplete_xml, sxml:error(Msg)}.

gav(Name, El) ->
    case sxml:get_attr_value(Name, El) of
	false ->
	    Msg = io_lib:fwrite("~p attribute missing", [Name]),
	    erlang:error({stop, incomplete_xml, sxml:error(Msg)});
	AttrValue -> 
	    AttrValue
    end.

gse(Name, El) ->
    case sxml:get_sub_element(Name, El) of
	false ->
	    Msg = io_lib:fwrite("~p subelement missing", [Name]),
	    erlang:error({stop, incomplete_xml, sxml:error(Msg)});
	Element -> 
	    Element
    end.

rec(State) ->
    ok = inet:setopts(State#state.socket, [{active, true}]),
    State.

%% this function handles Players's specific part of XML protocol. 
-spec handle_xml(#xmlElement{}, state()) ->
			{ok, NewState::state()} |
			{ok, NewState::state(), Response::msg()} |
			{stop, Error::atom(), ErrorMsg::msg()} |
			{stop, Reason::atom(), NewState::state()}. %% this one is for graceful termination
handle_xml(E, State) ->
    ?D("~p", [{E, State}]),
    Type = gav(type, E),
    ?D("msg type: ~p", [Type]),
    case Type of
	"logout" ->
	    ?D("msg type: ~p", [Type]),
	    {ok, State};
	"error" ->
	    {stop, received_error, State};
	"ping" ->
	    {ok, State, sxml:pong()};
	"playerLogin" ->
	    E1 = gse(playerLogin, E),
	    Nick = gav(nick, E1), 
	    GameType = gav(gameType, E1),
	    ?D("msg type ~p, nick ~p, gametype ~p", [Type, Nick, GameType]),
	    login(Nick, GameType, State);
	"move" ->
	    E1 = gse(gameId, E), 
	    E2 = gse(move, E),
	    TheId = gav(id, E1),
	    ?D("msg type: ~p, id: ~p", [Type, TheId]),
	    {ok, State};
	"leaveGame" ->
	    E1 = gse(gameId, E),
	    TheId = gav(id, E1),
	    ?D("msg type: ~p, id: ~p", [Type, TheId]),
	    {ok, State};
	"thank you" ->
	    E1 = gse(gameId, E),
	    TheId = gav(id, E1),
	    ?D("msg type: ~p, id: ~p", [Type, TheId]),
	    {ok, State};
	X ->
	    ErrMsg = io_lib:fwrite("unknown message type: ~p", [X]),
	    ?D(ErrMsg, []),
	    ?ERROR(ErrMsg, []),
	    {stop, unknown_xml_message_type, ErrMsg}
    end.

%%%===================================================================
%%% Commands
%%%===================================================================

login(Nick, GameType, State = #state{pl_state = undefined}) ->
    try gproc:add_local_name({player, Nick}) of
	true ->
	    %% gproc:add_local_property({registered_for_game, GameType}, Nick),
	    %% gproc:add_local_property({registered}, GameType),
	    {ok, ?s{pl_state = registered}, sxml:login_response()}
    catch 
	_:_ ->
	    {stop, wrong_nick, sxml:login_response(wrong_nick)}
    end;
login(_, _, _) ->
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
