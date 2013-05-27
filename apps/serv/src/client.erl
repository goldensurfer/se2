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

%% API
-export([start_link/4, join_game/4, join_ch_game/4, game_ended/1,
	 join_championship/2, send/2]).

-export([t/0, t1/0, t2/0, t3/0, t4/0, t5/0]).

-import(sxml, [gse/2, gav/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  nick :: any(),
	  socket,
	  transport,
	  buffer = [],
	  pl_state :: gs(),
	  is_tournament = false :: boolean()
	 }).

-type gs() :: undefined | error | registered | playing.
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

send(Pid, Msg) ->
    gen_server:cast(Pid, {send, Msg}).

join_game(Pid, GameType, GamePid, GameId) ->
    gen_server:cast(Pid, {join_game, GameType, GamePid, GameId}).

join_ch_game(Pid, GameType, GamePid, GameId) ->
    gen_server:cast(Pid, {join_ch_game, GameType, GamePid, GameId}).

join_championship(Pid, GameType) ->
    gen_server:cast(Pid, {join_championship, GameType}).

game_ended(Pid) ->
    gen_server:cast(Pid, game_ended).

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

handle_cast({join_game, GameType, GamePid, _GameId} = Msg, 
	    State = #state{pl_state = registered}) ->
    ?INFO("join_game ~p", [Msg]),
    gproc:munreg(p, l, [{registered_for_game, GameType}, {registered}]),
    case room:join(GamePid, ?s.nick) of
	true ->
	    {noreply, ?s{pl_state = playing}};
	{error, _} ->
	    gproc:reg(p, l, [{{registered_for_game, GameType}, ?s.nick}, 
			     {{registered}, GameType}]),
	    {noreply, State}
    end;
handle_cast({join_ch_game, _GameType, GamePid, _GameId} = Msg, 
	    State = #state{pl_state = registered, is_tournament = true}) ->
    ?INFO("join_game ~p", [Msg]),
    case room:join(GamePid, ?s.nick) of
	true ->
	    {noreply, ?s{pl_state = playing}};
	{error, _} ->
	    {noreply, State}
    end;
handle_cast(game_ended, State = #state{pl_state = playing}) ->
    {noreply, ?s{pl_state = registered}};
handle_cast({join_championship, GameType}, State = #state{pl_state = registered}) ->
    gproc:munreg(p, l, [{registered_for_game, GameType}, {registered}]),
    {noreply, ?s{pl_state = registered, is_tournament = true}};
handle_cast({send, Msg}, State) ->
    gen_tcp:send(?s.socket, Msg),
    {noreply, State};
handle_cast(Msg, State) ->
    {stop, {odd_cast, Msg}, State}.

handle_info({tcp, S, DataBin}, State) ->
    Data0 = binary_to_list(DataBin),
    %% ?DBG("got data: ~p", [Data0]),
    Data = ?s.buffer++Data0,
    try xmerl_scan:string(Data, [{quiet, true}]) of
	{Element, Tail} ->
	    try handle_xml(Element, State) of
		{ok, State1} ->
		    handle_info({tcp, S, <<>>}, 
				rec(State1#state{buffer = Tail}));
		{ok, State1, Msg} ->
		    gen_tcp:send(?s.socket, Msg),
		    handle_info({tcp, S, <<>>}, 
				rec(State1#state{buffer = Tail}));
		{stop, Reason, State1 = #state{socket = Socket}} ->
		    gen_tcp:close(Socket),
		    {stop, Reason, State1};
		{stop, Reason, Msg} ->
		    gen_tcp:send(?s.socket, Msg),
		    gen_tcp:close(?s.socket),
		    {stop, Reason, State}
	    catch 
		error:{stop, Reason, State = #state{}} ->
		    gen_tcp:close(?s.socket),
		    {stop, Reason, State};
		error:{stop, Reason, Msg} ->
		    gen_tcp:send(?s.socket, Msg),
		    gen_tcp:close(?s.socket),
		    {stop, Reason, State}
	    end
    catch
	ErrType:ErrMsg ->
	    %% ?DBG("parsing xml: ~p", [{ErrType, ErrMsg}]),
	    {noreply, rec(?s{buffer = Data})}
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
    inet:setopts(?s.socket, [{active, true}]),
    State.

%% this function handles Players's specific part of XML protocol. 
-spec handle_xml(#xmlElement{}, state()) ->
			{ok, NewState::state()} |
			{ok, NewState::state(), Response::msg()} |
			{stop, Error::atom(), ErrorMsg::msg()} |
			{stop, Reason::atom(), NewState::state()}. %% this one is for graceful termination
handle_xml(E, State) ->
    ?DBG("~p", [{E, State}]),
    Type = gav(type, E),
    ?DBG("msg type: ~p", [Type]),
    case Type of
	"logout" ->
	    {ok, State};
	"error" ->
	    ?WARNING("GOT ERROR:~n~p", [E]),
	    {stop, {received_error, E}, State};
	"ping" ->
	    {ok, State, sxml:pong()};
	"playerLogin" ->
	    E1 = gse(playerLogin, E),
	    Nick = gav(nick, E1), 
	    GameType = gav(gameType, E1),
	    ?NOTICE("msg type ~p, nick ~p, gametype ~p", 
		    [Type, Nick, GameType]),
	    login(Nick, GameType, State);
	"move" ->
	    E1 = gse(gameId, E), 
	    MoveEl = gse(move, E),
	    TheId = gav(id, E1),
	    case gproc:where({n, l, {room, TheId}}) of
		Pid when is_pid(Pid) ->
		    E3 = gse(tic, MoveEl),
		    X = gav(x, E3),
		    Y = gav(y, E3),
		    ?INFO("~p moves to ~p, id: ~p", [?s.nick, {X, Y}, TheId]),
		    room:to_gm(Pid, sxml:move(TheId, MoveEl)),
		    {ok, State};
		_ ->
		    T = "game with gameId = ~p does not exist",
		    ErrMsg = io_lib:fwrite(T, [TheId]),
		    {stop, {error, no_such_game_id, TheId}, 
		     sxml:error(ErrMsg)}
	    end;
	"leaveGame" ->
	    E1 = gse(gameId, E),
	    TheId = gav(id, E1),
	    ?NOTICE("msg type: ~p, id: ~p", [Type, TheId]),
	    {ok, State};
	"thank you" ->
	    E1 = gse(gameId, E),
	    TheId = gav(id, E1),
	    ?NOTICE("~p says: ~p, id: ~p", [?s.nick, Type, TheId]),
	    {ok, State};
	X ->
	    ErrMsg = io_lib:fwrite("unknown message type: ~p", [X]),
	    ?ERROR(ErrMsg, []),
	    {stop, unknown_xml_message_type, ErrMsg}
    end.

%%%===================================================================
%%% Commands
%%%===================================================================

login(Nick, GameType, State = #state{pl_state = undefined}) ->
    try gproc:add_local_name({player, Nick}) of
	true ->
	    gproc:add_local_property({registered_for_game, GameType}, Nick),
	    gproc:add_local_property({registered}, GameType),
	    game_host:check_game(GameType),
	    {ok, ?s{nick = Nick, pl_state = registered}, sxml:login_response()}
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
