%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@kari.lan>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2013 by Paul Peregud <pawel@kari.lan>
%%%-------------------------------------------------------------------
-module(gm).

-behaviour(gen_server).

%% API
-export([start_link/4]).

-export([t/0, t1/0, t2/0, t3/0, t4/0, t5/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(s, State#state).

-type gs() :: undefined | error | registered | tournament | playing.

-record(state, {
	  socket,
	  transport,
	  buffer = [],
	  state :: gs()
	 }).

-include_lib("serv/include/logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

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

gav(Name, El) ->
    sxml:get_attr_value(Name, El).

gse(Name, El) ->
    sxml:get_sub_element(Name, El).

rec(State) ->
    ok = inet:setopts(State#state.socket, [{active, true}]),
    State.

handle_xml(E, State) ->
    ?D("~p", [{E, State}]),
    case gav(type, E) of
	"ping" ->
	    {ok, State, sxml:pong()};
	"gameMasterLogin" ->
	    E1 = gse(gameMasterLogin, E),
	    case {gav(id, E1), gav(gameType, E1), gav(playersMin, E1), gav(playersMax, E1)} of
		{false, _, _, _} ->
		    {stop, incomplete_xml, sxml:error("id attribute missing")};
		{_, false, _, _} ->
		    {stop, incomplete_xml, sxml:error("gameType attribute missing")};
		{_, _, false, _} ->
		    {stop, incomplete_xml, sxml:error("playersMin attribute missing")};
		{_, _, _, false} ->
		    {stop, incomplete_xml, sxml:error("playersMax attribute missing")};
		{Nick, GameType, PlayersMin, PlayersMax} ->
		    login(Nick, GameType, PlayersMin, PlayersMax, State)
	    end;
	X ->
	    ErrMsg = io_lib:fwrite("unknown message type: ~p", [X]),
	    ?D(ErrMsg, []),
	    ?ERROR(ErrMsg, []),
	    {stop, unknown_xml_message_type, ErrMsg}
    end.

%%%===================================================================
%%% Commands
%%%===================================================================

login(Id, "tick tack toe" = GameType, PlayersMin, PlayersMax, State = #state{state = undefined}) ->
    try gproc:add_local_name({game_master, GameType}) of
	true ->
	    gproc:add_local_property({gm_for_game, GameType}, {Id, PlayersMin, PlayersMax}),
	    gproc:add_local_property({gm}, {GameType}),
	    {ok, ?s{state = registered}, sxml:gm_login_response()}
    catch 
	_:_ ->
	    {stop, wrong_id, sxml:gm_login_response(gm_already_registered)}
    end;
login(_, OtherGame, _, _, #state{state = undefined}) ->
    ?ERROR("logging with improper game type: ~p", [OtherGame]),
    {stop, improper_game_type, sxml:gm_login_response(improper_game_type)};
login(_, _, _, _, _) ->
    {stop, already_registered, sxml:gm_login_response(already_registered)}.

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
