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

-compile(export_all).

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

-include_lib("serv/include/se2.hrl").
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

gav(Name, El) ->
    sxml:get_attr_value(Name, El).

gse(Name, El) ->
    sxml:get_sub_element(Name, El).

rec(State) ->
    ok = inet:setopts(State#state.socket, [{active, true}]),
    State.

icl(Msg) ->
    {stop, incomplete_xml, sxml:error(Msg)}.

handle_xml(E, State) ->
    ?D("~p", [{E, State}]),
    Type = gav(type, E),
    case Type of
	"error" ->
	    {stop, received_error, State};
	"ping" ->
	    {ok, State, sxml:pong()};
	"beginGame" ->
	    case {gse(gameId, E), gse(player, E)} of
		{false, _} ->
		    icl("gameId subelement missing");
		{_, false} ->
		    icl("player subelement missing");
		{E1, E2} ->
		    case {gav(id, E1),gav(nick, E2)} of
			{false, _} ->
			    icl("id attribute missing");
			{_, false} ->
			    icl("nick attribute missing");
			{Id, Nick} ->
			    ?D("msg type: ~p, nick: ~p, id: ~p", [Type, Nick, Id]),
			    {ok, State}
		    end
	    end;
	"move" ->
	    case {gse(gameId, E), gse(move, E)} of
		{false, _} ->
		    icl("gameId subelement missing");
		{_, false} ->
		    icl("move subelement missing");
		{E1, _E2} ->
		    case gav(id, E1) of
			false ->
			    icl("id attribute missing");
			_TheId ->
			    ?D("msg type: ~p, id: ~p", [Type, _TheId]),
			    {ok, State}
		    end
	    end;
	"playerLeftGame" ->
	    case {gse(player, E), gse(gameId, E)} of
		{false, _} ->
		    icl("player subelement missing");
		{_, false} ->
		    icl("gameId subelement missing");
		{E1, E2} ->
		    case {gav(nick, E1),gav(id, E2)} of
			{false, _} ->
			    icl("nick attribute missing");
			{_, false} ->
			    icl("id attribute missing");
			{Nick, Id} ->
			    ?D("msg type: ~p, nick: ~p, id: ~p", [Type, Nick, Id]),
			    {ok, State}
		    end
	    end;
	"serverShutdown" ->
	    ?D("msg type: ~p", [Type]),
	    {ok, State};
	"gameState" ->
	    case gse(gameId, E) of
		false ->
		    icl("gameId subelement missing");
		GameId ->
		    case {gse(nextPlayer, E), gse(gameOver, E)} of
			{false, false} ->
			    icl("please provide nextPlayer OR gameOver subelement");
			{E1, false} ->
			    case gav(nick, E1) of
				false ->
				    icl("nick attribute missing");
				Nick ->
				    ?D("msg type: ~p/nextPlayer, gameId: ~p, nick: ~p", [Type, GameId, Nick]),
				    {ok, State}
			    end;
			{false, E2} ->
			    case {gav(nick, E2),gav(result, E2)}  of
				{false, _} ->
				    icl("nick attribute missing");
				{_, false} ->
				    icl("result attribute missing");
				{_, Res} when Res /= "winner", Res /= "loser" ->
				    icl("result attribute: allowed values are loser or winner");
				{Nick, Res} ->
				    ?D("msg type: ~p/gameOver, gameId: ~p, nick: ~p, result", [Type, GameId, Nick, Res]),
				    {ok, State}
			    end
		    end
	    end;
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

login(Id, ?MAGIC = GameType, PlayersMin, PlayersMax, State = #state{state = undefined}) ->
    %% try gproc:add_local_name({game_master, GameType}) of
    %% 	true ->
    gproc:add_local_property({gm_for_game, GameType}, {Id, PlayersMin, PlayersMax}),
    gproc:add_local_property({gm}, {GameType}),
    {ok, ?s{state = registered}, sxml:login_response()};
%% login() ->

    %% catch 
    %% 	_:_ ->
    %% 	    {stop, wrong_id, sxml:gm_login_response(gm_already_registered)}
    %% end;
login(_, OtherGame, _, _, #state{state = undefined}) ->
    ?ERROR("logging with improper game type: ~p", [OtherGame]),
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
