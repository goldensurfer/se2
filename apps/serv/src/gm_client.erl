%%%-------------------------------------------------------------------
%%% @author 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%% @copyright (C) 2013, 190033 peregud pavel
%%% @doc
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

-define(SERVER, ?MODULE). 

-record(state, {
	  socket,
	  buffer = []
	 }).

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
    gen_server:start_link(?MODULE, [Host, Port, Id, Game], []).

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

handle_xml(E, State) ->
    ?D("~p", [{E, State}]),
    case gav(type, E) of
	"ping" ->
	    {ok, State, sxml:pong()};
	"loginResponse" ->
	    E1 = gse(response, E),
	    case gav(accept, E1) of
		false ->
		    {stop, incomplete_xml, sxml:error("accept attribute missing")};
		"yes" ->
		    finish_login(State);
		"no" ->
		    E2 = gse(error, E),
		    ErrId = gav(accept, E2),
		    {stop, {login_rejected_by_server, ErrId}}
	    end;
	X ->
	    ErrMsg = io_lib:fwrite("unknown message type: ~p", [X]),
	    ?D(ErrMsg, []),
	    ?ERROR(ErrMsg, []),
	    {stop, unknown_xml_message_type, ErrMsg}
    end.

finish_login(State) ->
    {ok, State}.

gav(Name, El) ->
    sxml:get_attr_value(Name, El).

gse(Name, El) ->
    sxml:get_sub_element(Name, El).

rec(State) ->
    ok = inet:setopts(State#state.socket, [{active, true}]),
    State.
