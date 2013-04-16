%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@kari.lan>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2013 by Paul Peregud <pawel@kari.lan>
%%%-------------------------------------------------------------------
-module(client).

-behaviour(gen_server).

%% API
-export([start_link/4]).

-export([t/0, t1/0, t2/0, t3/0, t4/0, t5/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  socket,
	  transport,
	  buffer = []
	 }).

-include_lib("serv/include/logging.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ListenerPid, Socket, Transport, _Opts = []]) ->
    gproc:add_local_property({?MODULE}, true),
    self() ! {accept_ack, ListenerPid},
    {ok, #state{
	    socket = Socket, 
	    transport = Transport
	   }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {stop, {odd_call, Request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {odd_cast, Msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Data0}, State) ->
    Data = State#state.buffer++Data0,
    try xmerl_scan:string(Data) of
	{Element, Tail} ->
	    case handle_xml(Element, State) of
		ok ->
		    {noreply, rec(State#state{buffer = Tail})};
		{stop, Reason, Msg} ->
		    gen_tcp:send(State#state.socket, Msg),
		    gen_tcp:close(State#state.socket),
		    {stop, Reason, rec(State#state{buffer = Tail})}
	    end
    catch
	_:_ ->
	    {noreply, rec(State#state{buffer = Data})}
    end;
handle_info({accept_ack, ListenerPid}, State) ->
    ranch:accept_ack(ListenerPid),
    {noreply, rec(State)};
handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

rec(State) ->
    ok = inet:setopts(State#state.socket, [{active, true}]),
    State.

handle_xml(_E, _State) ->
    erlang:error(not_impl).

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
			     io:fwrite(user, "~p~n", 
				       [{ErrType, ErrMsg}]),
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
