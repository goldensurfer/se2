-module(gamer).

-behaviour(gen_server).

%% API
-export([start_link/3,
        logout/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

%% for communication testing
%-export([testThankYou/0,
%	testLeaveGame/0,
%	testLogout/0,
%	%testError/0,
%	testPlayerLogin/0]).

-record(state, {address,
		port,
		positions,
		buffer = [],
		socket,
		gameId,
		nick
		}).

-define(DBG(F), io:fwrite(user, "(~p)~p:~p "++F++"~n", [self(), ?MODULE, ?LINE])).
-define(DBG(F, A), io:fwrite(user, "(~p)~p:~p "++F++"~n", [self(), ?MODULE, ?LINE]++A)).

-include_lib("xmerl/include/xmerl.hrl").

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
start_link(Address, Port, Nick) ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [Address, Port, Nick], []).

logout() ->
        gen_server:cast(gamer, logout).
        
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
init([Address, Port, Nick0]) ->
	Nick = atom_to_list(Nick0),
	{ok, Socket} = gen_tcp:connect(Address, Port, [{mode, list}]),
	?DBG("Connecting...~n",[]),
	String = io_lib:fwrite("<message type=\"playerLogin\"><playerLogin nick=\"~s\" gameType=\"5-in-line-tic-tac-toe\"/></message>", [Nick]),
	gen_tcp:send(Socket, String),
	crypto:start(),
	{ok, #state{address=Address, port=Port, socket=Socket, nick=Nick, 
			positions=ets:new(positions, [])}}.

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
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages. This is used for testing sending messages to a server.
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(logout, State) ->
        Msg = msg({message, [{type, "logout"}], []}),
        gen_tcp:send(State#state.socket, Msg),
        gen_tcp:close(State#state.socket),
        {noreply, State};

handle_cast(Msg, State) ->
	gen_tcp:send(State#state.socket, Msg),
	{noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages. This is used for receiving data over TCP.
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, DataBin}, State) ->
	Data0 = DataBin,
	Data = State#state.buffer++Data0,
	?DBG("~ngot tcp packet:~n~p~n", [Data]),
	try xmerl_scan:string(Data) of  %trying to parse Data as XML
		{Element, Tail} ->
			try handle_xml(Element,State) of
				{ok, State1} ->          % the case when player does not need to asnwer
					{noreply, State1#state{buffer=Tail}};
				{ok, State1, Msg} ->      % the case when player has to answer
					gen_tcp:send(State#state.socket,Msg),
					?DBG("gameid: ~p", [State1#state.gameId]),
					{noreply, State1#state{buffer=Tail}};
				{stop, Reason, Msg} ->    % error
					gen_tcp:send(State#state.socket,Msg),
					gen_tcp:close(State#state.socket),
					{stop,Reason,State}
			catch 
				error:{stop, Reason, State = #state{}} ->
					gen_tcp:close(State#state.socket),
					{stop, Reason, State};
				error:{stop, Reason, Msg} ->
					?DBG("xml parsing error: ~p, ~n~p", [Reason, Msg]),
					gen_tcp:send(State#state.socket, Msg),
					gen_tcp:close(State#state.socket),
					{stop, Reason, State}
			end
	catch
		ErrType:ErrMsg ->
			?DBG("Error parsing: ~p~n", [{ErrType,ErrMsg}]),
			{noreply,State#state{buffer = Data}}
	end;
handle_info({tcp_closed, _Socket}, State) ->
	?DBG("TCP connection closed.~n", []),
	{stop, normal, State};
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
terminate(Reason, _State) ->
	?DBG("Player terminating because of ~p~n.", [Reason]),
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

%%%===================================================================
%%%XML parsing
%%%===================================================================

%% %% Gets an XML subelement with tag 'Name' from an element 'El'.
%% get_sub_element(Name, #xmlElement{} = El) ->
%% 	#xmlElement{content = Content} = El,
%% 	case lists:keyfind(Name, #xmlElement.name, Content) of
%% 		false ->
%% 			false;
%% 		Tuple ->
%% 			Tuple
%% 	end.

%% %% Gets value of the 'Name' attribute of element 'El'
%% get_attr_value(Name, #xmlElement{} = El) ->
%% 	#xmlElement{attributes = Attrs} = El,
%% 	case lists:keyfind(Name, #xmlAttribute.name, Attrs) of
%% 		false ->
%% 			false;
%% 		At ->
%% 			At#xmlAttribute.value
%% 	end.

gav(A, B) ->
	sxml:gav(A, B).

gse(A, B) ->
	sxml:gse(A, B).

%% Generates an XML element using 'El'
msg(El) ->
	lists:flatten(xmerl:export_simple_content([El], xmerl_xml)).

%% Gets a list of players from an XML element 'List' containing multiple 'Player' tags
getPlayers(List) ->
	[E || {xmlElement,player,_,_,_,_,_,_,_,_,_,_} = E <- List].

%% Handling incoming messages.
handle_xml(E, State) ->
	%?DBG("gameid: ~p~n, Got xml: ~n~p", [State#state.gameId, E]),
	case gav(type, E) of
		"error" ->
			msgInfo(error,State),
			#xmlElement{content=Content}  = E,
			[#xmlText{value=Error}] = Content,
			?DBG("Received error: ~p~n",[Error]),
			{stop, Error, errorMsg(State#state.nick)};
		"loginResponse" ->
			msgInfo(loginResponse, State),
			E1 = gse(response, E),
			Accept = gav(accept,E1),
			case Accept of
				"no" ->
					?DBG("Login denied!~n", []),
					E2 = gse(error,E),
					ErrorId = gav(id, E2),
					?DBG("Error id = ~p: ", [ErrorId]),
					case ErrorId of
						"1" ->
							?DBG("wrong nick.~n",[]);
						"2" ->
							?DBG("improper game type.~n",[]);
						"3" ->
							?DBG("players pool overflow.~n",[]);
						"5" ->
							?DBG("wrong game type description data")
					end;
				"yes" ->
					?DBG("Login accepted by server ~p!~n", [State#state.address])
			end,
			{ok,State};
		"gameState" ->
			msgInfo(gameState, State),
			_GameId = gav(id, gse(gameId, E)),
			case gse({nextPlayer, gameOver}, E) of
				{false, E3} ->
					?DBG("Game Over!~n", []),
					#xmlElement{content=Content} = E3,
					Players = getPlayers(Content),
					Players1 = lists:foldl(fun(Elem, Result) -> [{gav(nick,Elem),gav(result, Elem)}|Result] end, [],Players),
					lists:foreach(fun({Nick,Result}) -> ?DBG("Player ~p is a ~p.~n",[Nick,Result]) end, Players1),
                                        true = ets:delete_all_objects(State#state.positions),
					{ok,State, thankYouMsg(State#state.gameId)};
				{E2, false} ->
					Nick = gav(nick, E2),
					E4 = gse(gameState, E),
					Me = State#state.nick,
					GameIdTag = gse(gameId, E),
					?DBG("me: ~p, move is his: ~p", [Me, Nick]),
					case {sxml:get_sub_element(tac, E4), Nick} of
						{false, Me} ->
							?DBG("first move, me!", []),
							Move = make_move(State#state.positions),
							State1 = State#state{gameId=gav(id, GameIdTag)},
							%% ?DBG("I have gameId ~p", [State1#state.gameId]),
							{ok, State1, ticMsg(Move,State1#state.gameId)};
						{false, _NotMe} ->
							?DBG("first move, not me :(", []),
							State1 = State#state{gameId=gav(id, GameIdTag)},
							%% ?DBG("I have gameId ~p", [State1#state.gameId]),
							{ok, State1};
						{E5, Me} ->
							?DBG("my move", []),
							X = gav(x, E5),
							Y = gav(y, E5),
							?DBG("Last move: tac, x=~p, y=~p",[X,Y]),
							note_move(X, Y, os, State),
							Move = make_move(State#state.positions),
							%% ?DBG("I have gameId ~p", [State#state.gameId]),
							{MyX, MyY} = Move,
							?DBG("I undersigned, ~p made the following move: x = ~p, y = ~p", [State#state.nick, MyX, MyY]),
							{ok, State, ticMsg(Move,State#state.gameId)};
						{E5, _NotMe} ->
							?DBG("his move", []),
							X = gav(x, E5),
							Y = gav(y, E5),
							?DBG("Last move: tac, x=~p, y=~p",[X,Y]),
							note_move(X, Y, xs, State),
							%% ?DBG("I have gameId ~p", [State#state.gameId]),
							{ok, State}
					end
			end;
		"serverShutdown" ->
			msgInfo(serverShutdown,State),
			{ok, State};
		"championsList" ->
			msgInfo(championsList,State),
			#xmlElement{content=Content} = E,
			Players = getPlayers(Content),
			Players1 = lists:foldl(fun(Elem, Result) -> [{gav(nick,Elem),gav(won, Elem),gav(lost,Elem)}|Result] end, [], Players),
			lists:foreach(fun({Nick,Won,Lost}) -> ?DBG("Player ~p: won - ~p, lost - ~p.~n",[Nick,Won,Lost]) end, Players1),
			{ok, State}
	end.

%%%===============================================================================================
%%% Message generation
%%% ==============================================================================================

%% Generates "Thank you" message
thankYouMsg(GameId) ->
	Msg = {message, [{type, "thank you"}], [{gameId, [{id, GameId}], []}] },
	msg(Msg).

%% Generates "leaveGame" message
leaveGameMsg(GameId)->
	Msg = {message, [{type,"leaveGame"}], [{gameId, [{id, GameId}], []}]},
	msg(Msg).

%% Generates "logout" message
logoutMsg() ->
	Msg = {message, [{type,"logout"}],[]},
	msg(Msg).

%% Generates "move" message with a 'tic'
ticMsg({X,Y},GameId) ->
	Msg = {message, [{type,"move"}], [
				{gameId, [{id,GameId}],[]},
				{move,[],[{tic,[{x,X},{y,Y}],[]}]}
				]},
	msg(Msg).

%% Generates exemplary "error" message
errorMsg(Nick) ->
	Error = io_lib:fwrite("Player ~p received error.", [Nick]),
	Msg = {message, [{type, "error"}], [Error]},
	msg(Msg).

%% Generates exemplary "playerLogin" message - used for testing
%playerLoginMsg() ->
%	Msg = {message, [{type, "playerLogin"}], [{playerLogin, [{nick,"pawelMichna"},{gameType,"5-in-line-tic-tac-toe"}], []}]},
%	msg(Msg).



%%%====================================================
%%% Functions for testing sending messages to a server
%%% ===================================================
%%testThankYou() ->
%%	gen_server:cast(gamer, thankYouMsg()).
%%
%%testLeaveGame() ->
%%	gen_server:cast(gamer, leaveGameMsg()).
%%
%%testLogout() ->
%%	gen_server:cast(gamer, logoutMsg()).
%%
%%%testTic() ->
%%%	gen_server:cast(gamer, ticMsg()).
%%
%%%testError() ->
%%%	gen_server:cast(gamer, errorMsg()).
%%
%%testPlayerLogin() ->
%%	gen_server:cast(gamer, playerLoginMsg()).
%%
%%

%%%===============================================================
%%% Helper functions
%%%===============================================================

%% Displays information what message was received and from whom.
msgInfo(Msg,State) ->
	%% ?DBG("Received ~p from server ~p, gameId:~p", [Msg,State#state.address,State#state.gameId]).
    ok.

note_move(X, Y, Who, #state{positions = Tid}) 
  when is_integer(X), is_integer(Y) ->
    true = ets:insert_new(Tid, {{X,Y}, Who});
note_move(X, Y, Who, State) 
  when is_list(X), is_list(Y) ->
    note_move(list_to_integer(X), list_to_integer(Y), Who, State).
    

make_move(Positions) ->
    {Retries, Move} = make_move0(Positions, 0),
    ?DBG("~p retries", [Retries]),
    Move.

make_move0(Positions, N0) ->
    N = N0 + 1,
	X = crypto:rand_uniform(0,20),
	Y = crypto:rand_uniform(0,20),
	case ets:lookup(Positions, {X,Y}) of
	    [] ->
		{N, {X,Y}};
	    _ ->
		make_move0(Positions, N)
	end.
