-module(gamer).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {address,
                port,
                positions=[]}).


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
    gen_server:start_link(?MODULE, [Address, Port, Nick], []).

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
init([Address, Port, Nick]) ->
        {ok, Socket} = gen_tcp:connect(Address, Port, []),
        String = io_lib:fwrite("<message type=\"playerLogin\"><playerLogin nick=\"~s\" gameType=\"5-in-line-tic-tac-toe\"/></message>", [Nick]),
        gen_tcp:send(Socket, String),
        gen_tcp:close(Socket),
        {ok, #state{address=Address, port=Port}}.

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
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info({tcp, Data}, State) ->
        {ok, Socket} = gen_tcp:connect(State#state.address, State#state.port, []),
        %%an error message which may appear from both sides as a response anytime
        %%<message type="error">[String with error message]</message>
        
        %% HANDLE IT HERE

        %%login request response sent by server
        %%<message type="loginResponse">
        %%      <response accept="yes/no"/>
        %%       <!--
        %%      
        %%       tag present only when accept="no"
        %%       Error ids:
        %%       1 - wrong nick
        %%       2 - improper game type
        %%       3 - players pool overflow
        %%       4 - master for this game already registered
        %%       5 - wrong game type description data
        %%       -->
        %%       <error id="[int]"/>
        %%</message>
        
        %%HANDLE HERE
        
        %% game state message sent from the game master to the server and then by
        %% the server to all players in the game. After a game has finished server waits for
        %% "thank you" or "error" message from all the players. 
        %% 
        %% <message type="gameState">
        %%      <gameId id="[string]"/>
        %%      <!--  one tag of the two below appears in message  -->
        %%      <nextPlayer nick="[string]"/>
        %%      <gameOver>
        %%      <!--  this tag appears repeatedly for all the players  -->
        %%              <player nick="[string]" result="loser/winner"/>
        %%      </gameOver>
        %%      <!--
        %%      this tag will always appear. Not read by the server.
        %%      -->
        %%      <gameState>
        %%      <!--
        %%      When a player receives game state only the move of the opponent is sent in
        %%      format <tac x='xPos' y='yPos'/>. No other information is sent as a game
        %%      state, thus a player must remember all previous moves of the opponent.
        %%      -->
        %%      </gameState>
        %%  </message>

        %% HANDLE HERE
        
        %% <!--
        %% message sent before shutting down server to all registered players and game master
        %% -->
        %% <message type="serverShutdown"/>

        %% HANDLE HERE

        %% <!--
        %% server message with championship winners sent to all participants of this championship. A player may expect
        %% this kind of message anytime when not playing any game. This list is sorted according to won and then lost values
        %% -->
        %% <message type="championsList">
        %%      <!--
        %%      this tag appears repeatedly for all registers players. This should be ordered by number of wins.
        %%      -->
        %%      <player nick="[string]" won="[int]" lost="[int]"/>
        %% </message>
        %%

        gen_tcp:close(Socket),
        {noreply, State}.

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
