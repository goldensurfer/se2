%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@kari.lan>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created :  9 Mar 2013 by Paul Peregud <pawel@kari.lan>
%%%-------------------------------------------------------------------
-module(gm_protocol).

-behaviour(gen_server).
%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  socket :: inet:socket(),
	  transport :: module(),
	  opts :: [proplists:property()]
}).

-type state() :: #state{}.

-include_lib("se2/include/logging.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================

%% pid(), inet:socket(), module(), any()
-spec init([any()]) -> {ok, state()}.
init([ListenerPid, Socket, Transport, Opts]) ->
    ok = ranch:accept_ack(ListenerPid),
    self() ! hello_there,
    {ok, #state{
	    socket = Socket, 
	    transport = Transport,
	    opts = Opts
	   }}.

handle_call(Request, _From, State) ->
    {stop, {odd_request, Request}, State}.

handle_cast(Msg, State) ->
    {stop, {odd_cast, Msg}, State}.

handle_info(Info, State) ->
    ?DBG("got info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
