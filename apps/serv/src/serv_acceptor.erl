%%%-------------------------------------------------------------------
%%% @author 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%% @copyright (C) 2013, 190033 peregud pavel
%%% @doc
%%% 
%%% TCP acceptor. Based on Ranch; simplified.
%%% 
%%% @end
%%% Created :  5 Mar 2013 by 190033 peregud pavel <peregudp@p20311.mini.pw.edu.pl>
%%%-------------------------------------------------------------------
-module(serv_acceptor).

%% API
-export([init/2]).

-spec init(inet:socket(), any()) -> no_return().
init(LSocket, Opts) ->
	async_accept(LSocket),
	loop(LSocket, Opts).

-spec loop(inet:socket(), any()) -> no_return().
loop(LSocket, Opts) ->
	receive
		{accept, continue} ->
			?MODULE:init(LSocket, Opts);
		{accept, CSocket} ->
			{ok, ConnPid} = supervisor:start_child(serv_clients_sup, [CSocket, Opts]),
			gen_tcp:controlling_process(CSocket, ConnPid),
			?MODULE:init(LSocket, Opts)
	end.

-spec async_accept(inet:socket()) -> ok.
async_accept(LSocket) ->
	AcceptorPid = self(),
	_ = spawn_link(fun() ->
		case gen_tcp:accept(LSocket, infinity) of
			{ok, CSocket} ->
				gen_tcp:controlling_process(CSocket, AcceptorPid),
				AcceptorPid ! {accept, CSocket};
			{error, Reason} when Reason =/= closed ->
				AcceptorPid ! {accept, continue}
		end
	end),
	ok.


