%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@kari.lan>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 10 Mar 2013 by Paul Peregud <pawel@kari.lan>
%%%-------------------------------------------------------------------
-module(connectivity_test).

%% API
-compile(export_all).
-export([]).

-define(HOST, localhost).
-define(CPORT, 1090).
-define(GPORT, 1091).
-define(INET_PARAMS, [binary]).

-define(TIMEOUT, 1000).

-include_lib("serv/include/se2.hrl").
-include_lib("serv/include/logging.hrl").
-include_lib("eunit/include/eunit.hrl").

stateful_test_() ->
    {foreach, fun () -> setup() end,
     fun (State) -> cleanup(State) end,
     [
      %% {timeout, 100, fun test_serv_hello_world/0},
      %% {timeout, 100, fun test_serv_playerLogin/0},
      {timeout, 100, fun test_serv_GMLogin/0}
     ]}.

setup() ->
    serv:start().

cleanup(_State) ->
    serv:stop().

test_serv_acceptor() ->
    {ok, Socket} = gen_tcp:connect(?HOST, ?CPORT, ?INET_PARAMS),
    ok = gen_tcp:close(Socket).
    
test_serv_hello_world() ->
    {ok, Socket} = gen_tcp:connect(?HOST, ?CPORT, ?INET_PARAMS),
    gen_tcp:send(Socket, sxml:ping()),
    receive 
	{tcp, _, DataBin} ->
	    ?D("~p", [DataBin]),
	    Data = binary_to_list(DataBin),
	    {El, _Tail} = xmerl_scan:string(Data),
	    ?D("~p", [El])
    after ?TIMEOUT ->
	    error(timeout)
    end.

test_serv_playerLogin() ->
    {ok, Socket} = gen_tcp:connect(?HOST, ?CPORT, ?INET_PARAMS),
    Bin = xml_playerLogin_msg(),
    gen_tcp:send(Socket, Bin),
    receive 
	{tcp, _, DataBin} ->
	    ?D("~p", [DataBin]),
	    Data = binary_to_list(DataBin),
	    {El, _Tail} = xmerl_scan:string(Data),
	    ?D("~p", [El])
    after ?TIMEOUT ->
	    error(timeout)
    end,
    ok = gen_tcp:close(Socket).

test_serv_GMLogin() ->
    {ok, Socket} = gen_tcp:connect(?HOST, ?GPORT, ?INET_PARAMS),
    gen_tcp:send(Socket, Bin),
    receive 
	{tcp, _, DataBin} ->
	    ?D("~p", [DataBin]),
	    Data = binary_to_list(DataBin),
	    {El, _Tail} = xmerl_scan:string(Data),
	    ?D("~p", [El])
    after ?TIMEOUT ->
	    error(timeout)
    end,
    ok = gen_tcp:close(Socket).
    
xml_playerLogin_msg() ->
    sxml:msg({message, [{type, playerLogin}], [{playerLogin, [{nick, "Jaedong"}, {gameType, ?MAGIC}], []}]}).

sax_xml() -> filename:join([codeDir(), "../test/sax_example.xml"]).
codeDir() -> filename:dirname(code:which(?MODULE)).
