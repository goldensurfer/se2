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
-define(PORT, 1090).
-define(INET_PARAMS, [binary]).

-include_lib("serv/include/logging.hrl").
-include_lib("eunit/include/eunit.hrl").

stateful_test_() ->
    {foreach, fun () -> setup() end,
     fun (State) -> cleanup(State) end,
     [%% {timeout, 100, fun test_serv_acceptor/0},
      {timeout, 100, fun test_serv_hello_world/0},
      {timeout, 100, fun test_serv_playerLogin/0}
      %% fun() -> ok end
     ]}.

setup() ->
    serv:start().

cleanup(_State) ->
    serv:stop().

test_serv_acceptor() ->
    {ok, Socket} = gen_tcp:connect(?HOST, ?PORT, ?INET_PARAMS),
    ok = gen_tcp:close(Socket).
    
test_serv_hello_world() ->
    {ok, Socket} = gen_tcp:connect(?HOST, ?PORT, ?INET_PARAMS),
    Bin = xml_error_msg(),
    gen_tcp:send(Socket, Bin),
    timer:sleep(timer:seconds(3)),
    ok = gen_tcp:close(Socket).

test_serv_playerLogin() ->
    {ok, Socket} = gen_tcp:connect(?HOST, ?PORT, ?INET_PARAMS),
    Bin = xml_playerLogin_msg(),
    gen_tcp:send(Socket, Bin),
    timer:sleep(timer:seconds(3)),
    ok = gen_tcp:close(Socket).
    
xml_error_msg() ->
    <<"<message type=\"error\">[String with error message]</message>">>.

xml_playerLogin_msg() ->
    <<"<message type=\"playerLogin\">
<playerLogin nick=\"Jaedong\" gameType=\"Starcraft\"/>
</message>">>.

sax_xml() -> filename:join([codeDir(), "../test/sax_example.xml"]).
codeDir() -> filename:dirname(code:which(?MODULE)).
