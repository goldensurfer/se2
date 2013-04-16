-module(serv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("serv/include/logging.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% note that all requrements from app.src file should be started 
%% before calling application:start(serv).
start(_StartType, _StartArgs) ->
    {ok, ClientPort} = application:get_env(serv, client_port),
    {ok, GMPort} = application:get_env(serv, gm_port),
    Res = serv_sup:start_link(),
    TcpOpts = [{mode, binary}, {active, true}],
    ranch:start_listener(play_socket, 1, ranch_tcp, [{port, ClientPort} | TcpOpts], client, []),
    ranch:start_listener(gm_socket, 1, ranch_tcp, [{port, GMPort} | TcpOpts], gm, []),
    Res.

stop(_State) ->
    ranch:stop_listener(play_socket),
    serv_sup:stop(),
    ok.

