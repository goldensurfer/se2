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
    Res = serv_sup:start_link(),
    TcpOpts = [{mode, list}, {port, 1090}, {active, true}],
    ranch:start_listener(play_socket, 1, ranch_tcp, TcpOpts, stream_parser, []),
    Res.

stop(_State) ->
    ranch:stop_listener(play_socket),
    serv_sup:stop(),
    ok.

