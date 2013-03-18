-module(serv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(gproc, permanent),
    application:start(lager, permanent),
    application:start(ranch, permanent),
    Res = serv_sup:start_link(),
    TcpOpts = [{mode, list}, {port, 1090}, {active, true}],
    ranch:start_listener(gm_socket, 1, ranch_tcp, TcpOpts, stream_parser, []),
    Res.

stop(_State) ->
    ranch:stop_listener(gm_socket),
    serv_sup:stop(),
    application:stop(ranch),
    application:stop(lager),
    application:stop(gproc),
    ok.
