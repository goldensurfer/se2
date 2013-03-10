-module(serv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(lager, permanent),
    application:start(ranch, permanent),
    Res = serv_sup:start_link(),
    ranch:start_listener(gm_socket, 1, ranch_tcp, [{port, 1090}], gm_protocol, []),
    Res.

stop(_State) ->
    ok.
