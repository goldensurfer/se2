%% Feel free to use, reuse and abuse the code in this file.
-module(stream_parser).
-export([start_link/4, init/4]).

-export([sax_event/2, sax_event0/2]).

-include_lib("serv/include/logging.hrl").

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(ListenerPid),
    gproc:add_local_property({?MODULE}, true),
    loop(Socket, Transport, start, []).

loop(Socket, Transport, State, Tail0) ->
    case Transport:recv(Socket, 0, timer:seconds(60)) of
	{ok, Data0} ->
	    Data1 = binary_to_list(Data0),
	    Data = Tail0++Data1,
	    %% ?DBG("Got some data: ~p", [Data]), 
	    {ok, _, Tail} = erlsom:parse_sax(Data, State, fun stream_parser:sax_event0/2),
	    %% ?DBG("Tail is: ~p", [Tail]), 
	    loop(Socket, Transport, State, Tail);
	_ ->
	    ok = Transport:close(Socket)
    end.

sax_event0(Event, State) ->
    State1 = sax_event(Event, State),
    ?DBG("~p: ~p -> ~p", [Event, State, State1]),
    State1.

sax_event(startDocument, start) ->
    startDocument;
sax_event({startElement, [], "message", [], Attrs}, startDocument) ->
    messageStart;
sax_event({characters, String}, messageStart) ->
    messageEnd;
sax_event({endElement, [], "message", []}, messageEnd) ->
    hadMessage;
sax_event(endDocument, hadMessage) ->
    start.




