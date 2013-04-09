-module(stream_parser).
-export([start_link/4, init/4]).

-export([handle_sax_event/2]).

-type cmd_name() :: unknown | register.
-type cmd() :: {cmd_name(), any()}.
-type gs() :: undefined | error | registered | tournament | playing.
-type ps() :: ok | {error, any()}.

-record(state, {
	  socket,
	  transport,
	  buffer = [],
	  parser = start :: atom() | {atom(), any()},
	  ps = ok :: ps(),
	  cmds = [] :: list(cmd()),
	  player_state :: gs()
	 }).

-define(s, State#state).

-include_lib("serv/include/logging.hrl").

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(ListenerPid),
    gproc:add_local_property({?MODULE}, true),
    State = #state{
	       socket = Socket, 
	       transport = Transport
	      },
    loop(State).

loop(State) ->
    case (?s.transport):recv(?s.socket, 0, timer:seconds(10)) of
	{ok, Data0} ->
	    Data1 = binary_to_list(Data0),
	    Data = ?s.buffer++Data1,
	    {ok, State1, Tail} = erlsom:parse_sax(Data, State, fun stream_parser:handle_sax_event/2),
	    handle_parsed(State1#state{buffer = Tail});
	_ ->
	    ok = (?s.transport):close(?s.socket)
    end.

handle_parsed(State = #state{ps = {error, Reason}}) ->
    Template = "<error>parsing error: ~p</error>",
    Xml = io_lib:fwrite(Template, [Reason]),
    (?s.transport):send(?s.socket, Xml),
    (?s.transport):close(?s.socket);
handle_parsed(State = #state{cmds = []}) ->
    loop(State);
handle_parsed(State = #state{cmds = [{playerLogin, {Game, Nickname}} | T], player_state = undefined}) ->
    gproc:add_local_property({registered_for_game, Game}, Nickname),
    gproc:add_local_property({registered}, Game),
    handle_parsed(?s{cmds = T, player_state = registered});
handle_parsed(State = #state{cmds = [{playerLogin, {_Game, _Nickname}} | T]}) ->
    (?s.transport):send(?s.socket, "<error>already registered</error>"),
    (?s.transport):close(?s.socket).

handle_sax_event(Tag, State = #state{parser = Parser, ps = ok}) ->
    Res = case sax_event(Tag, Parser) of
	      {ok, NewParser} ->
		  io:fwrite(user, "~p -> ~p~n", [{Parser, Tag}, NewParser]),
		  ?s{parser = NewParser};
	      {ok, NewParser, Cmd} ->
		  io:fwrite(user, "~p -> ~p~n", [{Parser, Tag}, NewParser]),
		  ?s{parser = NewParser, cmds = ?s.cmds ++ [Cmd]};
	      {error, Reason} ->
		  io:fwrite(user, "{_, ~p} ==== ~p~n", [Tag, Reason]),
		  ?s{ps = {error, Reason}}
	  end;

handle_sax_event(Tag, State = #state{ps = {error, Reason}}) ->
    io:fwrite(user, "{_, ~p} ==== ~p~n", [Tag, Reason]),
    State.

%% sax_event/2 works as FSM where 
%% the first arg is the symbol and 
%% the second arg is state of FSM
%% if it complites the parse of any message, it returns it as a command

%% skip all junk
sax_event({ignorableWhitespace,_}, State) ->
    {ok, State};

%% start of message
sax_event(startDocument, start) ->
    {ok, startDocument};

%% determine the type of message
sax_event({startElement, [], "message", [], Attrs}, startDocument) ->
    case get_attr("type", Attrs) of
	{ok, "playerLogin"} ->
	    {ok, playerLoginStart};
	{ok, "error"} ->
	    {ok, errorMessageStart};
	false ->
	    {error, {parsing, message_should_have_type_attribute}}
    end;

%% error message
sax_event({characters, String}, errorMessageStart) ->
    ?INFO("got error message: ~p", [String]),
    {ok, errorMessageEnd};
sax_event({endElement, [], "message", []}, errorMessageEnd) ->
    {ok, hadMessage};

%% playerLogin message
sax_event({startElement, [], "playerLogin", [], Attrs}, playerLoginStart) ->
    case {get_attr("nick", Attrs), get_attr("gameType", Attrs)}  of
	{{ok, Nick}, {ok, Game}} ->
		    {ok, {playerLoginEnd, {Game, Nick}}};
	_ ->
	    {error, {parsing, playerLogin_should_have_name_and_game_attrs}}
    end;
sax_event({endElement,[],"playerLogin",[]}, {playerLoginEnd, Params}) ->
    {ok, endMessage, {playerLogin, Params}};
sax_event({endElement, [], "message", []}, endMessage) ->
    {ok, hadMessage};

%% ready to parse new message
sax_event(endDocument, hadMessage) ->
    {ok, start}.

get_attr(Name, Attrs) ->
    case lists:keyfind(Name, 2, Attrs) of
	{attribute, Name, _, _, Value} ->
	    {ok, Value};
	false ->
	    false
    end.
    
