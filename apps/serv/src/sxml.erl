%%%-------------------------------------------------------------------
%%% @author Paul Peregud <>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%% 
%%% This module contains helpers for work with XML and some templates
%%% for xml commands. All templates should be moved here eventually.
%%% 
%%% @end
%%% Created : 16 Apr 2013 by Paul Peregud <>
%%%-------------------------------------------------------------------
-module(sxml).

%% helpers
-export([msg/1, get_sub_element/2, get_sub_elements/2, get_attr_value/2, 
	 gse/2, gsen/3, gav/2
	]).

%% protocol client
-export([login_response/0, login_response/1]).
-export([error/1]).

%% protocol gm
-export([begin_game/2]).

%% testing stuff
-export([ping/0, pong/0]).

-include_lib("serv/include/logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%===================================================================
%%% XML parsing helpers
%%%===================================================================

get_sub_elements(Name, #xmlElement{} = El) ->
    #xmlElement{content = Content} = El,
    Fun = fun(#xmlElement{name = AName}) when AName =:= Name -> true;
	     (_) -> false
	  end,
    lists:filter(Fun, Content).

get_sub_element(Name, #xmlElement{} = El) ->
    #xmlElement{content = Content} = El,
    case lists:keyfind(Name, #xmlElement.name, Content) of
	false ->
	    false;
	At ->
	    At
    end.

get_attr_value(Name, #xmlElement{} = El) ->
    #xmlElement{attributes = Attrs} = El,
    case lists:keyfind(Name, #xmlAttribute.name, Attrs) of
	false ->
	    false;
	At ->
	    At#xmlAttribute.value
    end.

%% throwing to simplify document parsing
gav(Tuple, El) when is_tuple(Tuple) ->
    L = tuple_to_list(Tuple),
    L1 = [ get_attr_value(Name, El) || Name <- L ],
    case lists:any(fun(X) -> X =/= false end, L1) of
	true ->
	    list_to_tuple(L1);
	false ->
	    Tmpl = "provide at least of the following attributes: ~p",
	    Msg = io_lib:fwrite(Tmpl, [L]),
	    erlang:error({stop, incomplete_xml, sxml:error(Msg)})
    end;
gav(Name, El) ->
    case get_attr_value(Name, El) of
	false ->
	    Msg = io_lib:fwrite("~p attribute missing", [Name]),
	    erlang:error({stop, incomplete_xml, sxml:error(Msg)});
	AttrValue -> 
	    AttrValue
    end.

%% throwing to simplify document parsing
gse(Tuple, El) when is_tuple(Tuple) ->
    L = tuple_to_list(Tuple),
    L1 = [ get_sub_element(Name, El) || Name <- L ],
    case lists:any(fun(X) -> X =/= false end, L1) of
	true ->
	    list_to_tuple(L1);
	false ->
	    MsgTemplate = "provide at least of the following subelements: ~p",
	    Msg = io_lib:fwrite(MsgTemplate, [L]),
	    erlang:error({stop, incomplete_xml, sxml:error(Msg)})
    end;
gse(Name, El) ->
    case sxml:get_sub_element(Name, El) of
	false ->
	    Msg = io_lib:fwrite("~p subelement missing", [Name]),
	    erlang:error({stop, incomplete_xml, sxml:error(Msg)});
	Element -> 
	    Element
    end.

gsen(Name, #xmlElement{} = El, N) ->
    L = get_sub_elements(Name, El),
    case length(L) of
	N ->
	    L;
	_ ->
	    Msg = io:fwrite("please provide exactly ~p tags ~p", [Name]),
	    erlang:error({stop, incomplete_xml, sxml:error(Msg)})
    end.
    
    

%%%===================================================================
%%% misc messages
%%%===================================================================

error(Msg) ->
    msg({message, [{type,error}], [#xmlText{value = Msg}]}).

ping() ->
    msg({message, [{type,ping}], [#xmlText{value = "Ping!"}]}).

pong() ->
    msg({message, [{type,pong}], [#xmlText{value = "Pong!"}]}).

%%%===================================================================
%%% client messages
%%%===================================================================

login_response() ->
    msg({message, [{type,loginResponse}], [{response, [{accept, yes}], []}]}).

login_response_error(Code) ->
    msg({message, [{type,loginResponse}], [{response, [{accept, no}], []}, 
					   {error, [{id, Code}], []}]}).
login_response(wrong_nick) ->
    login_response_error(1);
login_response(already_registered) ->
    login_response_error(6);
login_response(xml_error) ->
    login_response_error(7);
%%%===================================================================
%%% gm messages
%%%===================================================================
login_response(gm_already_registered) ->
    login_response_error(4);
login_response(improper_game_type) ->
    login_response_error(2).

gm_login(Id, Magic) ->
    GML = {gameMasterLogin, 
	   [{id, Id}, 
	    {gameType, Magic},
	    {playersMin, 2},
	    {playersMax, 2}
	   ], []},
    sxml:msg({message, [{type, gameMasterLogin}], [GML]}).

begin_game(Id, Nicks) ->
    GameId = {gameId, [{id, Id}], []},
    Players = [ {player, [{nick, Nick}], []} || Nick <- Nicks ],
    Msg = {message, [{type,beginGame}], [GameId | Players]},
    msg(Msg).

%%%===================================================================
%%% Internal functions
%%%===================================================================

msg(El) ->
    lists:flatten(xmerl:export_simple_content([El], xmerl_xml)).
