%%%-------------------------------------------------------------------
%%% @author Paul Peregud <>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 16 Apr 2013 by Paul Peregud <>
%%%-------------------------------------------------------------------
-module(sxml).

%% helpers
-export([msg/1, get_sub_element/2, get_attr_value/2]).

%% protocol client
-export([login_response/0, login_response/1]).
-export([error/1]).

%% protocol gm
-export([gm_login/2]).

%% testing stuff
-export([ping/0, pong/0]).

-include_lib("serv/include/logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%===================================================================
%%% XML parsing helpers
%%%===================================================================

get_sub_element(Name, #xmlElement{} = El) ->
    #xmlElement{content = Content} = El,
    R = case lists:keyfind(Name, #xmlElement.name, Content) of
	false ->
	    false;
	At ->
	    At
    end,
    %% ?D("gse. get ~p with result ~p from ~p", [Name, R, El]),
    R.

get_attr_value(Name, #xmlElement{} = El) ->
    #xmlElement{attributes = Attrs} = El,
    R = case lists:keyfind(Name, #xmlAttribute.name, Attrs) of
	false ->
	    false;
	At ->
	    At#xmlAttribute.value
    end,
    %% ?D("gav. get ~p with result ~p from ~p", [Name, R, El]),
    R.
%%%===================================================================
%%% messages construction
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


%%%===================================================================
%%% Internal functions
%%%===================================================================

msg(El) ->
    lists:flatten(xmerl:export_simple_content([El], xmerl_xml)).
