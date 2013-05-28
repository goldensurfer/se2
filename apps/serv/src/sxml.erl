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
	 get_all_subs/1,
	 gse/2, gsen/3, gav/2
	]).

%% protocol client
-export([login_response/0, login_response/1, champions_list/1]).
-export([error/1]).

%% protocol gm
-export([begin_game/2, next_player/3, game_over/3, gm_login/2, move/2]).

%% testing stuff
-export([ping/0, pong/0]).

-include_lib("serv/include/logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%%===================================================================
%%% XML parsing helpers
%%%===================================================================

get_all_subs(#xmlElement{} = El) ->
    El#xmlElement.content.

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
	    Msg = io_lib:fwrite("please provide exactly ~p tags ~p", [N, Name]),
	    erlang:error({stop, incomplete_xml, sxml:error(Msg)})
    end.
    
    

%%%===================================================================
%%% misc messages
%%%===================================================================

msg(El) ->
    lists:flatten(xmerl:export_simple_content([El], xmerl_xml)).

error(Msg) ->
    msg({message, [{type,error}], [#xmlText{value = Msg}]}).

ping() ->
    msg({message, [{type,ping}], [#xmlText{value = "Ping!"}]}).

pong() ->
    msg({message, [{type,pong}], [#xmlText{value = "Pong!"}]}).

%%%===================================================================
%%% client messages
%%%===================================================================

champions_list(Results) ->
    Players = [ {player, [{nick, Nick}, {won, Won}, {lost, Lost}], []}
		|| {Nick, Won, Lost} <- Results ],
    msg({message, [{type, championsList}], Players}).

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

next_player(Id, Nick, GameState) ->
    GameId = {gameId, [{id, Id}], []},
    Player = {nextPlayer, [{nick, Nick}], []},
    Msg = {message, [{type, gameState}], [GameId, Player, GameState]},
    msg(Msg).

move(GameId, MoveEl) ->
    GI = {gameId, [{id, GameId}], []},
    Msg = {message, [{type, move}], [GI, MoveEl]},
    msg(Msg).

game_over(Id, {Winner, Loser}, GS) ->
    GameId = {gameId, [{id, Id}], []},
    PlayerA = {player, [{nick, Winner}, {result, "winner"}], []},
    PlayerB = {player, [{nick, Loser}, {result, "loser"}], []},
    GO = {gameOver, [], [PlayerA, PlayerB]},
    GameState = case GS of 
		    undefined ->
			{gameState, [], []};
		    {X, Y} ->
			Tac = {tac, [{x, X}, {y, Y}], []},
			{gameState, [], [Tac]};
		    _Xml ->
			_Xml
		end,
    Msg = {message, [{type, gameState}], [GameId, GO, GameState]},
    msg(Msg).

%%%===================================================================
%%% Internal functions
%%%===================================================================

