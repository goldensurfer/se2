-define(MAGIC, "5-in-line-tic-tac-toe").
-type game_id() :: binary().
-type nick() :: binary().
-type player() :: {pid(), nick()}.

-record(game, {
	  room :: pid(),
	  id :: game_id(),
	  players :: [player()]
	 }).
-type game() :: #game{}.
