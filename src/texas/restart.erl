-module(restart).

-include("texas.hrl").
-export([start/3]).

start(Game, Ctx, []) ->
  	g:broadcast(Game, #notify_end_game{ game = Game#game.gid }),
	g:broadcast_player_state(Game),
    {goto, top, Game#game{pot = pot:new()}, Ctx}.


