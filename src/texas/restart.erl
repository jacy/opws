-module(restart).

-export([start/3]).

start(Game, Ctx, []) ->
	g:broadcast_player_state(Game),
    {goto, top, Game, Ctx}.


