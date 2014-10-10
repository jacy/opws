-module(restart).

-include("texas.hrl").
-export([start/3]).

start(Game, Ctx, []) ->
    {goto, top, Game#game{pot = pot:new()}, Ctx}.


