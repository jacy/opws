-module(delay).

-export([start/3, delay/3]).

-include("texas.hrl").

start(Game, Ctx, []) ->
    Game1 = g:restart_timer(Game, Ctx#texas.win_duration),
    {next, delay, Game1, Ctx}.

delay(Game, Ctx, {timeout, _, _}) ->
    {stop, Game, Ctx};

delay(Game, Ctx, R = #leave{}) ->
	Game1 = g:leave(Game, R),
	{stop, Game1, Ctx};

delay(Game, Ctx, _) ->
  {skip, Game, Ctx}.
