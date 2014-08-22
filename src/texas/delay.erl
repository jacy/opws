-module(delay).

-export([start/3, delay/3]).

-include("texas.hrl").

start(Game, Ctx, []) ->
	Delay = 2000 + length(Ctx#texas.winners) * 2000,
	io:format("Dealy time=~w~n", [Delay]),
    Game1 = g:restart_timer(Game, Delay),
    {next, delay, Game1, Ctx}.

delay(Game, Ctx, {timeout, _, _}) ->
    {stop, Game, Ctx};

delay(Game, Ctx, R = #leave{}) ->
	Game1 = g:leave(Game, R),
	{stop, Game1, Ctx}.
