-module(wait_players).

-export([start/3, wait_for_players/3]).

-include("texas.hrl").
-include_lib("eunit/include/eunit.hrl").

start(Game, Ctx, [Delay]) ->
  Game1 = g:restart_timer(Game, Delay),
  %% reset call amount
  Ctx1 = Ctx#texas{ call = 0 },
  {next, wait_for_players, Game1, Ctx1}.

wait_for_players(Game, Ctx, {timeout, _, _}) ->
  Ready = g:get_seats(Game, ?PS_READY),
  ReqCount = Game#game.required_player_count,
  Start = (length(Ready) >= ReqCount),
  if
     Start ->
            Game1 = g:notify_start_game(Game),
            {stop, Game1, Ctx};
        true ->
            {repeat, Game, Ctx}
  end;

wait_for_players(Game, Ctx, R = #join{}) ->
  Game1 = g:join(Game, R#join { state = ?PS_PLAY }),
  {continue, Game1, Ctx};

wait_for_players(Game, Ctx, R = #leave{}) ->
  io:format("Wait for players got LEAVE EVENT=~w~n", [R]),
  Game1 = g:leave(Game, R#leave { state = ?PS_ANY }),
  {continue, Game1, Ctx};

wait_for_players(Game, Ctx, _R = #raise{}) ->
  {continue, Game, Ctx};

wait_for_players(Game, Ctx, _) ->
  {skip, Game, Ctx}.
