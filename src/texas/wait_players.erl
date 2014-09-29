-module(wait_players).

-export([start/3, wait_for_players/3]).

-include("texas.hrl").

start(Game, Ctx, []) ->
  Game1 = g:restart_timer(Game, Game#game.start_delay),
  %% reset call amount
  Ctx1 = Ctx#texas{ call = 0, stage=?GS_CANCEL },
  {next, wait_for_players, Game1, Ctx1}.

wait_for_players(Game, Ctx, {timeout, _, _}) ->
  Ready = seat:get_seats(Game, ?PS_READY),
  ReqCount = Game#game.required_player_count,
  Start = (length(Ready) >= ReqCount),
  if
     Start ->
            Game1 = g:notify_start_game(Game),
            {stop, Game1, Ctx#texas{stage=?GS_GAME_START}};
        true ->
             Game1 = g:restart_timer(Game, Game#game.start_delay),
			  {continue, Game1, Ctx}
  end;

wait_for_players(Game, Ctx, R = #join{}) ->
  Game1 = seat:join(Game, R#join { state = ?PS_PLAY }),
  {continue, Game1, Ctx};

wait_for_players(Game, Ctx, R = #leave{}) ->
  Game1 = g:leave(Game, R),
  {continue, Game1, Ctx};

wait_for_players(Game, Ctx, _) ->
  {skip, Game, Ctx}.
