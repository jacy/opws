-module(game_start).

-export([start/3, game_start/3]).

-include("texas.hrl").

%% For test only, simulate multiple games are in progress together
%% Game only starts when all games are ready 

start(Game, Ctx, []) ->
  process_flag(trap_exit, true),
  link(Game#game.barrier),
  Game1 = g:restart_timer(Game, Game#game.start_delay),
  Ctx1 = Ctx#texas{ call = 0, bump=0, stage=?GS_CANCEL, 
			sb = none, bb = none,  b = none },  %% Clean Context for Distributed Test
  {next, game_start, Game1, Ctx1}.

game_start(Game, Ctx, {timeout, _, _}) ->
  Ready = seat:get_seats(Game, ?PS_READY),
  ReqCount = Game#game.required_player_count,
  Start = (length(Ready) >= ReqCount),
  Ctx1 = if Start andalso Ctx#texas.bump == 0 ->
     	barrier:bump(Game#game.barrier),
		Ctx#texas{bump = 1 };
	 true ->
		 Ctx
  end,
  Game1 = g:restart_timer(Game, Game#game.start_delay),
  {continue, Game1, Ctx1};

game_start(Game, Ctx, {'EXIT', Barrier, _}) 
  when Barrier == Game#game.barrier ->
	?FLOG("------Starting games---------"),
   	Game1 = g:notify_start_game(Game),
    {stop, Game1, Ctx#texas{stage=?GS_GAME_START}};

game_start(Game, Ctx, R = #join{}) ->
  Game1 = seat:join(Game, R#join { state = ?PS_PLAY }),
  {continue, Game1, Ctx};

game_start(Game, Ctx, R = #leave{}) ->
  Game1 = g:leave(Game, R),
  {continue, Game1, Ctx};

game_start(Game, Ctx, _) ->
  {skip, Game, Ctx}.
