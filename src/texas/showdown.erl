-module(showdown).

-export([start/3]).

-include("texas.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(COMMISSION,0.00).
-define(DELAY_UNIT,2000).

start(Game, Ctx=#texas{stage=PreviousStage}, []) ->
  Ctx1 = Ctx#texas{stage=?GS_SHOWDOWN},
  Game1 = g:new_stage(Game),
  Game2 = g:broadcast(Game1, #game_stage{ game = Game1#game.gid, stage = Ctx1#texas.stage}),
  g:show_cards(Game2, Ctx1#texas.b),

  ?LOG([{"Show down", Game2}]),
  Ranks = g:rank_hands(Game2),
  notify_hands(Game2, Ranks),

  Pots = g:pots(Game2),
  Winners = hand:winners(Ranks, Pots),
  
  ?LOG([{winners, Winners}]),
  Game3 = notify_winners(Game2, Winners),

  %% TODO 将所有金额不足的玩家重新设置状态
  {_, Big} = (Game3#game.limit):blinds(Game3#game.low, Game3#game.high),
  Game4 = check_inplay(seat:get_seats(Game, ?PS_GAMING), Big, Game3),

  WinDelay = length(Winners)  * ?DELAY_UNIT,
  Delay = case PreviousStage of
	  ?GS_RIVER -> ?DELAY_UNIT + WinDelay;
	  _ -> WinDelay
  end,
  {stop, Game4, Ctx1#texas{win_duration=Delay} }.

%%%
%%% Utility
%%%
notify_hands(_, []) ->
    ok;

notify_hands(Game, [H|T]) ->
    Hand = hand:player_hand(H),
    Event = #notify_hand{
      player = H#hand.pid,
      game = Game#game.gid,
      hand = Hand
     },
    Game1 = g:broadcast(Game, Event),
    notify_hands(Game1, T).

notify_winners(Game, []) ->
    Game;

notify_winners(Game, [ W = #winner{} |T]) ->
	Game1 = notify_winner(Game, W),
    notify_winners(Game1, T).

notify_winner(Game, #winner{player = Player,amount = Amount, pid=PID, potid=PotId} ) ->
	Cost = trunc(Amount * ?COMMISSION),
    Game1 = g:inplay_plus(Game, Player, Amount - Cost),
    Event = #notify_win{ 
      game = Game1#game.gid, 
      player = PID,
      amount = Amount,
	  cost = Cost,
	  potid=PotId
     },
    g:broadcast(Game1, Event).

check_inplay([], _Big, Game) ->
  Game;

check_inplay([SeatNum|T], Big, Game) -> 
  Seat = element(SeatNum, Game#game.seats),
  Inplay = Seat#seat.inplay,
  Game1 = if
    Inplay =< Big ->
      ?LOG([{player_out, SeatNum, Big, Inplay}]),
      g:leave(Game, #leave{player=Seat#seat.player,game=Game});
    true ->
      Game
  end,
  check_inplay(T, Big, Game1).