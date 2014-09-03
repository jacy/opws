-module(showdown).

-export([start/3]).

-include("texas.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(COMMISSION,0.00).
-define(DELAY_UNIT,1500).

start(Game, Ctx, []) ->
  g:show_cards(Game, Ctx#texas.b),

  Ranks = g:rank_hands(Game),
  notify_hands(Game, Ranks),

  Pots = g:pots(Game),
  Winners = hand:winners(Ranks, Pots),
  
  ?LOG([{winners, Winners}]),
  Game1 = notify_winners(Game, Winners),

  %% TODO 将所有金额不足的玩家重新设置状态
  {_, Big} = (Game1#game.limit):blinds(Game1#game.low, Game1#game.high),
  Game2 = check_inplay(g:get_seats(Game, ?PS_ANY), Big, Game1),

  WinDelay = length(Winners)  * ?DELAY_UNIT,
  Delay = case Ctx#texas.stage of
	  ?GS_RIVER -> ?DELAY_UNIT + WinDelay;
	  _ -> WinDelay
  end,
  ?LOG([{delay, Delay}]),
  Ctx1 = Ctx#texas{ winners = Winners,stage=?GS_SHOWDOWN,win_duration=Delay },
  
  Event = #game_stage{ game = Game2#game.gid, stage = Ctx1#texas.stage, pot = pot:total(Game#game.pot)},
  Game3 = g:broadcast(Game2, Event),
  
  {stop, Game3, Ctx1}.

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
	?LOG([{notifyWinner,Event}]),
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