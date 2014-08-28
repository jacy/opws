-module(showdown).

-export([start/3]).

-include("texas.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(COMMISSION,0.02).
-define(DELAY_UNIT,2000).

start(Game, Ctx, []) ->
  g:show_cards(Game, Ctx#texas.b),

  Ranks = g:rank_hands(Game),
  notify_hands(Game, Ranks),

  Pots = g:pots(Game),
  Winners = gb_trees:to_list(winners(Ranks, Pots)),
  ?LOG([{winners, Winners}]),
  Game1 = notify_winners(Game, Winners),

  %% TODO 将所有金额不足的玩家重新设置状态
  {_, Big} = (Game1#game.limit):blinds(Game1#game.low, Game1#game.high),
  Game2 = check_inplay(g:get_seats(Game, ?PS_ANY), Big, Game1),

  WinDelay = winner_length(Winners, 0)  * ?DELAY_UNIT,
  Delay = case Ctx#texas.stage of
	  ?GS_RIVER -> ?DELAY_UNIT + WinDelay;
	  _ -> WinDelay
  end,
  ?LOG([{delay, Delay}]),
  Ctx1 = Ctx#texas{ winners = Winners,stage=?GS_SHOWDOWN,win_duration=Delay },
  {stop, Game2, Ctx1}.

%%%
%%% Utility
%%%

winner_length([],Count) ->
  Count;

winner_length([{_, Pots}|T],Count) ->
  WinCount = length(gb_trees:to_list(Pots)),
  winner_length(T, WinCount + Count).

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

notify_winners(Game, [{H, Pots}|T]) ->
    WinPots = gb_trees:to_list(Pots),
	Game1 = notify_winner(Game, H, WinPots),
    notify_winners(Game1, T).

notify_winner(Game, _H, []) ->
    Game;

notify_winner(Game, H, [{ PotId, Amount}|T]) ->
    Player = H#hand.player,
    PID = H#hand.pid,
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
    g:broadcast(Game1, Event),
    notify_winner(Game1,H, T).

winners(Ranks, Pots) ->
    winners(Ranks, Pots, gb_trees:empty()).

winners(_Ranks, [], Winners) ->
    Winners;

winners(Ranks, [{Total, Members, PotId}|Rest], Winners) ->
    M = lists:filter(fun(Hand) -> 
                             gb_trees:is_defined(Hand#hand.player, Members) 
                     end, Ranks),
    %% sort by rank and leave top ranks only
    M1 = lists:reverse(lists:keysort(5, M)),
    TopRank = element(5, hd(M1)),
    M2 = lists:filter(fun(R) -> element(5, R) == TopRank end, M1),
    %% sort by high card and leave top high cards only
    M3 = lists:reverse(lists:keysort(6, M2)),
    TopHigh1 = element(6, hd(M3)),
    M4 = lists:filter(fun(R) -> element(6, R) == TopHigh1 end, M3),
    M5 = lists:reverse(lists:keysort(7, M4)),
    TopHigh2 = element(7, hd(M5)),
    M6 = lists:filter(fun(R) -> element(7, R) == TopHigh2 end, M5),
    %% sort by top score and leave top scores only
    M7 = lists:reverse(lists:keysort(8, M6)),
    TopScore = element(8, hd(M7)),
    M8 = lists:filter(fun(R) -> element(8, R) == TopScore end, M7),
    Win = Total / length(M8),
   Winners1 = update_winners(M8, Win,PotId, Winners),
    winners(Ranks, Rest, Winners1).

update_winners([], _Amount, _PotId, Tree) ->
    Tree;

update_winners([Player|Rest], Amount, PotId, Tree) ->
    update_winners(Rest, Amount, PotId,
                   update_counter(Player, Amount, PotId, Tree)).

update_counter(Key, Amount, PotId, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Old} ->
            gb_trees:update(Key, update_value(PotId, Amount, Old), Tree);
        none ->
            gb_trees:insert(Key,  gb_trees:insert(PotId, Amount, gb_trees:empty()), Tree)
    end.

update_value(Key, Amount, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Old} ->
            Old = gb_trees:get(Key, Tree),
            gb_trees:update(Key, Old + Amount, Tree);
        none ->
            gb_trees:insert(Key, Amount, Tree)
    end.


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