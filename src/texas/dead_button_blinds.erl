-module(dead_button_blinds).
-export([start/3]).

-include("texas.hrl").

%% Serves for tourney only
%% http://www.homepokertourney.com/button.htm
%% The rules can be sum up to few rules only:
	%1. Moveing Clockwise, button could be empty
	%2. Big blind bust not heads up, button could be empty, SB becomes button, the one next to BB gets BB,,not SB.
	%3. todo...

start(Game, Ctx, []) ->
  {Small, Big} = (Game#game.limit):blinds(Game#game.low, Game#game.high),

  Ctx1 = Ctx#texas{
    sb_amt = Small,
	bb_amt = Big,
	sb_bet = 0.0,
    no_sb = false,
	sb_all_in = false
  },
  
  {Button1, Bust} = advance_button(Game, Ctx1),
  Game1 = g:broadcast(Game, 
    #notify_button{ game = Game#game.gid, button = Button1 }
  ),

  %% collect blinds
   SBPlayers = seat:get_seats(Game1, Button1, ?PS_ACTIVE),
   BBPlayers = seat:get_seats(Game1, Button1, ?PS_BB_ACTIVE),
  
   L1 = length(SBPlayers),
   BB_N = L2 = length(BBPlayers),
   HeadsUp = ((L1 == 2) and (L2 == 2)) % two active, 0 waiting for bb
        or ((L1 == 1) and (L2 == 2)), % one active, one waiting for bb

  if
    BB_N < 2 ->
      {goto, top, Game1, Ctx1};
	Bust and not HeadsUp ->
        %% Big blind is bust and is not replaced by a player and is not HeadUp
		%% the button moves to the player who was the small blind 
		%% and the player to the left of the bust big blind posts the big blind. There is no small blind for this hand.
        Ctx2 = Ctx1#texas{
                 b = Button1,
                 no_sb = true,
                 sb = Ctx1#texas.bb
                },
        Seat = hd(BBPlayers),
        ask_for_blind(Game1, Ctx2, Seat, Ctx2#texas.bb_amt, big_blind);
	Bust and HeadsUp ->
            %% Big blind is bust
            %% The small blind is once again the small blind (and button). 
            Ctx2 = Ctx1#texas{ b = Button1 },
            Seat = lists:last(SBPlayers),
            ask_for_blind(Game1, Ctx2, Seat, Ctx2#texas.sb_amt, small_blind);
	 HeadsUp ->
      %% Heads-up play. The small blind is the button and acts first 
	  %% before the flop and last after the flop. The player 
	  %% who does not have the button is dealt the first card.
      	Ctx2 = Ctx1#texas{ b = Button1, headsup = true },
      	ask_for_blind(Game1, Ctx2, Button1, Ctx2#texas.sb_amt, small_blind);
    true ->
      Ctx2 = Ctx1#texas{ b = Button1, sb = hd(SBPlayers) },
      ask_for_blind(Game1, Ctx2, Ctx2#texas.sb, Ctx2#texas.sb_amt, small_blind)
  end.


%%
%% Utility
%%
advance_button(Game, Ctx) ->
    B = Ctx#texas.b,
	%% Button could be an empty seat if player eliminated and not replace by player from another table.
    if
        B == none ->
            %% first hand of the game
            %% start with the first player
            Players = seat:get_seats(Game, ?PS_GAMING),
            Button = hd(Players),
            Bust = false;
        true ->
            %% start with the first 
            %% player after the button
            Players = seat:get_seats(Game, B, ?PS_GAMING),
            Button = hd(Players),
            %% big blind is bust
            Seat = seat:get_seat(Game, Ctx#texas.bb),
            Bust = ?PS_FOLD == Seat#seat.state
    end,
    {Button, Bust}.

ask_for_blind(Game, Ctx, N, Amount, State) ->
  Seat = seat:get_seat(Game, N),
  Player = Seat#seat.player,
  Ctx1 = Ctx#texas{ exp_player = Player, exp_seat = N, exp_amt = Amount },

  R = #raise{ player = Player, raise = 0.0 },
  Game1 = g:cancel_timer(Game),

  case State of
    small_blind -> 
      g:broadcast(Game1, _ = #notify_sb{sb = N, game = Game1#game.gid}),
      post_sb(Game1, Ctx1, R);
    big_blind ->
      g:broadcast(Game1, _ = #notify_bb{bb = N, game = Game1#game.gid}),
      post_bb(Game1, Ctx1, R)
  end.

post_sb(Game, Ctx, #raise{ player = Player, raise = 0.0 }) ->
  N = Ctx#texas.exp_seat,
  Amt = Ctx#texas.exp_amt,
  Seat = seat:get_seat(Game, N),

  Ctx1 = Ctx#texas{ sb = N, sb_bet = Amt },
  Game1 = g:add_bet(Game, Player, Amt),
  Game2 = g:broadcast(Game1, #notify_blind { 
      game = Game1#game.gid, 
      player = Seat#seat.pid,
      call = Amt
    }),

  Game3 = g:notify_state(Game2, N),

  BBPlayers =seat:get_seats(Game3, N, ?PS_ACTIVE),
  ask_for_blind(Game3, Ctx1, hd(BBPlayers), Ctx1#texas.bb_amt, big_blind).

post_bb(Game, Ctx, #raise{ player = Player, raise = 0.0 }) ->
  N = Ctx#texas.exp_seat,
  Amt = Ctx#texas.exp_amt,
  Seat = seat:get_seat(Game, N),

  Ctx1 = Ctx#texas{ bb = N, call = Amt,
    exp_seat = none,
    exp_player = none,
    exp_amt = 0.0
  },

  Game1 = g:add_bet(Game, Player, Amt),
  Game2 = g:broadcast(Game1, #notify_blind { 
      game = Game1#game.gid, 
      player = Seat#seat.pid,
      call = Amt
    }
  ),
  Game3 = g:notify_state(Game2, N),

  {stop, Game3, Ctx1}.