-module(blinds).
-export([start/3]).

-include("texas.hrl").

start(Game, Ctx, []) ->
  {Small, Big} = (Game#game.limit):blinds(Game#game.low, Game#game.high),

  Ctx1 = Ctx#texas{
    sb_amt = Small,
	bb_amt = Big,
	sb_bet = 0.0,
    no_sb = false,
	sb_all_in = false
  },
  
  Button = advance_button(Game, Ctx1),
  Game1 = g:broadcast(Game, 
    #notify_button{ game = Game#game.gid, button = Button }
  ),

  AllPlayers =seat:get_seats(Game1, Button, ?PS_ACTIVE),
  L = length(AllPlayers),

  if
    L < 2 ->
      {goto, top, Game1, Ctx1};
%%  L == 2 ->
%%       %% 一对一时特殊规则生效
%%       %% 庄家下小盲注，对家下大盲注
%%       %% 首次行动由庄家先叫，之后每次都为对家先叫
%%       Ctx2 = Ctx1#texas{ b = Button, headsup = true },
%%       ask_for_blind(Game1, Ctx2, Button, Ctx2#texas.sb_amt, small_blind);
    true ->
      Ctx2 = Ctx1#texas{ b = Button, sb = hd(AllPlayers) },
      ask_for_blind(Game1, Ctx2, Ctx2#texas.sb, Ctx2#texas.sb_amt, small_blind)
  end.

%%
%% Utility
%%
advance_button(Game, Ctx) ->
   B = Ctx#texas.b,
   if
        B == none ->
            %% first hand of the game the first player would be the sb
            Players = seat:get_seats(Game, ?PS_PLAY),
            lists:last(Players);
        true ->
            %% start with the first 
            %% player after the button
            Players = seat:get_seats(Game, B, ?PS_PLAY),
            hd(Players)
    end.

ask_for_blind(Game, Ctx, N, Amount, State) ->
  Seat = seat:get_seat(Game, N),
  Player = Seat#seat.player,
  Ctx1 = Ctx#texas{ exp_player = Player, exp_seat = N, exp_amt = Amount },

  %% 每局自动下盲注
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

  %% 为玩家下小盲注
  Ctx1 = Ctx#texas{ sb = N, sb_bet = Amt },
  Game1 = g:add_bet(Game, Player, Amt),
  Game2 = g:broadcast(Game1, #notify_blind { 
      game = Game1#game.gid, 
      player = Seat#seat.pid,
      call = Amt
    }),

  Game3 = g:notify_state(Game2, N),

  %% 为玩家下大盲注
  BBPlayers =seat:get_seats(Game3, N, ?PS_ACTIVE),
  ask_for_blind(Game3, Ctx1, hd(BBPlayers), Ctx1#texas.bb_amt, big_blind).

post_bb(Game, Ctx, #raise{ player = Player, raise = 0.0 }) ->
  N = Ctx#texas.exp_seat,
  Amt = Ctx#texas.exp_amt,
  Seat = seat:get_seat(Game, N),

  Ctx1 = Ctx#texas{ bb = N, call = Amt,
    %% 由其他模块决定谁应该行动
    exp_seat = none,
    exp_player = none,
    exp_amt = 0.0
  },

  %% 为玩家下大盲注
  Game1 = g:add_bet(Game, Player, Amt),
  Game2 = g:broadcast(Game1, #notify_blind { 
      game = Game1#game.gid, 
      player = Seat#seat.pid,
      call = Amt
    }
  ),
  Game3 = g:notify_state(Game2, N),

  %% 结束盲注
  {stop, Game3, Ctx1}.