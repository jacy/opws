-module(g).

-export([reset/1, broadcast/2, broadcast/3,
         notify_start_game/1, notify_cancel_game/1,
         leave/2, kick/1, set_state/3,
		  setup/8, request_bet/5, process_autoplay/2,
         cancel_timer/1, restart_timer/2, 
         add_bet/3, new_stage/1, reset_player_state/3,
         pot_size/1, draw/3, draw_shared/2, 
         inplay_plus/3, show_cards/2, rank_hands/1, 
         pots/1, watch/3, unwatch/2,
         notify_state/2,broadcast_player_state/1,notify_pot/2,uuid/0
        ]).

-include("texas.hrl").


notify_start_game(Game) ->
    %Msg = lang:msg(?GAME_STARTING),
    Game1 = reset(Game),
    GID = Game1#game.gid,
    %Game2 = broadcast(Game1, #chat{ game = GID, message = Msg }),
    broadcast(Game1, #notify_start_game{ game = GID }).

notify_cancel_game(Game) ->
    %Msg = lang:msg(?GAME_CANCELLED),
    GID = Game#game.gid,
    %Game1 = broadcast(Game, #chat{ game = GID, message = Msg }),
    broadcast(Game, #notify_cancel_game{ game = GID }).

%%% Broadcast

broadcast_player_state(Game) ->
    L = seat:seat_query(Game),
    F = fun(S) -> 
        broadcast(Game, S)
    end,
    lists:foreach(F, L).

notify_player_state(Player, Game) ->
    L = seat:seat_query(Game),
    F = fun(S) -> 
        Pid = pp:id_to_player(S#seat_state.player),
        gen_server:cast(Player, #notify_seat_detail {
          game = S#seat_state.game,
          seat = S#seat_state.seat,
          state = S#seat_state.state,
          player = S#seat_state.player,
          inplay = S#seat_state.inplay,
          nick = S#seat_state.nick,
		  bet = pot:player_pending(Game#game.pot, Pid)
        })
    end,
    lists:foreach(F, L).

make_players(Game, Seats) ->
    make_players(Game, Seats, []).

make_players(_Game, [], Acc) ->
    lists:reverse(Acc);

make_players(Game, [SeatNum|Rest], Acc) ->
    Seat = element(SeatNum, Game#game.seats),
    Player = Seat#seat.player,
    make_players(Game, Rest, [Player|Acc]).

broadcast(Game, Event) ->
    broadcast(Game, Event, none).

broadcast(Game, Event, Except) ->
    Seats = seat:get_seats(Game, ?PS_GAMING bor ?PS_OUT),
    Players = make_players(Game, Seats),
    broadcast(Game, Players, Event, Except), 
    broadcast(Game, Game#game.observers, Event, none).

broadcast(Game, [Player|Rest], Event, Except) ->
    if
        Player /= Except ->
            gen_server:cast(Player, Event);
        true ->
            ok
    end,
    broadcast(Game, Rest, Event, Except);

broadcast(Game, [], _, _) ->
    Game.

%%% Reset

reset(Game) ->
    Deck = deck:reset(Game#game.deck),
    Game1 = Game#game{
              deck = Deck,
              board = [],
              raise_count = 0,
              pot = pot:reset(Game#game.pot)
             },
    Seats = reset_hands(Game1#game.seats),
    Game2 = reset_bets(Game1#game{ seats = Seats }),
    ResetMask = ?PS_GAMING band (bnot ?PS_WAIT_BB),
    Game3 = reset_player_state(Game2, ResetMask, ?PS_PLAY),
    broadcast_player_state(Game3),
    Game3.

reset_player_state(Game, From, To) ->
    reset_player_state(Game, From, To, size(Game#game.seats)).

reset_player_state(Game, _From, _To, 0) ->
    Game;

reset_player_state(Game, From, To, Count) ->
  Seat = element(Count, Game#game.seats),
  Game1 = if
    (Seat#seat.state band From) > 0 ->   % PS_ANY except PS_WAIT_BB
      Game#game {
        seats = setelement(Count,
          Game#game.seats,
          Seat#seat{ state = To })
      };
    true ->
      Game
  end,
  reset_player_state(Game1, From, To, Count - 1).

reset_bets(Game) ->
    reset_bets(Game, size(Game#game.seats)).

reset_bets(Game, 0) ->
    Game;

reset_bets(Game, Count) ->
    Seat = element(Count, Game#game.seats),
    Game1 = Game#game {
              seats = setelement(Count,
                                 Game#game.seats,
                                 Seat#seat{ bet = 0 })
             },
    reset_bets(Game1, Count - 1).

reset_hands(Seats) ->
    reset_hands(Seats, size(Seats)).

reset_hands(Seats, 0) ->
    Seats;

reset_hands(Seats, Count) ->
    Seat = element(Count, Seats),
    Player = Seat#seat.player,
    PID = Seat#seat.pid,
    Seats1 = setelement(Count, Seats, Seat#seat{ 
                                        hand = hand:new(Player, PID),
                                        muck = false
                                       }),
    reset_hands(Seats1, Count - 1).

unwatch(Game, R) ->
  Obs = lists:delete(R#unwatch.player, Game#game.observers),
  gen_server:cast(R#unwatch.player, #notify_unwatch{ game = Game#game.gid }),
  Game#game{ observers = Obs }.

watch(Game, Ctx, R) ->
  Players = seat:get_seats(Game, ?PS_GAMING),

  Detail = #notify_game_detail{
    game = Game#game.gid, 
    players = length(Players),
    seats = size(Game#game.seats),
    stage = Ctx#texas.stage,
    min = Game#game.min,
    max = Game#game.max,
    low = Game#game.low,
    high = Game#game.high
  },

  gen_server:cast(R#watch.player, Detail),
  notify_player_state(R#watch.player, Game),
  
  if Ctx#texas.stage < ?GS_FLOP 
		-> ok ;
    true ->
		  notify_shared(lists:reverse(Game#game.board), Game, R#watch.player),
		  notify_pot(Game#game.pot, R#watch.player)
  end,
  
  if Ctx#texas.stage band ?GS_BETTING > 0 -> % Show Bet Request Timer
		 gen_server:cast(R#watch.player,#notify_actor{ game = Game#game.gid, seat = Ctx#texas.exp_seat});
	 true -> 
		  ok
  end,
  watch(R, Game).

watch(R, Game) ->
  Obs = Game#game.observers,
  Game#game{ observers = [R#watch.player|Obs] }.



leave(Game, R) ->
  XRef = Game#game.xref,
  Seats = Game#game.seats,
  Player = R#leave.player,
  OurPlayer = gb_trees:is_defined(Player, XRef),
  GID = Game#game.gid,
  if
    OurPlayer ->
      SeatNum = gb_trees:get(Player, XRef),
      Seat = element(SeatNum, Seats),
  	  ?LOG([{seat_to_leave, Seat}]),
      PID = Seat#seat.pid,
          %% tell player
          R1 = #notify_leave{ 
            game = GID, 
            player = PID,
            proc = self()
          },
          %% notify players
          Game1 = broadcast(Game, R1),
          XRef1 = gb_trees:delete(Player, XRef),
          Game2 = Game1#game {
            xref = XRef1,
            seats = setelement(SeatNum,
              Seats,
              Seat#seat {
                player = none,
                state = ?PS_EMPTY,
                muck = false
              })
          },
          %% update inplay balance
          Inplay = Seat#seat.inplay,
          mdb:update_balance(PID, Inplay),
          ok = mdb:delete(tab_inplay, {GID, PID}),
          Game3 = watch(#watch{player = Player}, Game2),
          Game3;
    %% not playing here
    true ->
      ?LOG([{not_ourplayer, leave, R}]),
      Game
  end.

kick(Game) ->
    Seats = Game#game.seats,
    kick(Game, Seats, size(Seats)).

kick(_, _, 0) ->
    ok;

kick(Game, Seats, N) ->
    Seat = element(N, Seats),
	?FLOG("Kicking player pid=~w",[Seat#seat.pid]),
    Player = Seat#seat.player,
    Game2 = case Player of 
                none ->
                    Game;
                _ ->
                    XRef = Game#game.xref,
                    %% notify others
                    Game1 = broadcast(Game, #notify_leave{
                                        game = Game#game.gid,
                                        player = Seat#seat.pid,
                                        proc = self()
                                       }),
                    XRef1 = gb_trees:delete(Player, XRef),
                    Game1#game {
                      xref = XRef1,
                      seats = setelement(N,
                                         Seats,
                                         Seat#seat {
                                           player = none,
                                           state = ?PS_EMPTY
                                          })
                     }
            end,
    kick(Game2, Seats, N - 1).

set_state(Game, Player, State)
  when is_pid(Player) ->
    case gb_trees:lookup(Player, Game#game.xref) of
        {value, SeatNum} ->
            set_state(Game, SeatNum, State);
        _ ->
            Game
    end;

set_state(Game, SeatNum, State)
  when is_integer(SeatNum) ->
    Seat = element(SeatNum, Game#game.seats),
    Game1 = Game#game {
              seats = setelement(SeatNum,
                                 Game#game.seats,
                                 Seat#seat{ state = State })
             },
    Event = #seat_state{
      game = Game1#game.gid,
      seat = SeatNum,
      state = State,
      player = Seat#seat.pid,
      inplay = Seat#seat.inplay,
      nick = Seat#seat.nick
     },
    broadcast(Game1, Event).

notify_pot(Pot, Player) ->
 Pots = pot:pots(Pot),
 F = fun({Amount,_,PotId}) ->
	 	 gen_server:cast(Player, #notify_pot{amount=Amount,id=PotId})
 end,
 lists:foreach(F, Pots).

notify_state(Game, Player)
  when is_pid(Player) ->
    case gb_trees:lookup(Player, Game#game.xref) of
        {value, SeatNum} ->
            notify_state(Game, SeatNum);
        _ ->
            Game
    end;

notify_state(Game, SeatNum)
  when is_integer(SeatNum) ->
    Seat = element(SeatNum, Game#game.seats),
    Event = #seat_state{
      game = Game#game.gid,
      seat = SeatNum,
      state = Seat#seat.state,
      player = Seat#seat.pid,
      inplay = Seat#seat.inplay,
      nick = Seat#seat.nick
     },
    broadcast(Game, Event).

request_bet(Game, SeatNum, Call, Min, Max) ->
  Seat = element(SeatNum, Game#game.seats),
  Game1 = broadcast(Game, #notify_actor{ game = Game#game.gid, seat = SeatNum}),

  if 
    %% auto-play enabled
    Seat#seat.cmd_que /= [] ->
      Seat1 = process_autoplay(Game1, Seat),
      Game1#game {
        seats = setelement(SeatNum,
          Game1#game.seats,
          Seat1)
      };
    %% regular bet request
    true ->
      BetReq = #bet_req{
        game = Game1#game.gid,
        call = Call,
        min = Min,
        max = Max
      },
      gen_server:cast(Seat#seat.player, BetReq),
      Game1
  end.

%%% Use stored commands instead of asking player

process_autoplay(Game, Seat) ->
    Que = Seat#seat.cmd_que,
    process_autoplay(Game, Seat, Que).

process_autoplay(_Game, Seat, []) ->
    Seat;

process_autoplay(_Game, Seat, [H|T]) ->
    autoplay(self(), H),
    Seat#seat{ cmd_que = T }.

autoplay(_, []) ->
    ok;

autoplay(Game, [H|T]) ->
    %% forward action as if coming from us
    spawn(fun() -> gen_server:cast(Game, H) end),
    autoplay(Game, T).

cancel_timer(Game = #game{}) ->
    catch cancel_timer(Game#game.timer),
    Game#game{ timer = none };

cancel_timer(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            receive {timeout, Ref, _} -> 0
            after 0 -> false
            end;
        RemainingTime ->
            RemainingTime
    end.

restart_timer(Game, Timeout) ->
	%io:format("Set timer Timeout=~w~n------------------------~n", [Timeout]),
    Game#game{ timer = erlang:start_timer(Timeout, self(), none) }.

add_bet(Game, Player, Amount)
  when is_pid(Player) ->
    N = gb_trees:get(Player, Game#game.xref),
    add_bet(Game, N, Amount);

add_bet(Game, SeatNum, Amount) ->
    Seat = element(SeatNum, Game#game.seats),
    Player = Seat#seat.player,
    Inplay = Seat#seat.inplay,
    ?LOG([{add_bet, {amount, Amount}, {inplay, Inplay}}]),
    if
        Amount > Inplay->
            set_state(Game, SeatNum, ?PS_OUT);
        true ->
            if
                Amount == Inplay ->
                    State = ?PS_ALL_IN,
                    AllIn = true,
                    Game1 = Game;
                true ->
                    State = Seat#seat.state,
                    AllIn = false,
                    Game1 = Game
            end,
            Pot = pot:pend(Game1#game.pot, Player, Amount, AllIn),
            Game1#game {
              pot = Pot,
              seats = setelement(SeatNum,
                                 Game1#game.seats,
                                 Seat#seat {
                                   bet = Seat#seat.bet + Amount,
                                   state = State,
                                   inplay = Inplay - Amount
                                  })
             }
    end.

new_stage(Game) ->
    Game1 = reset_bets(Game),
    Pot = Game1#game.pot,
    Game1#game{ pot = pot:new_stage(Pot) }.

pot_size(Game) ->
    pot:total(Game#game.pot).

draw(Game, _, 0) ->
    Game;

draw(Game, Seats, N) ->
    Game1 = draw(Game, Seats),
    draw(Game1, Seats, N - 1).

draw(Game, []) ->
    Game;

draw(Game, [H|T]) ->
    Game1 = draw(Game, H),
    draw(Game1, T);

draw(Game, SeatNum) 
  when is_number(SeatNum) ->
    {Deck, Card} = deck:draw(Game#game.deck),
    Seat = element(SeatNum, Game#game.seats),
    Player = Seat#seat.player,
    Hand = hand:add(Seat#seat.hand, Card),
    Seats = setelement(SeatNum, Game#game.seats, Seat#seat{ hand = Hand }),
    Private = #notify_private{ 
      game = Game#game.gid, 
      player = Seat#seat.pid, 
      card = Card },
    Draw = #notify_draw{ 
      game = Game#game.gid, 
      player = Seat#seat.pid, 
      card = 0},
    gen_server:cast(Player, Private),
    broadcast(Game, Draw),
    Game#game{ seats = Seats, deck = Deck }.

draw_shared(Game, 0) ->
    Game;

draw_shared(Game, N) ->
    {Deck, Card} = deck:draw(Game#game.deck),
    Game1 = Game#game {
              deck = Deck,
              board = [Card|Game#game.board]
             },
    Shared = #notify_shared{ game = Game#game.gid, card = Card },
    Game2 = broadcast(Game1, Shared),
    draw_shared(Game2, N - 1).

notify_shared([Card|T], Game, Player) ->
  Shared = #notify_shared{ game = Game#game.gid, card = Card },
  gen_server:cast(Player, Shared),
  notify_shared(T, Game, Player);

notify_shared([], _Game, _Player) ->
  ok.

inplay_plus(Game, SeatNum, Amount) 
  when is_integer(SeatNum) ->
    Seat = element(SeatNum, Game#game.seats),
    Game#game {
      seats = setelement(SeatNum,
                         Game#game.seats,
                         Seat#seat{
                           inplay = Seat#seat.inplay + Amount
                          })
     };

inplay_plus(Game, Player, Amount)
  when is_pid(Player) ->
    case gb_trees:lookup(Player, Game#game.xref) of
        {value, SeatNum} ->
            inplay_plus(Game, SeatNum, Amount);
        _ ->
            Game
    end.

show_cards(Game, Button) 
  when is_integer(Button) ->
    Seats = seat:get_seats(Game, Button, ?PS_GAMING),
    show_cards(Game, Seats);

show_cards(_, []) -> 
    ok;

show_cards(Game, [H|T]) ->
    Seat = element(H, Game#game.seats),
    if 
        Seat#seat.muck == false ->
            Player = Seat#seat.player,
            Event = #show_cards{
              game = Game#game.gid,
              player = Seat#seat.pid,
              cards = (Seat#seat.hand)#hand.cards
             },
            broadcast(Game, Event, Player);
        true ->
            ok
    end,
    show_cards(Game, T).

rank_hands(Game) ->
    Seats = seat:get_seats(Game, ?PS_SHOWDOWN),
    rank_hands(Game, Seats).

rank_hands(Game, Seats) ->
    F = fun(SeatNum) ->
                Seat = element(SeatNum, Game#game.seats),
                Seat#seat.hand
        end,
    Hands = lists:map(F, Seats), %% 获取每个有效座位的手牌
    Cards = Game#game.board,
    F1 = fun(Card, Acc) ->
                 F2 = fun(Hand) -> hand:add(Hand, Card) end, 
                 lists:map(F2, Acc)
         end,
    Hands1 = lists:foldl(F1, Hands, Cards), %% 将公共牌派发到每一个手牌当中
    F2 = fun(Hand) -> 
        hand:rank(Hand) 
    end, %% 对所有的手牌进行排名并返回
    lists:map(F2, Hands1).

pots(Game) ->
    pot:pots(Game#game.pot).

setup(Id, Code, Name, GameType, SeatCount, Limit, Delay, Timeout) ->
    Game = #tab_game_config {
      id = Id,
	  code = Code,
	  name=Name,
      type = GameType,
      seat_count = SeatCount,
      limit = Limit,
      start_delay = Delay,
      player_timeout = Timeout
     },
    ok = mdb:write(Game).

uuid() ->
	 erlang:phash2(now(), 1 bsl 32).

%% credit_player(GID, PID, Amount) ->
%%     mdb:update_balance(PID, Amount),
%%     ok = mdb:delete(tab_inplay, {GID, PID}).
%% 
%% debit_player(GID, PID, Amount) 
%%   when is_number(GID),
%%        is_number(PID),
%%        is_number(Amount) ->
%%     BuyIn = trunc(Amount * 10000),
%%     case db:read(tab_balance, PID) of
%%         [] ->
%%             {error, no_balance_found};
%%         [B] when BuyIn > B#tab_balance.amount ->
%%             {error, not_enough_money};
%%         [_] ->
%%             %% may need to perform these two in a transaction!
%%             mdb:update_balance({GID, PID}, Amount),
%%             mdb:update_balance(PID, - Amount),
%%             ok;
%%         Any ->
%%             Any
%%     end.