-module(g).

%%%
%%% Game utility functions
%%%

-export([get_seats/2, get_seats/3, is_empty/1,
         reset/1, broadcast/2, broadcast/3,
         notify_start_game/1, notify_cancel_game/1,
         join/2, leave/2, kick/1, set_state/3,
         get_seat/2, find/8, seat_query/1, 
         setup/6, create_seats/1,
         request_bet/5, process_autoplay/2,
         cancel_timer/1, restart_timer/2, 
         add_bet/3, new_stage/1, reset_player_state/3,
         pot_size/1, draw/3, draw_shared/2, 
         inplay_plus/3, show_cards/2, rank_hands/1, 
         pots/1, make/1, make/3, watch/3, unwatch/2,
         notify_state/2
        ]).

-include("texas.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").


make(R = #start_game{}) ->
    %% create a game stack. context is used to propagate 
    %% game information from module to module, e.g. button
    %% and blinds position for texas hold'em.
    Mods = case R#start_game.type of
               ?GT_TEXAS_HOLDEM ->
                   texas_holdem_mods(R#start_game.start_delay) 
           end,
    make(R, #texas{}, Mods).

make(R = #start_game{}, none, none) ->
    make(R);

make(R = #start_game{}, Ctx, Mods) ->
    Game = exch:new(game, Ctx, Mods),
    Game:start(self(), [R]).

%%% Initialize seats

create_seats(SeatCount) ->
    Seats = erlang:make_tuple(SeatCount, none),
    create_seats(Seats, SeatCount).

create_seats(Seats, I) when I =:= 0 ->
    Seats;

create_seats(Seats, I) ->
    Seat = #seat {
      player = none,
      bet = 0,
      hand = none,
      state = ?PS_EMPTY,
      cmd_que = []
     },
    Seats1 = setelement(I, Seats, Seat),
    create_seats(Seats1, I - 1).

%%% Create a list of seats matching a certain state

get_seats(Game, none, Mask) ->
    get_seats(Game, Mask);

get_seats(Game, From, Mask) ->
    Size = size(Game#game.seats),
    get_seats(Game#game.seats, Size, From, Size, Mask, []).

get_seats(Game, Mask) ->
    Size = size(Game#game.seats),
    get_seats(Game#game.seats, Size, Size, Size, Mask, []).

get_seats(_Seats, 0, _At, _, _Mask, _Acc) ->
    [];

get_seats(_Seats, _Size, _At, 0, _Mask, Acc) ->
    lists:reverse(Acc);

get_seats(Seats, Size, At, Counter, Mask, Acc) ->
    SeatNum = (At rem Size) + 1,
    Seat = element(SeatNum, Seats),
    IsMember = (Seat#seat.state band Mask) > 0,
    List = if
      IsMember ->
        [SeatNum|Acc];
      true ->
        Acc
    end,
    get_seats(Seats, Size, At + 1, Counter - 1, Mask, List).

is_empty(Game) ->
    Seats = get_seats(Game, ?PS_ANY),
    (Game#game.observers == []) and (Seats == []).

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
    L = seat_query(Game),
    F = fun(S) -> 
        Player = pp:id_to_player(S#seat_state.player),
        Nick = case is_pid(Player) of
          true ->
            gen_server:call(Player, 'NICK QUERY');
          false ->
            undefined
        end,

        broadcast(Game, S#seat_state{ nick = Nick })
    end,
    lists:foreach(F, L).

notify_player_state(Player, Game) ->
    L = seat_query(Game),
    F = fun(S) -> 
        Pid = pp:id_to_player(S#seat_state.player),
        Nick = case is_pid(Pid) of
          true ->
            gen_server:call(Pid, 'NICK QUERY');
          false ->
            undefined
        end,

        gen_server:cast(Player, #notify_seat_detail {
          game = S#seat_state.game,
          seat = S#seat_state.seat,
          state = S#seat_state.state,
          player = S#seat_state.player,
          inplay = S#seat_state.inplay,
          nick = Nick
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
    %% notify players
    Seats = get_seats(Game, ?PS_ANY bor ?PS_OUT),
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
    ResetMask = ?PS_ANY band (bnot ?PS_WAIT_BB),
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
    (Seat#seat.state band From) > 0 ->
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
  Players = get_seats(Game, ?PS_ANY),

  %Limit = #limit {
    %type = Game#game.limit, 
    %min = Game#game.min, 
    %max = Game#game.max, 
    %high = Game#game.high, 
    %low = Game#game.low
  %},

  Detail = #notify_game_detail{
    game = Game#game.gid, 
    pot = pot:total(Game#game.pot),
    players = length(Players),
    seats = size(Game#game.seats),
    stage = Ctx#texas.stage,
    min = Game#game.min,
    max = Game#game.max,
    low = Game#game.low,
    high = Game#game.high
  },

  Detail1 = case Detail#notify_game_detail.stage of
    undefined ->
      Detail#notify_game_detail{stage = ?GS_CANCEL};
    _ ->
      Detail
  end,

  gen_server:cast(R#watch.player, Detail1),

  % Notify Game Shared Card
  notify_shared(lists:reverse(Game#game.board), Game, R#watch.player),

  notify_player_state(R#watch.player, Game),
  watch(R, Game).

watch(R, Game) ->
  Obs = Game#game.observers,
  Game#game{ observers = [R#watch.player|Obs] }.

get_empty_seat(Game) ->
  get_empty_seat(Game#game.seats, 1).

get_empty_seat(Seats, SeatNumber) when size(Seats) < SeatNumber -> 0;

get_empty_seat(Seats, SeatNumber) ->
  Seat = element(SeatNumber, Seats),
  if
    Seat#seat.state == ?PS_EMPTY ->
      SeatNumber;
    true ->
      get_empty_seat(Seats, SeatNumber + 1)
  end.

join(Game, R) when R#join.amount > Game#game.max; R#join.amount < Game#game.min ->
  Game;

join(Game, R) when R#join.seat == 0 ->
  AutoEmptySeat = get_empty_seat(Game),
  case AutoEmptySeat of
    0 ->
      Game;
    _ ->
      join(Game, R#join {seat = AutoEmptySeat})
  end;

join(Game, R) ->
  Seats = Game#game.seats,
  XRef = Game#game.xref,
  Seat = element(R#join.seat, Seats),
  Player = R#join.player,
  OurPlayer = gb_trees:is_defined(Player, XRef),
  GID = Game#game.gid,
  PID = R#join.pid,
  if
    %% seat is taken
    Seat#seat.state /= ?PS_EMPTY ->
      Game;
    %% already sitting at this table
    OurPlayer ->
      Game;
    true ->
      %% try to move the buy-in amount 
      %% from balance to inplay
      case do_buy_in(GID, PID, R#join.amount) of
        ok ->
          %% tell player
          R1 = #notify_join{ 
            game = GID, 
            player = PID,
            seat = R#join.seat,
            amount = R#join.amount,
            nick = gen_server:call(R#join.player, 'NICK QUERY'),
            proc = self()
          },
          %% take seat and broadcast the fact
          Game1 = do_join(Game, R, R#join.state),
          broadcast(Game1, R1);
        _Any ->
          %% no money or other error
          %% gen_server:cast(Player, {stop, Any}),
          Game
      end
  end.

do_buy_in(GID, PID, Amt) 
  when is_number(GID),
       is_number(PID),
       is_number(Amt) ->
    BuyIn = trunc(Amt * 10000),
    case db:read(tab_balance, PID) of
        [] ->
            {error, no_balance_found};
        [B] when BuyIn > B#tab_balance.amount ->
            {error, not_enough_money};
        [_] ->
            %% may need to perform these two in a transaction!
            db:update_balance(tab_inplay, {GID, PID}, Amt),
            db:update_balance(tab_balance, PID, - Amt),
            ok;
        Any ->
            Any
    end.

do_join(Game, R, State) ->
    Seats = Game#game.seats,
    SeatNum = R#join.seat,
    Seat = element(SeatNum, Seats),
    Player = R#join.player,
    XRef = Game#game.xref,
    XRef1 = gb_trees:insert(Player, SeatNum, XRef),
    %% remove from the list of observers
    Observers = lists:delete(Player, Game#game.observers),
    Game#game {
      xref = XRef1,
      seats = setelement(SeatNum,
                         Seats,
                         Seat#seat {
                           player = Player,
                           pid = R#join.pid,
                           inplay = R#join.amount,
                           state = State,
                           hand = hand:new(Player, R#join.pid),
                           cmd_que = []
                          }),
      observers = Observers
     }.

leave(Game, R) ->
  ?LOG([{g_leave, R}]),
  XRef = Game#game.xref,
  Seats = Game#game.seats,
  Player = R#leave.player,
  OurPlayer = gb_trees:is_defined(Player, XRef),
  GID = Game#game.gid,
  if
    OurPlayer ->
      SeatNum = gb_trees:get(Player, XRef),
      Seat = element(SeatNum, Seats),
      PID = Seat#seat.pid,
      if 
        %% leave unless playing
        Seat#seat.state band R#leave.state > 0 ->
          ?LOG([{can_leave}]),
          %% tell player
          R1 = #notify_leave{ 
            game = GID, 
            player = PID,
            proc = self()
          },
          %% notify players
          ?LOG([{notify_player, R1}]),
          Game1 = broadcast(Game, R1),
          ?LOG([{notify_player_end, R1}]),
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
          db:update_balance(tab_balance, PID, Inplay),
          ok = db:delete(tab_inplay, {GID, PID}),
          ?LOG([{leave_auto_watch}]),
          Game3 = watch(#watch{player = Player}, Game2),
          ?LOG([{leave_auto_watch_end}]),
          Game3;
        %% cannot leave now, use auto-play
        true ->
          ?LOG([{auto_play}]),
          Fold = #fold{ game = self(), player = Player },
          Leave = #leave{ game = self(), player = Player },
          Seat1 = Seat#seat{ cmd_que = [[Fold, Leave]] },
          Game#game {
            seats = setelement(SeatNum, Seats, Seat1)
          }
      end;
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
      nick = gen_server:call(Seat#seat.player, 'NICK QUERY')
     },
    broadcast(Game1, Event).

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
      nick = gen_server:call(Seat#seat.player, 'NICK QUERY')
     },
    broadcast(Game, Event).

get_seat(Game, SeatNum) 
  when is_record(Game, game),
       is_integer(SeatNum) ->
    element(SeatNum, Game#game.seats);

get_seat(Game, Player)
  when is_record(Game, game),
       is_pid(Player) ->
    case gb_trees:lookup(Player, Game#game.xref) of
        {value, SeatNum} ->
            {SeatNum, element(SeatNum, Game#game.seats)};
        _ ->
            none
    end.

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
            Pot = pot:add(Game1#game.pot, Player, Amount, AllIn),
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
    Seats = get_seats(Game, Button, ?PS_ANY),
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
    Seats = get_seats(Game, ?PS_SHOWDOWN),
    rank_hands(Game, Seats).

rank_hands(Game, Seats) ->
    F = fun(SeatNum) ->
                Seat = element(SeatNum, Game#game.seats),
                Seat#seat.hand
        end,
    Hands = lists:map(F, Seats), %% 获取每个有效作为的手牌
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

query_op(Arg, Op, Value) 
  when is_number(Arg),
       is_number(Value) ->
    case Op of
        ?OP_IGNORE ->
            true;
        ?OP_EQUAL ->
            Arg == Value;
        ?OP_LESS ->
            Arg < Value;
        ?OP_GREATER ->
            Arg > Value;
        _ ->
            false
    end.

find(GameType, LimitType,
     ExpOp, Expected, 
     JoinOp, Joined,
     WaitOp, Waiting) ->
    F = fun() -> find(GameType, LimitType) end,
    {atomic, L} = mnesia:transaction(F),
    F1 = fun(R = #game_info{}) ->
                 query_op(R#game_info.required, ExpOp, Expected) 
                     and query_op(R#game_info.joined, JoinOp, Joined) 
                     and query_op(R#game_info.waiting, WaitOp, Waiting)
         end,
    {atomic, lists:filter(F1, L)}.

find(GameType, LimitType) ->
    Q = qlc:q([G || G <- mnesia:table(tab_game_xref),
                    G#tab_game_xref.type == GameType,
                    (G#tab_game_xref.limit)#limit.type == LimitType]),
    L = qlc:e(Q),
    lists:map(fun(R) ->
                      Game = R#tab_game_xref.process,
                      GID = R#tab_game_xref.gid,
                      Joined = gen_server:call(Game, 'JOINED'),
                      Waiting = 0, % not implemented
                      _ = #game_info{
                        game = GID,
                        table_name = names:get_table_name(GID),
                        type = R#tab_game_xref.type,
                        limit = R#tab_game_xref.limit,
                        seat_count = R#tab_game_xref.seat_count,
                        required = R#tab_game_xref.required,
                        joined = Joined,
                        waiting = Waiting
                       }
              end, L).

seat_query(Game) ->
    Size = size(Game#game.seats),
    seat_query(Game, Size, []).

seat_query(_Game, 0, Acc) ->
    Acc;

seat_query(Game, SeatNum, Acc) ->
    Seat = element(SeatNum, Game#game.seats),
    SeatState = #seat_state{
      game = Game#game.gid,
      seat = SeatNum,
      state = Seat#seat.state,
      player = Seat#seat.pid,
      inplay = Seat#seat.inplay
     },
    Acc1 = [SeatState|Acc],
    seat_query(Game, SeatNum - 1, Acc1).

setup(GameType, SeatCount, Limit, Delay, Timeout, Max) ->
    Game = #tab_game_config {
      id = erlang:phash2(now(), 1 bsl 32),
      type = GameType,
      seat_count = SeatCount,
      limit = Limit,
      start_delay = Delay,
      player_timeout = Timeout,
      max = Max
     },
    ok = db:write(Game).

credit_player(GID, PID, Amount) ->
    db:update_balance(tab_balance, PID, Amount),
    ok = db:delete(tab_inplay, {GID, PID}).

debit_player(GID, PID, Amount) 
  when is_number(GID),
       is_number(PID),
       is_number(Amount) ->
    BuyIn = trunc(Amount * 10000),
    case db:read(tab_balance, PID) of
        [] ->
            {error, no_balance_found};
        [B] when BuyIn > B#tab_balance.amount ->
            {error, not_enough_money};
        [_] ->
            %% may need to perform these two in a transaction!
            db:update_balance(tab_inplay, {GID, PID}, Amount),
            db:update_balance(tab_balance, PID, - Amount),
            ok;
        Any ->
            Any
    end.

core_texas_mods() ->
    [
     %% blind rules
     {blinds, []},
     %% deal 2 cards to each player
     {deal_cards, [2, private]}, 
     {rank, []}, 
     %% start after BB, 3 raises
     {betting, [?MAX_RAISES, ?GS_PREFLOP, true]}, 
     %% show 3 shared cards
     {deal_cards, [3, shared]}, 
     {rank, []}, 
     %% flop
     {betting, [?MAX_RAISES, ?GS_FLOP]}, 
     %% show 1 more shared card
     {deal_cards, [1, shared]}, 
     {rank, []}, 
     %% turn
     {betting, [?MAX_RAISES, ?GS_TURN]}, 
     %% show 1 more shared card
     {deal_cards, [1, shared]}, 

     {rank, []}, 
     %% river
     {betting, [?MAX_RAISES, ?GS_RIVER]}, 
     %% showdown
     {showdown, []}
    ].
     
texas_holdem_mods(StartDelay) ->
    [ {wait_players, [StartDelay]} ] 
        ++ core_texas_mods() 
        ++ [ {restart, []} ].
