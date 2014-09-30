-module(seat).
-export([create_seats/1, get_seats/2, get_seats/3, get_seat/2, is_empty/1, seat_query/1, join/2]).

-include("common.hrl").
-include("pp.hrl").
-include("game.hrl").

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

get_seats(Game, Mask) ->
    Size = size(Game#game.seats),
    get_seats(Game#game.seats, Size, Size, Size, Mask, []).


get_seats(Game, none, Mask) ->
    get_seats(Game, Mask);

get_seats(Game, From, Mask) ->
    Size = size(Game#game.seats),
    get_seats(Game#game.seats, Size, From, Size, Mask, []).


get_seats(_Seats, 0, _ , _, _, _) ->
    [];

get_seats(_Seats, _Size, _At, 0, _Mask, Acc) ->
    lists:reverse(Acc);

get_seats(Seats, Size, At, Counter, Mask, Acc) ->
    SeatNum = (At rem Size) + 1,  %
    Seat = element(SeatNum, Seats),
    IsMember = (Seat#seat.state band Mask) > 0,
    List = if
      IsMember ->
        [SeatNum|Acc];
      true ->
        Acc
    end,
    get_seats(Seats, Size, At + 1, Counter - 1, Mask, List).

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

is_empty(Game) ->
    Seats = get_seats(Game, ?PS_GAMING),
    (Game#game.observers == []) and (Seats == []).

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
      %% move buy-in amount from balance to inplay
      case mdb:buy_in(GID, PID, R#join.amount) of
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
    Seats = get_seats(Game, ?PS_GAMING bor ?PS_OUT),
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