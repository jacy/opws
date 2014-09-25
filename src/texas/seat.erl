-module(seat).
-export([create_seats/1, get_seats/2, get_seats/3, get_seat/2, is_empty/1, seat_query/1]).

-include("../common.hrl").
-include("../pp.hrl").
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
    Seats = get_seats(Game, ?PS_ANY),
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


