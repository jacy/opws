-module(texas).

-behavior(exch).

-export([id/0, start/1, modules/1, context/0, stop/1, call/2, cast/3]).

-include("texas.hrl").

id() ->
  counter:bump(texas).

start([R = #start_game{id = GID}]) ->
    store_game_info(GID, R),
    Game = #game {
      gid = GID,
      type = R#start_game.type, 
      limit = case (R#start_game.limit)#limit.type of
                  ?LT_FIXED_LIMIT -> fixed_limit;
                  ?LT_POT_LIMIT -> pot_limit;
                  ?LT_NO_LIMIT -> no_limit
              end,
      low = (R#start_game.limit)#limit.low, 
      high = (R#start_game.limit)#limit.high, 
      min = (R#start_game.limit)#limit.min,
      max = (R#start_game.limit)#limit.max,
      deck = deck:new(R#start_game.rigged_deck),
      pot = pot:new(),
      seats = seat:create_seats(R#start_game.seat_count),
      required_player_count = R#start_game.required,
      timeout = R#start_game.player_timeout,
	  start_delay = R#start_game.start_delay,
      tourney = none,
	  barrier=R#start_game.barrier
     },
    {Game, R}.

modules([R= #start_game{type=Type}]) ->
    case Type of
       ?GT_IRC_TEXAS ->
           irc_texas_mods(R#start_game.barrier);
       ?GT_TEXAS_HOLDEM ->
           texas_holdem_mods() 
   end.

context() ->
	#texas{}.

stop(Game) 
  when is_record(Game, game) ->
	?ERROR([terminate_game, {id, Game#game.gid}]),
    Game1 = g:cancel_timer(Game),
    %% force players to leave
    g:kick(Game1),
    %% remove ourselves from the db
    ok = mdb:delete(tab_game_xref, Game1#game.gid).

call('ID', Game) ->
  Game#game.gid;

call('REQUIRED', Game) ->
  Game#game.required_player_count;

call('JOINED', Game) ->
  Seats =seat:get_seats(Game, ?PS_GAMING),
  length(Seats);

call('WAITING', _) ->
  0;

call('SEAT QUERY', Game) ->
  seat:seat_query(Game);

call({'INPLAY', Player}, Game) ->
  {_, Seat} = seat:get_seat(Game, Player),
  Seat#seat.inplay;

call('DEBUG', Game) ->
  Game.

cast({timeout, _, {out, SeatNum, PID}}, _Ctx, Game) ->
  Seat = element(SeatNum, Game#game.seats),
  case Seat#seat.pid of
    PID ->
      GID = global:whereis_name({?MODULE, Game#game.gid}),
      gen_server:cast(Seat#seat.player, #leave{game = GID });
    _ ->
      ok
  end,
  Game;

cast(R = #watch{}, Ctx, Game) ->
  g:watch(Game, Ctx, R);

cast(R = #unwatch{}, _Ctx, Game) ->
  ?LOG([{game_unwatch, R}]),
 g:unwatch(Game, R);

cast(R = #sit_out{},_Ctx, Game) ->
    change_state(Game, R#sit_out.player, ?PS_SIT_OUT);

cast(R = #come_back{},_Ctx, Game) ->
    change_state(Game, R#come_back.player, ?PS_PLAY);

cast({'SET STATE', Player, State}, _Ctx, Game) ->
    change_state(Game, Player, State);

cast(R, _Ctx, Game) ->
   ?LOG([{unknown_dispatch, {msg, R}, {game, Game}}]),
  Game.

change_state(Game, Player, State) ->
    Game1 = g:set_state(Game, Player, State),
    {SeatNum, Seat} = seat:get_seat(Game1, Player),
    R = #seat_state{
      game = Game1#game.gid,
      seat = SeatNum,
      state = State,
      player = Seat#seat.pid,
      inplay = Seat#seat.inplay
     },
    g:broadcast(Game1, R),
    Game1.

store_game_info(GID, R) ->
    Game = #tab_game_xref {
      gid = GID,
      process = self(),
      type = R#start_game.type, 
      limit = R#start_game.limit,
      table_name = R#start_game.table_name,
      seat_count = R#start_game.seat_count,
      timeout = R#start_game.player_timeout,
      required = R#start_game.required
     },
    ok = mdb:write(Game).

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
     {showdown, []},
     {delay, []}, % delay according to winner counts
     {end_game, []}
    ].
     
texas_holdem_mods() ->
    [ {wait_players, []} ] 
        ++ core_texas_mods() 
        ++ [ {restart, []} ].

irc_texas_mods(Barrier) ->
    Mods = [
            %% irc blind rules
            {blinds, []},
            %% deal 2 cards to each player
            {deal_cards, [2, private]}, 
            %% start after BB, 100 raises
            {betting, [100, ?GS_PREFLOP, true]}, 
            %% show 3 shared cards
            {deal_cards, [3, shared]}, 
            %% flop
            {betting, [100, ?GS_FLOP]}, 
            %% show 1 more shared card
            {deal_cards, [1, shared]}, 
            %% turn
            {betting, [100, ?GS_TURN]}, 
            %% show 1 more shared card
            {deal_cards, [1, shared]}, 
            %% river
            {betting, [100, ?GS_RIVER]}, 
            %% showdown
            {showdown, []},
            {end_game, []}
           ],
    if 
        is_pid(Barrier) ->
            %% all games run together
            [{game_start, []}|Mods]
                 ++ [ {delay, [5000]} ];
        true ->
            %% start delay
            [{wait_players, []}| Mods]
                ++ [{restart, []}]
    end.