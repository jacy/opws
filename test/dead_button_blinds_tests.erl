-module(dead_button_blinds_tests).

-include("texas.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% http://www.homepokertourney.com/button.htm


%% Heads-up play. The small blind is the button and acts first 
%% before the flop and last after the flop. The player 
%% who does not have the button is dealt the first card.
headsup_test() ->
    {Game, Players} = make_game_heads_up(),
    [A, B] = Players,
	?LOG(Players),
    Ctx = #texas {
      b = element(2, A),
      sb = element(2, A), 
      bb = element(2, B),
      call = 10.0
     },
    {'EXCH EXIT', Game1, Ctx1} = wait(),
    ?assertEqual(Game, Game1),
    ?assertEqual(ctx(Ctx), ctx(Ctx1)),
    game_stop(Game),
    cleanup_players(Players),
    ok.


%% 3 players, button is bust
three_players_button_bust_test() ->
    {Game, Players} = make_game_3_bust(),
    [A, B, C] = Players,
    install_trigger(fun bust_trigger/3, Game, element(1, A)),
	%% the small blind becomes the big blind, and the big blind becomes the small blind and button
    Ctx = #texas {
      b = element(2, C),
      sb = element(2, C),
      bb = element(2, B),
      call = 10.0
     },
    {'EXCH EXIT', Game1, Ctx1} = wait(),
    ?assertEqual(Game, Game1),
    ?assertEqual(ctx(Ctx), ctx(Ctx1)),
    game_stop(Game),
    cleanup_players(Players),
    ok.

%%% 3 players, small blind is bust

three_players_sb_bust_test() ->
    {Game, Players} = make_game_3_bust(),
    [A, B, C] = Players,
    install_trigger(fun bust_trigger/3, Game, element(1, B)),
    Ctx = #texas {
      b = element(2, C),
      sb = element(2, C),
      bb = element(2, A),
      call = 10.0
     },
    {'EXCH EXIT', Game1, Ctx1} = wait(),
    ?assertEqual(Game, Game1),
    ?assertEqual(ctx(Ctx), ctx(Ctx1)),
    game_stop(Game),
    cleanup_players(Players),
    ok.

%%% 3 players, big blind is bust

three_players_bb_bust_test() ->
    {Game, Players} = make_game_3_bust(),
    [A, B, C] = Players,
    install_trigger(fun bust_trigger/3, Game, element(1, C)),
    Ctx = #texas {
      b = element(2, B),
      sb = element(2, B),
      bb = element(2, A),
      call = 10.0
     },
    {'EXCH EXIT', Game1, Ctx1} = wait(),
    ?assertEqual(Game, Game1),
    ?assertEqual(ctx(Ctx), ctx(Ctx1)),
    game_stop(Game),
    cleanup_players(Players),
    ok.

%%% 5 players, small blind is bust

five_players_sb_bust_test() ->
    {Game, Players} = make_game_5_bust(),
    [_, B, C, D, _E] = Players,
    install_trigger(fun bust_trigger/3, Game, element(1, B)),
    Ctx = #texas {
      b = element(2, B),
      sb = element(2, C),
      bb = element(2, D),
      call = 10.0
     },
    {'EXCH EXIT', Game1, Ctx1} = wait(),
    ?assertEqual(Game, Game1),
    ?assertEqual(ctx(Ctx), ctx(Ctx1)),
    game_stop(Game),
    cleanup_players(Players),
    ok.

five_players_bust_test() ->
    {Game, Players} = make_game_5_bust(2, 3, 4),
    [_, B, C, D, E] = Players,
    install_trigger(fun bust_trigger/3, Game, element(1, B)),
    Ctx = #texas {
      b = element(2, C),
      sb = element(2, D),
      bb = element(2, E),
      call = 10.0
     },
    {'EXCH EXIT', Game1, Ctx1} = wait(),
    ?assertEqual(Game, Game1),
    ?assertEqual(ctx(Ctx), ctx(Ctx1)),
    game_stop(Game),
    cleanup_players(Players),
    ok.

%%% 5 players, big blind is bust

five_players_bb_bust_test() ->
    {Game, Players} = make_game_5_bust(),
    [_, B, C, D, _E] = Players,
    install_trigger(fun bust_trigger/3, Game, element(1, C)),
    Ctx = #texas {
      b = element(2, B),
      sb = element(2, C),
      bb = element(2, D),
      call = 10.0
     },
    {'EXCH EXIT', Game1, Ctx1} = wait(),
    ?assertEqual(Game, Game1),
    ?assertEqual(ctx(Ctx), ctx(Ctx1)),
    game_stop(Game),
    cleanup_players(Players),
    ok.

five_players_bust1_test() ->
    {Game, Players} = make_game_5_bust(2, 3, 4),
    [_, _B, C, D, E] = Players,
    install_trigger(fun bust_trigger/3, Game, element(1, C)),
    Ctx = #texas {
      b = element(2, C),
      sb = element(2, D),
      bb = element(2, E),
      call = 10.0
     },
    {'EXCH EXIT', Game1, Ctx1} = wait(),
    ?assertEqual(Game, Game1),
    ?assertEqual(ctx(Ctx), ctx(Ctx1)),
    game_stop(Game),
    cleanup_players(Players),
    ok.

%%% 5 players, both blinds are bust

five_players_blinds_bust_test() ->
    {Game, Players} = make_game_5_bust(),
    [_, B, C, D, _E] = Players,
    install_trigger(fun bust_trigger/3, Game, element(1, B)),
    install_trigger(fun bust_trigger/3, Game, element(1, C)),
    Ctx = #texas {
      b = element(2, B),
      sb = element(2, C),
      bb = element(2, D),
      call = 10.0
     },
    {'EXCH EXIT', Game1, Ctx1} = wait(),
    ?assertEqual(Game, Game1),
    ?assertEqual(ctx(Ctx), ctx(Ctx1)),
    game_stop(Game),
    cleanup_players(Players),
    ok.

five_players_blinds_bust1_test() ->
    {Game, Players} = make_game_5_bust(2, 3, 4),
    [_, B, C, D, E] = Players,
    install_trigger(fun bust_trigger/3, Game, element(1, B)),
    install_trigger(fun bust_trigger/3, Game, element(1, C)),
    Ctx = #texas {
      b = element(2, C),
      sb = element(2, D),
      bb = element(2, E),
      call = 10.0
     },
    {'EXCH EXIT', Game1, Ctx1} = wait(),
    ?assertEqual(Game, Game1),
    ?assertEqual(ctx(Ctx), ctx(Ctx1)),
    game_stop(Game),
    cleanup_players(Players),
    ok.


%%%
%%% Utility
%%%

make_game_heads_up() ->
    Players = make_players(2),
    Ctx = #texas{
      b = none,
      sb = none,
      bb = none
     },
    Game = make_test_game(Players, Ctx, modules()),
    {Game, Players}.

make_game_3_bust() ->
    Players = make_players(3),
    Ctx = #texas {
      b = element(2, lists:nth(1, Players)),
      sb = element(2, lists:nth(2, Players)),
      bb = element(2, lists:nth(3, Players))
     },
    Game = make_test_game(Players, Ctx, modules()),
    {Game, Players}.

make_game_5_bust() ->
    make_game_5_bust(1, 2, 3).

make_game_5_bust(Button_N, SB_N, BB_N) ->
    {A, AP} = make_player(nick()),
    {B, BP} = make_player(nick()),
    {C, CP} = make_player(nick()),
    {D, DP} = make_player(nick()),
    {E, EP} = make_player(nick()),
    AF = fun() -> stop_player(A, AP) end,
    BF = fun() -> stop_player(B, BP) end,
    CF = fun() -> stop_player(C, CP) end,
    DF = fun() -> stop_player(D, DP) end,
    EF = fun() -> stop_player(E, EP) end,
    Players = [{A, 2, AF}, {B, 4, BF}, {C, 6, CF}, {D, 8, DF}, {E, 9, EF}],
    Ctx = #texas {
      sb = element(2, lists:nth(SB_N, Players)),
      bb = element(2, lists:nth(BB_N, Players)),
      b = element(2, lists:nth(Button_N, Players))
     },
    Game = make_test_game(10, Players, Ctx, modules()),
    {Game, Players}.

bust_trigger(Game, Event, RegName) ->
    case Event of
        {in, {'$gen_cast', #notify_start_game{}}} ->
            Pid = global:whereis_name(RegName),
            gen_server:cast(Game, {'SET STATE', Pid, ?PS_FOLD}),
            done;
        _ ->
            Game
    end.

modules() -> 
    [{wait_players, [1000]}, 
     {delay, [1000]}, %% delay the blinds, so palyers still can send request before blinds, easier to mock player bust.
     {dead_button_blinds, []}].

make_player(Nick) 
  when is_binary(Nick) ->
    {ok, ID} = player:create(Nick, Nick, Nick, <<"">>, 1000.0),
    {ok, Pid} = player:start(ID,Nick,<<"">>),
    {Pid, ID}.

make_players(0, Acc) ->
    Acc;

make_players(N, Acc) ->
    {Pid, ID} = make_player(nick()),
    F = fun() -> stop_player(Pid, ID) end,
    make_players(N - 1, [{Pid, N, F}|Acc]).

make_players(N) when is_number(N) ->
    make_players(N, []).

make_test_game(Players, Context, Modules) ->
    make_test_game(length(Players), Players, Context, Modules).

make_test_game(SeatCount, Players, Context, Modules) ->
    Cmd = #start_game{
	  id=g:uuid(),
      table_name = <<"test game">>,
      type = ?GC_TEXAS_HOLDEM,
	  game_code=?GC_TEXAS_HOLDEM,
	  cbk=?GC_TEXAS_HOLDEM,
      limit = #limit{ type = ?LT_FIXED_LIMIT, low = 10.0, high = 20.0, min = 0, max= 20000000},
      seat_count = SeatCount,
      required = length(Players),
      start_delay = 1000,
      player_timeout = 1000
     },
    {ok, Game} =  exch:start(Cmd, Modules, Context,self()),
    join_game(Game, Players),
    Game.

join_game(_Game, []) ->
    ok;

join_game(Game, [{Player, SeatNum, _}|Rest]) ->
    gen_server:cast(Game, #join{ 
                            game = Game,
                            player = Player,
                            pid = gen_server:call(Player, 'ID'),
                            seat = SeatNum,
                            amount = 1000.00,
                            state = ?PS_PLAY
                           }),
    join_game(Game, Rest).

nick() ->
    nick("").

nick(Prefix) ->
    list_to_binary(pid_to_list(self()) ++ Prefix ++
                   integer_to_list(random:uniform(10000000))).
stop_player(Player, ID) ->
    gen_server:cast(Player, #logout{}),
    ok = mdb:delete(tab_player_info, ID),
    stop_proc(Player, fun player:stop/1).

stop_proc(Pid, F) ->
    Ref = erlang:monitor(process, Pid),
    F(Pid),
    receive 
        {'DOWN', Ref, _, _, _} ->
            ok
    after 1000 ->
            {error, timeout}
    end.

wait() ->
    wait(5000).

wait(Timeout)
  when is_integer(Timeout) ->
    wait(Timeout, ['CANCEL', chat, ping, pong, notify_cancel_game]).

wait(Timeout, Skip) ->
    case receive
             {packet, M1} ->
                 M1;
             {tcp, _, M2} ->
                 pp:read(M2);
             M3 ->
                 M3
         after Timeout ->
                 {error, timeout}
         end of
        {error, timeout} = X ->
            X;
        M ->
            DoSkip = lists:member(element(1, M), Skip),
            if 
                DoSkip ->
                    wait(Timeout, Skip);
                true ->
                    M
            end
    end.


game_stop(Game) ->
    gen_server:cast(Game, stop).


ctx(Ctx) ->
    {Ctx#texas.b, % button position
     Ctx#texas.sb, % small blind position
     Ctx#texas.bb, % big blind position
     Ctx#texas.call}. % call amount

install_trigger(Fun, State, Pids) 
  when is_list(Pids) ->
    lists:foreach(fun({Pid, _, _}) ->
                          sys:install(Pid, {Fun, State})
                  end, Pids);

install_trigger(Fun, State, Pid) 
  when is_pid(Pid) ->
    sys:install(Pid, {Fun, State}).

cleanup_players([]) ->
    ok;

cleanup_players([{_, _, F}|T]) ->
    ?assertEqual(ok, F()),
    cleanup_players(T).