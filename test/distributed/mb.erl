-module(mb).
-behaviour(gen_server).

-compile([export_all]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1, run/0, run/1, run/2]).

-include("common.hrl").
-include("ircdb.hrl").
-include("pp.hrl").
-include("schema.hrl").
-define(START_PORT, 3100).
-record(mb, {
          host, 
          port,
          games = gb_trees:empty(),
          start_time,
          trace = false,
          started,
          finished,
          failed
         }).

new(Trace, Host, Port) ->
    #mb{
     start_time = erlang:now(),
     trace = Trace,
     host = Host,
     port = Port,
     started = 0,
     finished = 0,
     failed = []
    }.

start(Trace) ->
    gen_server:start(mb, [Trace], []).

init([Trace]) ->
    process_flag(trap_exit, true),
    [Pid] = pg2:get_local_members(?LOBBYS),
    {Host, Port} = gen_server:call(Pid, 'WHERE'),
    pg2:create(?GAME_LAUNCHERS),
    ok = pg2:join(?GAME_LAUNCHERS, self()),
    {ok, new(Trace, Host, Port)}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(normal, _Data) ->
    ok;

terminate(Reason, _Data) ->
	?ERROR([{"terminate game launchers", Reason}]),
    ok.

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast({'RUN', Game, Barrier, Delay, Trace}, Data) 
  when is_record(Game, irc_game) ->
    T1 = now(),
    Game1 = mbu:fix_usrs(Game),
    mbu:update_players(Game1),
	
	?LOG({start_game,Game}),
	
    Host = Data#mb.host,
    Port = Data#mb.port,
    if 
        Data#mb.trace ->
            ?FLOG("RUN: ~p, ~p:~p, ~p~n", [Game1#irc_game.id, Host, Port, now()]);
        true ->
            ok
    end,
    T2 = now(),
    {ok, GID} = start_game(Game1, Delay, Barrier),
    T3 = now(),
    {ok, Bb} = util:get_random_pid(?PLAYER_LAUNCHERS),
    bb:launch(Bb, self(), GID, Game1, Host, Port, Trace),
    TestGame = #test_game {
      irc_id = Game1#irc_game.id,
      winners = mbu:ircdb_winners(Game1),
      usrs = mbu:ircdb_usrs(Game1),
      trace = Trace
     },
    Games = Data#mb.games,
    Games1 = gb_trees:insert(GID, TestGame, Games),
    Data1 = Data#mb{ 
              games = Games1, 
              started = Data#mb.started + 1 
             },
    T4 = now(),
    stats:sum(games_launched, 1),
    stats:max(max_game_launch_time, timer:now_diff(T4, T1)),
    stats:avg(game_launch_time, timer:now_diff(T4, T1)),
    stats:avg(game_start_time, timer:now_diff(T3, T2)),
    {noreply, Data1};

handle_cast(Event, Data) ->
    ?ERROR([{event, Event}, {data, Data}]),
    {noreply, Data}.

handle_call(Event, From, Data) ->
    ?ERROR([{event, Event}, {from, From}, {data, Data}]),
    {noreply, Data}.

handle_info({'START', _GID}, Data) ->
    stats:sum(games_started, 1),
    stats:add(total_games_started, 1),
    {noreply, Data};

handle_info({'END', GID, Winners}, Data) ->
    stats:sum(games_ended, 1),
    stats:add(total_games_ended, 1),
    %% score it
    Games = Data#mb.games,
    Game = gb_trees:get(GID, Games),
    Winners1 = mbu:fixup_winners(Game, Winners),
    Success = mbu:match_winners(Game#test_game.winners, Winners1),
    if
        Data#mb.trace ->
            ?FLOG("END: ~w, Success: ~w~n", [GID, Success]);
        true ->
            ok
    end,
    Data1 = if
                Success ->
                    Data;
                true ->
                    if 
                        Data#mb.trace ->
                            ?FLOG("~w: Expected winners: ~w~n", 
                                      [GID, Game#test_game.winners]),
                            ?FLOG("~w: Received winners: ~w~n", 
                                      [GID, Winners1]);
                        true ->
                            ok
                    end,
                    Failed = Game#test_game.irc_id,
                    Data#mb { failed = [Failed|Data#mb.failed] }
            end,
    %% clean up
    Games1 = gb_trees:delete(GID, Games),
    Data2 = Data1#mb{
              finished = Data1#mb.finished + 1,
              games = Games1
             },
    if 
        (Data2#mb.finished rem 50) == 0 ->
            ?FLOG("~w games finished~n", [Data2#mb.finished]);
        true ->
            ok
    end,
    if
        Data2#mb.finished == Data2#mb.started ->
            if 
                Data2#mb.failed /= [] ->
                    {stop, Data2#mb.failed, Data2};
                true ->
                    {stop, normal, Data2}
            end;
        true ->
            {noreply, Data2}
    end;

handle_info({'CANCEL', GID}, Data) ->
    Games = Data#mb.games,
    Game = gb_trees:get(GID, Games),
    if
        Data#mb.trace ->
            ?FLOG("CANCEL: ~w~n", [GID]);
        true ->
            ok
    end,
    Games1 = gb_trees:delete(GID, Games),
    Data1 = Data#mb {
              failed = [Game#test_game.irc_id|Data#mb.failed],
              finished = Data#mb.finished + 1,
              games = Games1
             },
    if 
        (Data1#mb.finished rem 50) == 0 ->
            ?FLOG("~w games finished~n", [Data1#mb.finished]);
        true ->
            ok
    end,
    if
        Data1#mb.finished == Data1#mb.started ->
            if 
                Data1#mb.failed /= [] ->
                    {stop, {failed, Data1#mb.failed}, Data1};
                true ->
                    {stop, normal, Data1}
            end;
        true ->
            {noreply, Data1}
    end;

handle_info({'CARDGAME EXIT', _, _}, Data) ->
    {noreply, Data};


handle_info({'DOWN', _Ref, process, _Pid,  Reason}, Data) ->
	case Reason of 
		normal ->
			skip;
		_ ->
   		 ?ERROR([{"Child Exit With Reason:", Reason}])
	end,
    {noreply, Data};

handle_info(Info, Data) ->
    ?ERROR([{message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

rig_deck(Game) 
  when is_record(Game, irc_game) ->
    Deck = deck:new(),
    Players = Game#irc_game.players,
    Count = size(Players),
    Cards1 = player_cards(Players, Deck, 1, Count, []),
    Cards2 = player_cards(Players, Deck, 2, Count, []),
    Cards3 = lists:map(fun hand:make_card/1, Game#irc_game.board),
	if 
		length(Cards3) < 3  -> 
		   ?LOG([{invalid_test_data, irc_game, Game},{board, Game#irc_game.board},{players,Players}]);
	    true ->
			ok
	end,		  	
    Cards1 ++ Cards2 ++ Cards3.

player_cards(_Players, _Deck, _N, 0, Acc) ->
    Acc;

player_cards(Players, Deck, N, Count, Acc) ->
    Player = element(Count, Players),
    {Deck1, Card} = 
        if
            length(Player#irc_player.cards) < N ->
                deck:draw(Deck);
            true ->
                C = lists:nth(N, Player#irc_player.cards),
                {Deck, hand:make_card(C)}
        end,
    player_cards(Players, Deck1, N, Count - 1, [Card|Acc]).

run(Host) ->
    run(Host, true).

run(Host, TestMode) 
  when is_atom(Host) ->
    run(atom_to_list(Host), TestMode);

run(Host, TestMode) ->
    mdb:start(),
    pg2:start(),
    Port = next_port(Host),
    error_logger:info_msg("~p: game server on port ~p~n", [node(), Port]),
    lobby:start(Host, Port, TestMode, socket),
    {ok, _} = start(TestMode),
    ok.

run() ->
	?SET_LOG_FILE(),
    run(localhost, false).

next_port(Host) ->
    pg2:create(?LOBBYS),
    pg2:get_members(?LOBBYS),
    timer:sleep(100),
    case pg2:get_members(?LOBBYS) of
        {error, X} ->
            ?FLOG("next_port: ~p~n", [X]),
            ?START_PORT;
        L when is_list(L) ->
            next_port(Host, L, ?START_PORT)
    end.

next_port(_, [], Max) ->
    Max + 1;

next_port(Host, [Pid|Rest], Max) ->
    {H, P} = gen_server:call(Pid, 'WHERE'),
    Max1 = if
               H == Host ->
                   if 
                       P > Max ->
                           P;
                       true ->
                           Max
                   end;
               true ->
                   Max
           end,
    next_port(Host, Rest, Max1).

start_game(G, Delay, Barrier)
  when is_record(G, irc_game) ->
  	?LOG([{delay, Delay}]),
    Cmd = #start_game{
      id=G#irc_game.id,
	  game_code=?GC_TEXAS_HOLDEM,
	  table_name = <<"test games">>,
      type = ?GT_IRC_TEXAS,
      limit = #limit{ type = ?LT_FIXED_LIMIT, high = 20, low = 10, min = 0, max = 10000000 },
      seat_count = G#irc_game.player_count,
      required = G#irc_game.player_count,
      start_delay = Delay,
      rigged_deck = rig_deck(G),
	  cbk=?GC_TEXAS_HOLDEM,
	  player_timeout=5000,
      barrier = Barrier
     },
    {ok, Game} = exch:start([Cmd]),
	erlang:monitor(process,Game),
    {ok, gen_server:call(Game, 'ID')}.
