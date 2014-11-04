-module(dmb).

%%%
%%% Distributed multi-bot test harness
%%%

-export([run/3, run/4, test/1, test/2, test/3, 
         debug/1, setup/0, cleanup/0]).

-include("ircdb.hrl").
-include("common.hrl").
-include("schema.hrl").

-record(dmb, {
          trace,
          max_games,
          start_time,
          start_delay,
          barrier,
          failed = [],
          started = 0,
          game_count = 0,
          player_count = 0,
          finished = 0
         }).

%%% Run a single game on the current VM

debug(GameID) ->
	?SET_LOG_FILE(),
    process_flag(trap_exit, true),
    bb:run(),
    mb:run(localhost, true),
    gateway:start(node(), 4000, 500000),
    io:format("Debugging ~p~n", [GameID]),
    DB = mbu:opendb(),
    Data = #dmb{
      start_time = now(),
      trace = true,
      max_games = 1,
      start_delay = 1000,
      barrier = undefined,
      started = length(pg2:get_members(?GAME_LAUNCHERS))
     },
    Data1 = test(DB, GameID, 1, 0, Data),
    io:format("dmb: waiting for games to end...~n"),
    wait_for_games(Data1).

%%% Simulate MaxGames concurrent games. Requires a set of 
%%% bot launchers (bb) and game launchers (mb) running 
%%% on other nodes of the same cluster. 

test(MaxGames) ->
    test(MaxGames, ?START_DELAY, false).

%%% Same as above but using a delay of Delay milliseconds
%%% before starting each game. 

test(MaxGames, Delay) 
  when is_number(Delay) ->
    test(MaxGames, Delay, false);

test(MaxGames, Trace) 
  when is_atom(Trace) ->
    test(MaxGames, ?START_DELAY, Trace).

test(MaxGames, Delay, Trace)
  when is_number(MaxGames),
       is_number(Delay),
       is_atom(Trace) ->
    process_flag(trap_exit, true),
    gateway:start(node(), 4000, 500000),
    io:format("Simulating gameplay with ~p games...~n", [MaxGames]),
    DB = mbu:opendb(),
    Key = dets:first(DB),

	{ok, Barrier} = barrier:start(counter, MaxGames), %% starts games at the same time
    Data = #dmb{
      start_time = now(),
      trace = Trace,
      max_games = MaxGames,
      start_delay = Delay,
      barrier = Barrier,
      started = length(pg2:get_members(?GAME_LAUNCHERS))
     },
    io:format("== DB ~p~n", [DB]),
    io:format("== Key ~p~n", [Key]),
    io:format("== Max ~p~n", [MaxGames]),
    io:format("== Data ~p~n", [Data]),
    Data1 = test(DB, Key, MaxGames, 0, Data),
    io:format("dmb: waiting for games to end...~n"),
    wait_for_games(Data1).

go(Barrier, N) ->
    io:format("dmb: ~p games will be launching simultaneously~n", [N]),
    gen_server:cast(Barrier, {'TARGET', N}).

test(DB, '$end_of_table', _, N, Data) ->
    io:format("dmb: End of database reached. No more games to launch!~n"),
    go(Data#dmb.barrier, N),
    mbu:closedb(DB),
    Data;

test(DB, _Key, 0, N, Data) ->
    go(Data#dmb.barrier, N),
    mbu:closedb(DB),
    Data;

test(DB, Key, Max, N, Data) ->
    {ok, Mb} = util:get_random_pid(?GAME_LAUNCHERS),
    [Game] = dets:lookup(DB, Key),
    Delay = Data#dmb.start_delay,
    Trace = Data#dmb.trace,
    gen_server:cast(Mb, {'RUN', Game, Data#dmb.barrier, Delay, Trace}),
    Count = Game#irc_game.player_count,
	stats:add({mb_tcp_count, Mb}, Count+1), % 1 game has 1 observer
    Data1 = Data#dmb{
              game_count = Data#dmb.game_count + 1,
              player_count = Data#dmb.player_count + Count
             },
    if
        (Data1#dmb.game_count rem 50) == 0 ->
            io:format("~w games started, ~w players~n", [Data1#dmb.game_count, Data1#dmb.player_count]);
        true ->
            ok
    end,
    link(Mb),
    Key1 = dets:next(DB, Key),
    test(DB, Key1, Max - 1, N + 1, Data1).

%%% Wait for started games to finish

wait_for_games(Data)
  when is_record(Data, dmb) ->
	io:format("wait_for_games with dmb: ~p~n", [Data]),
    receive
        {'EXIT', _, Reason} ->
            Data1 = Data#dmb{ finished = Data#dmb.finished + 1 },
            Data2 = case Reason of 
                        normal ->
                            Data1;
                        Games when is_list(Games) ->
                            Failed = Data1#dmb.failed,
                            Data1#dmb{ failed = Failed ++ Games }
                    end,
            if
                Data2#dmb.finished == Data2#dmb.started ->
                    ok;
                true ->
                    wait_for_games(Data2)
            end;
        Other ->
            io:format("wait_for_games, receive other message: ~p~n", [Other]),
            wait_for_games(Data)
    end,
    T1 = Data#dmb.start_time,
    T2 = erlang:now(),
    Elapsed = timer:now_diff(T2, T1) / 1000 / 1000,
	stats:dump_stat(),
    io:format("dmb: exited successfully, result:~p~n, ~w seconds elapsed~n", [Data, Elapsed]).

setup() ->
    schema:install(),
    mbu:create_players(),
    timer:sleep(1000).

%%% Delete the results of a previous test run
cleanup() ->
    mdb:start(),
    case mdb:wait_for_tables([tab_game_config], 10000) of 
        ok ->
            io:format("dmb:cleanup: deleting game info...~n"),
            mdb:clear_table(tab_game_xref),
            mdb:clear_table(tab_timeout_history),
            counter:reset(game),
            CC = #tab_cluster_config{ id = 0, enable_dynamic_games = true},
            ok = mdb:write(CC);
        Any ->
            io:format("dmb:cleanup: mnesia error ~w~n", [Any])
    end,
    ok.

%%% Launch a given number of slave VMs (Lobbys, BotServers)
%%% and then run #Games on them. Works well on a multicore server.

run(Games, Lobbys, BotServers) ->
    run(Games, Lobbys, BotServers, 20*1000).

run(Games, Lobbys, BotServers, Interval) 
  when is_integer(Games),
       is_integer(Lobbys),
       is_integer(BotServers) ->
	error_logger:tty(false),
	?SET_LOG_FILE(),
	io:format("Setting up slave servers, please wait...~n"),
    mdb:start(),
    pg2:start(),
    start_bot_slaves(BotServers),
    start_game_slaves(Lobbys),
    io:format("cluster: ~p~n", [nodes()]),
    util:wait_for_group(?PLAYER_LAUNCHERS),
    util:wait_for_group(?GAME_LAUNCHERS),
    util:wait_for_group(?LOBBYS),
    io:format("player launchers  : ~p~n", [pg2:get_members(?PLAYER_LAUNCHERS)]),
    io:format("game launchers : ~p~n", [pg2:get_members(?GAME_LAUNCHERS)]),
    io:format("lobbys   : ~p~n", [pg2:get_members(?LOBBYS)]),
    if 
        Interval =/= none ->
            stats:start(Interval);
       true ->
            skip
    end,
    dmb:test(Games).

start_bot_slaves(0) ->
    ok;

start_bot_slaves(N) ->
    Name = list_to_atom("bot" ++ integer_to_list(N)),
    Args = common_args(),
    Node = start_slave_node(Name, Args),
    timer:sleep(100),
    R = rpc:call(Node, bb, run, []),
    io:format("Start bot:~w, Got result:~w ~n", [Node, R]),
    start_bot_slaves(N - 1).

start_game_slaves(0) ->
    ok;

start_game_slaves(N) ->
    Name = list_to_atom("lobby" ++ integer_to_list(N)),
    Args = common_args() ++ menisa_args(),
    Node = start_slave_node(Name, Args),
	start_mmesia_slave(Node),
    timer:sleep(100),
    R = rpc:call(Node, mb, run, []),
    io:format("Start game launcher:~w, Got result:~w ~n", [Node, R]),
    start_game_slaves(N - 1).

common_args() ->
	Path = code:get_path(),
    "+P 65536 +K true -smp disable -setcookie " ++  atom_to_list(erlang:get_cookie()) ++ " -pz " ++ string:join(Path, " ").

menisa_args() ->
    "  -env ERL_MAX_ETS_TABLES 1000000".
    %" -mnesia dump_log_write_threshold 1000000 -mnesia dc_dump_limit 100  -env ERL_MAX_ETS_TABLES 1000000".

start_mmesia_slave(Node) ->
	timer:sleep(1000),
    rpc:call(Node, mnesia, start, []),
    rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
    timer:sleep(1000),
	Node.

start_slave_node(Name, Args) ->
	case slave:start_link(net_adm:localhost(), Name, Args) of
        {ok, Node} ->
			Node;
		{error,{already_running, Node}} ->
			io:format("Already started slave node: ~p ~n", [Name]),
			Node;
        Reason ->
            io:format("Failed to start slave node: ~p, Reason:~p, Retrying in 1 second.~n", [Name, Reason]),
            timer:sleep(1000),
            start_slave_node(Name, Args)
    end.
