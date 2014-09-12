-module(cluster).

-export([prepare_data/0, start_lobby/0, sync_data/1, start_games/0, kill_games/0, kill_game/1]).

-include("common.hrl").
-include("schema.hrl").
-include("pp.hrl").

prepare_data() ->
	schema:install(),
	player:create(<<"jacy">>,<<"jacy">>,<<"JacyHong">>,<<"location_SG">>,10000),
	player:create(<<"lena">>,<<"lena">>,<<"Lena">>,<<"location_SG">>,10000),
	player:create(<<"hanhan">>,<<"hanhan">>,<<"HanHan">>,<<"location_SG">>,10000).


start_lobby() ->
	lobby:start(['8002','192.168.1.10']).

%% Only need to sync once at the first deploy, restart will auto sync.
sync_data(MasterNode) ->
	db:start(),
	mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab,node(), disc_copies) || Tab <- Tabs].
	

start_games() ->
    {atomic, Games} = db:find(tab_game_config),
    start_games(Games).

start_games([]) ->
    ok;

start_games([Game|Rest]) ->
    start_game(Game),
    start_games(Rest).

start_game(Game) ->
    g:make(#start_game{ 
		     id= Game#tab_game_config.id,
			 table_name=Game#tab_game_config.name,
             type = Game#tab_game_config.type, 
             limit = Game#tab_game_config.limit, 
             start_delay = Game#tab_game_config.start_delay,
             player_timeout = Game#tab_game_config.player_timeout,
             seat_count = Game#tab_game_config.seat_count
            }).

kill_games() ->
    {atomic, Games} = db:find(tab_game_xref),
    kill_games(Games).

kill_games([]) ->
    ok;

kill_games([H|T]) ->
    kill_game(H#tab_game_xref.process),
    kill_games(T).

kill_game(Pid) ->
    gen_server:cast(Pid, stop).

