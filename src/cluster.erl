-module(cluster).

-export([mnesia_master/0, mnesia_slave/1, setup_lobby/1, setup_games/1]).
-export([add_node/1, sync_all_data/1, start_lobby/0, start_games/0, kill_games/0, kill_game/1, disconnect_mnesia/1]).

-include("common.hrl").
-include("schema.hrl").
-include("pp.hrl").

-define(SET_LOG_FILE(), error_logger:logfile({open, "/tmp/elog/" ++ atom_to_list(node()) ++ ".log"})).

mnesia_master() ->
	?SET_LOG_FILE(),
	schema:install(),
	
	g:setup(1, ?GC_TEXAS_HOLDEM, <<"5paw6JGh5Lqs5aib5qiC5aC0">>, ?GT_TEXAS_HOLDEM, 9,
          #limit{ type = ?LT_NO_LIMIT, low = 5, high = 10, min = 100, max = 2000 },
          ?START_DELAY, ?PLAYER_TIMEOUT),

	player:create(<<"jacy">>,<<"jacy">>,<<"JacyHong">>,<<"location_SG">>,10000),
	player:create(<<"lena">>,<<"lena">>,<<"Lena">>,<<"location_SG">>,10000),
	player:create(<<"hanhan">>,<<"hanhan">>,<<"HanHan">>,<<"location_SG">>,10000),

	player:update_photo(1, <<"def_face_1">>),
	player:update_photo(2, <<"def_face_2">>),
	player:update_photo(3, <<"def_face_3">>).

mnesia_slave([MasterNode]) when is_atom(MasterNode)->
	?SET_LOG_FILE(),
	sync_all_data(MasterNode).

setup_lobby([MasterNode]) when is_atom(MasterNode) ->
	?SET_LOG_FILE(),
	add_node(MasterNode),
	start_lobby().

setup_games([MasterNode]) when is_atom(MasterNode)->
	?SET_LOG_FILE(),
	add_node(MasterNode),
	start_games().



%% ===========================================================================================================================================

start_lobby() ->
	lobby:start(['8002','192.168.1.10']).


%% The function call mnesia:del_table_copy(schema, mynode@host) deletes the node 'mynode@host' from the Mnesia system. 
%% The call fails if mnesia is running on 'mynode@host'. The other mnesia nodes will never try to connect to that node again. Note, 
%% if there is a disc resident schema on the node 'mynode@host', the entire mnesia directory should be deleted. 
%% This can be done with mnesia:delete_schema/1. If mnesia is started again on the the node 'mynode@host' and the directory has not been cleared,
%% mnesia's behaviour is undefined.
sync_all_data(MasterNode) when is_atom(MasterNode)->
	schema:remove([node()]),
	mnesia:start(),
	mnesia:change_config(extra_db_nodes, [MasterNode]),
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab, node(), disc_copies) || Tab <- Tabs].


%% All the tables will be in master node, so it can use sticky lock.
%% Game server talk with player by global pid not need to use mnesia tables info.
%% So can skip copy datas from master node. Can set up a menisa slave node to copy datas for fault tolerance.
add_node(MasterNode) when is_atom(MasterNode)->
	schema:remove([node()]),
	mnesia:start(),
	Result = mnesia:change_config(extra_db_nodes, [MasterNode]),
	?FLOG("Node:~w connected to MasterNode:~w,Result:~p",[node(),MasterNode,Result]),
	mnesia:change_table_copy_type(schema, node(), disc_copies).
	
disconnect_mnesia(Node) when is_atom(Node)->
	mnesia:del_table_copy(schema, Node).

start_games() ->
	 case mdb:wait_for_tables([tab_game_config, tab_game_xref], 10000) of
        ok ->
    		{atomic, Games} = mdb:find(tab_game_config),
    		start_games(Games);
		  Other ->
            ?ERROR([{wait_tables_timeout, {msg, Other}}]),
            Other
    end.

start_games([]) ->
    ok;

start_games([Game|Rest]) ->
    start_game(Game),
    start_games(Rest).

start_game(Game)->
    make(#start_game{ 
		     id= Game#tab_game_config.id,
			 game_code=Game#tab_game_config.code,
			 table_name=Game#tab_game_config.name,
             type = Game#tab_game_config.type, 
             limit = Game#tab_game_config.limit, 
             start_delay = Game#tab_game_config.start_delay,
             player_timeout = Game#tab_game_config.player_timeout,
             seat_count = Game#tab_game_config.seat_count
            }).

make(R= #start_game{game_code = ?GC_TEXAS_HOLDEM})->
    exch:start([R#start_game{cbk = texas}]);

make(_)->
    game_not_supported.


kill_games() ->
    {atomic, Games} = mdb:find(tab_game_xref),
    kill_games(Games).

kill_games([]) ->
    ok;

kill_games([H|T]) ->
    kill_game(H#tab_game_xref.process),
    kill_games(T).

kill_game(Pid) ->
    gen_server:cast(Pid, stop).

