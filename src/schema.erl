-module(schema).

%%%
%%% Database schema
%%%

-export([install/1, install/0, remove/1]).

-include("schema.hrl").
-include("common.hrl").
-include("pp.hrl").

install() ->
    install([node()]).

remove(Nodes) ->
	mnesia:stop(),
    mnesia:delete_schema(Nodes).

install(Nodes) when is_list(Nodes) ->
    remove(Nodes),
    catch(mnesia:create_schema(Nodes)),
    mnesia:start(),
    install_counter(Nodes),
    install_player_info(Nodes),
    install_player(Nodes),
    install_balance(Nodes),
    install_inplay(Nodes),
    install_game_xref(Nodes),
    install_cluster_config(Nodes),
    install_game_config(Nodes),
    install_tourney_config(Nodes),
    reset_counters(),
    ok.

install_player_info(Nodes) ->
    %% static player info
    {atomic, ok} =
        mnesia:create_table(tab_player_info, 
                            [
                             {disc_copies, Nodes}, 
                             {index, [usr]},  % index on usr field
                             {type, set}, 
                             {attributes, record_info(fields, tab_player_info)}
                            ]).

install_player(Nodes) ->
    %% player 
    {atomic, ok} =
        mnesia:create_table(tab_player, 
                            [
                             {ram_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_player)}
                            ]).

install_balance(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(tab_balance, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_balance)}
                            ]).
install_inplay(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(tab_inplay, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_inplay)}
                            ]).

install_game_xref(Nodes) ->
    %% online game
    {atomic, ok} =
        mnesia:create_table(tab_game_xref, 
                            [
                             {ram_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_game_xref)}
                            ]).

install_cluster_config(Nodes) ->
    %% cluster configuration
    {atomic, ok} =
        mnesia:create_table(tab_cluster_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_cluster_config)}
                            ]),
    Conf = #tab_cluster_config {
      id = 0,
      mnesia_masters = Nodes,
      test_game_pass = <<"@!%#%2E35D$%#$^">>
     },
    F = fun() -> mnesia:write(Conf) end,
    {atomic, ok} = mnesia:transaction(F).

install_game_config(Nodes) ->
    {atomic, ok} = 
        mnesia:create_table(tab_game_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_game_config)}
                            ]).

install_tourney_config(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(tab_tourney_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_tourney_config)}
                            ]).

install_counter(Nodes) ->
    %% counter
    {atomic, ok} = 
        mnesia:create_table(tab_counter, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_counter)}
                            ]).
reset_counters()->
    counter:reset(game),
    counter:reset(player),
    counter:reset(inplay_xref),
    counter:reset(round),
    ok.

