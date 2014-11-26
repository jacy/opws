-module(util).

-export([is_process_alive/1,
         init_db_slave/1,
         get_random_pid/1,start_syslog/0,
		 nowstring/0, procs/0, wait_for_group/1
        ]).

is_process_alive(Pid) 
  when is_pid(Pid) ->
	%% node(P) Returns the node where Arg is located; is_process_alive(Pid) Pid must refer to a process at the local node.
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]). % use rpc call so as to ignore knowing the group name like game or player or others.

init_db_slave(MasterNode) ->
    mdb:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab, node(), disc_copies) || Tab <- Tabs].

nowstring() ->
	{YY,MM,DD} = date(),
	{Hour,Min,Sec} = time(),
	io_lib:format("~4..0w-~2..0w-~2..0w-~2..0w:~2..0w:~2..0w", [YY, MM, DD, Hour, Min, Sec]).

%%% Grab a random member of the process group

get_random_pid(Name) ->
    L = case pg2:get_members(Name) of
            {error, _} ->
                timer:sleep(100),
                pg2:get_members(Name);
            Other when is_list(Other) ->
                Other
        end,
    if 
        L == [] ->
            {error, {no_process, Name}};
        true ->
            {_,_,X} = erlang:now(),
            {ok, lists:nth((X rem length(L)) + 1, L)}
    end.

%%% Largest memory hogs first!

procs() ->
    F = fun(Pid) ->
                Name = case process_info(Pid, registered_name) of
                           [] ->
                               Pid;
                           Other ->
                               element(2, Other)
                       end,
                Heap = element(2, process_info(Pid, total_heap_size)),
                Stack = element(2, process_info(Pid, stack_size)),
                {Name, {Heap, Stack}}
        end,
    lists:reverse(lists:keysort(2, lists:map(F, processes()))).

%%% Wait for a process group to become available

wait_for_group(Name) ->
    case pg2:get_members(Name) of
        {error, _} ->
            error_logger:info_msg("Group ~p is not available. Retrying in 1 second.~n", [Name]),
            timer:sleep(1000),
            wait_for_group(Name);
        _ ->
            ok
    end.

start_syslog() ->
	MaxQueue=500,
	ok = application:start(sasl),
        ok = application:set_env(syslog, no_progress, true),
        ok = application:set_env(syslog, msg_queue_limit, MaxQueue), % affects error_logger only
        ok = application:set_env(syslog, async_limit, MaxQueue + 1),
    application:start(syslog).
