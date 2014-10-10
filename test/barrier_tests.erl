-module(barrier_tests).

-include_lib("eunit/include/eunit.hrl").

counter_test() ->
    {ok, B} = barrier:start(counter, 2),
    ?assertEqual(true, util:is_process_alive(B)),
    barrier:bump(B),
    ?assertEqual(true, util:is_process_alive(B)),
    barrier:bump(B),
    timer:sleep(100),
    ?assertEqual(false, util:is_process_alive(B)).

timer_test() ->    
    {ok, B} = barrier:start(time, {seconds, 2}),
    ?assertEqual(true, util:is_process_alive(B)),
    timer:sleep(2000),
    ?assertEqual(false, util:is_process_alive(B)).

wait_test() ->
    {ok, B} = barrier:start(time, {seconds, 2}),
    ?assertEqual(ok, barrier:wait(B)).