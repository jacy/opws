-module(mdb_test).
-include("../src/common.hrl").
-include("../src/pp.hrl").
-include("../src/schema.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Any function ending with test_ tells EUnit that it should return a list of tests
start_stop_test_() ->
	{
	 	setup,
		fun start/0,
		fun stop/1,
		{	inparallel, % inparallel | inorder
			[
				fun() -> all() end
			]
		}
	}.
 
%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
	Nodes = [node()],
	mnesia:stop(),
	mnesia:delete_schema(Nodes),
    mnesia:start(),
	mnesia:create_schema(Nodes),
	mnesia:create_table(tab_inplay, [{attributes, record_info(fields, tab_inplay)}]),
	mnesia:create_table(tab_balance, [{attributes, record_info(fields, tab_balance)}]).
 
stop(_) ->
	mnesia:stop(),
	mnesia:delete_schema(node()).

all() ->
	update_balance_auto_create_record(),
	update_balance_insufficient_balance(),
	update_balance_success(),
	update_balance_concurrent_from_exist_balance(),
	update_balance_concurrent_from_nonexist_balance(),
	update_inplay_auto_create_record(),
	update_inplay_insufficient_balance(),
	update_inplay_success(),
	update_inplay_concurrent_from_exist_balance(),
	update_inplay_concurrent_from_nonexist_balance(),
	buyin_insufficent_balance_abort_all(),
	buyin_insufficent_inplay_abort_all(),
	buyin_success(),
	buyin_concurrent().
 
%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
	

update_balance_auto_create_record() ->
	K = update_balance_auto_create_record,
	?assertEqual([],mdb:read(tab_balance, K)),

	ok = mdb:update_balance(K, 50),
	?assertEqual(50, get_balance(K)).

update_balance_insufficient_balance() ->
	K = update_balance_insufficient_balance,
	create_balance(K, 50),

	?assertEqual({error, not_enough_money}, mdb:update_balance(K, -50.1)),
	?assertEqual(50, get_balance(K)).

update_balance_success() ->
	K = update_balance_success,
	create_balance(K, 100),

	mdb:update_balance(K, 10),
	?assertEqual(110, get_balance(K)),

	mdb:update_balance(K, -55),
	?assertEqual(55, get_balance(K)).


update_balance_concurrent_from_exist_balance() ->
	K = update_balance_concurrent_from_exist_balance,
	create_balance(K, 10000),
	
	Self = self(),
	F = fun() ->
		mdb:update_balance(K, -10),
		Self ! done
	end,
	[spawn(F) || _ <- lists:seq(1, 999)],
	barrier(999),
	?assertEqual(10, get_balance(K)).

update_balance_concurrent_from_nonexist_balance() ->
	K = update_balance_concurrent_from_nonexist_balance,

	Self = self(),
	F = fun() ->
		mdb:update_balance(K, 10),
		Self ! done
	end,
	Loop = 1000,
	[spawn(F) || _ <- lists:seq(1, Loop)],
	barrier(Loop),
	?assertEqual(10*Loop, get_balance(K)).


update_inplay_auto_create_record() ->
	K = {G = texas, P = update_inplay_auto_create_record},
	?assertEqual([],mdb:read(tab_inplay, K)),

	ok = mdb:update_inplay(G, P, 50),
	?assertEqual(50, get_inplay(K)).

update_inplay_insufficient_balance() ->
	K = {G = texas, P = update_inplay_insufficient_balance},
	create_inplay(K, 50),

	?assertEqual({error, not_enough_money}, mdb:update_inplay(G, P,-50.1)),
	?assertEqual(50, get_inplay(K)).

update_inplay_success() ->
	K = {G = texas, P = update_inplay_success},
	create_inplay(K, 100),

	mdb:update_inplay(G, P, 10),
	?assertEqual(110, get_inplay(K)),

	mdb:update_inplay(G, P, -55),
	?assertEqual(55, get_inplay(K)).


update_inplay_concurrent_from_exist_balance() ->
	K = {G = texas, P = update_inplay_concurrent_from_exist_balance},
	create_inplay(K, 10000),
	
	Self = self(),
	F = fun() ->
		mdb:update_inplay(G, P, -10),
		Self ! done
	end,
	[spawn(F) || _ <- lists:seq(1, 999)],
	barrier(999),
	?assertEqual(10, get_inplay(K)).

update_inplay_concurrent_from_nonexist_balance() ->
	K = {G = texas, P = update_inplay_concurrent_from_nonexist_balance},

	Self = self(),
	F = fun() ->
		mdb:update_inplay(G, P , 10),
		Self ! done
	end,
	Loop = 1000,
	[spawn(F) || _ <- lists:seq(1, Loop)],
	barrier(Loop),
	?assertEqual(10*Loop, get_inplay(K)).

buyin_insufficent_balance_abort_all()->
	K = {GID = 1, PID = erlang:phash2(buyin_insufficent_balance_abort_all, 1 bsl 32)},
	create_balance(PID, 10),
	create_inplay(K, 10),
	?assertEqual({error, not_enough_money}, mdb:buy_in(GID, PID, 11)),
	?assertEqual(10, get_balance(PID)),
	?assertEqual(10, get_inplay(K)).

buyin_insufficent_inplay_abort_all()->
	K = {GID= 1, PID = erlang:phash2(buyin_insufficent_inplay_abort_all, 1 bsl 32)},
	create_balance(PID, 10),
	create_inplay(K, -11),
	?assertEqual({error, not_enough_money}, mdb:buy_in(GID, PID, 10)),
	?assertEqual(10, get_balance(PID)),
	?assertEqual(-11, get_inplay(K)).

buyin_success()->
	K = {GID= 1, PID=erlang:phash2(buyin_success, 1 bsl 32)},
	create_balance(PID, 10),
	ok = mdb:buy_in(GID, PID, 10),
	?assertEqual(0, get_balance(PID)),
	?assertEqual(10, get_inplay(K)).

buyin_concurrent() ->
	K = {G = 1, P = erlang:phash2(buyin_concurrent, 1 bsl 32)},
	create_balance(P, 10000),
	
	Self = self(),
	F = fun() ->
		mdb:buy_in(G, P, 10),
		Self ! done
	end,
	[spawn(F) || _ <- lists:seq(1, 999)],
	barrier(999),
	?assertEqual(10, get_balance(P)),
	?assertEqual(9990, get_inplay(K)).

%%%%%%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%%%%%%
create_balance(K, Amount)->
	ok = mdb:write(R = #tab_balance{pid=K, amount=Amount * 10000}),
	?assertEqual([R],mdb:read(tab_balance, K)),
	[R].

create_inplay(K, Amount)->
	ok = mdb:write(R = #tab_inplay{gidpid=K, amount=Amount * 10000}),
	?assertEqual([R], mdb:read(tab_inplay, K)),
	[R].

get_balance(K) ->
	[#tab_balance{amount=Amount}] = mdb:read(tab_balance, K),
	trunc(Amount/10000).

get_inplay(K) ->
	[#tab_inplay{amount=Amount}] = mdb:read(tab_inplay, K),
	trunc(Amount/10000).

barrier(0) ->
	ok;

barrier(Count) ->
	receive done ->
			barrier(Count -1)
	after 10000 ->
		 throw(timeout)
	end.