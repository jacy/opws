-module(test).

-export([all/0]).

-include_lib("eunit/include/eunit.hrl").

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
	schema:install().
 
stop(_) ->
	schema:remove([node()]).

all() ->
	mdb_tests:test(),
	hand_tests:test(),
	pot_tests:test(),
	barrier_tests:test(),
	blind_tests:test(),
	ok.