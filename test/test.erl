-module(test).

-export([all/0]).

-include("common.hrl").
-include_lib("eunit/include/eunit.hrl").

start_stop_test_() ->
	{
	 	setup,
		fun start/0,
		fun stop/1,
		{	inparallel, % inparallel | inorder
			[
				{timeout, 30, [fun all/0 ]}
			]
		}
	}.
 
%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
	?FLOG("Start running tests, please wait...~n"),
	schema:install().
 
stop(_) ->
	schema:remove([node()]),
	init:stop().

all() ->
	mdb_tests:test(),
	hand_tests:test(),
	pot_tests:test(),
	barrier_tests:test(),
	blind_tests:test(),
	ok.