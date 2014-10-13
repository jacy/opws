-module(test).

-include("common.hrl").
-include_lib("eunit/include/eunit.hrl").

generator_test_() ->
	{
	 	setup,
		fun start/0,
		fun stop/1,
		{	inparallel, % inparallel | inorder
			[
				{timeout, 
				 30, 
				 [
				 	{module, mdb},  % {inorder, [{module, mdb}]},
				 	{module, hand},
				 	{module, pot},
				 	{module, barrier}
				 ]
				
				}
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
	schema:remove([node()]).