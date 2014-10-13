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
				 	mdb,   % Or if still get too many db errors can use {inorder, mdb} instead
				 	hand,
				 	pot,
				 	barrier
				 ]
				
				}
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