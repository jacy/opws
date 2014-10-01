-module(test).

-export([all/0]).
-include("texas.hrl").

%% 	whenever you ask EUnit to test the module m, it will also look for the module m_tests 
all() ->
	mdb_tests:test(),
	hand_tests:test(),
	pot_tests:test().


            
