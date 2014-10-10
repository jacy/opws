-module(test).

-export([all/0]).

%% 	whenever you ask EUnit to test the module m, it will also look for the module m_tests 
all() ->
	mdb_tests:test(),
	hand_tests:test(),
	pot_tests:test(),
	barrier_tests:test(),
	ok.