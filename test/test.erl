-module(test).

-export([all/0]).
-include("texas.hrl").

all() ->
	mdb_test:test(),
    hand_test:test(),
    pot_test:test().


            
