-module(mdb).

-export([dirty_read/2, dirty_index_read/3, update_balance/2, read/2, find/8]).

-include("common.hrl").
-include("pp.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%  1. Accessing mnesia tables from a QLC list comprehension must always be done within a transaction.

%%  2. Transaction: 
%% 	Atomicity -> Ensures that a transaction take effect on all nodes, or none at all.
%% 	Consistency -> Mnesia ensures that inconsistencies will not occur if Erlang, Mnesia or the computer crashes while a write operation is in progress.
%% 	Isolation -> Makes it possible to concurrently execute two or more processes which manipulate the same record
%% 	Durability -> All changes made to the database are durable so those are written safely to disc will not be disappear.

%% Following functions must be embedded in a transaction. If no enclosing transaction they will all fail.
%% 	mnesia:read, mnesia:wread, mnesia:write, mnesia:delete, mnesia:delete_object
%% 	it is slightly more efficient to set the write lock immediately. In cases where we issue a mnesia:read/1, followed by a mnesia:write/1, 
%% 	the first read lock must be upgraded to a write lock when the write operation is executed.

%% 3. Sticky Locks
%% 	A sticky lock is a lock which stays in place at a node after the transaction which first acquired the lock has terminated
%% 	The s_write/1 function sets a sticky lock instead of a normal lock. If the table is not replicated, sticky locks have no special effect. 
%% 	If the table is replicated, and we set a sticky lock on node N1, this lock will then stick to node N1. 
%% 	The next time we try to set a sticky lock on the same record at node N1, Mnesia will see that the lock is already set 
%% 	and will not do a network operation in order to acquire the lock.
%% 	If a record is stuck at node N1 and we try to set a sticky lock for the record on node N2, the record must be unstuck. 
%% 	This operation is expensive and will reduce performance. The unsticking is done automatically if we issue s_write/1 requests at N2.

%% 4. Dirty Operations
%% 	The major advantage of dirty operations is that they execute much faster than equivalent operations that are processed as functional objects within a transaction.

dirty_read(T, K) ->
	mnesia:dirty_read(T, K).

dirty_index_read(T, K, F) ->
	mnesia:dirty_index_read(T, K, F).

update_balance(K, Raise) ->
    V1 = trunc(Raise * 10000),
	F = fun() ->
		[B] = mnesia:wread({tab_balance, K}),
        Balance = B#tab_balance.amount + V1,
        R = B#tab_balance{amount = Balance},
        mnesia:write(R)
	end,
    {atomic, V} = mnesia:transaction(F),
	V.


read(T, K) ->
	F = fun() ->
		mnesia:read({T, K})
	end,
    {atomic, V} = mnesia:transaction(F),
	 V.

query_op(Arg, Op, Value) 
  when is_number(Arg),
       is_number(Value) ->
    case Op of
        ?OP_IGNORE ->
            true;
        ?OP_EQUAL ->
            Arg == Value;
        ?OP_LESS ->
            Arg < Value;
        ?OP_GREATER ->
            Arg > Value;
        _ ->
            false
    end.

find(GameType, LimitType,
     ExpOp, Expected, 
     JoinOp, Joined,
     WaitOp, Waiting) ->
    F = fun() -> find(GameType, LimitType) end,
    {atomic, L} = mnesia:transaction(F),
    F1 = fun(R = #game_info{}) ->
                 query_op(R#game_info.required, ExpOp, Expected) 
                     and query_op(R#game_info.joined, JoinOp, Joined) 
                     and query_op(R#game_info.waiting, WaitOp, Waiting)
         end,
    {atomic, lists:filter(F1, L)}.

find(GameType, LimitType) ->
    Q = qlc:q([G || G <- mnesia:table(tab_game_xref),
                    G#tab_game_xref.type == GameType,
                    (G#tab_game_xref.limit)#limit.type == LimitType]),
    L = qlc:e(Q),
    lists:map(fun(R) ->
          Game = R#tab_game_xref.process,
          GID = R#tab_game_xref.gid,
          Joined = gen_server:call(Game, 'JOINED'),
          Waiting = 0, % not implemented
          _ = #game_info{
            game = GID,
            table_name = R#tab_game_xref.table_name,
            type = R#tab_game_xref.type,
            limit = R#tab_game_xref.limit,
            seat_count = R#tab_game_xref.seat_count,
            required = R#tab_game_xref.required,
            joined = Joined,
            waiting = Waiting
           }
   end, L).
	