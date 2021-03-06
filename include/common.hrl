-define(FLOG(Msg), error_logger:info_msg(Msg)).
-define(FLOG(Format, Data), error_logger:info_msg(Format,Data)).
-define(LOG(L), error_logger:info_report(L)).
-define(ERROR(L), error_logger:error_report(L)).

-define(MAX_RAISES, 3).
-define(MAX_PLAYERS, 500000).

-define(LOBBYS, 'LOBBYS').
-define(GAME_LAUNCHERS, 'GAME_LAUNCHERS').
-define(PLAYER_LAUNCHERS, 'PLAYER_LAUNCHERS').

%%% Global stats server

-define(STATS, {global, stats}).

-record(test_game, {
					irc_id,
					winners,
					usrs,
					trace
				 }).

-define(PLAYER_TIMEOUT, 15000).
-define(PLAYER_OUT_TIMEOUT, 15000).
-define(START_DELAY, 1000).
-define(NICK_QUERY_TIMEOUT, 20000).

%%% Error codes

-define(ERR_UNKNOWN, 0).
-define(ERR_BAD_LOGIN, 1).
-define(ERR_ACCOUNT_DISABLED, 2).
-define(ERR_START_DISABLED, 3).

%%% Tournaments

-define(TT_SIT_GO, 0). % starts when N players register
-define(TT_NORMAL, 1). % starts at a given time
-define(TT_REBUY, 2). 

%%% Game stage

-define(GS_CANCEL, 0).
-define(GS_GAME_START, 1). %Game start
-define(GS_PREFLOP, 2).
-define(GS_FLOP, 4).
-define(GS_TURN, 8).
-define(GS_RIVER, 16).
-define(GS_SHOWDOWN, 32).

-define(GS_BETTING, (?GS_PREFLOP bor ?GS_FLOP bor ?GS_TURN bor ?GS_RIVER)).

%%% Game Code

-define(GC_TEXAS_HOLDEM, texas).

%%% TEXAS Type

-define(GT_TEXAS_HOLDEM, 0).
-define(GT_IRC_TEXAS, 1).% Testing game
-define(GT_SIT_N_GO, 2). 

%%% Limit type

-define(LT_FIXED_LIMIT, 0).
-define(LT_NO_LIMIT, 1).
-define(LT_POT_LIMIT, 2).

-record(limit, {
					type,
					low,
					high,
          min,
          max
				 }).

%%% Query operator

-define(OP_IGNORE, 0).
-define(OP_EQUAL, 1).
-define(OP_LESS, 2).
-define(OP_GREATER, 3).

-record(query_op, {
					op,
					val
				 }).

%%% Player state

-define(PS_EMPTY, 0).
-define(PS_PLAY, 1). % Joining, starting Game, ask for bet
-define(PS_FOLD, 2).
-define(PS_WAIT_BB, 4).
-define(PS_SIT_OUT, 8).
-define(PS_MAKEUP_BB, 16).
-define(PS_ALL_IN, 32).
-define(PS_BET, 64). % No Raise
-define(PS_RESERVED, 128). % reserved seat
-define(PS_AUTOPLAY, 256).
-define(PS_MUCK, 512). % will show cards
-define(PS_OUT, 1024). % can't play anymore

-define(PS_GAMING, 
				(?PS_PLAY bor
				?PS_FOLD bor
				?PS_WAIT_BB bor
				?PS_SIT_OUT bor
				?PS_MAKEUP_BB bor
				?PS_ALL_IN bor
				?PS_BET bor 
				?PS_AUTOPLAY)).

-define(PS_ACTIVE, 
				(?PS_PLAY bor 
				?PS_MAKEUP_BB)).

-define(PS_BB_ACTIVE, 
				(?PS_PLAY bor
				?PS_WAIT_BB bor
				?PS_MAKEUP_BB)).

-define(PS_READY,
				(?PS_STANDING bor
				?PS_BB_ACTIVE bor
				?PS_FOLD)).

-define(PS_SHOWDOWN, 
				(?PS_PLAY bor
				?PS_BET bor
				?PS_ALL_IN)).

-define(PS_STANDING, 
				(?PS_PLAY bor
				?PS_ALL_IN bor
				?PS_BET)).

-define(PS_CAN_LEAVE,
				(?PS_FOLD bor
				?PS_OUT bor
				?PS_WAIT_BB bor
				?PS_SIT_OUT bor
				?PS_MAKEUP_BB)).

%%% Face

-define(CF_ACE, 13).
-define(CF_KING, 12).
-define(CF_QUEEN, 11).
-define(CF_JACK, 10).
-define(CF_TEN, 9).
-define(CF_NINE, 8).
-define(CF_EIGHT, 7).
-define(CF_SEVEN, 6).
-define(CF_SIX, 5).
-define(CF_FIVE, 4).
-define(CF_FOUR, 3).
-define(CF_THREE, 2).
-define(CF_TWO, 1).
-define(CF_NONE, 0).

%%% Suit

-define(CS_SPADES, 4).
-define(CS_HEARTS, 3).
-define(CS_DIAMONDS, 2).
-define(CS_CLUBS, 1).
-define(CS_NONE, 0).

%%% Hand combination

-define(HC_HIGH_CARD, 0).
-define(HC_PAIR, 1).
-define(HC_TWO_PAIR, 2).
-define(HC_THREE_KIND, 3).
-define(HC_STRAIGHT, 4).
-define(HC_FLUSH, 5).
-define(HC_FULL_HOUSE, 6).
-define(HC_FOUR_KIND, 7).
-define(HC_STRAIGHT_FLUSH, 8).

-record(hand, {
					player = none,
					pid = none,
					cards = [], 
					rank = none,
					high1 = none,
					high2 = none,
          			suit = ?CS_NONE,
					score = 0
				 }).

-record(player_hand, {
					rank = ?HC_HIGH_CARD,
					high1 = ?CF_NONE,
					high2 = ?CF_NONE,
          			suit = ?CS_NONE
				 }).

-record(winner, {
					player,
					pid,
					potid,
					amount
				 }).

-define(error1(Expr, Expected, Actual),
				io:format("~s is ~w instead of ~w at ~w:~w~n",
									[??Expr, Actual, Expected, ?MODULE, ?LINE])).

-define(error2(Message),
				io:format("~s at ~w:~w~n",
									[Message, ?MODULE, ?LINE])).

-define(waitexit(Pid, Timeout),
				fun() ->
								receive
										{'CARDGAME EXIT', Pid, Data} ->
												{success, Data};
										Other ->
												{error, Other}
								after Timeout ->
												{error, timeout}
								end
				end()).

-define(tcpsend(Protocol, Socket, Data), pp:send(Protocol, Socket, Data)).
-define(SET_LOG_FILE(), util:start_syslog()).
