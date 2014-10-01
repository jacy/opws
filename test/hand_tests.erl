-module(hand_tests).

-include_lib("eunit/include/eunit.hrl").
-include("texas.hrl").

members(Members) ->
            members(Members, gb_trees:empty()).

members([], Members) ->
            Members;

members([M | T ], Members) ->
            members(T, gb_trees:enter(M, 0, Members)).

make_rep_test() ->
    %%  AKQJT98765432A
    [2#00000010000000,
     2#00101000011000,
     2#00010001000000,
     2#00000000000000]
  = hand:make_rep(hand:make_cards("4D JH 5D 8C QD TD 7H")).

rank_test_hand(Cards) ->
    rank_test_hand(0, Cards).

rank_test_hand(Player,Cards) ->
    Hand = hand:new(Player, Player, hand:make_cards(Cards)),
    hand:rank(Hand).

rank_test_player_hand(Cards) ->
  hand:player_hand(rank_test_hand(Cards)).

rank_high_card1_test() ->
    H = rank_test_hand("4D JH 5D 8C QD TD 7H"),
    ?assertEqual(?HC_HIGH_CARD, H#hand.rank),
    ?assertEqual(2#00111011000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_high_card2_test() ->
    H = rank_test_hand("8C AD 5H 3S KD 9D 4D"),
    ?assertEqual(?HC_HIGH_CARD, H#hand.rank),
    ?assertEqual(2#11000110010000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_high_card3_test() ->
    H = rank_test_hand("4C JH 5C 8D QC 2C 3D"),
    ?assertEqual(?HC_HIGH_CARD, H#hand.rank),
    ?assertEqual(2#00110010011000, H#hand.high1), 
    ?assertEqual(0, H#hand.score).
    
rank_pair1_test() ->
    H = rank_test_hand("KD 3S 5H 3D 6C QH 9S"),
    ?assertEqual(?HC_PAIR, H#hand.rank),
    ?assertEqual(2#00000000000100, H#hand.high1),
    ?assertEqual(2#01100100000000, H#hand.score).

rank_pair2_test() ->
    H = rank_test_hand("AC 2D 5D AS 4H 9D KD"),
    ?assertEqual(?HC_PAIR, H#hand.rank),
    ?assertEqual(2#10000000000000, H#hand.high1),
    ?assertEqual(2#01000100010000, H#hand.score).

rank_pair3_test() ->
    H = rank_test_hand("9S JH 5D TS 3C KC 3H"),
    ?assertEqual(?HC_PAIR, H#hand.rank),
    ?assertEqual(2#00000000000100, H#hand.high1),
    ?assertEqual(2#01011000000000, H#hand.score).

rank_two_pair1_test() ->
    H = rank_test_hand("QC KD JD QD JC 5C KC"),
    ?assertEqual(?HC_TWO_PAIR, H#hand.rank),
    ?assertEqual(2#01000000000000, H#hand.high1),
    ?assertEqual(2#00100000000000, H#hand.high2),
    ?assertEqual(2#00010000000000, H#hand.score).

rank_two_pair2_test() ->
    H = rank_test_hand("7H 3H 6C TD 7C JH 6H"),
    ?assertEqual(?HC_TWO_PAIR, H#hand.rank),
    ?assertEqual(2#00000001000000, H#hand.high1),
    ?assertEqual(2#00000000100000, H#hand.high2),
    ?assertEqual(2#00010000000000, H#hand.score).

rank_two_pair3_test() ->
    H = rank_test_hand("4D 3S 5H JD JC QH 5S"),
    ?assertEqual(?HC_TWO_PAIR, H#hand.rank),
    ?assertEqual(2#00010000000000, H#hand.high1),
    ?assertEqual(2#00000000010000, H#hand.high2),
    ?assertEqual(2#00100000000000, H#hand.score).

rank_two_pair4_test() ->
    H = rank_test_hand("AC 2D 5D AS 5H 9D 4D"),
    ?assertEqual(?HC_TWO_PAIR, H#hand.rank),
    ?assertEqual(2#10000000000000, H#hand.high1),
    ?assertEqual(2#00000000010000, H#hand.high2),
    ?assertEqual(2#00000100000000, H#hand.score).

rank_two_pair5_test() ->
    H = rank_test_hand("9S JH 5D JS 5C KC 3D"),
    ?assertEqual(?HC_TWO_PAIR, H#hand.rank),
    ?assertEqual(2#00010000000000, H#hand.high1),
    ?assertEqual(2#00000000010000, H#hand.high2),
    ?assertEqual(2#01000000000000, H#hand.score).

rank_three_kind1_test() ->
    H = rank_test_hand("KH 9S 5H QD QC QH 3S"),
    ?assertEqual(?HC_THREE_KIND, H#hand.rank),
    ?assertEqual(2#00100000000000, H#hand.high1),
    ?assertEqual(2#01000100000000, H#hand.score).

rank_three_kind2_test() ->
    H = rank_test_hand("AC KC KD KS 7H 9D 4D"),
    ?assertEqual(?HC_THREE_KIND, H#hand.rank),
    ?assertEqual(2#01000000000000, H#hand.high1),
    ?assertEqual(2#10000100000000, H#hand.score).

rank_three_kind3_test() ->
    H = rank_test_hand("KS TS QD QS QH 4C 5D"),
    ?assertEqual(?HC_THREE_KIND, H#hand.rank),
    ?assertEqual(2#00100000000000, H#hand.high1),
    ?assertEqual(2#01001000000000, H#hand.score).

rank_straight1_test() ->
    H = rank_test_hand("KC QS JH TC 9C 4D 3S"),
    ?assertEqual(?HC_STRAIGHT, H#hand.rank),
    ?assertEqual(2#01111100000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight2_test() ->
    H = rank_test_hand("AC KS QH JC TC 9D 4D"),
    ?assertEqual(?HC_STRAIGHT, H#hand.rank),
    ?assertEqual(2#11111000000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight3_test() ->
    H = rank_test_hand("KS QD JS TC 9S 2D 7S"),
    ?assertEqual(?HC_STRAIGHT, H#hand.rank),
    ?assertEqual(2#01111100000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight4_test() ->
    H = rank_test_hand("5C 4D 3H 2C AD 7H 9S"),
    ?assertEqual(?HC_STRAIGHT, H#hand.rank),
    ?assertEqual(2#00000000011111, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight5_test() ->
    H = rank_test_hand("5H 4S JC 8S 7D 6C 3C"),
    ?assertEqual(?HC_STRAIGHT, H#hand.rank),
    ?assertEqual(2#00000011111000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_flush1_test() ->
    H = rank_test_hand("4D JD 5D JC QD 2D 7H"),
    ?assertEqual(?HC_FLUSH, H#hand.rank),
    ?assertEqual(2#00110000011010, H#hand.high1),
    ?assertEqual(2#00110000011010, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_flush2_test() ->
    H = rank_test_hand("8C AD 5D AS KD 9D 4D"),
    ?assertEqual(?HC_FLUSH, H#hand.rank),
    ?assertEqual(2#11000100011000, H#hand.high1),
    ?assertEqual(2#11000100011000, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_flush3_test() ->
    H = rank_test_hand("4C JC 5C 8D QC 3C 7S"),
    ?assertEqual(?HC_FLUSH, H#hand.rank),
    ?assertEqual(2#00110000011100, H#hand.high1),
    ?assertEqual(2#00110000011100, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_flush4_test() ->
    H = rank_test_hand("4C JC 5C QC 3C KC TD"),
    ?assertEqual(?HC_FLUSH, H#hand.rank),
    ?assertEqual(2#01110000011000, H#hand.high1),
    ?assertEqual(2#01110000011100, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_full_house1_test() ->
    H = rank_test_hand("4D JS 5H JD JC QH QS"),
    ?assertEqual(?HC_FULL_HOUSE, H#hand.rank),
    ?assertEqual(2#00010000000000, H#hand.high1),
    ?assertEqual(2#00100000000000, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_full_house2_test() ->
    H = rank_test_hand("AC AD KD AS KH 9D 4D"),
    ?assertEqual(?HC_FULL_HOUSE, H#hand.rank),
    ?assertEqual(2#10000000000000, H#hand.high1),
    ?assertEqual(2#01000000000000, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_full_house3_test() ->
    H = rank_test_hand("3S JH JD JS KH KC 5D"),
    ?assertEqual(?HC_FULL_HOUSE, H#hand.rank),
    ?assertEqual(2#00010000000000, H#hand.high1),
    ?assertEqual(2#01000000000000, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_full_house4_test() ->
    H = rank_test_hand("TD QH TH TC 6C QD QC"),
    ?assertEqual(?HC_FULL_HOUSE, H#hand.rank),
    ?assertEqual(2#00100000000000, H#hand.high1),
    ?assertEqual(2#00001000000000, H#hand.high2),
    ?assertEqual(0, H#hand.score).

rank_four_kind1_test() ->
    H = rank_test_hand("4D AS 5H QD QC QH QS"),
    ?assertEqual(?HC_FOUR_KIND, H#hand.rank),
    ?assertEqual(2#00100000000000, H#hand.high1),
    ?assertEqual(2#10000000000000, H#hand.score).

rank_four_kind2_test() ->
    H = rank_test_hand("AC KC KD KS KH 9D 4D"),
    ?assertEqual(?HC_FOUR_KIND, H#hand.rank),
    ?assertEqual(2#01000000000000, H#hand.high1),
    ?assertEqual(2#10000000000000, H#hand.score).

rank_four_kind3_test() ->
    H = rank_test_hand("KS TS QD QS QH QC 5D"),
    ?assertEqual(?HC_FOUR_KIND, H#hand.rank),
    ?assertEqual(2#00100000000000, H#hand.high1),
    ?assertEqual(2#01000000000000, H#hand.score).

rank_straight_flush1_test() ->
    H = rank_test_hand("KC QC JC TC 9C 4D AS"),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H#hand.rank),
    ?assertEqual(2#01111100000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight_flush2_test() ->
    H = rank_test_hand("AC KC QC JC TC 9D 4D"),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H#hand.rank),
    ?assertEqual(2#11111000000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight_flush3_test() ->
    H = rank_test_hand("KS QS JS TS 9S AD 7S"),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H#hand.rank),
    ?assertEqual(2#01111100000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

rank_straight_flush4_test() ->
    H = rank_test_hand("AS QS JS TS 9S 8S AD"),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H#hand.rank),
    ?assertEqual(2#00111110000000, H#hand.high1),
    ?assertEqual(0, H#hand.score).

high_card_win_test() ->
    H1 = rank_test_hand('A',"4D JH 5D 8C QD TD 7H"),
    H2 = rank_test_hand('B',"8C AD 5H 3S KD 9D 4D"),
    H3 = rank_test_hand('C',"4C JH 5C 8D QC 2C 3D"),
    ?assertEqual(?HC_HIGH_CARD, H1#hand.rank),
    ?assertEqual(?HC_HIGH_CARD, H2#hand.rank),
    ?assertEqual(?HC_HIGH_CARD, H3#hand.rank),
    ?assertEqual(true, hand:short(H2) > hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H1) > hand:short(H3)),

	Members = members(['A','B','C']),
	PotAmount = 100,
	PotId = 1,
	Winners = hand:winners([H1,H2,H3], [{PotAmount, Members, PotId}], []),
	?assertEqual(1, length(Winners)),
	?assertEqual(#winner{amount=PotAmount,pid='B',player='B',potid=PotId}, lists:nth(1,Winners)).

high_card_win1_test() ->
    H1 = rank_test_hand('A',"AD KH TD 8C 6D 5D 3S"),
    H2 = rank_test_hand('B',"AH KD TS 8D 6C 5C 2S"),
    H3 = rank_test_hand('C',"AH KC TD 8C 5S 2S 3S"),
    H4 = rank_test_hand('D',"KD TS 8D 6C 5S QS 9D"),
    ?assertEqual(?HC_HIGH_CARD, H1#hand.rank),
    ?assertEqual(?HC_HIGH_CARD, H2#hand.rank),
    ?assertEqual(true, hand:short(H2) == hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H3) > hand:short(H4)),
	
	Members = members(['A','B','C','D']),
	PotAmount = 100,
	PotId = 1,
	Winners = hand:winners([H1,H2,H3,H4], [{PotAmount, Members, PotId}], []),
	?assertEqual(2, length(Winners)),
	?assertEqual(#winner{amount=50,pid='A',player='A',potid=PotId}, lists:nth(1,Winners)),
	?assertEqual(#winner{amount=50,pid='B',player='B',potid=PotId}, lists:nth(2,Winners)).

pair_win_on_pair_test() ->
    H1 = rank_test_hand('A',"KD 3S 5H 3D 6C QH 9S"),
    H2 = rank_test_hand('B',"AC 2D 5D AS 4H 9D KD"),
    H3 = rank_test_hand('C',"9S JH 5D TS 3C KC 3H"),
    ?assertEqual(?HC_PAIR, H1#hand.rank),
    ?assertEqual(?HC_PAIR, H2#hand.rank),
    ?assertEqual(?HC_PAIR, H3#hand.rank),
    ?assertEqual(true, hand:short(H2) > hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H1) > hand:short(H3)),
	
	Members = members(['A','B','C']),
	PotAmount = 100,
	PotId = 1,
	Winners = hand:winners([H1,H2,H3], [{PotAmount, Members, PotId}], []),
	?assertEqual(1, length(Winners)),
	?assertEqual(#winner{amount=PotAmount,pid='B',player='B',potid=PotId}, lists:nth(1,Winners)).

pair_win_on_score_test() ->
    H1 = rank_test_hand('A',"KD KS 9H 8D 7C 5H 4S"),
    H2 = rank_test_hand('B',"KD KS 9H 8D TC 5H 4S"),
    H3 = rank_test_hand('C',"KD KS 9H 8D 7C 5H 4S"),
    ?assertEqual(?HC_PAIR, H1#hand.rank),
    ?assertEqual(?HC_PAIR, H2#hand.rank),
    ?assertEqual(?HC_PAIR, H3#hand.rank),
    ?assertEqual(true, hand:short(H2) > hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H1) == hand:short(H3)),
	
	Members = members(['A','B','C']),
	PotAmount = 100,
	PotId = 1,
	Winners = hand:winners([H1,H2,H3], [{PotAmount, Members, PotId}], []),
	?assertEqual(1, length(Winners)),
	?assertEqual(#winner{amount=PotAmount,pid='B',player='B',potid=PotId}, lists:nth(1,Winners)).

pair_win_multiple_winners_test() ->
    H1 = rank_test_hand('A',"KD KS 9H 8D 7C 5H 4S"),
    H2 = rank_test_hand('B',"KD KS 9H 8D 7C 3H 4S"),
    H3 = rank_test_hand('C',"KD KS 9H 8D 7C 2H 4S"),
    ?assertEqual(?HC_PAIR, H1#hand.rank),
    ?assertEqual(?HC_PAIR, H2#hand.rank),
    ?assertEqual(?HC_PAIR, H3#hand.rank),
    ?assertEqual(true, hand:short(H2) == hand:short(H1)),
    ?assertEqual(true, hand:short(H2) == hand:short(H3)),
    ?assertEqual(true, hand:short(H1) == hand:short(H3)),
	
	Members = members(['A','B','C']),
	PotAmount = 100,
	PotId = 1,
	Winners = hand:winners([H1,H2,H3], [{PotAmount, Members, PotId}], []),
	?assertEqual(3, length(Winners)),
	?assertEqual(#winner{amount=33,pid='A',player='A',potid=PotId}, lists:nth(1,Winners)),
	?assertEqual(#winner{amount=33,pid='B',player='B',potid=PotId}, lists:nth(2,Winners)),
	?assertEqual(#winner{amount=33,pid='C',player='C',potid=PotId}, lists:nth(3,Winners)).


two_pair_win_test() ->
    H1 = rank_test_hand('A',"4D 3S 5H JD JC QH 5S"),
    H2 = rank_test_hand('B',"AC 2D 5D AS 5H 9D 4D"),
    H3 = rank_test_hand('C',"9S JH 5D JS 5C KC 3D"),
    ?assertEqual(?HC_TWO_PAIR, H1#hand.rank),
    ?assertEqual(?HC_TWO_PAIR, H2#hand.rank),
    ?assertEqual(?HC_TWO_PAIR, H3#hand.rank),
    ?assertEqual(true, hand:short(H2) > hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H3) > hand:short(H1)),
	
	Members = members(['A','B','C']),
	Winners = hand:winners([H1,H2,H3], [{100, Members, 1}], []),
	?assertEqual(1, length(Winners)),
	?assertEqual(#winner{amount=100,pid='B',player='B',potid=1}, lists:nth(1,Winners)).

two_pair_multiple_winners_test() ->
    H1 = rank_test_hand('A',"5C TC 7H KH 5S TS KS"),
    H2 = rank_test_hand('B',"5C TC 7H KH 5S KC TH"),
    ?assertEqual(?HC_TWO_PAIR, H1#hand.rank),
    ?assertEqual(?HC_TWO_PAIR, H2#hand.rank),
    ?assertEqual(true, hand:short(H1) == hand:short(H2)),
	
	Members = members(['A','B']),
	Winners = hand:winners([H1,H2], [{100, Members, 1}], []),
	?assertEqual(2, length(Winners)),
	?assertEqual(#winner{amount=50,pid='A',player='A',potid=1}, lists:nth(1,Winners)),
	?assertEqual(#winner{amount=50,pid='B',player='B',potid=1}, lists:nth(2,Winners)).

three_kind_win_test() ->    
    H1 = rank_test_hand('A',"KH 9S 5H QD QC QH 3S"),
    H2 = rank_test_hand('B',"AC KC KD KS 7H 9D 4D"),
    H3 = rank_test_hand('C',"KS TS QD QS QH 4C 5D"),
    ?assertEqual(?HC_THREE_KIND, H1#hand.rank),
    ?assertEqual(?HC_THREE_KIND, H2#hand.rank),
    ?assertEqual(?HC_THREE_KIND, H3#hand.rank),
    ?assertEqual(true, hand:short(H2) > hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H3) > hand:short(H1)),
	
	Members = members(['A','B','C']),
	Winners = hand:winners([H1,H2,H3], [{100, Members, 1}], []),
	?assertEqual(1, length(Winners)),
	?assertEqual(#winner{amount=100,pid='B',player='B',potid=1}, lists:nth(1,Winners)).

straight_win_test() ->
    H1 = rank_test_hand('A',"KC QS JH TC 9C 4D 3S"),
    H2 = rank_test_hand('B',"AC KS QH JC TC 9D 4D"),
    H3 = rank_test_hand('C',"KS QD JS TC 9S 2D 7S"),
    ?assertEqual(?HC_STRAIGHT, H1#hand.rank),
    ?assertEqual(?HC_STRAIGHT, H2#hand.rank),
    ?assertEqual(?HC_STRAIGHT, H3#hand.rank),
    ?assertEqual(true, hand:short(H2) > hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H1) == hand:short(H3)),
	
	Members = members(['A','B','C']),
	Winners = hand:winners([H1,H2,H3], [{100, Members, 1}], []),
	?assertEqual(1, length(Winners)),
	?assertEqual(#winner{amount=100,pid='B',player='B',potid=1}, lists:nth(1,Winners)).

flush_win_test() ->
    H1 = rank_test_hand('A',"4D JD 5D JC QD 2D 7H"),
    H2 = rank_test_hand('B',"8C AD 5D AS KD 9D 4D"),
    H3 = rank_test_hand('C',"4C JC 5C 8D QC 3C 7S"),
    H4 = rank_test_hand('D',"4C JC 7C 8D QC 5C 7S"),
    ?assertEqual(?HC_FLUSH, H1#hand.rank),
    ?assertEqual(?HC_FLUSH, H2#hand.rank),
    ?assertEqual(?HC_FLUSH, H3#hand.rank),
    ?assertEqual(?HC_FLUSH, H4#hand.rank),
    ?assertEqual(true, hand:short(H2) > hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H3) > hand:short(H1)),
    ?assertEqual(true, hand:short(H4) > hand:short(H1)),
	
	Members = members(['A','B','C','D']),
	Winners = hand:winners([H1,H2,H3,H4], [{100, Members, 1}], []),
	?assertEqual(1, length(Winners)),
	?assertEqual(#winner{amount=100,pid='B',player='B',potid=1}, lists:nth(1,Winners)).

four_kind_win_test() ->
    H1 = rank_test_hand('A',"4D AS 5H QD QC QH QS"),
    H2 = rank_test_hand('B',"AC KC KD KS KH 9D 4D"),
    H3 = rank_test_hand('C',"KS TS QD QS QH QC 5D"),
    ?assertEqual(?HC_FOUR_KIND, H1#hand.rank),
    ?assertEqual(?HC_FOUR_KIND, H2#hand.rank),
    ?assertEqual(?HC_FOUR_KIND, H3#hand.rank),
    ?assertEqual(true, hand:short(H2) > hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H1) > hand:short(H3)),
	
	Members = members(['A','B','C']),
	Winners = hand:winners([H1,H2,H3], [{100, Members, 1}], []),
	?assertEqual(1, length(Winners)),
	?assertEqual(#winner{amount=100,pid='B',player='B',potid=1}, lists:nth(1,Winners)).

straight_flush_win_test() ->
    H1 = rank_test_hand('A',"KC QC JC TC 9C 4D AS"),
    H2 = rank_test_hand('B',"AC KC QC JC TC 9D 4D"),
    H3 = rank_test_hand('C',"KS QS JS TS 9S AD 7S"),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H1#hand.rank),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H2#hand.rank),
    ?assertEqual(?HC_STRAIGHT_FLUSH, H3#hand.rank),
    ?assertEqual(true, hand:short(H2) > hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H1) == hand:short(H3)),
	
	Members = members(['A','B','C']),
	Winners = hand:winners([H1,H2,H3], [{100, Members, 1}], []),
	?assertEqual(1, length(Winners)),
	?assertEqual(#winner{amount=100,pid='B',player='B',potid=1}, lists:nth(1,Winners)).


full_house_win_test() ->
    H1 = rank_test_hand('A',"4D JS 5H JD JC QH QS"),
    H2 = rank_test_hand('B',"AC AD KD AS KH 9D 4D"),
    H3 = rank_test_hand('C',"3S JH JD JS KH KC 5D"),
    ?assertEqual(?HC_FULL_HOUSE, H1#hand.rank),
    ?assertEqual(?HC_FULL_HOUSE, H2#hand.rank),
    ?assertEqual(?HC_FULL_HOUSE, H3#hand.rank),
    ?assertEqual(true, hand:short(H2) > hand:short(H1)),
    ?assertEqual(true, hand:short(H2) > hand:short(H3)),
    ?assertEqual(true, hand:short(H3) > hand:short(H1)),
	
	Members = members(['A','B','C']),
	Winners = hand:winners([H1,H2,H3], [{100, Members, 1}], []),
	?assertEqual(1, length(Winners)),
	?assertEqual(#winner{amount=100,pid='B',player='B',potid=1}, lists:nth(1,Winners)).

full_house_multiple_winners_test() ->
  H1 = rank_test_hand('A',"2H 2C 5H 5S 5C 7C 4D"),
  H2 = rank_test_hand('B',"2H 2C 5H 5S 5D 4D 2D"),
  ?assertEqual(?HC_FULL_HOUSE, H1#hand.rank),
  ?assertEqual(?HC_FULL_HOUSE, H2#hand.rank),
  ?assertEqual(true, hand:short(H1) == hand:short(H2)),
  
  Members = members(['A','B']),
	Winners = hand:winners([H1,H2], [{100, Members, 1}], []),
	?assertEqual(2, length(Winners)),
	?assertEqual(#winner{amount=50,pid='A',player='A',potid=1}, lists:nth(1,Winners)),
	?assertEqual(#winner{amount=50,pid='B',player='B',potid=1}, lists:nth(2,Winners)).


gr(L) ->
  F = fun({Cards, Rank, High1, High2, Suit}) ->
      R = rank_test_player_hand(Cards),
      ?assertEqual(Rank, R#player_hand.rank),
      ?assertEqual(High1, R#player_hand.high1),
      ?assertEqual(High2, R#player_hand.high2),
      ?assertEqual(Suit, R#player_hand.suit)
  end,
  lists:map(F, L).

rank_player_pair_test() ->
  L = [
    {"2H 2C", ?CF_TWO}, 
    {"5H 5C", ?CF_FIVE}, 
    {"4D 3S 3H", ?CF_THREE}, 
    {"AD 4S 9H 8D 9S 2D TS", ?CF_NINE}
  ],
  gr([{Cards, ?HC_PAIR, High1, ?CF_NONE, ?CS_NONE} || {Cards, High1} <- L]).

rank_player_two_pair_test() ->
  L = [
    {"2H 2C AD AS", ?CF_ACE, ?CF_TWO},
    {"4H 4C 8D AS AD", ?CF_ACE, ?CF_FOUR},
    {"4H 4C 8D AS AD 8S", ?CF_ACE, ?CF_EIGHT},
    {"KH KC 8D AS AD 8S 7D", ?CF_ACE, ?CF_KING}
  ],
  gr([{Cards, ?HC_TWO_PAIR, High1, High2, ?CS_NONE} || {Cards, High1, High2} <- L]).

rank_player_three_kind_test() ->
  L = [
    {"2H 2C 2D AS", ?CF_TWO},
    {"4H 9C AH AS AD", ?CF_ACE},
    {"4H 8C 8D AS 2D 8S", ?CF_EIGHT},
    {"KH KC 6D AS KD 8S 7D", ?CF_KING}
  ],
  gr([{Cards, ?HC_THREE_KIND, High1, ?CF_NONE, ?CS_NONE} || {Cards, High1} <- L]).

rank_player_four_kind_test() ->
  L = [
    {"2H 2C 2D 2S", ?CF_TWO},
    {"4H AC AH AS AD", ?CF_ACE},
    {"4H 8C 8D 8H 2D 8S", ?CF_EIGHT},
    {"KH KC KS AS KD 8S 7D", ?CF_KING}
  ],
  gr([{Cards, ?HC_FOUR_KIND, High1, ?CF_NONE, ?CS_NONE} || {Cards, High1} <- L]).

rank_player_full_house_test() ->
  L = [
    {"2H 2C 2D 4S 4D", ?CF_TWO, ?CF_FOUR},
    {"KD AH AC AS 3S 3D", ?CF_ACE, ?CF_THREE},
    {"TS TD KH KC KD 8S 8C", ?CF_KING, ?CF_TEN}
  ],
  gr([{Cards, ?HC_FULL_HOUSE, High1, High2, ?CS_NONE} || {Cards, High1, High2} <- L]).

rank_player_staright_test() ->
  L = [
    {"2D 3S 4D 5S 6H", ?CF_SIX},
    {"2D 3S 4D 5S AH", ?CF_FIVE},
    {"2D 3S 4D 5S AH 8D 2S", ?CF_FIVE},
    {"2D 3S 4D 5S 6H 8D 2S", ?CF_SIX},
    {"7D 3S 4D 5S 6H 2D AD", ?CF_SEVEN},
    {"AD KS QD JS TH 9S 8H", ?CF_ACE}
  ],
  gr([{Cards, ?HC_STRAIGHT, High1, ?CF_NONE, ?CS_NONE} || {Cards, High1} <- L]).

rank_player_flush_test() ->
  L = [
    {"2D 8D 4D 9D AD", ?CF_ACE, ?CS_DIAMONDS},
    {"2D 3D 4D 5D 6S 7D", ?CF_SEVEN, ?CS_DIAMONDS},
    {"3H 9H TH KH 4H QH 5H", ?CF_KING, ?CS_HEARTS},
    {"3H 9H TH KS 4H QH 5H", ?CF_QUEEN, ?CS_HEARTS}
  ],
  gr([{Cards, ?HC_FLUSH, High1, ?CF_NONE, Suit} || {Cards, High1, Suit} <- L]).

rank_player_staright_flush_test() ->
  L = [
    {"2D 3D 4D 5D 6D", ?CF_SIX, ?CS_DIAMONDS},
    {"2D 3D 4D 5D AD", ?CF_FIVE, ?CS_DIAMONDS},
    {"KD JD TD QD AD", ?CF_ACE, ?CS_DIAMONDS},
    {"KD JD TD QD AS 9D 8D", ?CF_KING, ?CS_DIAMONDS}
  ],
  gr([{Cards, ?HC_STRAIGHT_FLUSH, High1, ?CF_NONE, Suit} || {Cards, High1, Suit} <- L]).