-module(hand).

-export([new/2, new/3, add/2, rank/1]).

-export([make_card/1, make_card/2, make_cards/1, print_bin/1, 
         print_rep/1, to_string/1, player_hand/1, card_to_string/1, face_from_mask/1,
		 winners/2]).
-export([make_rep/1, short/1, winners/3]). % For test
%% -ifdef(TEST).
%%    -include_lib("eunit/include/eunit.hrl").
%% -endif.

-include("texas.hrl").


new(Player, PID) ->
    new(Player, PID, []).

new(Player, PID, Cards) ->
    #hand{ 
     player = Player, 
     pid = PID,
     cards = Cards 
    }.

add(Hand, Card) ->
    Hand#hand{ cards = [Card|Hand#hand.cards] }.

rank(Hand) ->
  Rep = make_rep(Hand),
  L = [fun is_straight_flush/2,
    fun is_four_kind/2,
    fun is_full_house/2,
    fun is_flush/2,
    fun is_straight/2,
    fun is_three_kind/2,
    fun is_two_pair/2,
    fun is_pair/2],

  rank(Hand, L, Rep).

rank(Hand, [Rank|T], Rep) ->
  case Rank(Hand, Rep) of
    none ->
      rank(Hand, T, Rep);
    Hand1 ->
      Hand1
  end;

rank(Hand, [], Rep) ->
  Mask = make_mask(Rep),
  High = bits:clear_extra_bits(Mask, 5),
  Hand#hand{ rank = ?HC_HIGH_CARD, high1 = High, score = 0 }.

is_straight_flush(Hand, Rep) ->
    case is_flush(Hand, Rep, ?CS_CLUBS) of
      none ->
        none;
      Hand1 ->
        High = Hand1#hand.high2,
        case is_straight(Hand, High) of
          none ->
            none;
          Hand2 ->
            Hand2#hand{ rank = ?HC_STRAIGHT_FLUSH, suit = Hand1#hand.suit }
      end
    end.

is_flush(Hand, Rep) ->
    is_flush(Hand, Rep, ?CS_CLUBS).

is_flush(Hand, [H|T], Suit) ->
  Count = bits:bits1(H),
  if 
    Count < 5 ->
      is_flush(Hand, T, Suit + 1);
    true ->
      High5Cards = bits:clear_extra_bits(H, 5),
      Hand#hand{ rank = ?HC_FLUSH, high1 = High5Cards,high2=H, suit = Suit }
  end;

is_flush(_, [], _) ->
    none.

is_straight(Hand, Rep) when is_list(Rep) ->
    Mask = make_mask(Rep),
    is_straight(Hand, Mask);

is_straight(Hand, Mask) ->
    if 
		Mask band 2#10000000000000 > 0 ->  %% contains card A
      		Mask1 = Mask bor 1;
  		true ->
     	 	Mask1 = Mask
    end,                       %AKQJT98765432A
    is_straight(Hand, Mask1, 2#11111000000000).


is_straight(_, _, Mask) when Mask < 2#11111 ->
    none;

is_straight(Hand, Value, Mask) 
  when Mask >= 2#11111 ->
    if 
  		Value band Mask =:= Mask ->
            Hand#hand{ rank = ?HC_STRAIGHT, high1 = Mask };
  		true ->
      		is_straight(Hand, Value, Mask bsr 1)
    end.
  
is_four_kind(Hand, Rep = [C, D, H, S]) ->
    Value = C band D band H band S,
    if
  Value > 0 ->
            Hand#hand{ 
              rank = ?HC_FOUR_KIND, 
              high1 = Value, 
              score = score(Rep, Value, 1)
             };
  true ->
      none
    end.

is_full_house(Hand, Rep) ->
    case is_three_kind(Hand, Rep) of
        none -> 
            none;
        Hand1 ->
            High3 = Hand1#hand.high1,
            case is_pair(Hand1, clear_high_bit(Rep, High3)) of
                none ->
                    none;
                Hand2 ->
                    High2 = Hand2#hand.high1,
                    Hand2#hand{ 
                      rank = ?HC_FULL_HOUSE, 
                      high1 = High3,
                      high2 = High2,
                      score = 0
                     }
            end
    end.

is_three_kind(Hand, [C, D, H, S]) ->
    L = lists:sort(fun(A, B) ->
         A > B
       end, [C band D band H,
       D band H band S,
       H band S band C,
       S band C band D]),
    is_three_kind(Hand, L, [C, D, H, S]).

is_three_kind(Hand, [H|T], Rep) ->
    if 
  H > 0 ->
            Hand#hand{
              rank = ?HC_THREE_KIND, 
              high1 = bits:clear_extra_bits(H,1), 
              score = score(Rep, H, 2)
             };
  true ->
      is_three_kind(Hand, T, Rep)
    end;

is_three_kind(_, [], _) ->
    none.

is_two_pair(Hand, Rep) ->
    case is_pair(Hand, Rep) of
        none ->
            none;
        Hand1 = #hand{ rank = ?HC_PAIR, high1 = High1 } ->
      Rep1 = clear_high_bit(Rep, High1),
      case is_pair(Hand1, Rep1) of
                none ->
                    none;
                Hand2 = #hand{ rank = ?HC_PAIR, high1 = High2 } ->
                    Hand2#hand{ 
                      rank = ?HC_TWO_PAIR,
                      high1 = High1,
                      high2 = High2,
                      score = score(Rep, High1 bor High2, 1)
                     }
            end
    end.

is_pair(Hand, [C, D, H, S]) ->
    L = lists:sort(fun(A, B) ->
         A > B
       end, [C band D,
       D band H,
       H band S,
       S band C,
       C band H,
       D band S]),
    is_pair(Hand, L, [C, D, H, S]).

is_pair(Hand, [H|T], Rep) ->
    if 
  H > 0 ->
            Hand#hand{ 
              rank = ?HC_PAIR, 
              high1 = bits:clear_extra_bits(H, 1), 
              score = score(Rep, H, 3)
             };
  true ->
      is_pair(Hand, T, Rep)
    end;

is_pair(_, [], _) ->
    none.

make_rep(Hand = #hand{}) ->
  make_rep(Hand#hand.cards);

make_rep(Cards) 
  when is_list(Cards) ->
    make_rep(Cards, {0, 0, 0, 0}).

make_rep([H|T], Rep) 
  when is_integer(H) -> 
    Face = 1 bsl (H bsr 8),
    Suit = H band 16#ff,
    Old = element(Suit, Rep),
    make_rep(T, setelement(Suit, Rep, Old bor Face));

make_rep([], Rep) ->
    tuple_to_list(Rep).

make_mask([C, D, H, S]) ->
    C bor D bor H bor S.


clear_high_bit([C, D, H, S], High) ->
    [C band (bnot High),
     D band (bnot High),
     H band (bnot High),
     S band (bnot High)].

score(Rep, High, Bits) ->
    Mask = make_mask(Rep),
    Mask1 = Mask band (bnot High),
    bits:clear_extra_bits(Mask1, Bits).

%% Make a list of cards from a space-delimited string 

make_cards(S)
  when is_list(S) ->
    lists:map(fun make_card/1, 
        string:tokens(S, " ")).

make_card({F, S}) ->
    Face = case F of 
         two -> ?CF_TWO;
         three -> ?CF_THREE;
         four -> ?CF_FOUR;
         five -> ?CF_FIVE;
         six -> ?CF_SIX;
         seven -> ?CF_SEVEN;
         eight -> ?CF_EIGHT;
         nine -> ?CF_NINE;
         ten -> ?CF_TEN;
         jack -> ?CF_JACK;
         queen -> ?CF_QUEEN;
         king -> ?CF_KING;
         ace -> ?CF_ACE
     end,
    Suit = case S of 
         clubs -> ?CS_CLUBS;
         diamonds -> ?CS_DIAMONDS;
         hearts -> ?CS_HEARTS;
         spades -> ?CS_SPADES
     end,
    make_card(Face, Suit);

make_card([H, T]) ->
    Face = case H of 
         $2 -> ?CF_TWO;
         $3 -> ?CF_THREE;
         $4 -> ?CF_FOUR;
         $5 -> ?CF_FIVE;
         $6 -> ?CF_SIX;
         $7 -> ?CF_SEVEN;
         $8 -> ?CF_EIGHT;
         $9 -> ?CF_NINE;
         $T -> ?CF_TEN;
         $J -> ?CF_JACK;
         $Q -> ?CF_QUEEN;
         $K -> ?CF_KING;
         $A -> ?CF_ACE
     end,
    Suit = case T of 
         $C -> ?CS_CLUBS;
         $D -> ?CS_DIAMONDS;
         $H -> ?CS_HEARTS;
         $S -> ?CS_SPADES
     end,
    make_card(Face, Suit).

make_card(Face, Suit) ->
    (Face bsl 8) bor Suit.

% Get the highest card face
face_from_mask(0) ->
    0;

face_from_mask(X) 
  when is_number(X) ->
    face_from_mask(X, [1 bsl Face || Face <- [?CF_ACE, ?CF_KING, ?CF_QUEEN, 
                                              ?CF_JACK, ?CF_TEN, ?CF_NINE,
                                              ?CF_EIGHT, ?CF_SEVEN, ?CF_SIX, 
                                              ?CF_FIVE, ?CF_FOUR, ?CF_THREE, 
                                              ?CF_TWO]]).

face_from_mask(_, []) ->
    0;

face_from_mask(X, [H|_])
  when (X band H) > 0 ->
    bits:log2(H);

face_from_mask(X, [_|T]) ->
    face_from_mask(X, T).

face_to_string(Face) 
  when is_integer(Face) ->
    case Face of
        ?CF_TWO -> "two";
        ?CF_THREE -> "three";
        ?CF_FOUR -> "four";
        ?CF_FIVE -> "five";
        ?CF_SIX -> "six";
        ?CF_SEVEN -> "seven";
        ?CF_EIGHT -> "eight";
        ?CF_NINE -> "nine";
        ?CF_TEN -> "ten";
        ?CF_JACK -> "jack";
        ?CF_QUEEN -> "queen";
        ?CF_KING -> "king";
        ?CF_ACE -> "ace"
    end.

suit_to_string(Suit)
  when is_integer(Suit) ->
    case Suit of 
        ?CS_CLUBS -> "clubs";
        ?CS_DIAMONDS -> "diamonds";
        ?CS_HEARTS -> "hearts";
        ?CS_SPADES -> "spades"
    end.

card_to_string(Card) ->
    Face = Card bsr 8,
    Suit = Card band 16#ff,
    face_to_string(Face) ++ " of " ++ suit_to_string(Suit).
    
to_string(H = #player_hand{ rank = ?HC_STRAIGHT_FLUSH }) ->
    "straight flush high " 
  ++ face_to_string(H#player_hand.high1)
  ++ "s";

to_string(H = #player_hand{ rank = ?HC_FOUR_KIND }) ->
    "four of a kind " 
  ++ face_to_string(H#player_hand.high1)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_FULL_HOUSE }) ->
    "house of " 
  ++ face_to_string(H#player_hand.high1) 
  ++ "s full of " 
  ++ face_to_string(H#player_hand.high2)
  ++ "s";

to_string(H = #player_hand{ rank = ?HC_FLUSH }) ->
    "flush high "
  ++ face_to_string(H#player_hand.high1)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_STRAIGHT }) ->
    "straight high "
  ++ face_to_string(H#player_hand.high1)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_THREE_KIND }) ->
    "three of a kind "
  ++ face_to_string(H#player_hand.high1)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_TWO_PAIR }) ->
    "two pairs of "
  ++ face_to_string(H#player_hand.high1)
  ++ "s and "
  ++ face_to_string(H#player_hand.high2)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_PAIR }) ->
    "pair of "
  ++ face_to_string(H#player_hand.high1)
  ++ "s";
  
to_string(H = #player_hand{ rank = ?HC_HIGH_CARD }) ->
    "high card "
  ++ face_to_string(H#player_hand.high1).

%%% Hand description for the poker client
  
player_hand(#hand{ rank = Rank, high1 = High3, high2 = High2 }) 
  when Rank == ?HC_FULL_HOUSE;
       Rank == ?HC_TWO_PAIR ->
    H1 = face_from_mask(High3),
    H2 = face_from_mask(High2),
    #player_hand{ rank = Rank, high1 = H1, high2 = H2 };

player_hand(#hand{ rank = Rank, high1 = High3, suit = Suit }) 
  when Rank == ?HC_FLUSH;
       Rank == ?HC_STRAIGHT_FLUSH ->
    H1 = face_from_mask(High3),
    #player_hand{ rank = Rank, high1 = H1, suit = Suit };

player_hand(#hand{ rank = Rank, high1 = High }) ->
    #player_hand{ rank = Rank, high1 = face_from_mask(High) }.


winners(Ranks, Pots) ->
    winners(Ranks, Pots, []).

winners(_Ranks, [], Winners) ->
    Winners;

winners(Ranks, [{Total, Members, PotId} | Rest], Winners) ->
    M = lists:filter(fun(Hand) -> 
                             gb_trees:is_defined(Hand#hand.player, Members) 
                     end, Ranks),
	F = fun(A, B) ->
         short(A) >= short(B)
    end,

	M1 = lists:sort(F, M),
	
	TopRank = hd(M1),
	
	M2 = lists:filter(fun(Hand) -> 
							  short(Hand) == short(TopRank) 
					  end, M1),
	
    Win = erlang:trunc(Total / length(M2)),
    Winners1 = add_winners(M2, PotId, Win, Winners),
    winners(Ranks, Rest, Winners1).

add_winners([],_,_,Winners) ->
	lists:reverse(Winners);

add_winners([H = #hand{} | T ], PotId, Win, Winners) ->
	Winner = #winner{
				player = H#hand.player,
    			pid = H#hand.pid,
				potid = PotId,
				amount=Win	 
			},
	add_winners(T, PotId, Win, [Winner | Winners]).

short(Hand) ->
    {Hand#hand.rank, 
     Hand#hand.high1, 
     Hand#hand.high2,
     Hand#hand.score}.


print_bin(X) ->
    io:format("AKQJT98765432A~n"),
    io:format("~14.2.0B~n", [X]).

print_rep([C, D, H, S]) ->
    print_rep({C, D, H, S});

print_rep({C, D, H, S}) ->
    io:format("   AKQJT98765432A~n"),
    io:format("C: ~14.2.0B~n", [C]),
    io:format("D: ~14.2.0B~n", [D]),
    io:format("H: ~14.2.0B~n", [H]),
    io:format("S: ~14.2.0B~n", [S]).