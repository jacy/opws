-module(pot).

-export([new/0, reset/1, new_stage/1,
         pend/4, pots/1, total/1, player_pending/2]).

-include_lib("eunit/include/eunit.hrl").

%% Questions
%% 1. What if A bet 10, B all in 8, and A fold ? B just win 8, A lose 10?
%% 2. What if A bet 10, B allin 8, C allin 5? how many side pots?

-record(side_pot, {
          members,
          all_in
         }).

-record(pot, {
	      pending = gb_trees:empty(),
          active = [],
          inactive = [],
          current = new_side_pot()
         }).

new_side_pot(AllInAmt, Members) ->
    #side_pot{ 
              all_in = AllInAmt, 
              members = Members
             }.

new_side_pot(AllInAmt) 
  when is_number(AllInAmt) ->
    new_side_pot(AllInAmt, gb_trees:empty()).

new_side_pot() ->
    new_side_pot(0, gb_trees:empty()).

new() ->
    #pot{}.

reset(Pot) 
  when is_record(Pot, pot) ->
    new().

new_stage(Pot) when is_record(Pot, pot) ->
	NewPot = from_pend_to_pot(Pot),
    Inactive = NewPot#pot.inactive 
        ++ NewPot#pot.active,
	
    NewPot#pot { 
      active = [], 
      inactive = Inactive,
	  pending = gb_trees:empty()
     }.

from_pend_to_pot(Pot) when is_record(Pot, pot) ->
	Pendings = sort_pending(Pot#pot.pending),
	F =  fun({Player, {Amount, IsAllIn} } , P) ->
				 add(P, Player, Amount, IsAllIn)
	end,
	lists:foldl(F, Pot,Pendings).

pots(Pot)
  when is_record(Pot, pot) ->
	SortedPots = sort_pots(side_pots(Pot)),
	{Winners,_} = lists:mapfoldl(fun(P,Index) ->  
										 {{total(P), P#side_pot.members, Index},Index + 1}
								end,
								0,SortedPots),
    lists:reverse(Winners).

pend(Pot, Player, Amount, IsAllIn) when is_record(Pot, pot) ->
	Pending = case gb_trees:lookup(Player, Pot#pot.pending) of
        {value, {Bet, _} } ->
            gb_trees:enter(Player, {Bet + Amount, IsAllIn }, Pot#pot.pending);
        _ -> 
            gb_trees:insert(Player, {Amount, IsAllIn }, Pot#pot.pending)
	end,
	Pot#pot{pending=Pending}.

sort_pending(Pending) ->
	P = gb_trees:to_list(Pending),
	lists:keysort(2, P).  % sort by {amount,allin}, so can handle allin amount getting smaller and same amount allin=false orders before allin=true.


add(Pot, Player, Amount, IsAllIn)
  when is_record(Pot, pot) ->
    {P, 0} = add_bet(Pot, Player, Amount, IsAllIn),
    P.

%% Ensure that player belongs to the pot

make_member(Pot, Player) ->
    case gb_trees:lookup(Player, Pot#side_pot.members) of
        {value, Bet } ->
            {Pot, Bet};
        _ -> 
            Members = gb_trees:insert(Player, 0, Pot#side_pot.members),
            NewPot = Pot#side_pot { 
                       members = Members 
                      },
            {NewPot, 0}
    end.


add_side_pot(Pot, Player, Amount) when is_record(Pot, side_pot) ->
    {NewPot, Bet} = make_member(Pot, Player),
    AllIn = NewPot#side_pot.all_in,
    {Unallocated, Members} = 
        if
            AllIn > 0 ->
                %% Pot is split, figure out
                %% the difference between amount bet
                %% so far and the all-in amount
                Delta = AllIn - Bet,
                if 
                    Delta > 0 ->
                        %% Post all-in
                        U = Amount - Delta,
                        M = gb_trees:enter(Player, AllIn, Pot#side_pot.members),
                        {U, M};
                    true ->
                        %% Posted enough already
                        {Amount, Pot#side_pot.members}
                end;
            true ->
                %% Pot is not split, post Amount
                M = update_counter(Player, Amount, Pot#side_pot.members),
                {0, M}
        end,
    NewPot1 = NewPot#side_pot{ 
                members = Members 
               }, 
    {NewPot1, Unallocated}.

add_bet(Pot, Player, Amount) when is_record(Pot, pot) ->
    add_bet(Pot, Player, Amount, false).

add_bet(Pot, Player, Amount, IsAllIn) when is_record(Pot, pot) ->
    {Active, Unallocated} = allocate_bet(Pot#pot.active, Player, Amount),
    Pot1 = Pot#pot {
             active = Active
            },
    if  
		Unallocated == 0 ->  % same all in amount, not need to split again
			Pot2 = Pot1,
			Rest = 0;
        IsAllIn ->
            %% split the pot
            Pot2 = split(Pot1, Player, Unallocated),
            Rest = 0;
        true ->
            {Current, Rest} = add_side_pot(Pot1#pot.current, Player, Unallocated),
            Pot2 = Pot1#pot {
                     current = Current
                    }
    end,
    {Pot2, Rest}.

allocate_bet(SidePots, Player, Amount) when is_list(SidePots) ->
    lists:mapfoldl(fun(Pot, Unallocated) ->
                           add_side_pot(Pot, Player, Unallocated)
                   end, 
                   Amount, SidePots).

side_pots(Pot) ->
    Temp = lists:append(Pot#pot.active, Pot#pot.inactive),
    Current = Pot#pot.current,
    lists:filter(fun(P) ->
                         gb_trees:size(P#side_pot.members) > 0
                             andalso total(P) > 0
                 end, [Current|Temp]).

sort_pots(SidePots) ->
    Sort = fun(S1,S2) ->
				Side1 = gb_trees:size(S1#side_pot.members),
			    Side2 = gb_trees:size(S2#side_pot.members),
				if 
					Side1 == Side2 -> 
						S1#side_pot.all_in >= S2#side_pot.all_in;
				   	true -> 
						Side1 > Side2
				end
	end,
	lists:sort(Sort, SidePots).

total(Pot) when is_record(Pot, side_pot) ->
    F = fun(X, Acc) -> X + Acc end,
    lists:foldl(F, 0, gb_trees:values(Pot#side_pot.members));

total(Pot) when is_record(Pot, pot) ->
    F = fun(X, Acc) -> 
                Acc + total(X)
        end,
    lists:foldl(F, 0, side_pots(Pot)).

player_pending(Pot, Player) when is_record(Pot, pot) ->
   	case gb_trees:lookup(Player, Pot#pot.pending) of
        {value, {Bet, _} } ->
            Bet;
        _ -> 
            0
	end.


bet_amount(SidePot,Player) ->
	{_, Amount} = make_member(SidePot, Player),
	Amount.

%% Split the pot. Last bet for this player plus
%% the current bet becomes the all-in amount.
%% Bets in excess of the all-in amount are moved 
%% to a new side pot.

split(Pot, Player, Amount) when is_record(Pot, pot) ->
    {OldPot, NewPot} = split(Pot#pot.current, Player, Amount),
    Active = lists:append(Pot#pot.active, [OldPot]),
    Pot#pot { 
      current = NewPot, 
      active = Active 
     };

split(SidePot, Player, Amount) ->
    M = update_counter(Player, Amount, SidePot#side_pot.members),
    Bet = gb_trees:get(Player, M),
    List = gb_trees:to_list(M),
    List1 = lists:filter(fun({Key, Value}) ->
                                 (Key /= Player) and (Value > Bet)
                         end, List),
    List2 = lists:map(fun({Key, Value}) ->
                              {Key, Value - Bet}
                      end, List1),
    NewPot = #side_pot {
      all_in = 0,
      members = gb_trees:from_orddict(List2)
     },
    Members2 = lists:map(fun({Key, Value}) -> 
                                 if 
                                     Value > Bet -> {Key, Bet};
                                     true -> {Key, Value}  % Someone already folds, just add amount into pot
                                 end
                         end, List),
    OldPot = SidePot#side_pot { 
               all_in = Bet, 
               members = gb_trees:from_orddict(Members2)
              },
    {OldPot, NewPot}.

update_counter(Key, Amount, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Old} ->
            Old = gb_trees:get(Key, Tree),
            gb_trees:update(Key, Old + Amount, Tree);
        none ->
            gb_trees:insert(Key, Amount, Tree)
    end.

%%%
%%% Test suite
%%%

is_member(Pot, Player) 
  when is_record(Pot, side_pot) ->
    is_member(Pot#side_pot.members,Player);

is_member(Members,Player)  ->
    gb_trees:is_defined(Player, Members).

%% Pot is split, Delta > 0

split_pot_positive_delta_test() ->
    Pot = new_side_pot(100),
    {NewPot, Amount} = add_side_pot(Pot, 'P', 120),
    ?assertEqual(20, Amount),
    ?assertEqual(true, is_member(NewPot, 'P')),
    ?assertEqual(100, total(NewPot)).

%% Pot is split, Delta = 0

split_pot_negative_delta_test() ->
    Pot = new_side_pot(100),
    {NewPot, Amount} = add_side_pot(Pot, 'P', 100),
    ?assertEqual(0, Amount),
    ?assertEqual(true, is_member(NewPot, 'P')),
    ?assertEqual(100, total(NewPot)).

%% Pot is not split

pot_not_split_test() ->
    Pot = new_side_pot(),
    {NewPot, Amount} = add_side_pot(Pot, 'P', 100),
    ?assertEqual(0, Amount),
    ?assertEqual(true, is_member(NewPot, 'P')),
    ?assertEqual(100, total(NewPot)),
    {NewPot1,  Amount1} = add_side_pot(NewPot, 'P', 100),
    ?assertEqual(0, Amount1),
    ?assertEqual(200, total(NewPot1)).

%% Split pot

pot_split_test() ->
    Pot = new_side_pot(0),
    Pot1 = Pot#side_pot{ 
             members = gb_trees:insert('A', 10, Pot#side_pot.members)
            },
    Pot2 = Pot1#side_pot{ 
             members = gb_trees:insert('B', 30, Pot1#side_pot.members)
            },
    Pot3 = Pot2#side_pot{ 
             members = gb_trees:insert('C', 40, Pot2#side_pot.members)
            },
    {OldPot, NewPot} = split(Pot3, 'A', 10),
    ?assertEqual(20, OldPot#side_pot.all_in),
    ?assertEqual(20, gb_trees:get('A', OldPot#side_pot.members)),
    ?assertEqual(20, gb_trees:get('B', OldPot#side_pot.members)),
    ?assertEqual(20, gb_trees:get('C', OldPot#side_pot.members)),
    ?assertEqual(0, NewPot#side_pot.all_in),
    ?assertEqual(10, gb_trees:get('B', NewPot#side_pot.members)),
    ?assertEqual(20, gb_trees:get('C', NewPot#side_pot.members)),
    ?assertEqual(false, is_member(NewPot, 'A')).

%% % ;;; http://www.homepokertourney.com/allin_examples.htm

all_in_example5_test() ->
    Pot = new(),
    { Pot1, Amt1 } = add_bet(Pot, 'A', 100),
    ?assertEqual(0, Amt1),
    { Pot2, Amt2 } = add_bet(Pot1, 'B', 60, true),
    ?assertEqual(0, Amt2),
    ?assertEqual(40, total(Pot2#pot.current)),
    ?assertEqual(true, is_member(Pot2#pot.current, 'A')),
    ?assertEqual(false, is_member(Pot2#pot.current, 'B')),
    ?assertEqual(120, total(hd(Pot2#pot.active))),
    ?assertEqual(true, is_member(hd(Pot2#pot.active), 'A')),
    ?assertEqual(true, is_member(hd(Pot2#pot.active), 'B')).

all_in_example6_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 100),
    { Pot2, 0 } = add_bet(Pot1, 'B', 100),
    { Pot3, 0 } = add_bet(Pot2, 'C', 60, true),
    ?assertEqual(80, total(Pot3#pot.current)),
    ?assertEqual(true, is_member(Pot3#pot.current, 'A')),
    ?assertEqual(true, is_member(Pot3#pot.current, 'B')),
    ?assertEqual(false, is_member(Pot3#pot.current, 'C')),
    ?assertEqual(180, total(hd(Pot3#pot.active))),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'A')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'B')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'C')).

all_in_example7_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 100),
    { Pot2, 0 } = add_bet(Pot1, 'B', 60, true),
    { Pot3, 0 } = add_bet(Pot2, 'C', 100),
    ?assertEqual(80, total(Pot3#pot.current)),
    ?assertEqual(true, is_member(Pot3#pot.current, 'A')),
    ?assertEqual(true, is_member(Pot3#pot.current, 'C')),
    ?assertEqual(false, is_member(Pot3#pot.current, 'B')),
    ?assertEqual(180, total(hd(Pot3#pot.active))),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'A')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'B')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'C')).

all_in_example8_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 100),
    { Pot2, 0 } = add_bet(Pot1, 'B', 60, true),
    { Pot3, 0 } = add_bet(Pot2, 'C', 100),
    { Pot4, 0 } = add_bet(Pot3, 'D', 500),
    { Pot5, 0 } = add_bet(Pot4, 'A', 250, true),
    { Pot6, 0 } = add_bet(Pot5, 'C', 400),
    %% there's a main pot between all 4 players
    Side1 = lists:nth(1, Pot6#pot.active),
    ?assertEqual(240, total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'C')),
    ?assertEqual(true, is_member(Side1, 'D')),
    %% there's a side pot between a, c and d
    Side2 = lists:nth(2, Pot6#pot.active),
    ?assertEqual(870, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(true, is_member(Side2, 'C')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'B')),
    %% there's another side pot between c and d
    Side3 = Pot6#pot.current,
    ?assertEqual(300, total(Side3)),
    ?assertEqual(true, is_member(Side3, 'C')),
    ?assertEqual(true, is_member(Side3, 'D')),
    ?assertEqual(false, is_member(Side3, 'A')),
    ?assertEqual(false, is_member(Side3, 'B')).

all_in_example9_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 20),
    { Pot5, 0 } = add_bet(Pot4, 'A', 10),
    { Pot6, 0 } = add_bet(Pot5, 'B', 20),
    { Pot7, 0 } = add_bet(Pot6, 'D', 10),
	
	
    %% player-a folds but is still
    %% member of the last side pot
    Side = lists:last(Pot7#pot.active),
    ?assertEqual(28, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side1 = Pot7#pot.current,
    ?assertEqual(59, total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'D')),
    ?assertEqual(false, is_member(Side1, 'C')).

all_in_example10_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 20),
    { Pot5, 0 } = add_bet(Pot4, 'A', 2, true),
    { Pot6, 0 } = add_bet(Pot5, 'B', 20),
    { Pot7, 0 } = add_bet(Pot6, 'D', 10),
	
	
	
    Side = lists:nth(1, Pot7#pot.active),
    ?assertEqual(28, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side1 = lists:nth(2, Pot7#pot.active),
    ?assertEqual(15, total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'D')),
    ?assertEqual(false, is_member(Side1, 'C')),
    Side2 = Pot7#pot.current,
    ?assertEqual(36, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'C')).

all_in_example11_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 5, true),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 8, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 10),
	
	
    Side = lists:nth(1, Pot4#pot.active),
    ?assertEqual(20, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side1 = lists:nth(2, Pot4#pot.active),
    ?assertEqual(9, total(Side1)),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'C')),
    ?assertEqual(true, is_member(Side1, 'D')),
    ?assertEqual(false, is_member(Side1, 'A')),
    Side2 = Pot4#pot.current,
    ?assertEqual(4, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'C')).

all_in_example12_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = add_bet(Pot3, 'D', 10),
	
    Side = lists:last(Pot4#pot.active),
    ?assertEqual(28, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side2 = Pot4#pot.current,
    ?assertEqual(9, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'C')).

folding_main_pot_test() ->
	Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = add_bet(Pot2, 'C', 10),
    { Pot4, 0 } = add_bet(Pot3, 'A', 5),
    { Pot5, 0 } = add_bet(Pot4, 'C', 3, true),
	
    Side = lists:last(Pot5#pot.active),
    ?assertEqual(36, total(Side)),
	
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(13, bet_amount(Side,'A')),

	?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(10, bet_amount(Side,'B')),
	
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(13, bet_amount(Side,'C')),
	
	Side2 = Pot5#pot.current,
    ?assertEqual(2, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'B')),
    ?assertEqual(false, is_member(Side2, 'C')).
	
pending_test() ->
    Pot = new(),
	Pot1 = pend(Pot, 'A', 10, false),
    Pot2 = pend(Pot1, 'B', 8, true),
    Pot3 = pend(Pot2, 'C', 9, true),
    Pot4 = pend(Pot3, 'D', 6, true),
    Pot5 = pend(Pot4, 'A', 10,false),
	
	Pendings = Pot5#pot.pending,
	Pendings1 = gb_trees:to_list(Pendings),
	 ?assertEqual(4, length(Pendings1)),
	 ?assertEqual({'A',{20,false}}, lists:nth(1, Pendings1)),
	 ?assertEqual({'B', {8, true}}, lists:nth(2, Pendings1)),
	 ?assertEqual({'C', {9, true}}, lists:nth(3, Pendings1)),
	 ?assertEqual({'D', {6, true}}, lists:nth(4, Pendings1)).

player_pending_amount_test() ->
    Pot = new(),
	Pot1 = pend(Pot, 'A', 10, false),
    Pot2 = pend(Pot1, 'B', 8, true),
    Pot3 = pend(Pot2, 'C', 9, true),
    Pot4 = pend(Pot3, 'D', 6, true),
    Pot5 = pend(Pot4, 'A', 10,false),
	
	?assertEqual(20, player_pending(Pot5,'A')),
	?assertEqual(8, player_pending(Pot5,'B')),
	?assertEqual(9, player_pending(Pot5,'C')),
	?assertEqual(6, player_pending(Pot5,'D')).

sort_pending_test() ->
	Pot = new(),
	Pot1 = pend(Pot, 'A', 10, false),
    Pot2 = pend(Pot1, 'B', 8, true),
    Pot3 = pend(Pot2, 'C', 9, true),
    Pot4 = pend(Pot3, 'D', 6, true),
    Pot5 = pend(Pot4, 'E', 8, false),
    Pot6 = pend(Pot5, 'A', 10,false),

	Sorted = sort_pending(Pot6#pot.pending),
	?assertEqual(5, length(Sorted)),
	?assertEqual({'D', {6, true}}, lists:nth(1, Sorted)),
	?assertEqual({'E', {8, false}}, lists:nth(2, Sorted)),
	?assertEqual({'B', {8, true}}, lists:nth(3, Sorted)),
	?assertEqual({'C', {9, true}}, lists:nth(4, Sorted)),
	?assertEqual({'A', {20, false}}, lists:nth(5, Sorted)).
	
	
multiple_all_in_with_same_amount_test() ->
    Pot = new(),
    { Pot1, 0 } = add_bet(Pot, 'A', 10),
    { Pot2, 0 } = add_bet(Pot1, 'B', 8, true),
    { Pot3, 0 } = add_bet(Pot2, 'C', 8, true),
    ?assertEqual(0, length(Pot3#pot.inactive)),
    ?assertEqual(1, length(Pot3#pot.active)),
	
    Side = lists:nth(1, Pot3#pot.active),
    ?assertEqual(24, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
	
    Side2 = Pot3#pot.current,
    ?assertEqual(2, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'B')),
    ?assertEqual(false, is_member(Side2, 'C')).

from_pend_to_pot_with_allin_amount_getting_smaller_test() ->
    Pot = new(),
    Pot1 = pend(Pot, 'A', 10,false),
    Pot2 = pend(Pot1, 'B', 8, true),
    Pot3 = pend(Pot2, 'C', 7, true),
	Pot4 = from_pend_to_pot(Pot3),
	
	?assertEqual(0, length(Pot4#pot.inactive)),
    ?assertEqual(2, length(Pot4#pot.active)),

    Side = lists:nth(1, Pot4#pot.active),
    ?assertEqual(21, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
	
    Side1 = lists:nth(2, Pot4#pot.active),
    ?assertEqual(2, total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(false, is_member(Side1, 'C')),

    Side2 = Pot4#pot.current,
    ?assertEqual(2, total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'B')),
    ?assertEqual(false, is_member(Side2, 'C')).

from_pend_to_pot_with_allin_amount_getting_smaller2_test() ->
    Pot = new(),
    Pot1 = pend(Pot, 'A', 5,true),
    Pot2 = pend(Pot1, 'B', 8, true),
    Pot3 = pend(Pot2, 'C', 7, true),
	Pot4 = from_pend_to_pot(Pot3),
	
	?assertEqual(0, length(Pot4#pot.inactive)),
    ?assertEqual(3, length(Pot4#pot.active)),

    Side = lists:nth(1, Pot4#pot.active),
    ?assertEqual(15, total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
	
    Side1 = lists:nth(2, Pot4#pot.active),
    ?assertEqual(4, total(Side1)),
    ?assertEqual(false, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'C')),

    Side2 = lists:last(Pot4#pot.active),
    ?assertEqual(1, total(Side2)),
    ?assertEqual(false, is_member(Side2, 'A')),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(false, is_member(Side2, 'C')),

    Side3 = Pot4#pot.current,
    ?assertEqual(0, total(Side3)),
    ?assertEqual(false, is_member(Side3, 'A')),
    ?assertEqual(false, is_member(Side3, 'B')),
    ?assertEqual(false, is_member(Side3, 'C')).
	
winner_test() ->
	Pot = new(),
    Pot1 = pend(Pot, 'A', 5,true),
    Pot2 = pend(Pot1, 'B', 8, true),
    Pot3 = pend(Pot2, 'C', 7, true),
	Pot4 = from_pend_to_pot(Pot3),
	
	Winners = pots(Pot4),
 	
	{SidePot2,SideMembers2,PotId2} = lists:nth(1, Winners),
    ?assertEqual(1, SidePot2),
    ?assertEqual(2, PotId2),
    ?assertEqual(false, is_member(SideMembers2, 'A')),
    ?assertEqual(true, is_member(SideMembers2, 'B')),
    ?assertEqual(false, is_member(SideMembers2, 'C')),

	{SidePot1,SideMembers1,PotId1} = lists:nth(2, Winners),
    ?assertEqual(4, SidePot1),
    ?assertEqual(1, PotId1),
    ?assertEqual(false, is_member(SideMembers1, 'A')),
    ?assertEqual(true, is_member(SideMembers1, 'B')),
    ?assertEqual(true, is_member(SideMembers1, 'C')),


	{MainPot,MainMembers,PotId} = lists:nth(3, Winners),
    ?assertEqual(15, MainPot),
    ?assertEqual(0, PotId),
    ?assertEqual(true, is_member(MainMembers, 'A')),
    ?assertEqual(true, is_member(MainMembers, 'B')),
    ?assertEqual(true, is_member(MainMembers, 'C')).
