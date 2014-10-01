-module(pot_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pot.hrl").


is_member(Pot, Player) 
  when is_record(Pot, side_pot) ->
    is_member(Pot#side_pot.members,Player);

is_member(Members,Player)  ->
    gb_trees:is_defined(Player, Members).

%% Pot is split, Delta > 0

split_pot_positive_delta_test() ->
    Pot = pot:new_side_pot(100),
    {NewPot, Amount} = pot:add_side_pot(Pot, 'P', 120),
    ?assertEqual(20, Amount),
    ?assertEqual(true, is_member(NewPot, 'P')),
    ?assertEqual(100, pot:total(NewPot)).

%% Pot is split, Delta = 0

split_pot_negative_delta_test() ->
    Pot = pot:new_side_pot(100),
    {NewPot, Amount} = pot:add_side_pot(Pot, 'P', 100),
    ?assertEqual(0, Amount),
    ?assertEqual(true, is_member(NewPot, 'P')),
    ?assertEqual(100, pot:total(NewPot)).

%% Pot is not split

pot_not_split_test() ->
    Pot = pot:new_side_pot(),
    {NewPot, Amount} = pot:add_side_pot(Pot, 'P', 100),
    ?assertEqual(0, Amount),
    ?assertEqual(true, is_member(NewPot, 'P')),
    ?assertEqual(100, pot:total(NewPot)),
    {NewPot1,  Amount1} = pot:add_side_pot(NewPot, 'P', 100),
    ?assertEqual(0, Amount1),
    ?assertEqual(200, pot:total(NewPot1)).

%% Split pot

pot_split_test() ->
    Pot = pot:new_side_pot(0),
    Pot1 = Pot#side_pot{ 
             members = gb_trees:insert('A', 10, Pot#side_pot.members)
            },
    Pot2 = Pot1#side_pot{ 
             members = gb_trees:insert('B', 30, Pot1#side_pot.members)
            },
    Pot3 = Pot2#side_pot{ 
             members = gb_trees:insert('C', 40, Pot2#side_pot.members)
            },
    {OldPot, NewPot} = pot:split(Pot3, 'A', 10),
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
    Pot = pot:new(),
    { Pot1, Amt1 } = pot:add_bet(Pot, 'A', 100),
    ?assertEqual(0, Amt1),
    { Pot2, Amt2 } = pot:add_bet(Pot1, 'B', 60, true),
    ?assertEqual(0, Amt2),
    ?assertEqual(40, pot:total(Pot2#pot.current)),
    ?assertEqual(true, is_member(Pot2#pot.current, 'A')),
    ?assertEqual(false, is_member(Pot2#pot.current, 'B')),
    ?assertEqual(120, pot:total(hd(Pot2#pot.active))),
    ?assertEqual(true, is_member(hd(Pot2#pot.active), 'A')),
    ?assertEqual(true, is_member(hd(Pot2#pot.active), 'B')).

all_in_example6_test() ->
    Pot = pot:new(),
    { Pot1, 0 } = pot:add_bet(Pot, 'A', 100),
    { Pot2, 0 } = pot:add_bet(Pot1, 'B', 100),
    { Pot3, 0 } = pot:add_bet(Pot2, 'C', 60, true),
    ?assertEqual(80, pot:total(Pot3#pot.current)),
    ?assertEqual(true, is_member(Pot3#pot.current, 'A')),
    ?assertEqual(true, is_member(Pot3#pot.current, 'B')),
    ?assertEqual(false, is_member(Pot3#pot.current, 'C')),
    ?assertEqual(180, pot:total(hd(Pot3#pot.active))),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'A')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'B')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'C')).

all_in_example7_test() ->
    Pot = pot:new(),
    { Pot1, 0 } = pot:add_bet(Pot, 'A', 100),
    { Pot2, 0 } = pot:add_bet(Pot1, 'B', 60, true),
    { Pot3, 0 } = pot:add_bet(Pot2, 'C', 100),
    ?assertEqual(80, pot:total(Pot3#pot.current)),
    ?assertEqual(true, is_member(Pot3#pot.current, 'A')),
    ?assertEqual(true, is_member(Pot3#pot.current, 'C')),
    ?assertEqual(false, is_member(Pot3#pot.current, 'B')),
    ?assertEqual(180, pot:total(hd(Pot3#pot.active))),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'A')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'B')),
    ?assertEqual(true, is_member(hd(Pot3#pot.active), 'C')).

all_in_example8_test() ->
    Pot = pot:new(),
    { Pot1, 0 } = pot:add_bet(Pot, 'A', 100),
    { Pot2, 0 } = pot:add_bet(Pot1, 'B', 60, true),
    { Pot3, 0 } = pot:add_bet(Pot2, 'C', 100),
    { Pot4, 0 } = pot:add_bet(Pot3, 'D', 500),
    { Pot5, 0 } = pot:add_bet(Pot4, 'A', 250, true),
    { Pot6, 0 } = pot:add_bet(Pot5, 'C', 400),
    %% there's a main pot between all 4 players
    Side1 = lists:nth(1, Pot6#pot.active),
    ?assertEqual(240, pot:total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'C')),
    ?assertEqual(true, is_member(Side1, 'D')),
    %% there's a side pot between a, c and d
    Side2 = lists:nth(2, Pot6#pot.active),
    ?assertEqual(870, pot:total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(true, is_member(Side2, 'C')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'B')),
    %% there's another side pot between c and d
    Side3 = Pot6#pot.current,
    ?assertEqual(300, pot:total(Side3)),
    ?assertEqual(true, is_member(Side3, 'C')),
    ?assertEqual(true, is_member(Side3, 'D')),
    ?assertEqual(false, is_member(Side3, 'A')),
    ?assertEqual(false, is_member(Side3, 'B')).

all_in_example9_test() ->
    Pot = pot:new(),
    { Pot1, 0 } = pot:add_bet(Pot, 'A', 10),
    { Pot2, 0 } = pot:add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = pot:add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = pot:add_bet(Pot3, 'D', 20),
    { Pot5, 0 } = pot:add_bet(Pot4, 'A', 10),
    { Pot6, 0 } = pot:add_bet(Pot5, 'B', 20),
    { Pot7, 0 } = pot:add_bet(Pot6, 'D', 10),
	
	
    %% player-a folds but is still
    %% member of the last side pot
    Side = lists:last(Pot7#pot.active),
    ?assertEqual(28, pot:total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side1 = Pot7#pot.current,
    ?assertEqual(59, pot:total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'D')),
    ?assertEqual(false, is_member(Side1, 'C')).

all_in_example10_test() ->
    Pot = pot:new(),
    { Pot1, 0 } = pot:add_bet(Pot, 'A', 10),
    { Pot2, 0 } = pot:add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = pot:add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = pot:add_bet(Pot3, 'D', 20),
    { Pot5, 0 } = pot:add_bet(Pot4, 'A', 2, true),
    { Pot6, 0 } = pot:add_bet(Pot5, 'B', 20),
    { Pot7, 0 } = pot:add_bet(Pot6, 'D', 10),
	
	
	
    Side = lists:nth(1, Pot7#pot.active),
    ?assertEqual(28, pot:total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side1 = lists:nth(2, Pot7#pot.active),
    ?assertEqual(15, pot:total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'D')),
    ?assertEqual(false, is_member(Side1, 'C')),
    Side2 = Pot7#pot.current,
    ?assertEqual(36, pot:total(Side2)),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'C')).

all_in_example11_test() ->
    Pot = pot:new(),
    { Pot1, 0 } = pot:add_bet(Pot, 'A', 5, true),
    { Pot2, 0 } = pot:add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = pot:add_bet(Pot2, 'C', 8, true),
    { Pot4, 0 } = pot:add_bet(Pot3, 'D', 10),
	
	
    Side = lists:nth(1, Pot4#pot.active),
    ?assertEqual(20, pot:total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side1 = lists:nth(2, Pot4#pot.active),
    ?assertEqual(9, pot:total(Side1)),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'C')),
    ?assertEqual(true, is_member(Side1, 'D')),
    ?assertEqual(false, is_member(Side1, 'A')),
    Side2 = Pot4#pot.current,
    ?assertEqual(4, pot:total(Side2)),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'C')).

all_in_example12_test() ->
    Pot = pot:new(),
    { Pot1, 0 } = pot:add_bet(Pot, 'A', 10),
    { Pot2, 0 } = pot:add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = pot:add_bet(Pot2, 'C', 7, true),
    { Pot4, 0 } = pot:add_bet(Pot3, 'D', 10),
	
    Side = lists:last(Pot4#pot.active),
    ?assertEqual(28, pot:total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(true, is_member(Side, 'D')),
    Side2 = Pot4#pot.current,
    ?assertEqual(9, pot:total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(true, is_member(Side2, 'D')),
    ?assertEqual(false, is_member(Side2, 'C')).

all_in_example13_test() ->
    Pot = pot:new(),
    { Pot1, 0 } = pot:add_bet(Pot, 'A', 10,true),
    { Pot2, 0 } = pot:add_bet(Pot1, 'B', 10,false),
	
    Side = lists:last(Pot2#pot.active),
    ?assertEqual(20, pot:total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    Side2 = Pot2#pot.current,
    ?assertEqual(0, pot:total(Side2)),
    ?assertEqual(false, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'B')).

folding_main_pot_test() ->
	Pot = pot:new(),
    { Pot1, 0 } = pot:add_bet(Pot, 'A', 10),
    { Pot2, 0 } = pot:add_bet(Pot1, 'B', 10),
    { Pot3, 0 } = pot:add_bet(Pot2, 'C', 10),
    { Pot4, 0 } = pot:add_bet(Pot3, 'A', 5),
    { Pot5, 0 } = pot:add_bet(Pot4, 'C', 3, true),
	
    Side = lists:last(Pot5#pot.active),
    ?assertEqual(36, pot:total(Side)),
	
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(13, pot:bet_amount(Side,'A')),

	?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(10, pot:bet_amount(Side,'B')),
	
    ?assertEqual(true, is_member(Side, 'C')),
    ?assertEqual(13, pot:bet_amount(Side,'C')),
	
	Side2 = Pot5#pot.current,
    ?assertEqual(2, pot:total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'B')),
    ?assertEqual(false, is_member(Side2, 'C')).
	
pending_test() ->
    Pot = pot:new(),
	Pot1 = pot:pend(Pot, 'A', 10, false),
    Pot2 = pot:pend(Pot1, 'B', 8, true),
    Pot3 = pot:pend(Pot2, 'C', 9, true),
    Pot4 = pot:pend(Pot3, 'D', 6, true),
    Pot5 = pot:pend(Pot4, 'A', 10,false),
	
	Pendings = Pot5#pot.pending,
	Pendings1 = gb_trees:to_list(Pendings),
	 ?assertEqual(4, length(Pendings1)),
	 ?assertEqual({'A',{20,false}}, lists:nth(1, Pendings1)),
	 ?assertEqual({'B', {8, true}}, lists:nth(2, Pendings1)),
	 ?assertEqual({'C', {9, true}}, lists:nth(3, Pendings1)),
	 ?assertEqual({'D', {6, true}}, lists:nth(4, Pendings1)).

player_pending_amount_test() ->
    Pot = pot:new(),
	Pot1 = pot:pend(Pot, 'A', 10, false),
    Pot2 = pot:pend(Pot1, 'B', 8, true),
    Pot3 = pot:pend(Pot2, 'C', 9, true),
    Pot4 = pot:pend(Pot3, 'D', 6, true),
    Pot5 = pot:pend(Pot4, 'A', 10,false),
	
	?assertEqual(20, pot:player_pending(Pot5,'A')),
	?assertEqual(8, pot:player_pending(Pot5,'B')),
	?assertEqual(9, pot:player_pending(Pot5,'C')),
	?assertEqual(6, pot:player_pending(Pot5,'D')).

sort_pending_test() ->
	Pot = pot:new(),
	Pot1 = pot:pend(Pot, 'A', 10, false),
    Pot2 = pot:pend(Pot1, 'B', 8, true),
    Pot3 = pot:pend(Pot2, 'C', 9, true),
    Pot4 = pot:pend(Pot3, 'D', 6, true),
    Pot5 = pot:pend(Pot4, 'E', 8, false),
    Pot6 = pot:pend(Pot5, 'A', 10,false),

	Sorted = pot:sort_pending(Pot6#pot.pending),
	?assertEqual(5, length(Sorted)),
	?assertEqual({'D', {6, true}}, lists:nth(1, Sorted)),
	?assertEqual({'E', {8, false}}, lists:nth(2, Sorted)),
	?assertEqual({'B', {8, true}}, lists:nth(3, Sorted)),
	?assertEqual({'C', {9, true}}, lists:nth(4, Sorted)),
	?assertEqual({'A', {20, false}}, lists:nth(5, Sorted)).
	
	
multiple_all_in_with_same_amount_test() ->
    Pot = pot:new(),
    { Pot1, 0 } = pot:add_bet(Pot, 'A', 10),
    { Pot2, 0 } = pot:add_bet(Pot1, 'B', 8, true),
    { Pot3, 0 } = pot:add_bet(Pot2, 'C', 8, true),
    ?assertEqual(0, length(Pot3#pot.inactive)),
    ?assertEqual(1, length(Pot3#pot.active)),
	
    Side = lists:nth(1, Pot3#pot.active),
    ?assertEqual(24, pot:total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
	
    Side2 = Pot3#pot.current,
    ?assertEqual(2, pot:total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'B')),
    ?assertEqual(false, is_member(Side2, 'C')).

from_pend_to_pot_with_allin_amount_getting_smaller_test() ->
    Pot = pot:new(),
    Pot1 = pot:pend(Pot, 'A', 10,false),
    Pot2 = pot:pend(Pot1, 'B', 8, true),
    Pot3 = pot:pend(Pot2, 'C', 7, true),
	Pot4 = pot:from_pend_to_pot(Pot3),
	
	?assertEqual(0, length(Pot4#pot.inactive)),
    ?assertEqual(2, length(Pot4#pot.active)),

    Side = lists:nth(1, Pot4#pot.active),
    ?assertEqual(21, pot:total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
	
    Side1 = lists:nth(2, Pot4#pot.active),
    ?assertEqual(2, pot:total(Side1)),
    ?assertEqual(true, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(false, is_member(Side1, 'C')),

    Side2 = Pot4#pot.current,
    ?assertEqual(2, pot:total(Side2)),
    ?assertEqual(true, is_member(Side2, 'A')),
    ?assertEqual(false, is_member(Side2, 'B')),
    ?assertEqual(false, is_member(Side2, 'C')).

from_pend_to_pot_with_allin_amount_getting_smaller2_test() ->
    Pot = pot:new(),
    Pot1 = pot:pend(Pot, 'A', 5,true),
    Pot2 = pot:pend(Pot1, 'B', 8, true),
    Pot3 = pot:pend(Pot2, 'C', 7, true),
	Pot4 = pot:from_pend_to_pot(Pot3),
	
	?assertEqual(0, length(Pot4#pot.inactive)),
    ?assertEqual(3, length(Pot4#pot.active)),

    Side = lists:nth(1, Pot4#pot.active),
    ?assertEqual(15, pot:total(Side)),
    ?assertEqual(true, is_member(Side, 'A')),
    ?assertEqual(true, is_member(Side, 'B')),
    ?assertEqual(true, is_member(Side, 'C')),
	
    Side1 = lists:nth(2, Pot4#pot.active),
    ?assertEqual(4, pot:total(Side1)),
    ?assertEqual(false, is_member(Side1, 'A')),
    ?assertEqual(true, is_member(Side1, 'B')),
    ?assertEqual(true, is_member(Side1, 'C')),

    Side2 = lists:last(Pot4#pot.active),
    ?assertEqual(1, pot:total(Side2)),
    ?assertEqual(false, is_member(Side2, 'A')),
    ?assertEqual(true, is_member(Side2, 'B')),
    ?assertEqual(false, is_member(Side2, 'C')),

    Side3 = Pot4#pot.current,
    ?assertEqual(0, pot:total(Side3)),
    ?assertEqual(false, is_member(Side3, 'A')),
    ?assertEqual(false, is_member(Side3, 'B')),
    ?assertEqual(false, is_member(Side3, 'C')).
	
winner_test() ->
	Pot = pot:new(),
    Pot1 = pot:pend(Pot, 'A', 5,true),
    Pot2 = pot:pend(Pot1, 'B', 8, true),
    Pot3 = pot:pend(Pot2, 'C', 7, true),
	Pot4 = pot:from_pend_to_pot(Pot3),
	
	Winners = pot:pots(Pot4),
 	
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
