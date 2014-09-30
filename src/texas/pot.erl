-module(pot).

-export([new/0, reset/1, new_stage/1,
         pend/4, pots/1, total/1, player_pending/2, new_side_pot/0 ]).


-export([add_bet/3, bet_amount/2, add_side_pot/3, new_side_pot/1, add_bet/4,split/3,sort_pending/1,from_pend_to_pot/1 ]). % For test

%% -ifdef(TEST).
%%    -include_lib("eunit/include/eunit.hrl").
%% -endif.

-include("pot.hrl").

%% Questions
%% 1. What if A bet 10, B all in 8, and A fold ? B just win 8, A lose 10?
%% 2. What if A bet 10, B allin 8, C allin 5? how many side pots?

new_side_pot(AllInAmt, Members) ->
    #side_pot{ 
              all_in = AllInAmt, 
              members = Members
             }.

new_side_pot(AllInAmt) 
  when is_number(AllInAmt) ->
    new_side_pot(AllInAmt, gb_trees:empty()).

new_side_pot() ->
    new_side_pot(0).

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