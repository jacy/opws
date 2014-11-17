-module(observer).

%%% 
%%% Observer bot. Reports game activity.
%%%

-export([start/1, stop/1, observe/2]).

-include("common.hrl").
-include("pp.hrl").

-record(obs, {
          id,
          trace,
          parent,
          gid,
          winners,
          seats,
          games_started,
          games_to_watch,
          cancel_count,
          stop,
          events
         }).

start([Parent, Trace, GamesToWatch]) ->
    Obs = #obs {
      trace = Trace,
      parent = Parent,
      winners = gb_trees:empty(),
      seats = gb_trees:empty(),
      games_started = 0,
      games_to_watch = GamesToWatch,
      cancel_count = -1,
      stop = false,
      events = []
     },
    {ok, observe, Obs}.

stop(_) ->
    ok.

observe(R, Data) ->
    Data1 = process(R, Data),
    if
        Data#obs.stop ->
            Next = stop,
            Events = Data#obs.events;
        true ->
            Next = continue,
            Events = []
    end,
    {Next, Data1, Events}.


%% Sometimes observers get to watch games after player already join
%% Both notify_seat_detail and notify_join are marking palyer join.
process(R = #notify_seat_detail{}, Data) ->
    Seats1 = gb_trees:enter(R#notify_seat_detail.player, 
                             R#notify_seat_detail.seat, 
                             Data#obs.seats),
    Data#obs{ seats = Seats1 };

process(R = #notify_join{}, Data) ->
    Seats1 = gb_trees:enter(R#notify_join.player, 
                             R#notify_join.seat, 
                             Data#obs.seats),
    Data#obs{ seats = Seats1 };

process(R = #notify_win{}, Data) ->
    Amt = R#notify_win.amount / 1.0,
    N = gb_trees:get(R#notify_win.player, Data#obs.seats),
	Old = case gb_trees:lookup(N, Data#obs.winners) of
      {value, Val} ->
          Val;
      none ->
          0
    end,
    Winners1 = gb_trees:enter(N, Amt+Old, Data#obs.winners),
    Data#obs{ winners = Winners1 };

process(R = #player_info{}, Data) ->
    PID = R#player_info.player, 
    Amount = gb_trees:get(PID, Data#obs.winners),
    T1 = gb_trees:delete(PID, Data#obs.winners),
    Winners1 = gb_trees:insert(R#player_info.nick, Amount, T1),
    Data#obs{ winners = Winners1 };

process(R = #notify_start_game{}, Data) ->
    GID = R#notify_start_game.game,
    Started = Data#obs.games_started, 
    Data#obs.parent ! {'START', GID},
    Data#obs{ winners = gb_trees:empty(), games_started = Started + 1 };

process(R = #notify_end_game{}, Data) ->
    GID = R#notify_end_game.game,
	?FLOG("Stoping parent:~w",[Data#obs.parent]),
	error_logger:info_msg("endgid:~p~n",  [GID]),
    Data#obs.parent ! {'END', GID, Data#obs.winners},
    N = Data#obs.games_to_watch,
    if 
        N == 1 ->
            Data#obs{ stop = true, events = [#unwatch{ game = GID }] };
        true ->
            Data#obs{ games_to_watch = N - 1 }
    end;

process(_, Data) ->
    Data.