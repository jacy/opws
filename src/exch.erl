-module(exch).
-behaviour(gen_server).

-export([behaviour_info/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/1 ]).

-include("common.hrl").
-include("pp.hrl").


-record(exch, {
		  callback,
          data, % #game{}
          note,
          modules,
          stack,
          state,
          ctx
         }).

behaviour_info(callbacks) ->
    [{start, 1},
     {modules, 1},
     {context, 0},
     {stop, 1}, 
     {call, 2},
     {cast, 3}].

start([R= #start_game{id=GID, cbk=Cbk}]) ->
	?LOG({start_game, R}),
    gen_server:start({global, {Cbk, GID}}, ?MODULE, [R], []).

init(Args = [#start_game{cbk=Cbk}]) ->
    process_flag(trap_exit, true),
    {Data, Start} = Cbk:start(Args),
	Modules = Cbk:modules(Args),
    Exch = #exch{
      data = Data,
      modules = Modules,
	  callback= Cbk,
      stack = Modules,
      ctx = Cbk:context()
     },
    case fsm_init(Exch, Start) of
        {stop, _, Exch1} ->
            {stop, Exch1};
        {noreply, Exch1} ->
            {ok, Exch1}
    end.

terminate(Reason, Exch= #exch{callback=Cbk}) ->
  ?LOG([{exch_terminate, {reason, Reason}, {exch, Exch}}]),
  Cbk:stop(Exch#exch.data),
  ok.

handle_cast({'NOTE', Note}, Exch) ->
    {noreply, Exch#exch{ note = Note }};

handle_cast(stop, Exch) ->
  ?LOG([{exch_terminate, handle_stop}]),
  {stop, normal, Exch};

handle_cast(Event, Exch) ->
    process_cast(Event, Exch).

handle_call(Event, _From, Exch) ->
    {reply, process_call(Event, Exch), Exch}.

%% handle_info({timeout, _Ref, none}, Exch) 
handle_info(Event, Exch) ->
    process_cast(Event, Exch).

code_change(_OldVsn, Exch, _Extra) ->
    {ok, Exch}.

process_call(Event, Exch= #exch{callback=Cbk}) ->
    Cbk:call(Event, Exch#exch.data).

process_cast(Event, Exch) ->
  {Mod, _} = hd(Exch#exch.stack),
  State = Exch#exch.state,
  Data = Exch#exch.data,
  Ctx = Exch#exch.ctx,

  Result = Mod:State(Data, Ctx, Event),
  case Event of
	{timeout,_,_} -> ok;
	_ -> ?FLOG("Executed Mod=~w, State=~w, Event=~w, Result=~w ~n",[Mod,State,Event,Result])
	end,
   advance(Exch, Event, Result).

fsm_init(Exch = #exch{ stack = [{Mod, Params}|_] }, Event) ->
    Ctx = Exch#exch.ctx,
    Exch1 = Exch#exch{state = none },
    Result = Mod:start(Exch1#exch.data, Ctx, Params),
	case Event of
		{timeout,_,_} -> ok;
		_ -> ?FLOG("	Fsm init Mod=~w, Result=~w ~n",[Mod,Result])
	end,
    advance(Exch1, Event, Result).

advance(Exch = #exch{}, _, {next, NextState, Data, Ctx}) ->
    {noreply, Exch#exch{ state = NextState, data = Data, ctx = Ctx }};

advance(Exch= #exch{callback=Cbk}, Event, {skip, Data, Ctx}) ->
  {noreply, Exch#exch{ data = Cbk:cast(Event, Ctx, Data)}};

%% stop game server for irc games.
advance(Exch = #exch{ stack = [_LastMod] }, _, {stop, Data, _Ctx}) ->
    {stop, normal, Exch#exch{ data = Data, stack = [] }};

advance(Exch = #exch{ stack = [_|NextMod] }, Event, {stop, Data, Ctx}) ->
    Exch1 = Exch#exch{ data = Data, ctx = Ctx, stack = NextMod },
    fsm_init(Exch1, Event);

advance(Exch = #exch{}, _, {continue, Data, Ctx}) ->
    {noreply, Exch#exch{ data = Data, ctx = Ctx }};

advance(Exch = #exch{}, Event, {goto, top, Data, _}) ->
    Exch1 = Exch#exch{ data = Data, stack = Exch#exch.modules },
    fsm_init(Exch1, Event);

advance(Exch = #exch{}, Event, {goto, Mod, Data, _}) ->
    Stack = trim_stack(Mod, Exch#exch.stack),
    Exch1 = Exch#exch{ data = Data, stack = Stack },
    fsm_init(Exch1, Event);

advance(_, Event, Result) ->
    ?ERROR([unknow_event, {event, Event}, {result, Result}]),
    {noreply, none}.

trim_stack(Mod, L = [{H, _}|_]) 
  when Mod == H ->
    L;

trim_stack(Mod, [_|T]) ->
    trim_stack(Mod, T).
