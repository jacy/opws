-module(exch, [Cbk, Context, Modules]). % Cbk meaning Callbacks, it refers to a specified module which implemns to this behaviour.
-behaviour(gen_server).

%%%
%%% A stack of game modules
%%%

-export([behaviour_info/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/1, stop/1, cast/2, call/2]).

-include("common.hrl").
-include("pp.hrl").
-include_lib("eunit/include/eunit.hrl").


-record(exch, {
          data, % #game{}
          note,
          %% fsm 
          modules, % original modules, not changing it so can reset the stacks 
          stack, % real time modules info, when modules finished, got removed
          state, % state of current module
          ctx
         }).

behaviour_info(callbacks) ->
    [{start, 1},  % call on init
     {stop, 1}, 
     {dispatch, 2},
     {call, 2},
     {cast, 2}].

%%%
%%% API
%%%

start([R= #start_game{id=GID}]) ->
    gen_server:start({global, {Cbk, GID}}, THIS, [R], []).

stop(Pid)
  when is_pid(Pid) ->
    gen_server:cast(Pid, stop);

stop(ID)
  when is_number(ID) ->
    gen_server:cast({global, {Cbk, ID}}, stop).

cast(Exch, Event) ->
    gen_server:cast(Exch, Event).

call(Exch, Event) ->
    gen_server:call(Exch, Event).

%%%
%%% Implementation
%%%

init(Args) ->
    process_flag(trap_exit, true),
    {Data, Start} = Cbk:start(Args),
    Exch = #exch{
      data = Data,
      modules = Modules,
      stack = Modules,
      ctx = Context
     },
    case fsm_init(Exch, Start) of
        {stop, _, Exch1} ->
            {stop, Exch1};
        {noreply, Exch1} ->
            {ok, Exch1}
    end.

terminate(Reason, Exch) ->
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

%%% {timeout, _, _} at the moment
%% handle_info({timeout, _Ref, none}, Exch) 
handle_info(Event, Exch) ->
    process_cast(Event, Exch).

code_change(_OldVsn, Exch, _Extra) ->
    {ok, Exch}.

process_call(Event, Exch) ->
    Cbk:call(Event, Exch#exch.data).

process_cast(Event, Exch) ->
  {Mod, _} = hd(Exch#exch.stack),
  State = Exch#exch.state,
  Data = Exch#exch.data,
  Ctx = Exch#exch.ctx,

  Result = Mod:State(Data, Ctx, Event),
  case Event of
	{timeout,_,_} -> ok;
	_ -> io:format("Executed Mod=~w, State=~w, Event=~w, Result=~w ~n",[Mod,State,Event,Result])
	end,
   advance(Exch, Event, Result).

fsm_init(Exch = #exch{ stack = [{Mod, Params}|_] }, Event) ->
    Ctx = Exch#exch.ctx,
    Exch1 = Exch#exch{state = none },
    Result = Mod:start(Exch1#exch.data, Ctx, Params),
	case Event of
		{timeout,_,_} -> ok;
		_ -> io:format("	Fsm init Mod=~w, Result=~w ~n",[Mod,Result])
	end,
    advance(Exch1, Event, Result).

advance(Exch = #exch{}, _, {next, State, Data, Ctx}) ->
    %% advance to the next state
    {noreply, Exch#exch{ state = State, data = Data, ctx = Ctx }};

advance(Exch = #exch{}, Event, {skip, Data, Ctx}) ->
  {noreply, Exch#exch{ data = Cbk:cast(Event, Ctx, Data)}};

advance(Exch = #exch{ stack = [_|T] }, Event, {stop, Data, Ctx}) ->
    %% this module is done
    Exch1 = Exch#exch{ data = Data, ctx = Ctx, stack = T },
    fsm_init(Exch1, Event);

advance(Exch = #exch{}, _, {continue, Data, Ctx}) ->
    %% continue processing in this state
    {noreply, Exch#exch{ data = Data, ctx = Ctx }};

advance(Exch = #exch{}, Event, {goto, top, Data, _}) ->
    %% go to the top of the stack
    %% and start from the beginning
    Exch1 = Exch#exch{ data = Data, stack = Exch#exch.modules },
    fsm_init(Exch1, Event);

advance(Exch = #exch{}, Event, {goto, Mod, Data, _}) ->
    %% go to a different module
    Stack = trim_stack(Mod, Exch#exch.stack),
    Exch1 = Exch#exch{ data = Data, stack = Stack },
    fsm_init(Exch1, Event);

advance(_, Event, Result) ->
    error_logger:error_report([{module, ?MODULE}, 
                               {line, ?LINE},
                               {self, self()}, 
                               {event, Event}, 
                               {result, Result}
                              ]),
    {noreply, none}.

trim_stack(Mod, L = [{H, _}|_]) 
  when Mod == H ->
    L;

trim_stack(Mod, [_|T]) ->
    trim_stack(Mod, T).
