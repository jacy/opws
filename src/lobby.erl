-module(lobby).
-behaviour(gen_server).


-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([start/1, start/2, start/3, stop/1]).

-include("common.hrl").
-include("pp.hrl").

-record(server, {
          port,
          host,
          test_mode
         }).

-record(client, {
          server = none,
          player = none
         }).

start([Port, Host])
  when is_atom(Port),
       is_atom(Host) ->
    Port1 = list_to_integer(atom_to_list(Port)),
    Host1 = atom_to_list(Host),
    start(Host1, Port1).

start(Host, Port) ->
    start(Host, Port, false).

start(Host, Port, TestMode) ->
    case gen_server:start(?MODULE, [Host, Port, TestMode], []) of
        {ok, Pid} ->
            pg2:create(?GAME_SERVERS),
            ok = pg2:join(?GAME_SERVERS, Pid),
            {ok, Pid};
        Result ->
			?ERROR([{start_lobby_faild, {unknow_result, Result}}]),
            Result
    end.

init([Host, Port, TestMode]) -> %% {{{ gen_server callback
    process_flag(trap_exit, true), 
    F = fun(Socket, Packet, LoopData) -> 
        case LoopData of
          none ->
            parse_packet(Socket, Packet, #client{ server = self()});
          {loop_data, Client} ->
            parse_packet(Socket, Packet, Client)
        end
    end, 
    mochiweb_websocket:stop(Port),
    {ok, _} = mochiweb_websocket:start(Host, Port, F),
    Server = #server{
      host = Host,
      port = Port,
      test_mode = TestMode
     },
    {ok, Server}.

stop(Server) ->
    gen_server:cast(Server, stop).

terminate(normal, Server) ->
    mochiweb_websocket:stop(Server#server.port),
    ok.

handle_cast({'BUMP', Size}, Server) ->
    stats:sum(packets_in, 1),
    stats:sum(bytes_in, Size),
    {noreply, Server};

handle_cast({'PONG', R = #pong{}}, Server) ->
    TC = timer:now_diff(R#pong.send_time, R#pong.orig_send_time),
    TS = timer:now_diff(R#pong.recv_time, R#pong.send_time),
    stats:avg(time_to_client, TC),
    stats:avg(time_to_server, TS),
    stats:max(max_time_to_client, TC),
    stats:max(max_time_to_server, TS),
    {noreply, Server};

handle_cast(stop, Server) ->
    {stop, normal, Server};

handle_cast(Event, Server) ->
    ?LOG([{handle_cast, {message, Event}}]),
    {noreply, Server}.


handle_call('WHERE', _From, Server) ->
    {reply, {Server#server.host, Server#server.port}, Server};

handle_call('USER COUNT', _From, Server) ->
    Children = tcp_server:children(Server#server.port),
    {reply, length(Children), Server};

handle_call('TEST MODE', _From, Server) ->
    {reply, Server#server.test_mode, Server};

handle_call(Event, From, Server) ->
    ?LOG([{handle_call, {from, From}, {message, Event}}]),
    {noreply, Server}.

handle_info({'EXIT', Pid, Reason}, Server) ->
  ?LOG([{server_error, {pid, Pid}, {reason, Reason}}]),
  {noreply, Server};

handle_info(Info, Server) ->
    ?LOG([{handle_info, Info}]),
    {noreply, Server}.

code_change(_OldVsn, Server, _Extra) ->
  {ok, Server}. %% }}}

process_login(Client, Socket, Usr, Pass) ->
  case login:login(Usr, Pass, self()) of
    {error, Error} ->
      ok = ?tcpsend(Socket, #bad{ cmd = ?CMD_LOGIN, error = Error}),
      Client;
    {ok, Player} ->
      if
        Client#client.player /= none ->
          %% disconnect visitor
          gen_server:cast(Client#client.player, 'DISCONNECT');
        true ->
          ok
      end,
      PID = gen_server:call(Player, 'ID'),
      ok = ?tcpsend(Socket, #you_are{ player = PID }),
      Client#client{ player = Player }
  end.

process_logout(Client, _Socket) ->
    gen_server:cast(Client#client.player, #logout{}),
    %% replace player process with a visitor
    {ok, Visitor} = visitor:start(self()),
    Client#client{ player = Visitor }.

process_ping(Client, Socket, R) ->
    ok = ?tcpsend(Socket, #pong{ orig_send_time = R#ping.send_time }),
    Client.

process_pong(Client, _Socket, R) ->
    R1 = R#pong{ recv_time = now() },
    gen_server:cast(Client#client.server, {'PONG', R1}),
    Client.

process_test_start_game(Client, Socket, R) ->
    case gen_server:call(Client#client.server, 'TEST MODE') of
        true ->
            ok = ?tcpsend(Socket, start_test_game(R));
        _ ->
            ok
    end,
    Client.

process_game_query(Client, Socket, Q) when is_record(Q, game_query) ->
    find_games(Socket, 
               Q#game_query.game_type, 
               Q#game_query.limit_type,
               Q#game_query.expected,
               Q#game_query.joined,
               Q#game_query.waiting),
    Client.

process_event(Client, _Socket, Event) ->
  if 
    Client#client.player == none ->
      %% start a proxy
      {ok, Visitor} = visitor:start(self()),
      Client1 = Client#client{ player = Visitor };
    true ->
      Client1 = Client
  end,
  gen_server:cast(Client1#client.player, Event),
  Client1. %% }}}

parse_packet(Socket, tcp_closed, Client) ->
  process_logout(Client, Socket);

parse_packet(Socket, {packet, Packet}, Client) ->
  ok = ?tcpsend(Socket, Packet),
  {loop_data, Client};

parse_packet(Socket, {socket, Packet}, Client) ->
  gen_server:cast(Client#client.server, {'BUMP', size(Packet)}),
  Data = (catch pp:read(Packet)),
  ?LOG([{receive_packet, {packet, Packet, Data}}]),
  Client1 = case Data of 
    {'EXIT', Error} ->
	  ?ERROR([{parse_command_faild, {error, Error}}]),
      Client;
    #login{ usr = Usr, pass = Pass} ->
      process_login(Client, Socket, Usr, Pass);
    #logout{} ->
      process_logout(Client, Socket);
    R = #ping{} ->
      process_ping(Client, Socket, R);
    R = #pong{} ->
      process_pong(Client, Socket, R);
    R = #start_game{ rigged_deck = [_|_] } ->
      process_test_start_game(Client, Socket, R);
    R = #game_query{} ->
      process_game_query(Client, Socket, R);
    Event ->
      process_event(Client, Socket, Event)
  end,
  {loop_data, Client1};

parse_packet(_Socket, Event, Client) ->
  ?LOG([{parse_packet, {event, Event}}]),
  {loop_data, Client}.

send_games(_, [], _) ->
    ok;

send_games(Socket, [H|T], C) ->
    N = H#game_info{game_count = C},
    ?tcpsend(Socket, N),
    send_games(Socket, T, C).

find_games(Socket, 
           GameType, LimitType,
           #query_op{ op = ExpOp, val = Expected }, 
           #query_op{ op = JoinOp, val = Joined },
           #query_op{ op = WaitOp, val = Waiting }) ->
    {atomic, L} = g:find(GameType, LimitType,
                         ExpOp, Expected, 
                         JoinOp, Joined,
                         WaitOp, Waiting),
    
    send_games(Socket, L, lists:flatlength(L)).

start_test_game(R) ->
    {ok, _} = game:start(R),
    #your_game{ game = R#start_game.id }.