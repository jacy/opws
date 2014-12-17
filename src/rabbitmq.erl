-module(rabbitmq).
-include("amqp_client.hrl").

-record(rabbitmq, {
	host
}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([connect/0]).


connect() ->
	Config = get_rabbitmq_configs(),
	{ok, Connection} = amqp_connection:start(#amqp_params_network{host = Config#rabbitmq.host}),
	%% Open a channel on the connection
	{ok, Channel} = amqp_connection:open_channel(Connection),
	
	%% Declare an exchange, a message can never be sent directly to the queue, it always needs to go through an exchange. 
	%% Durable exchanges only means that the exchange itself will still be there if you restart your broke,
	%% But it does not mean that any messages sent to that exchange are automatically persisted.
	 Exchange = #'exchange.declare'{exchange = <<"hand history">>, durable = true},
     #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
	
	%% Declare a queue
	%% Durable queue stores any messages that are routed to it even if there are no active consumers.
	Q = <<"hand history">>,
	#'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{durable=true, queue=Q}),
	
	%% Publish a message
	Payload = <<"foobar">>,
	% The queue name needs to be specified in the routing_key parameter
	Publish = #'basic.publish'{exchange = <<"Hand History">>, routing_key = Q},

    Props = #'P_basic'{delivery_mode = 2}, %% persistent message
    Msg = #amqp_msg{props = Props, payload = Payload},
    amqp_channel:cast(Channel, Publish, Msg).



%% ====================================================================
%% Internal functions
%% ====================================================================
get_rabbitmq_configs() ->
	Props = application:get_all_env(log_roller_server),
	#rabbitmq{
			host = proplists:get_value(rabbitmq, Props, localhost)
	}.