-module(sessiond).
-include("include/amqp_client.hrl").
-export([start/0, start_queue/0, start_web/0, webloop/1]).
-export([open_session_store/0, close_session_store/0]).
-export([route/2]).
-export([create_session/1, renew_session/1, live_session/1, kill_session/1]).


% {session, SessionID, UserID, Expiration}

-define(WEBLOOP, {?MODULE, webloop}).

start() ->
	open_session_store(),
	start_queue(),
	start_web().

start_queue() ->
	Params = #amqp_params{},
	Conn = amqp_connection:start_network(Params),
	Channel = amqp_connection:open_channel(Conn),

	QueueName = <<"sessiond">>,

	QueueDeclare = #'queue.declare'{queue = QueueName},
	#'queue.declare_ok'{queue = Queue
		, message_count = _MessageCount
		, consumer_count = _ConsumerCount}
			= amqp_channel:call(Channel, QueueDeclare),
	%Queue = amqp_channel:call(Channel, QueueDeclare),

	ExchangeDeclare = #'exchange.declare'{exchange=QueueName},
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

	QueueBind = #'queue.bind'{queue = Queue
		, exchange = QueueName},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

	Child = spawn(fun queue_loop_stub/0),
	BasicConsumer = #'basic.consume'{queue = Queue
		, consumer_tag = <<"">>},
	#'basic.consume_ok'{consumer_tag = ConsumerTag}
		= amqp_channel:subscribe(Channel, BasicConsumer, Child),

	io:format("~p~n", [Queue]).

queue_loop_stub() ->
	receive
		#'basic.consume_ok'{consumer_tag = ConsumerTag} -> ok
	end,
	queue_loop().


queue_loop() ->
	receive
		{#'basic.deliver'{consumer_tag = ConsumerTag
				, delivery_tag = DeliveryTag
				, redelivered = Redelivered
				, exchange = Exchange
				, routing_key = RoutingKey}
				, Content} ->
			io:format("Message: ~p", [Content]),
			queue_loop();
		Any ->
			io:format("received unexpected Any: ~p", [Any]),
			queue_loop()
	end.


start_web() ->
	Options = [{name, ?MODULE}, {loop, ?WEBLOOP}, {port, 8443}],
	mochiweb_http:start(Options).

webloop(Req) ->
	Method = Req:get(method),
	Params = case Method of
		'GET'  -> Req:parse_qs();
		'POST' -> Req:parse_post();
		_ -> []
	end,
	Path = Req:get(path),
	Result = route(Path, Params),
	Body = mochijson:encode(Result),
	Req:ok({value, Body}).

route("/create", Params) ->
	{"userid", UserID} = proplists:lookup("userid", Params),
	{ok, SessionID} = create_session(UserID),
	{struct, [{"sessionid", SessionID}]};
route("/renew", Params) ->
	{"sessionid", SessionID} = proplists:lookup("sessionid", Params),
	case renew_session(SessionID) of
		{live, UserID} -> {struct, [{live, true}, {userid, UserID}]};
		{dead, _} -> {struct, [{live, false}]}
	end;
route("/live", Params) ->
	{"sessionid", SessionID} = proplists:lookup("sessionid", Params),
	{ok, Live} = live_session(SessionID),
	{struct, [{live, Live}]};
route("/kill", Params) ->
	{"sessionid", SessionID} = proplists:lookup("sessionid", Params),
	{struct, [{killed, kill_session(SessionID)}]};
route(_Other, _Params) ->
	{404, "404 Resource not found"}.



make_session_id(UserID) ->
	"session" ++ UserID.

expiration_time() ->
	Timeout = 20 * 60,
	calendar:datetime_to_gregorian_seconds(erlang:universaltime())+Timeout.
expired(Expiration) ->
	Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
	Now > Expiration.


% Create a new session
create_session(UserID) ->
	SessionID = make_session_id(UserID),
	store_session(SessionID, UserID),
	{ok, SessionID}.

live_session(SessionID) ->
	{State, _UserID} = session_state(SessionID),
	case State of
		live -> {ok, true};
		dead -> {ok, false};
		true -> erlang:display("State not live or dead")
	end.

renew_session(SessionID) ->
	{State, UserID} = session_state(SessionID),
	case {State, UserID} of
		{live, _} -> store_session(SessionID, UserID);
		{dead, nil} -> nil;
		{dead, _} -> kill_session(SessionID)
	end,
	{State, UserID}.

kill_session(SessionID) ->
	{ok, Deleted} = live_session(SessionID),
	ets:delete(session, SessionID),
	Deleted.


store_session(SessionID, UserID) ->
	SessionObject = {SessionID, UserID, expiration_time()},
	ets:insert(session, {SessionID, SessionObject}).

session_state(SessionID) ->
	case ets:lookup(session, SessionID) of
		[] ->
			{dead, nil};
		[{SessionID, {SessionID, UserID, Expiration}}] ->
			case expired(Expiration) of
				true -> {dead, UserID};
				false -> {live, UserID}
			end;
		true ->
			erlang:display("bad session state")
	end.


% Open the session store table
% Open it as a named table called "session"
open_session_store() ->
	ets:new(session, [public, named_table, set]).

% Close the session table
close_session_store() ->
	ets:delete(session).

