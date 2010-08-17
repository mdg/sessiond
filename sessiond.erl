-module(sessiond).
-include("include/amqp_client.hrl").
-export([start/0, start_queue/0, start_web/0, webloop/1]).
-export([open_session_store/0, close_session_store/0]).
-export([route/2]).
-export([create_session/1, renew_session/1, kill_session/1]).


% {session, SessionID, UserID, Expiration}

-define(WEBLOOP, {?MODULE, webloop}).

-record(session,
	{ sessionid
	, userid
	, timeout = 20 * 60 % 20 minutes * 60 seconds
	, expiration
	}).

-record(session_state,
	{ state = dead
	, live = false
	, expiration_delta
	}).


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
		, consumer_tag = <<"">>, no_ack = true},
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
			#amqp_msg{payload=Payload} = Content,
			% Route to the queue processor
			io:format("msg is ||~p||~n", [Payload]),
			ListPayload = binary_to_list(Payload),
			spawn(fun() -> route_from_queue(ListPayload) end),
			% Tail recurse
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
		{#session_state{state=live}, #session{userid=UserID}} ->
			{struct, [{live, true}, {userid, UserID}]};
		{#session_state{state=dead}, _} -> {struct, [{live, false}]}
	end;
route("/live", Params) ->
	{"sessionid", SessionID} = proplists:lookup("sessionid", Params),
	{#session_state{live=Live}=State, Session} = load_session(SessionID),
	case Live of
		true ->
			#session_state{expiration_delta=Remaining} = State,
			#session{expiration=Expiration} = Session,
			{struct, [{live, true}, {relative_exp, Remaining}
				, {absolute_exp, Expiration}]};
		false ->
			{struct, [{live, false}]}
	end;
route("/kill", Params) ->
	{"sessionid", SessionID} = proplists:lookup("sessionid", Params),
	{struct, [{killed, kill_session(SessionID)}]};
route(_Other, _Params) ->
	{404, "404 Resource not found"}.


route_from_queue("renew " ++ SessionID) ->
	renew_session(SessionID);
route_from_queue(_) ->
	erlang:error("Unknown Queue Command").


make_session_id(UserID) ->
	"session" ++ UserID.

expiration_time(default) ->
	expiration_time(20*60); % 20 minutes * 60 seconds
expiration_time(Timeout) ->
	calendar:datetime_to_gregorian_seconds(erlang:universaltime())+Timeout.


% Create a new session
create_session(UserID) ->
	SessionID = make_session_id(UserID),
	Timeout = 20 * 60,
	Session = #session{sessionid=SessionID, userid=UserID
		, timeout=Timeout, expiration=expiration_time(Timeout)},
	store_session(Session),
	{ok, SessionID}.

renew_session(SessionID) ->
	{State, Session} = load_session(SessionID),
	#session_state{state=DeadOrAlive} = State,
	case {DeadOrAlive, Session} of
		{live, #session{timeout=Timeout}} ->
			Expiration = expiration_time(Timeout),
			NewSession = Session#session{expiration=Expiration},
			store_session(NewSession);
		{dead, nil} -> nil;
		{dead, _} -> delete_session(SessionID)
	end,
	{State, Session}.

kill_session(SessionID) ->
	{#session_state{live=WasLive}=State, Session} = load_session(SessionID),
	delete_session(SessionID),
	WasLive.


make_session_state(#session{expiration=Expiration}) ->
	Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
	ExpirationDelta = Expiration - Now,
	case ExpirationDelta >= 0 of
		true -> #session_state{state=live
				, live=true
				, expiration_delta=ExpirationDelta};
		false -> #session_state{state=dead
				, live=false
				, expiration_delta=ExpirationDelta}
	end.


store_session(#session{sessionid=SessionID}=Session) ->
	ets:insert(session, {SessionID, Session}).

load_session(SessionID) ->
	case ets:lookup(session, SessionID) of
		[] ->
			{#session_state{}, nil};
		[{SessionID, Session}] ->
			{make_session_state(Session), Session};
		_Else ->
			erlang:display("bad session state")
	end.

delete_session(SessionID) ->
	ets:delete(session, SessionID).



% Open the session store table
% Open it as a named table called "session"
open_session_store() ->
	ets:new(session, [public, named_table, set]).

% Close the session table
close_session_store() ->
	ets:delete(session).

