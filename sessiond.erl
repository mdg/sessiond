-module(sessiond).
-export([start/0, serve/2]).


% {session, SessionID, UserID, Expiration}

start() ->
	spawn(fun run_server/0),
	yaws:start_embedded("yaws").

serve(Command, Param) ->
	{Server, Store} = find_server(),
	Server ! {self(), Store, {Command, Param}},
	receive
		{Server, Result} ->
			Result
	end.


make_session_id(UserID) ->
	"session" ++ UserID.

expiration_time() ->
	Timeout = 20 * 60,
	calendar:datetime_to_gregorian_seconds(erlang:universaltime())+Timeout.
expired(Expiration) ->
	Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
	Now > Expiration.


% Create a new session
create_session(Store, UserID) ->
	erlang:display(UserID),
	SessionID = make_session_id(UserID),
	store_session(Store, SessionID, UserID),
	{ok, SessionID}.

live_session(Store, SessionID) ->
	{State, _UserID} = session_state(Store, SessionID),
	case State of
		live -> {ok, true};
		dead -> {ok, false};
		true -> erlang:display("State not live or dead")
	end.

renew_session(Store, SessionID) ->
	{State, UserID} = session_state(Store, SessionID),
	case State of
		live -> store_session(Store, SessionID, UserID);
		dead -> true
	end,
	{ok, UserID}.

kill_session(Store, SessionID) ->
	Result = live_session(Store, SessionID),
	ets:delete(Store, SessionID),
	Result.


store_session(Store, SessionID, UserID) ->
	ets:insert(Store, {SessionID, {SessionID, UserID, expiration_time()}}).

session_state(Store, SessionID) ->
	case ets:lookup(Store, SessionID) of
		[] ->
			{dead, "-"};
		[{SessionID, {SessionID, UserID, Expiration}}] ->
			case expired(Expiration) of
				true -> {dead, "-"};
				false -> {live, UserID}
			end;
		true ->
			erlang:display("bad session state")
	end.




run_server() ->
	publish_server(),
	server_loop(),
	close_server().

publish_server() ->
	SessionStore = ets:new(session, [set]),
	Server = {self(), SessionStore},
	ServerStore = ets:new(server, [set, named_table]),
	ets:insert(ServerStore, {server, Server}).

find_server() ->
	[{server, Server}] = ets:lookup(server, server),
	Server.

close_server() ->
	{_ServerProc, SessionStore} = find_server(),
	ets:delete(SessionStore),
	ets:delete(server).

server_loop() ->
	receive
		{Client, Store, {create, UserID}} ->
			Client ! {self(), create_session(Store, UserID)},
			server_loop();
		{Client, Store, {live, SessionID}} ->
			Client ! {self(), live_session(Store, SessionID)},
			server_loop();
		{Client, Store, {renew, SessionID}} ->
			Client ! {self(), renew_session(Store, SessionID)},
			server_loop();
		{Client, Store, {'kill', SessionID}} ->
			Client ! {self(), kill_session(Store, SessionID)},
			server_loop();
		{Client, Other} ->
			Client ! {self(), {error, Other}},
			server_loop()
	end.

