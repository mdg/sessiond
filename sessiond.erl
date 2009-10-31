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

% Create a new session
create_session(Store, UserID) ->
	{ok, "session" ++ UserID}.

live_session(SessionID) ->
	SessionID == SessionID.

renew_session(SessionID) ->
	SessionID.

kill_session(SessionID) ->
	SessionID.



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
		{Client, {live, SessionID}} ->
			Client ! {self(), live_session(SessionID)},
			server_loop();
		{From, {renew, SessionID}} ->
			From ! {self(), renew_session(SessionID)},
			server_loop();
		{From, {'kill', SessionID}} ->
			From ! {self(), kill_session(SessionID)},
			server_loop();
		{From, Other} ->
			From ! {self(), {error, Other}},
			server_loop()
	end.

