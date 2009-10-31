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
	SessionID = "session" ++ UserID,
	NewSession = {SessionID, UserID, 0},
	ets:insert(Store, {SessionID, NewSession}),
	{ok, SessionID}.

live_session(Store, SessionID) ->
	Result = ets:lookup(Store, SessionID),
	{ok, is_live(Result)}.

is_live([]) ->
	false;
is_live([_Session]) ->
	true.

renew_session(Store, SessionID) ->
	{ok, SessionID}.

kill_session(Store, SessionID) ->
	{ok, SessionID == SessionID}.



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

