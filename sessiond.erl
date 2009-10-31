-module(sessiond).
-export([start/0, serve/3]).


% {session, SessionID, UserID, Expiration}

start() ->
	{spawn(fun server_loop/0), ets:new(session, [set])}.

serve({Server, Store}, Command, Param) ->
	Server ! {self(), Store, {Command, Param}},
	receive
		{Server, Result} ->
			Result
	end.

create_session_id(UserID) ->
	UserID.


% Create a new session
create_session(Store, UserID) ->
	"session" ++ UserID.

live_session(SessionID) ->
	SessionID == SessionID.

renew_session(SessionID) ->
	SessionID.

kill_session(SessionID) ->
	SessionID.


server_loop() ->
	receive
		{Client, Store, {create, UserID}} ->
			Client ! {self(), create_session(Store, UserID)},
			server_loop;
		{Client, {live, SessionID}} ->
			Client ! {self(), live_session(SessionID)},
			server_loop;
		{From, {renew, SessionID}} ->
			From ! {self(), renew_session(SessionID)},
			server_loop;
		{From, {'kill', SessionID}} ->
			From ! {self(), kill_session(SessionID)},
			server_loop;
		{From, {set_store, NewStore}} ->
			Store = NewStore,
			server_loop;
		{From, Other} ->
			From ! {self(), {error, Other}},
			server_loop
	end.

