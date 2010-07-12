-module(sessiond).
-export([start/0, webloop/1]).


% {session, SessionID, UserID, Expiration}

-define(WEBLOOP, {?MODULE, webloop}).

start() ->
	open_session_store(),
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
route(Other, Params) ->
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

