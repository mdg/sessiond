# About
sessiond is an external store for web sessions

It is built around the premise that web applications are usually good at
authenticating, but not good at sharing that authentication.  Therefore,
sessiond attempts to implement Single Sign On by being a shared store of
session liveness while remaining ignorant of the actual authentication.

# Example
This example assumes the server is running at localhost, port 8000.

Create a new session for user abc.  Returns a new sessionid.
    $ curl http://localhost:8000/create.yaws?userid=abc
    {"ok":"sessionabc"}
Check that new session, it will return the original userid.
    $ curl http://localhost:8000/renew.yaws?sessionid=sessionabc
    {"ok":"abc"}
Simulate a log off and kill the session.  If there's no log off, don't worry, the session will expire at the timeout you set.
    $ curl http://localhost:8000/kill.yaws?sessionid=sessionabc
    {"ok":true}
Check it again, but see that - is returned indicating there is no session with that id.
    $ curl http://localhost:8000/renew.yaws?sessionid=sessionabc
    {"ok":"-"}

# Goals
 * Let web apps share sessions across servers
 * Let web servers be restarted without losing session

# How it Works
sessiond is a resty web service.  Please note that sessiond stores a logical
user login sessions and is separate from persisting the HTTP session on an
individual web server.

## Creating Sessions
When a user logs in, create a new session with the `/create` request.
This will return a new session id which should be stored in the browser as a
cookie.

## Checking Sessions
On subsequent web requests from the browser, check if the session is still
active with the `/renew` request.  This will return the user id for the
active session or nothing if the session has expired according to the rules in
sessiond.

The sessionid is stored in the browsers cookies and the session information
is stored in sessiond, external to the web server.  No session information is
expected to persist on the web server so the browser can switch web servers
without losing its session.

## Deleting Sessions
When a user logs out, remove the session with the `/kill` request.

# Dependencies
 * erlang

