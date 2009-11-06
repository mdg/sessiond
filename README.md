# About
sessiond is an external store for web sessions

# Goals
 * Let web apps share sessions across servers
 * Let web servers be restarted without losing session

# How it Works
sessiond is a resty web service.  Please note that sessiond stores a logical
user login sessions and is separate from persisting the HTTP session on an
individual web server.

## Creating Sessions
When a user logs in, create a new session with the '/create' request.
This will return a new session id which should be stored in the browser as a
cookie.

## Checking Sessions
On subsequent web requests from the browser, check if the session is still
active with the '/renew' request.  This will return the user id for the
active session or nothing if the session has expired according to the rules in
sessiond.

The sessionid is stored in the browsers cookies and the session information
is stored in sessiond, external to the web server.  No session information is
expected to persist on the web server so the browser can switch web servers
without losing its session.

## Deleting Sessions
When a user logs out, remove the session with the '/kill' request.

# Dependencies
 * erlang

