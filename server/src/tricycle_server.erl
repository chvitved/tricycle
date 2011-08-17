-module(tricycle_server).

-export([start_link/0]).
-export([init_listener/1, init_handler/1]).

-define(DEFAULT_PORT, 9193).

%%%---------- API ----------------------------------------
start_link() ->
    Port = default(application:get_env(tricycle, port), ?DEFAULT_PORT),
    proc_lib:start_link(?MODULE, init_listener, [Port]).

%%%---------- Implementation - listener -----------------------------
init_listener(Port) ->
    {ok,SrvSock} = gen_tcp:listen(Port, [binary,
					 {reuseaddr, true},
					 {active, false},
					 {packet, line}]),
    error_logger:info_msg("Listening on port ~p\n", [Port]),
    listener_loop(SrvSock).

listener_loop(SrvSock) ->
    case gen_tcp:accept(SrvSock) of
	{ok, Sock} ->
	    proc_lib:spawn(?MODULE, init_handler, [Sock]),
	    listener_loop(SrvSock);
	{error, Err} ->
	    error_logger:error_msg("Error from accept(): ~p -- stopping\n", [Err])
    end.


%%%---------- Implementation - client handler -------------------
init_handler(Sock) ->
    error_logger:info_msg("[~p] Incoming connection\n", [Sock]),
    handler_loop(Sock).

handler_loop(Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Line} ->
	    error_logger:info_msg("[~p] Got data: ~s\n", [Sock, Line]),
	    handle_line(Line, Sock),
	    handler_loop(Sock);
	{error, closed} ->
	    error_logger:info_msg("[~p] Client closed connection\n", [Sock]),
	    ok;
	{error, Err} ->
	    error_logger:error_msg("[~p] Error from recv(): ~p\n", [Sock, Err]),
	    ok = gen_tcp:close(Sock)
    end.

handle_line(_Line, _Sock) ->
    'TODO'.

%%%---------- Utilities ---------------------------------------
default(undefined, Default) -> Default;
default(Value, _Default) -> Value.
