-module(tricycle_server).

-export([start_link/0]).
-export([init_listener/1, init_handler/1]).

%%%---------- API ----------------------------------------
start_link() ->
    Port = tricycle_config:server_port(),
    proc_lib:start_link(?MODULE, init_listener, [Port]).

%%%---------- Implementation - listener -----------------------------
init_listener(Port) ->
    {ok,SrvSock} = gen_tcp:listen(Port, [list,
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
    try
	handler_loop(Sock)
    after
	gen_tcp:close(Sock)
    end.

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
	    ok
    end.

handle_line(Line, _Sock) ->
    tricycle_handle_commands:handle_command(Line).

