-module(tricycle_handle_commands).

-export([handle_command/1]).

handle_command(Line) ->
	Tokens = string:tokens(Line, " \r\n"),
	handle_line(Tokens).

handle_line(["Start-CI", Revision]) ->
    error_logger:info_msg("Triggering build of revision ~p ~n", [Revision]),
    {Host,Port} = tricycle_config:hudson_address(),
    %% TODO: Make project name configurable as well...
    URL = lists:flatten(io_lib:format("http://~s:~b/job/tricycle/buildWithParameters?revision=~s", [Host,Port,Revision])),
    ok = httpc:request(get, {URL, []}, [], []).
