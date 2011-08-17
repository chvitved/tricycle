-module(tricycle_handle_commands).

-export([handle_command/1]).

handle_command(Line) ->
	Tokens = string:tokens(Line, " \r\n"),
	handle_line(Tokens).

handle_line(["Start-CI", Revision]) ->
    error_logger:info_msg("Triggering build of revision ~p ~n", [Revision]),
    URL = "http://localhost:8080/job/tricycle/buildWithParameters?revision=" ++ Revision,
    httpc:request(get, {URL, []}, [], []).
