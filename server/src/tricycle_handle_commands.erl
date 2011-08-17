-module(tricycle_handle_commands).

-export([handle_command/1]).

handle_command(Line) ->
	Tokens = string:tokens(Line, " \r\n"),
	handle_line(Tokens).

handle_line(["Start-CI", Revision]) ->
	io:format("triggering build of revision ~p ~n", [Revision]),
	httpc:request(get, {"http://localhost:8080/job/tricycle/buildWithParameters?revision=" ++ Revision, []}, [], []).
	