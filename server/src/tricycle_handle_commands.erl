-module(tricycle_handle_commands).

-export([handle_command/1]).

handle_command(Line) ->
    Tokens = string:tokens(Line, " \r\n"),
    handle_line(Tokens).

handle_line(["Start-CI", Project_name, Revision]) ->
    {Host,Port} = tricycle_config:hudson_address(),
    
    %%Getting build id for this build
    %%TODO this is not a safe way of gettng the build id
    Status_url = lists:flatten(io_lib:format("http://~s:~b/job/~s/api/json?depth=0", [Host,Port, Project_name])),
    error_logger:info_msg("Getting next build id from jenkins at ~s~n", [Status_url]),
    Body = fetch_url(Status_url),
    {match, [[Next_build_id]]} = re:run(Body, "\\\"nextBuildNumber\\\":\\s*(\\d+)\\s*", [global,{capture,[1],list}]),
    
    %% TODO: Make project name configurable as well...
    %% Trigger build server
    URL = lists:flatten(io_lib:format("http://~s:~b/job/~s/buildWithParameters?revision=~s", [Host,Port,Project_name,Revision])),
    fetch_url(URL),
    
    error_logger:info_msg("Triggered build server with revision ~p~n", [Revision]),
    Build_url = build_url(Host,Port,Project_name,Next_build_id),
    error_logger:info_msg("build url ~s~n", [Build_url]),
    {ok, [Next_build_id, Build_url]};

handle_line(["CI-Build-Status", Project_name, BuildID]) ->
    {Host,Port} = tricycle_config:hudson_address(),
    URL = build_url(Host,Port,Project_name,BuildID),
    JSonURL = URL ++ "/api/json",
    Body = fetch_url(JSonURL),

    error_logger:info_msg("fetched build url ~s~n", [URL]),
    {match, [[Is_building_string]]} = re:run(Body, "\\\"building\\\":\\s*(\\w+)\\s*", [global,{capture,[1],list}]),
    {match, [[Result_string]]} = re:run(Body, "\\\"result\\\":\\s*\\\"(\\w+)\\\"\\s*", [global,{capture,[1],list}]),

    StatusCode =
        case {Is_building_string, Result_string} of
            {"false", "null"}    -> 404;
            {"true", "null"}     -> 202;
            {"false", "SUCCESS"} -> 200;
            {"false", "FAILURE"} -> 400
        end,
    {ok, [integer_to_list(StatusCode), URL]};

handle_line([UnknownCommand | Args]) ->
    error({unknown_command, UnknownCommand, Args}).

fetch_url(Url) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(get, {Url, []}, [], []),
    Body.

build_url(Host, Port, Project_name, BuildID) ->
    URL = io_lib:format("http://~s:~b/job/~s/~s", [Host,Port,Project_name,BuildID]),
    %%error_logger:info_msg("url: ~s~n", [URL]),
    lists:flatten(URL).
