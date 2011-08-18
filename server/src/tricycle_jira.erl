-module(tricycle_jira).

-export([mark_as_inprogress/0, comment/1]).

mark_as_inprogress() -> ok.
    
comment(Comment) ->
        URL = "https://jira.atlassian.com/login.jsp",
        {ok, {{_Version, 200, _ReasonPhrase}, Headers, _Body}} = httpc:request(post, 
        {URL, [], "application/x-www-form-urlencoded","os_username=tricycle&os_password=tricycle"},[], []),
        
        Cookies = [C || {"set-cookie", C} <- Headers],
        CookieHeader = {"Cookie", string:join(Cookies, ",")},
        
        Comment_url = https://jira.atlassian.com/secure/AddComment.jspa?atl_token=AKVY-YUFR-9LM7-97AB%7C7caf3603cf9a1ccb1b1d0dcae274ee4a1450c545%7Clin
        {ok, {{_Version, 200, _ReasonPhrase}, Headers, _Body}} = httpc:request(post, 
        {URL, [CookieHeader], "application/x-www-form-urlencoded","body"},[], []),
        
              
        
