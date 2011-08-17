-module(tricycle_config).

-export([server_port/0, hudson_address/0, jira_address/0]).

-define(DEFAULT_PORT, 9193).


%%%---------- API ----------------------------------------
server_port() -> check_port(get_env(port, ?DEFAULT_PORT)).

hudson_address() -> check_address(get_env(hudson_address)).

jira_address() -> check_address(get_env(jira_address)).


%%%---------- Helpers ---------------------------------------
get_env(Key, Default) ->
    case application:get_env(tricycle, Key) of
	undefined -> Default;
	{ok, Value} -> Value
    end.

get_env(Key) ->
    case application:get_env(tricycle, Key) of
	undefined -> error({not_configured, Key});
	{ok,Value} -> Value
    end.

check_address({Host, Port}=Addr) when is_list(Host), is_integer(Port) -> Addr;
check_address(X) -> error({not_a_valid_address, X}).

check_port(Port) when is_integer(Port), Port > 0, Port < (1 bsl 16) -> Port;
check_port(X) -> error({not_a_valid_port, X}).
