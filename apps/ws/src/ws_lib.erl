-module(ws_lib).
-export([broadcast_users/2]).
-export([get_other_users/1]).
-export([get_all_users/0]).
-export([get_port/0]).

-include("ws.hrl").

broadcast_users(Usrs, Msg) ->
    [Pid ! Msg || [_Usr, Pid, _DName] <- Usrs].

%% FromUsr: username
get_other_users(FromUsr) ->
    AllUsrs = get_all_users(),
    [X || X = [UsrName, _WsPid, _DName] <- AllUsrs, UsrName /= FromUsr].

%% Get all online users
%% Return list of [Username, WsPid, DisplayName]
get_all_users() ->
    Result = dbI:select_ms(#{username => '$1',
                             ws_pid => '$2',
                             display_name => '$3'},
                           {[{'/=', '$2', undefined}],
                            [['$1', '$2', '$3']]}),
    if Result == nothing -> [];
       true -> Result
    end.

get_port() ->
    list_to_integer(dek_demo_lib:get_argument(ws_port, ?DEFAULT_PORT)).
