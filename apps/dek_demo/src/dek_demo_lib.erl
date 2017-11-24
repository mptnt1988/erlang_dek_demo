-module(dek_demo_lib).
-export([start_apps/1]).
-export([get_argument/2]).

start_apps(Apps) ->
    [{ok, _} = application:ensure_all_started(App) || App <- Apps].

get_argument(Arg, Default) ->
    case init:get_argument(Arg) of
        {ok , [[ValStr]]} -> ValStr;
        error -> Default
    end.
