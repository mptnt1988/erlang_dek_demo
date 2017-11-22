-module(dek_demo_lib).
-export([start_apps/1]).

start_apps(Apps) ->
    [{ok, _} = application:ensure_all_started(App) || App <- Apps].
