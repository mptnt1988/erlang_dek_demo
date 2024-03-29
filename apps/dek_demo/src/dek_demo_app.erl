%%%-------------------------------------------------------------------
%% @doc dek_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(dek_demo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    dek_demo_lib:start_apps([lager, wapp, ws, db, math]),
    dek_demo_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
