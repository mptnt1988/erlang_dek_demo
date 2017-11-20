%%%-------------------------------------------------------------------
%% @doc ws public API
%% @end
%%%-------------------------------------------------------------------

-module(ws_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    application:ensure_all_started(cowboy),
    application:ensure_started(udg),
    Routes = define_routes(),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_clear(ws_name,
                                 [{port, 8888}],
                                 #{env => #{dispatch => Dispatch}}),
    ws_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
define_routes() ->
    [{"/websocket", ws_handler, []}].
