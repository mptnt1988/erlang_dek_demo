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
    Routes = define_routes(),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_clear(http,
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
    [{"/", cowboy_static, {priv_file, ws, "client.html"}},
     {"/websocket", ws_handler, []}].
