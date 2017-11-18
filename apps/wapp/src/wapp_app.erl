%%%-------------------------------------------------------------------
%% @doc wapp public API
%% @end
%%%-------------------------------------------------------------------

-module(wapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    application:ensure_all_started(cowboy),
    Routes = define_routes(),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_clear(wapp_name,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    wapp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
define_routes() ->
    [{"/", cowboy_static, {priv_file, wapp, "client.html"}}].
