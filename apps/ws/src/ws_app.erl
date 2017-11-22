%%%-------------------------------------------------------------------
%% @doc ws public API
%% @end
%%%-------------------------------------------------------------------

-module(ws_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 8888).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    dek_demo_lib:start_apps([cowboy, jsx, udg]),
    Routes = define_routes(),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Port = case init:get_argument(ws_port) of
               {ok,[[PortNoStr]]} ->
                   list_to_integer(PortNoStr);
               error ->
                   ?DEFAULT_PORT
           end,
    {ok, _} = cowboy:start_clear(ws_name,
                                 [{port, Port}],
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
