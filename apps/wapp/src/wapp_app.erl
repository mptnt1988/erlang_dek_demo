%%%-------------------------------------------------------------------
%% @doc wapp public API
%% @end
%%%-------------------------------------------------------------------

-module(wapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 8080).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    dek_demo_lib:start_apps([cowboy]),
    Routes = define_routes(),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Port = case init:get_argument(wapp_port) of
               {ok,[[PortNoStr]]} ->
                   list_to_integer(PortNoStr);
               error ->
                   ?DEFAULT_PORT
           end,
    {ok, _} = cowboy:start_clear(wapp_name,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares => [cowboy_router,
                                                   wapp_middleware,
                                                   cowboy_handler]}),
    wapp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
define_routes() ->
    PublicConstraints =
        fun(_, V = <<"js">>) -> {ok, V};
           (_, V = <<"images">>) -> {ok, V};
           (_, V = <<"css">>) -> {ok, V};
           (_, _) -> {error, public_resources_not_found}
        end,
    [%% Bypass middleware for public resources request
     {"/public/:res_type/[...]",
      [{res_type, PublicConstraints}],
      cowboy_static,
      #{bypass_middleware => true,
        opts => fun(Bindings) ->
                        ResType = maps:get(res_type, Bindings),
                        {priv_dir, wapp, <<"public/", ResType/binary>>}
                end}},
     {"/login", wapp_session, []},
     {"/logout", wapp_session, []},
     {"/register", wapp_register, []},
     {"/[...]", wapp_handler, []}].
