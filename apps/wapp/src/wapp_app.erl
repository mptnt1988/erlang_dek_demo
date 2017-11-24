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
    dek_demo_lib:start_apps([erlydtl, cowboy]),
    wapp_lib:erlydtl_compile_load(),
    Routes = define_routes(),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    Port = wapp_lib:get_port(),
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
     {"/register", wapp_register, []},
     {"/[...]", wapp_session, []}].
