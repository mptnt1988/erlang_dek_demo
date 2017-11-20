%%%-------------------------------------------------------------------
%% @doc math public API
%% @end
%%%-------------------------------------------------------------------

-module(math_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(MATH, math_server).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ?MATH = ets:new(?MATH, [named_table, public]),
    true = ets:insert(?MATH, {history, []}),
    math_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    true = ets:delete(?MATH),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
