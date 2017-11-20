%%%-------------------------------------------------------------------
%% @doc math top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(math_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Child = child(math_server),
    {ok, { {one_for_one, 5, 5000}, [Child]} }.

%%====================================================================
%% Internal functions
%%====================================================================
child(Name) ->
    child(Name, []).

child(Name, Args) ->
    {Name, {Name, start_link, Args}, permanent, 5000, worker, [Name]}.
