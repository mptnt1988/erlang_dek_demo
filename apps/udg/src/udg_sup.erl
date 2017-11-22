%%%-------------------------------------------------------------------
%% @doc udg top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(udg_sup).

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
    Child = child(udg_state),
    {ok, { {one_for_all, 0, 1}, [Child]} }.

%%====================================================================
%% Internal functions
%%====================================================================
child(Name) ->
    child(Name, []).

child(Name, Args) ->
    {Name, {Name, start_link, Args}, permanent, 5000, worker, [Name]}.
