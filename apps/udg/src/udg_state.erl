-module(udg_state).
-behaviour(gen_server).

%% API
-export([start_link/0,
         get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(INC, 1).

-record(state, {count = 0}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get(inc) ->
    gen_server:call(?SERVER, get_inc);
get(dec) ->
    gen_server:call(?SERVER, get_dec).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call(get_inc, _From, State = #state{count = Count}) ->
    Reply = Count + ?INC,
    {reply, Reply, State#state{count = Count + ?INC}};
handle_call(get_dec, _From, State = #state{count = Count}) ->
    Reply = Count - ?INC,
    {reply, Reply, State#state{count = Count - ?INC}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
