-module(math_server).
-behaviour(gen_server).

%% API
-export([start_link/0,
         eval_history/0,
         eval_expr/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

eval_history() ->
    gen_server:call(?SERVER, eval_history).

eval_expr(Expr) ->
    gen_server:call(?SERVER, {eval_expr, Expr}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    Usrs = wsI:get_all_users(),
    wsI:broadcast_users(Usrs, {math_update, self()}),
    {ok, #state{}}.

handle_call(eval_history, _From, State) ->
    [{history, History}] = ets:lookup(?SERVER, history),
    {reply, History, State};
handle_call({eval_expr, Expr}, _From, State) ->
    Result = do_expr(Expr),
    [{history, History}] = ets:lookup(?SERVER, history),
    NewHistory = [#{expr => Expr, result => Result} | History],
    true = ets:insert(?SERVER, {history, NewHistory}),
    {reply, Result, State};
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
do_expr(Expression) ->
    RealExpr = Expression ++ ".",
    {ok, Tokens, _} = erl_scan:string(RealExpr),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    Result.
