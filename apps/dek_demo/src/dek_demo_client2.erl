-module(dek_demo_client2).
-export([start/1, stop/1, eval_expr/2, eval_history/1, handle_request/2]).

start(Client) -> 
    dek_demo_server:start(Client, fun handle_request/2, []).

stop(Client) -> 
    dek_demo_server:stop(Client).

eval_expr(Client, Expr) ->
    dek_demo_server:rpc(Client, {eval_expr, Expr}).

eval_history(Client) ->
    dek_demo_server:rpc(Client, eval_history).

handle_request({eval_expr, Expr}, State) ->
    Result = evaluate_expression(Expr),
    {Result, [{Expr, Result}| State]};
handle_request(eval_history, History = State) ->
    {History, State}.

evaluate_expression(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    try erl_eval:exprs(Parsed, []) of
	{value, Result, _} ->
	    Result
    catch
	_Error:_Reason ->
	    invalid_expr
    end.


