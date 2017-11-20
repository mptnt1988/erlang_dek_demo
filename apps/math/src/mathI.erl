-module(mathI).
-export([do/1,
         history/0]).

do(Expr) ->
    math_server:eval_expr(Expr).

history() ->
    math_server:eval_history().
