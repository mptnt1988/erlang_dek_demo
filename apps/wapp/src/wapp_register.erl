-module(wapp_register).
-export([init/2]).

init(Req0, State) ->
    Req1 = wapp_lib:redirect_to("login", Req0),
    {ok, Req1, State}.
