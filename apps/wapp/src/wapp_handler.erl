-module(wapp_handler).
-export([init/2]).

init(Req, State) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}]]),
    {ok, Req, State}.
