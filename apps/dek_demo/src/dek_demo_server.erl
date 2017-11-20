-module(dek_demo_server).
-export([loop/3, start/3, stop/1, rpc/2, swap_code/2]).

start(Name, F, State) ->
    register(Name, spawn(fun() -> loop(Name,F,State) end)).

stop(Name) -> Name ! stop.

swap_code(Name, F) -> rpc(Name, {swap_code, F}).

rpc(Name, Query) ->
    Name ! {self(), Query},
    receive
	{Name, crash} -> exit(rpc);
	{Name, ok, Reply} -> Reply
    end.

loop(Name, F, State) ->
    receive
	stop -> void;
	{From, {swap_code, F1}} ->
	    From ! {Name, ok, ack},
	    loop(Name, F1, State);
	{From, Query} ->
	    case (catch F(Query, State)) of
		{'EXIT', Why} ->
		    log_error(Name, Query, Why),
		    From ! {Name, crash},
		    loop(Name, F, State);
		{Reply, State1} ->
		    From ! {Name, ok, Reply},
		    loop(Name, F, State1)
	    end
    end.

log_error(Name, Query, Why) ->
    io:format("Server ~p query ~p caused exception ~p~n",
	      [Name, Query, Why]).
