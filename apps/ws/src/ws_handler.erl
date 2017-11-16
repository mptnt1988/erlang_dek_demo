-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {reply, {text, <<"Hello there!!!">>}, State}.

websocket_handle({text, Msg}, State) ->
    Prefix = <<"OK, server got: ">>,
    {reply, {text, <<Prefix/binary, Msg/binary>>}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
