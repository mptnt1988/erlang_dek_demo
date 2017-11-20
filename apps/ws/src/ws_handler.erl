-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    schedule_notify_user(),
    {reply, {text, <<"Hello there!!!">>}, State}.

websocket_handle({text, Msg}, State) ->
    Prefix = <<"OK, server got: ">>,
    {reply, {text, <<Prefix/binary, Msg/binary>>}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({notify_user, Msg}, State) ->
    schedule_notify_user(),
    {reply, {text, Msg}, State};
websocket_info(Info, State) ->
    {ok, State}.

schedule_notify_user() ->
    Msg = <<"Hey!!! I just want to disturb: ">>,
    Num = integer_to_binary(udg_state:get(inc)),
    timer:send_after(1000, {notify_user, <<Msg/binary, Num/binary>>}).
