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
    Prefix = <<"My answer is: ">>,
    Result = (catch mathI:do(binary_to_list(Msg))),
    Answer = format_result(Result),
    AnswerBin = <<Prefix/binary, Answer/binary>>,
    Msg2Send = jsx:encode(#{<<"dest">> => <<"reply">>,
                            <<"msg">> => AnswerBin}),
    {reply, {text, Msg2Send}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({notify_user, Msg}, State) ->
    schedule_notify_user(),
    Msg2Send = jsx:encode(#{<<"dest">> => <<"ping">>,
                            <<"msg">> => Msg}),
    {reply, {text, Msg2Send}, State};
websocket_info(_Info, State) ->
    {ok, State}.

schedule_notify_user() ->
    Msg = <<"Hey!!! I just want to disturb: ">>,
    Num = integer_to_binary(udgI:get(inc)),
    timer:send_after(1000, {notify_user, <<Msg/binary, Num/binary>>}).

format_result(R) ->
    RStr = lists:flatten(io_lib:format("~p", [R])),
    list_to_binary(RStr).
