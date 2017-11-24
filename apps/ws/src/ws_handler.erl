-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%% Callback init
init(Req, _State) ->
    InitState = #{req => Req},
    {cowboy_websocket, Req, InitState, #{idle_timeout => infinity}}.

%% Callback websocket_init
websocket_init(State = #{req := #{headers := #{<<"cookie">> := Session}}}) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, State]]),
    schedule_notify_user(),
    #{<<"sessionid">> := SessionID} = wappI:parse_qs_body(Session),
    case dbI:find_user(#{session_id => SessionID}) of
        nothing ->
            {stop, State};
        User ->
            Username = maps:get(username, User),

            %% Update ws_pid in DB
            dbI:update_user(User, #{ws_pid => self()}),

            %% Get display name to update WS client status
            DName = maps:get(display_name, User),
            MsgBin = <<"Hello there, ", DName/binary, "!!!">>,

            %% Get list of other online users & send to current user
            OtherUsrsDetail = ws_lib:get_other_users(Username),
            OtherUsrsList = [{Usr, DN} || [Usr, _WsPid, DN] <- OtherUsrsDetail],
            OtherUsrs = maps:from_list(OtherUsrsList),

            %% Broadcast to other WS clients that current user online
            ws_lib:broadcast_users(OtherUsrsDetail, {user_online, User}),

            %% Get math server PID
            MathSvPid = format_result(whereis(math_server)),

            %% Build msg
            Msg2Send = jsx:encode(#{<<"dest">> => <<"status">>,
                                    <<"msg">> => MsgBin,
                                    <<"math">> => MathSvPid,
                                    <<"user">> => #{username => Username,
                                                    display_name => DName},
                                    <<"other_users">> => OtherUsrs}),
            {reply, {text, Msg2Send}, State#{user_info => User}}
    end;
websocket_init(State) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, State]]),
    {stop, State}.

%% Callback websocket_handle
websocket_handle({text, Msg}, State) ->
    #{user_info := #{username := FromUsername}} = State,
    DecodedMsg = jsx:decode(Msg, [return_maps]),
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, Msg, DecodedMsg]]),
    GotMsg = maps:get(<<"msg">>, DecodedMsg, <<"">>),
    case maps:get(<<"to">>, DecodedMsg, undefined) of
        %% Message to math server
        undefined ->
            lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, {"Go to", "math_server"}]]),
            Prefix = <<"My answer is: ">>,
            Result = (catch mathI:do(binary_to_list(GotMsg))),
            Answer = format_result(Result),
            AnswerBin = <<Prefix/binary, Answer/binary>>,
            Msg2Send = jsx:encode(#{<<"dest">> => <<"reply">>,
                                    <<"msg">> => AnswerBin}),
            {reply, {text, Msg2Send}, State};
        %% Message to other user
        ToUsername ->
            lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, {"Go to", ToUsername}]]),
            send_msg_to_other_user(FromUsername, ToUsername, GotMsg),
            {ok, State}
    end;
websocket_handle(Data, State) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, {unknown, Data}]]),
    {ok, State}.

%% Callback websocket_info
websocket_info({notify_user, Msg}, State) ->
    schedule_notify_user(),
    Msg2Send = jsx:encode(#{<<"dest">> => <<"ping">>,
                            <<"msg">> => Msg}),
    {reply, {text, Msg2Send}, State};
websocket_info({user_online, User}, State) ->
    #{username := Who, display_name := DWho} = User,
    Msg2Send = jsx:encode(#{<<"dest">> => <<"user_online">>,
                            <<"user">> => #{username => <<Who/binary>>,
                                            display_name => <<DWho/binary>>},
                            <<"msg">> => <<Who/binary, " says hello!">>}),
    {reply, {text, Msg2Send}, State};
websocket_info({user_offline, User}, State) ->
    Who = maps:get(username, User),
    Msg2Send = jsx:encode(#{<<"dest">> => <<"user_offline">>,
                            <<"user">> => #{username => <<Who/binary>>},
                            <<"msg">> => <<Who/binary, " says goodbye">>}),
    {reply, {text, Msg2Send}, State};
websocket_info({math_update, Pid}, State) ->
    MsgBin = format_result(Pid),
    Msg2Send = jsx:encode(#{<<"dest">> => <<"math">>,
                            <<"msg">> => MsgBin}),
    {reply, {text, Msg2Send}, State};
websocket_info({chatting, FromUsername, Msg}, State) ->
    Msg2Send = jsx:encode(#{<<"dest">> => <<"chatting">>,
                            <<"from_user">> => FromUsername,
                            <<"msg">> => Msg}),
    {reply, {text, Msg2Send}, State};
websocket_info(_Info, State) ->
    {ok, State}.

%% Callback terminate
terminate(_Reason, _PartialReq, State) ->
    User = maps:get(user_info, State),
    Username = maps:get(username, User),
    OtherUsrs = ws_lib:get_other_users(Username),
    ws_lib:broadcast_users(OtherUsrs, {user_offline, User}),
    ok.

schedule_notify_user() ->
    Msg = <<"Hey!!! I just want to disturb: ">>,
    Num = integer_to_binary(udgI:get(inc)),
    timer:send_after(1000, {notify_user, <<Msg/binary, Num/binary>>}).

format_result(R) ->
    RStr = lists:flatten(io_lib:format("~p", [R])),
    list_to_binary(RStr).

send_msg_to_other_user(FromUsername, ToUsername, Msg2Send) ->
    case dbI:find_user(#{username => ToUsername}) of
        nothing -> ok;
        #{ws_pid := undefined} -> ok;
        #{ws_pid := Pid} when is_pid(Pid) ->
            Pid ! {chatting, FromUsername, Msg2Send},
            ok
    end.
