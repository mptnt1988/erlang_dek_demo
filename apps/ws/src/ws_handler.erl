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
                                    <<"other_users">> => OtherUsrs}),
            {reply, {text, Msg2Send}, State#{user_info => User}}
    end;
websocket_init(State) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, State]]),
    {stop, State}.

%% Callback websocket_handle
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

%% Callback websocket_info
websocket_info({notify_user, Msg}, State) ->
    schedule_notify_user(),
    Msg2Send = jsx:encode(#{<<"dest">> => <<"ping">>,
                            <<"msg">> => Msg}),
    {reply, {text, Msg2Send}, State};
websocket_info({user_online, User}, State) ->
    Who = maps:get(username, User),
    Msg2Send = jsx:encode(#{<<"msg">> => <<Who/binary, " says hello!">>}),
    {reply, {text, Msg2Send}, State};
websocket_info({user_offline, User}, State) ->
    Who = maps:get(username, User),
    Msg2Send = jsx:encode(#{<<"msg">> => <<Who/binary, " says goodbye">>}),
    {reply, {text, Msg2Send}, State};
websocket_info({math_update, Pid}, State) ->
    MsgBin = format_result(Pid),
    Msg2Send = jsx:encode(#{<<"dest">> => <<"math">>,
                            <<"msg">> => MsgBin}),
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
