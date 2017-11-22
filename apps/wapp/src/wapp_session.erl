-module(wapp_session).
-export([init/2]).

init(#{path := <<"/login">>} = Req0, State) ->
    {ok, Body} = wapp_lib:read_body(Req0, <<>>),
    LoginInfo = #{<<"usr">> := User} = wapp_lib:parse_qs_body(Body),
    Req1 = case do_login(LoginInfo) of
               ok ->
                   SessionID = generate_session_id(),
                   dbI:update_user(User, #{sessionId => SessionID,
                                           node => node()}),
                   Req0_1 = cowboy_req:set_resp_cookie(<<"sessionid">>,
                                                       SessionID,
                                                       Req0),
                   wapp_lib:redirect_to(main, Req0_1);
               nok ->
                   wapp_lib:redirect_to("login", Req0)
           end,
    {ok, Req1, State};
init(#{path := <<"/logout">>,
       headers := #{<<"cookie">> := Session}} = Req0, State) ->
    #{<<"sessionid">> := SessionID} = wapp_lib:parse_qs_body(Session),
    dbI:update_user(#{sessionId => SessionID},
                    #{sessionId => undefined,
                      node => undefined}),
    Req1 = cowboy_req:set_resp_cookie(<<"sessionid">>, <<"">>,
                                      Req0, #{max_age => 0}),
    Req2 = wapp_lib:redirect_to("login", Req1),
    {ok, Req2, State}.


generate_session_id() ->
    Now = {_, _, Micro} = erlang:timestamp(),
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
    list_to_binary(Prefix ++ to_hex(crypto:strong_rand_bytes(9))).

to_hex([]) -> [];
to_hex(Bin) when is_binary(Bin) -> to_hex(binary_to_list(Bin));
to_hex([H|T]) -> [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N - 10.

do_login(#{<<"usr">> := User, <<"pwd">> := Pwd}) ->
    case dbI:find_user(User) of
        {User, Pwd, _Name, _Sid, undefined} -> ok;
        _ -> nok
    end;
do_login(_) -> nok.
