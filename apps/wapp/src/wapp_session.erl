-module(wapp_session).
-export([init/2]).

init(#{path := <<"/login">>} = Req0, State) ->
    {ok, _Body} = read_body(Req0, <<>>),
    SessionID = generate_session_id(),
    Req1 = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, Req0),
    Req2 = wapp_lib:redirect_to(main, Req1),
    {ok, Req2, State};
init(#{path := <<"/logout">>} = Req0, State) ->
    Req1 = cowboy_req:set_resp_cookie(<<"sessionid">>, <<"">>,
                                      Req0, #{max_age => 0}),
    Req2 = wapp_lib:redirect_to("login", Req1),
    {ok, Req2, State}.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} ->
            {ok, <<Acc/binary, Data/binary>>};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.

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
