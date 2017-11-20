-module(wapp_lib).
-export([redirect_to/2]).
-export([read_body/2]).
-export([parse_qs_body/1]).

redirect_to(main, Req) -> redirect_to("", Req);
redirect_to(RelPath,
            #{headers := #{<<"host">> := Host}, scheme := Scheme} = Req) ->
    AdjPath = adjust_path(RelPath),
    RedirectPath = list_to_binary(AdjPath),
    LocHeader = #{<<"location">> =>
                      <<Scheme/binary,
                        "://",
                        Host/binary,
                        RedirectPath/binary>>},
    cowboy_req:reply(303, LocHeader, Req).

adjust_path([$/|_] = Path) -> Path;
adjust_path(Path) -> [$/|Path].

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} ->
            {ok, <<Acc/binary, Data/binary>>};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.

parse_qs_body(QsBody) ->
    QList = cowboy_req:parse_qs(#{qs => QsBody}),
    lists:foldl(fun({Field, Value}, Acc) ->
                        Acc#{Field => Value}
                end, #{}, QList).
