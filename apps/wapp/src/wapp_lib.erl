-module(wapp_lib).
-export([redirect_to/2]).
-export([read_body/2]).
-export([parse_qs_body/1]).
-export([set_no_browser_cache/1]).
-export([erlydtl_compile_load/0]).
-export([get_port/0]).

-include("wapp.hrl").

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

%% QsBody has format of <<"key1=val1&key2=val2...">>
parse_qs_body(QsBody) ->
    QList = cowboy_req:parse_qs(#{qs => QsBody}),
    lists:foldl(fun({Field, Value}, Acc) ->
                        Acc#{Field => Value}
                end, #{}, QList).

set_no_browser_cache(Req) ->
    CacheSetting = #{<<"cache-control">> => <<"no-cache,"
                                              "no-store,"
                                              "must-revalidate">>,
                     <<"pragma">> => "no-cache",
                     <<"expires">> => <<"0">>},
    cowboy_req:set_resp_headers(CacheSetting, Req).

erlydtl_compile_load() ->
    App = wapp,
    Template = filename:join(code:priv_dir(App), "auth/client.html"),
    OutDir = filename:join(code:lib_dir(App), "ebin"),
    Client = wapp_erlydtl_client,
    {ok, Client} = erlydtl:compile_file(Template, Client, [{out_dir, OutDir}]),
    ok.

get_port() ->
    list_to_integer(dek_demo_lib:get_argument(wapp_port, ?DEFAULT_PORT)).
