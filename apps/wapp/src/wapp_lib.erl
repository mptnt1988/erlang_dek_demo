-module(wapp_lib).
-export([redirect_to/2]).

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
