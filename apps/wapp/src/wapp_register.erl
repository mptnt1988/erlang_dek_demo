-module(wapp_register).
-export([init/2]).

init(#{path := <<"/register">>} = Req0, State) ->
    {ok, Body} = wapp_lib:read_body(Req0, <<>>),
    RegInfo = wapp_lib:parse_qs_body(Body),
    Req1 = case do_register(RegInfo) of
               ok -> wapp_lib:redirect_to("login", Req0);
               nok -> wapp_lib:redirect_to("register", Req0)
           end,
    {ok, Req1, State}.

do_register(#{<<"usr">> := User, <<"name">> := Name,
              <<"pwd">> := Pwd, <<"cpwd">> := Pwd}) ->
    case dbI:find_user(User) of
        {User, _Pwd, _Name} -> nok;
        nothing -> dbI:add_user(User, Pwd, Name)
    end;
do_register(_) -> nok.
