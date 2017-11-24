-module(wapp_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

%% Request for public resources can bypass middleware check
execute(#{bindings := Bindings} = Req,
        #{handler_opts := #{bypass_middleware := true,
                            opts := HandlerOpts}} = Env)
  when is_function(HandlerOpts) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, Req, Env]]),
    {ok, Req, Env#{handler_opts => HandlerOpts(Bindings)}};

%% Cookie in HTTP headers (already logged-in)
execute(#{headers := #{<<"cookie">> := _Cookie}} = Req, Env) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, Req, Env]]),
    {ok, Req, Env};

%% Login page
%% => Return login page
execute(#{path := <<"/login">>, method := <<"GET">>} = Req, Env) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}]]),
    Handler = cowboy_static,
    HandlerOpts = {priv_file, wapp, "public/login.html"},
    {ok, Req, Env#{handler => Handler,
                   handler_opts => HandlerOpts}};
%% => Check username/password
%%      True  -> return cookie and main page
%%      False -> return login page
execute(#{path := <<"/login">>, method := <<"POST">>} = Req, Env) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, Req, Env]]),
    {ok, Req, Env};

%% Get register page
%% => Return register page
execute(#{path := <<"/register">>, method := <<"GET">>} = Req, Env) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, Req, Env]]),
    Handler = cowboy_static,
    HandlerOpts = {priv_file, wapp, "public/register.html"},
    {ok, Req, Env#{handler => Handler,
                   handler_opts => HandlerOpts}};
%% => Verification & return login page
execute(#{path := <<"/register">>, method := <<"POST">>} = Req, Env) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, Req, Env]]),
    {ok, Req, Env};

%% Else
%% => Return redirect to login page
execute(Req, Env) ->
    lager:debug("DEBUG: ~p~n~n", [[{?MODULE, ?LINE}, Req, Env]]),
    NewReq = wapp_lib:redirect_to("login", Req),
    {stop, NewReq}.
