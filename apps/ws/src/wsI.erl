-module(wsI).
-export([broadcast_users/2]).
-export([get_other_users/1]).
-export([get_all_users/0]).
-export([get_port/0]).

broadcast_users(FromUsr, Msg) ->
    ws_lib:broadcast_users(FromUsr, Msg).

get_other_users(FromUsr) ->
    ws_lib:get_other_users(FromUsr).

get_all_users() ->
    ws_lib:get_all_users().

get_port() ->
    ws_lib:get_port().
