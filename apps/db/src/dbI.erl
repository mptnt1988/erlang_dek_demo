-module(dbI).
-export([add_user/3]).
-export([find_user/1]).
-export([delete_user/1]).
-include("db.hrl").

add_user(User, Pwd, Name) ->
    db_user:add(User, Pwd, Name).

find_user(User) ->
    db_user:find(User).

delete_user(User) ->
    db_user:delete(User).
