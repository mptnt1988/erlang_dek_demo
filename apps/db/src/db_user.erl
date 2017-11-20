-module(db_user).
-export([add/3]).
-export([find/1]).
-export([delete/1]).
-include("db.hrl").

add(User, Pwd, Name) ->
    mnesia:dirty_write({?USER_TABLE, User, Pwd, Name}).

find(User) ->
    case mnesia:dirty_read(?USER_TABLE, User) of
        [{?USER_TABLE, User, Pwd, Name}] -> {User, Pwd, Name};
        [] -> nothing
    end.

delete(User) ->
    mnesia:dirty_delete(?USER_TABLE, User).
