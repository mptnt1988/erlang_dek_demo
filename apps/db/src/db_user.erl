-module(db_user).
-export([add/3]).
-export([find/1]).
-export([delete/1]).
-export([update/2]).

-include("db.hrl").

add(User, Pwd, Name) ->
    add(User, Pwd, Name, undefined, undefined).

add(User, Pwd, Name, SId, Node) ->
    mnesia:dirty_write({?USER_TABLE, User, Pwd, Name, SId, Node}).

find(User) ->
    case mnesia:dirty_read(?USER_TABLE, User) of
        [{?USER_TABLE, User, Pwd, Name, SId, Node}] ->
            {User, Pwd, Name, SId, Node};
        _ -> nothing
    end.

delete(User) ->
    mnesia:dirty_delete(?USER_TABLE, User).

update(User, KVMap) when is_binary(User) ->
    update(#{username => User}, KVMap);
update(Keys, KVMap) when is_map(Keys) ->
    ObjPattern = db_lib:build_obj_pattern(?USER_TABLE, Keys),
    F = fun() ->
                [Rec] = mnesia:dirty_match_object(ObjPattern),
                UpdatedRec = db_lib:update_record(?USER_TABLE, Rec, KVMap),
                mnesia:write(UpdatedRec)
        end,
    mnesia:activity(transaction, F).
