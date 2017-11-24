-module(db_user).
-export([add/1]).
-export([find/1]).
-export([delete/1]).
-export([update/2]).
-export([select_ms/2]).

-include("db.hrl").

select_ms(Key, {Guards, Results} = _GRSpec) ->
    ObjPattern = db_lib:map2rec(?USER_TABLE, Key, '_'),
    case mnesia:dirty_select(?USER_TABLE, [{ObjPattern, Guards, Results}]) of
        [] -> nothing;
        L -> L
    end.

add(UserInfo) ->
    Rec = db_lib:map2rec(?USER_TABLE, UserInfo, undefined),
    mnesia:dirty_write(Rec).

find(Key) ->
    ObjPattern = db_lib:map2rec(?USER_TABLE, Key, '_'),
    case mnesia:dirty_match_object(ObjPattern) of
        [Rec] -> db_lib:rec2map(?USER_TABLE, Rec);
        _ -> nothing
    end.

delete(Key) ->
    ObjPattern = db_lib:map2rec(?USER_TABLE, Key, '_'),
    case mnesia:dirty_match_object(ObjPattern) of
        Recs when is_list(Recs) ->
            [mnesia:dirty_delete_object(Rec) || Rec <- Recs],
            ok;
        _ -> ok
    end.

update(Key, KVMap) ->
    ObjPattern = db_lib:map2rec(?USER_TABLE, Key, '_'),
    F = fun() ->
                [Rec] = mnesia:dirty_match_object(ObjPattern),
                UpdatedRec = db_lib:update_record(?USER_TABLE, Rec, KVMap),
                mnesia:write(UpdatedRec)
        end,
    mnesia:activity(transaction, F).
