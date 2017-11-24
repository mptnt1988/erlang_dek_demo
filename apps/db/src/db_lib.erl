-module(db_lib).
-export([init/1,
         cleanup/0,
         change_config/2]).
-export([update_record/3]).
-export([rec2map/2]).
-export([map2rec/3]).

-include("db.hrl").

init(master) ->
    Node = [node()],
    stopped = mnesia:stop(),
    ok = create_schema(Node),
    ok = mnesia:start(),
    ok = create_table(Node);
init(client) ->
    cleanup(),
    ok = mnesia:start().

cleanup() ->
    Node = [node()],
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema(Node).

change_config(master, Client) ->
    {ok, _} = mnesia:change_config(extra_db_nodes, nodes()),
    ok = change_table_copy_type(Client);
change_config(client, Master) ->
    Tabs = mnesia:system_info(tables),
    TabInfos = [{T, mnesia:table_info(T, where_to_commit)} || T <- Tabs],
    Fun = fun({Table, List}) ->
                  add_table_copy(Table, List, Master)
          end,
    lists:foreach(Fun, TabInfos).

change_table_copy_type(Client) ->
    case catch mnesia:change_table_copy_type(schema, Client, disc_copies) of
        {atomic, ok} ->
            lager:info("Change table copy type done."),
            ok;
        {aborted, {already_exists, schema, Client, disc_copies}} ->
            lager:info("Change table copy type already done before."),
            ok;
        R ->
            lager:error("Problem when changing table copy type: ~p",
                        [[{?MODULE, ?LINE}, R]]),
            nok
    end.

add_table_copy(Table, List, Master) ->
    case lists:keysearch(node(), 1, List) of
        {value, _} ->
            ok;
        false ->
            {value, {Master, Type}} = lists:keysearch(Master, 1, List),
            mnesia:add_table_copy(Table, node(), Type)
    end.

create_schema(Nodes) ->
    case catch mnesia:create_schema(Nodes) of
        ok ->
            lager:info("Mnesia schema created.", []),
            ok;
        {error, {ErrNode, {already_exists, ErrNode}}} ->
            lager:info("Mnesia schema already exists.", []),
            ok;
        R ->
            lager:error("Problem with creating mnesia schema: ~p~n",
                        [[{?MODULE, ?LINE}, R]]),
            nok
    end.

create_table(Nodes) ->
    case mnesia:system_info(tables) of
        [schema] ->
            [ok = create_table(Table, Nodes) || Table <- ?TABLES],
            ok;
        _Tables ->
            ok
    end.

create_table(Table, Nodes) ->
    Fields = ?FIELDS(Table),
    Attrs = [{disc_copies, Nodes}, {attributes, Fields}],
    case catch mnesia:create_table(Table, Attrs) of
        {atomic, ok} ->
            lager:info("Table ~p is created.", [Table]),
            ok;
        {aborted, {already_exists, Table}} ->
            lager:info("Table ~p is already existed.", [Table]),
            ok;
        R ->
            lager:error("Problem with creating table: ~p~n",
                        [[{?MODULE, ?LINE}, R]]),
            nok
    end.

update_record(Table, Rec, KVMap) ->
    TabFields = ?FIELDS(Table),
    [Table | RecList] = tuple_to_list(Rec),
    RecMap = maps:from_list(lists:zip(TabFields, RecList)),
    Fun = fun(MapK, MapV, Acc) ->
                  Acc#{MapK => MapV}
          end,
    UpdatedMap = maps:fold(Fun, RecMap, KVMap),
    UpdatedList = [maps:get(X, UpdatedMap) || X <- TabFields],
    list_to_tuple([Table | UpdatedList]).

map2rec(Table, Map, Default) ->
    TabFields = ?FIELDS(Table),
    RecList = [maps:get(X, Map, Default) || X <- TabFields],
    list_to_tuple([Table | RecList]).

rec2map(Table, Rec) ->
    TabFields = ?FIELDS(Table),
    [Table | RecList] = tuple_to_list(Rec),
    maps:from_list(lists:zip(TabFields, RecList)).
