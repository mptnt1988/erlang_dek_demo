-module(db_lib).
-export([install/1]).

-include("db.hrl").

install(Nodes) ->
    ok = create_schema(Nodes),
    rpc:multicall(Nodes, mnesia, start, []),
    create_tables(Nodes),
    rpc:multicall(Nodes, mnesia, stop, []).

create_schema(Nodes) ->
    case catch mnesia:create_schema(Nodes) of
        ok ->
            lager:info("Mnesia schema created."),
            ok;
        {error, {ErrNode, {already_exists, ErrNode}}} ->
            lager:info("Mnesia schema already exists."),
            ok;
        R ->
            lager:error("Problem with creating mnesia schema: ~p~n",
                        [[{?MODULE, ?LINE}, R]]),
            nok
    end.

create_tables(Nodes) ->
    case mnesia:system_info(tables) of
        [schema] ->
            [create_table(Table, Nodes) || Table <- ?TABLES];
        _Tables ->
            ok
    end.

create_table(Table, Nodes) ->
    Fields = ?FIELDS(Table),
    {atomic, ok} = mnesia:create_table(Table,
                                       [{attributes, Fields},
                                        {disc_copies, Nodes}]).
