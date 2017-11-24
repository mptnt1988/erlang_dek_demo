-define(TABLES_DEF,
        #{user => [username,
                   password,
                   display_name,
                   session_id,
                   ws_pid,
                   node
                  ],
          csrf => [peer, token]}).
-define(TABLES, maps:keys(?TABLES_DEF)).
-define(FIELDS(Table), maps:get(Table, ?TABLES_DEF)).

-include("dbI.hrl").
