-define(TABLES_DEF, #{user => [username, password, session_id],
                      csrf => [peer, token]}).
-define(TABLES, maps:keys(?TABLES_DEF)).
-define(FIELDS(Table), maps:get(Table, ?TABLES_DEF)).
