-module(dbI).
-export([install/1]).

install(Nodes) ->
    db_lib:install(Nodes).
