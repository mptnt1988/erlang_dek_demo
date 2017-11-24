-module(dbI).
-export([add_user/1]).
-export([find_user/1]).
-export([delete_user/1]).
-export([update_user/2]).
-export([select_ms/2]).

-include("db.hrl").

%%------------------------------------------------------------------------------
%% add_user/1
%%
%% Description:
%% UserInfo is a map with fields based on ones of user map in db.hrl
%% If not all fields present, the missed ones will be replaced with 'undefined'
add_user(UserInfo) ->
    db_user:add(UserInfo).

%%------------------------------------------------------------------------------
%% find_user/1
%%
%% Description:
%% Key is a map with fields based on ones of user map in db.hrl
%% If not all fields present, the missed ones will be replaced with '_'
find_user(Key) ->
    db_user:find(Key).

%%------------------------------------------------------------------------------
%% delete_user/1
%%
%% Description:
%% Key is a map with fields based on ones of user map in db.hrl
%% If not all fields present, the missed ones will be replaced with '_'
delete_user(Key) ->
    db_user:delete(Key).

%%------------------------------------------------------------------------------
%% update_user/2
%%
%% Description:
%% Key is a map with fields based on ones of user map in db.hrl
%% If not all fields present, the missed ones will be replaced with '_'
update_user(Key, KVMap) ->
    db_user:update(Key, KVMap).

%%------------------------------------------------------------------------------
%% select_ms/2
%%
%% Description:
%% Key is a map with fields based on ones of user map in db.hrl
%% If not all fields present, the missed ones will be replaced with '_'
select_ms(Key, GRSpec) ->
    db_user:select_ms(Key, GRSpec).
