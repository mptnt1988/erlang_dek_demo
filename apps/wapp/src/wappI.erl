-module(wappI).
-export([parse_qs_body/1]).

parse_qs_body(QsBody) ->
    wapp_lib:parse_qs_body(QsBody).
