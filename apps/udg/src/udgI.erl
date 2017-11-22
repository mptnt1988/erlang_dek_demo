-module(udgI).
-export([get/1]).

%% Trend = inc | dec
get(Trend) ->
    udg_state:get(Trend).
