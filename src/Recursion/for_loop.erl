%%%-------------------------------------------------------------------
%%% @author Cameron G.
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 12. Aug 2021 11:28
%%%-------------------------------------------------------------------
-module(for_loop).
-author("Cameron Gallichan").

%% API
-export([for/2]).

for(Min, Max) when Min =:= Max -> ok;
for(Min, Max) when Min < Max ->
  io:fwrite("Num: ~p~n", [Min]),
  for(Min+1, Max).
