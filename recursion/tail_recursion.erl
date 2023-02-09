%%%-------------------------------------------------------------------
%%% @author Cameron G.
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 11. Aug 2021 14:04
%%%-------------------------------------------------------------------
-module(tail_recursion).
-author("Cameron Gallichan").

%% API
-export([sum/1]).

sum([Hd|Tl]) ->
  sum_acc([Hd|Tl], 0).

sum_acc([], Sum) -> Sum;
sum_acc([H|T], Sum) -> sum_acc(T, H+Sum).

% sum_acc([2,3], 1 + 0)
% => sum_acc([3], 2 + 1)
% => sum_acc([], 3 + 3) -> 6
