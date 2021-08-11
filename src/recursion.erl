%%%-------------------------------------------------------------------
%%% @author Cameron G.
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 11. Aug 2021 14:19
%%%-------------------------------------------------------------------
-module(recursion).
-author("Cameron Gallichan").

%% API
-export([myList/1]).

myList([Hd|Tl]) ->
  plusOne([Hd|Tl]).

plusOne([]) -> [];
plusOne([H|T]) -> [H+1 | plusOne(T)].

% plusOne([1,2,3])
% => plusOne([1|2,3]) -> [1+1 | plusOne([2,3])]
% => plusOne([2|3]) -> [2+1 | plusOne([3])] -^ 3
% => plusOne([3|]) -> [3+1 | plusOne([])] -^ 4
% => plusOne([]) -> []
