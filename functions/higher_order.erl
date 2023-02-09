%%%-------------------------------------------------------------------
%%% @author Cameron G
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2021 09:59
%%%-------------------------------------------------------------------
-module(higher_order).
-author("Cameron Gallichan").

%% API
-export([hof/0]).

triple(X) -> X * 3.

hof() ->
	MyList = [1,2,3],
	lists:map(fun triple/1, MyList).
