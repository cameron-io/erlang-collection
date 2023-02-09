%%%-------------------------------------------------------------------
%%% @author Cameron G.
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2021 10:13
%%%-------------------------------------------------------------------
-module(lambda).
-author("Cameron Gallichan").

%% API
-export([lamb1/0,  lamb2/0]).

lamb1() ->
	Name = "Bobby",
	MyFun = fun() -> io:fwrite("Hey ~p~n", [Name]) end,
	MyFun().

lamb2() ->
	X = 4,
	Y = 5,
	Z = fun() -> io:fwrite("Sum: ~p~n", [X + Y]) end,
	Z().
