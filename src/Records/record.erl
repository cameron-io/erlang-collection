%%%-------------------------------------------------------------------
%%% @author Cameron G.
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2021 09:41
%%%-------------------------------------------------------------------
-module(record).
-author("Cameron Gallichan").

%% API
-export([start/0]).

-record(person, {species="", name = "", bal_owed = 0.00}).

start() ->
	Bobby = #person{species="human", name="Bobby Jones", bal_owed = 50.00},
	Gerald = Bobby#person{name="Bobby", bal_owed = 30.00},

	io:fwrite(
		"~p owes ~p approx. ~p bottle caps",
		[
			Bobby#person.name,
			Gerald#person.name,
			Bobby#person.bal_owed
		]
	).
