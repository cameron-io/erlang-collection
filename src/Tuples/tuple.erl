%%%-------------------------------------------------------------------
%%% @author Cameron G.
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 12. Aug 2021 12:30
%%%-------------------------------------------------------------------
-module(tuple).
-author("Cameron Gallichan").

%% API
-export([create/0]).

create() ->
	{ok, {FirstName, LastName} = FullName } = io:read("What is your name? "),
	FirstName,
	LastName,
	FullName.
