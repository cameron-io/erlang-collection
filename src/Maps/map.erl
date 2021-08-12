%%%-------------------------------------------------------------------
%%% @author Cameron G.
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 12. Aug 2021 12:06
%%%-------------------------------------------------------------------
-module(map).
-author("Cameron Gallichan").

%% API
-export([create/2]).

create(FirstName, LastName) ->
	Map = #{
		first_name => FirstName,
		last_name => LastName
	},
	io:format("~p", [Map]).
