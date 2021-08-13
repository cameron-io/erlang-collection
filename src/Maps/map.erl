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
-export([start/2]).

start(FirstName, LastName) ->
	Map = #{
		first_name => FirstName,
		last_name => LastName
	},
	io:format("~p~n", [Map]),

	io:format("~p~n", [maps:keys(Map)]),
	io:format("~p~n", [maps:values(Map)]),

	io:format("~p~n", [maps:find(first_name, Map)]),             % Check if key exists
	io:format("~p~n", [maps:get(first_name, Map)]),              % Retrieve value

	io:format("~p~n", [maps:put(email, "name@mail.com", Map)]),
	io:format("~p~n", [maps:remove(first_name, Map)]).
