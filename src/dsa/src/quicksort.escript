%%#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname quicksort -mnesia debug verbose

main([N, Limit]) ->
    io:format(user, "Running Quick Sort...~n", []),
	InputList = lists:map(
        fun(_E) -> trunc(rand:uniform() * list_to_integer(Limit)) end,
        lists:seq(1, list_to_integer(N))
    ),
	SortedList = quicksort(InputList, 1, erlang:length(InputList)),
    
    io:format(user, "Input List: ~p~n", [InputList]),
    io:format(user, "Sorted List: ~p~n~n", [SortedList]);
main(_) ->
    io:format("usage: quicksort <N> <Limit>\n"),
    halt(1).

%% API functions

quicksort(List0, Low, High) when Low < High ->
	% Get Partition point (where [.. < Pi > ..])
	{Pi, List1} = partition(List0, Low, High),
	% Recurse left then right of pivot where low < high for each 
	List2 = quicksort(List1, Low, Pi - 1), % from 0 to pivot
	quicksort(List2, Pi + 1, High);		   % from pivot to end
quicksort(List, _Low, _High) -> 
	List.

%% Internal functions

partition(List0, Low, High) ->
	Pivot = lists:nth(High, List0),
	I0 = Low - 1,
	{List1, I1} = sort({Low, High}, I0, Pivot, List0),
	List2 = swap(List1, I1 + 1, High),
	{I1 + 1, List2}.

sort({Low, High} = _Range, I, Pivot, List0) ->
	{_, {List1, I1}} = lists:mapfoldl(
		fun(J, {List, I0}) ->
			case lists:nth(J, List) =< Pivot of
				true ->
					I1 = I0 + 1,
					List1 = swap(List, I1, J),
					{J, {List1, I1}};
				false ->
					{J, {List, I0}}
			end
		end,
		{List0, I},
		lists:seq(Low, High-1)
	),
	{List1, I1}.

swap(List0, A, B) ->
	{List1, _} = lists:mapfoldl(
		fun(E, Index) ->
			case Index == B orelse Index == A of
				true ->
					case Index == B of
						true ->
							{lists:nth(A, List0), Index + 1};
						false ->
							{lists:nth(B, List0), Index + 1}
					end;
				false ->
					{E, Index + 1}
			end
		end,
		1,
		List0
	),
	List1.
