%%#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname bubblesort -mnesia debug verbose

main([N, Limit]) ->
    io:format(user, "Running Bubble Sort...~n", []),
	InputList = lists:map(
        fun(_E) -> trunc(rand:uniform() * list_to_integer(Limit)) end,
        lists:seq(1, list_to_integer(N))
    ),
    SortedList = bubble_sort(InputList),

    io:format(user, "Input List: ~p~n", [InputList]),
    io:format(user, "Sorted List: ~p~n~n", [SortedList]);
main(_) ->
    io:format("usage: bubblesort <N> <Limit>\n"),
    halt(1).

%% API functions

bubble_sort(List0) ->
    {_, List3} = lists:mapfoldl(
        fun(I, List1) ->
            {_, List2} = compare(List1, I),
            {I, List2}
        end,
        List0,
        lists:seq(1, length(List0))
    ),
    List3.

%% Internal functions

compare(List0, I) ->
    lists:mapfoldl(
        fun(J, List1) ->
            case lists:nth(J, List1) > lists:nth(J+1, List1) of
                true ->
                    List2 = swap(List1, J),
                    {J, List2};
                false ->
                    {J, List1}
            end
        end,
        List0,
        lists:seq(1, length(List0) - I)
    ).

swap(List0, A) ->
	{List1, _} = lists:mapfoldl(
		fun(E, Index) ->
			case Index == A orelse Index == A+1 of
				true ->
					case Index == A of
						true ->
							{lists:nth(A + 1, List0), Index + 1};
						false ->
							{lists:nth(A, List0), Index + 1}
					end;
				false ->
					{E, Index + 1}
			end
		end,
		1,
		List0
	),
	List1.
