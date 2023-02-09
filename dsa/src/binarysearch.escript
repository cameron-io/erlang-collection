%%#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname binarysearch -mnesia debug verbose

%% Example:
%%  escript binarysearch.escript 11 128
%%  - Sorted List: [2,4,8,16,32,64,128,256,512,1024,2048]
%%  - Pointer: 7

main([Len, X]) ->
    io:format(user, "Running Binary Search...~n", []),
    {List, _} = lists:mapfoldl(
        fun(_E, Acc) ->
            Nval = Acc * 2,
            {Nval, Nval}
        end,
        1,
        lists:seq(1, list_to_integer(Len))
    ),
    Ptr = binary_search(
        List,
        list_to_integer(X),
        lists:nth(1, List),
        length(List)
    ),
    io:format(user, "Sorted List: ~p~n", [List]),
    io:format(user, "Pointer: ~p~n~n", [Ptr]);
main(_) ->
    io:format("usage: binarysearch <ListLength> <Value>\n"),
    halt(1).

%% API functions

binary_search(List, X, Low0, High0) when Low0 =< High0 ->
    Mid = Low0 + (High0 - Low0) div 2 div 2,
    
    case lists:nth(Mid, List) == X of
        true ->
            Mid;
        false ->
            case lists:nth(Mid, List) < X of
                true ->
                    Low = Mid + 1,
                    binary_search(List, X, Low, High0);
                false ->
                    High = Mid - 1,
                    binary_search(List, X, Low0, High)
            end
    end;
binary_search(List, _X, _Low, _High) ->
    List.

