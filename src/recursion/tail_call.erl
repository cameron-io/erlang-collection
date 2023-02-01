%%%-------------------------------------------------------------------
%%% @author Cameron G.
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2021 11:26
%%%-------------------------------------------------------------------
-module(tail_call).
-author("Cameron Gallichan").

%% API
-export([merge/2]).

% Merge two lists (of same length)
merge(Xs, Ys) ->
	lists:reverse(mergeL(Xs, Ys, [])).

mergeL([X|Xs], Ys, Zs) ->
	% Pull off X head, prepend to Zs
	% Pass shortened X & Y tails to mergeR
	mergeR(Xs, Ys, [X|Zs]);
mergeL([], [], Zs) ->
	Zs.

mergeR(Xs, [Y|Ys], Zs) ->
	% Pull off Y head, prepend to Zs
	% Pass shortened X & Y tails to mergeL
	mergeL(Xs, Ys, [Y|Zs]);
mergeR([], [], Zs) ->
	Zs.
