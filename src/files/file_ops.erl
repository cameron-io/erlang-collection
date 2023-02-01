%%%-------------------------------------------------------------------
%%% @author Cameron G.
%%% @copyright (C) 2021
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2021 10:19
%%%-------------------------------------------------------------------
-module(file_ops).
-author("Cameron Gallichan").

%% API
-export([write_txt/0, append_txt/0, read_txt/0, clear_txt/0]).

% Macros
-define(N, "Text\n").
-define(Nn, "More text\n").

write_txt() ->
	{ok, FileHandler} = file:open("MyFile.txt", [write]),
	file:write(FileHandler, ?N),
	file:close(FileHandler).

append_txt() ->
	{ok, FileHandler} = file:open("MyFile.txt", [append]),
	file:write(FileHandler, ?Nn),
	file:close(FileHandler).

read_txt() ->
	{ok, File} = file:open("MyFile.txt", [read]),
	Words = file:read(File, 1024 * 1024),
	io:fwrite("~p~n", [Words]),
	file:close(File).

clear_txt() ->
	{ok, FileHandler} = file:open("MyFile.txt", [write]),
	file:write(FileHandler, ""),
	file:close(FileHandler).
