%%%-------------------------------------------------------------------
%%% @author Cameron G
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Aug 2021 10:07
%%%-------------------------------------------------------------------
-module(numbers).
-author("Cameron Gallichan").

-import(string, [to_lower/1]).

%% API
-export([start/0]).

greeting() ->
  io:fwrite("-----------------------~n"),
  io:fwrite("WELCOME TO NUMBERS~n"),
  io:fwrite("-----------------------~n~n").

info() ->
  io:fwrite("P - Print numbers~n"),
  io:fwrite("A - Add a number~n"),
  io:fwrite("M - Display mean of the numbers~n"),
  io:fwrite("S - Display the smallest number~n"),
  io:fwrite("L - Display the largest number~n"),
  io:fwrite("Q - Quit~n~n").

start() ->
  greeting(),
  info(),
  play(),
  io:fwrite("Goodbye!~n").

ask() ->
  io:get_chars("Enter your selection> ", 1).

play() ->
  UserInput = ask(),

  case to_lower(UserInput) of
    "p" ->
      io:fwrite("You pressed p!"),
      play()
  end.
