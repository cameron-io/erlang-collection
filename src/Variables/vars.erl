% variables program
-module(vars).

-import(io,[fwrite/1]).

-export([main/0]). 

main() ->
    Bikes = 5,
    small_atom,
    Big_atom = 'This sentence',
    Couldbe = (Bikes >= 5) or (Bikes =< 12),
    Listing = [10, 20, 30], 
    Mapper = #{name=>john,age=>25},

    io:fwrite("List of Variable Types:\n\n"),

    io:fwrite("-Number: ~w\n", [Bikes]),
    io:fwrite("-Atom: ~w\n", [small_atom]),
    io:fwrite("-Big Atom: ~w\n", [Big_atom]),
    io:fwrite("-Boolean: ~w\n", [Couldbe]),
    io:fwrite("-Listing: ~w\n", [Listing]),
    io:fwrite("-Map: ~w\n", [Mapper]).
