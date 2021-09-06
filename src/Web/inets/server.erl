%%%
%%% A simple "Hello, world" server in the Erlang.
%%%

-module(server).

-export([main/1, run_server/0, start/0]).

main(_) -> start(), receive stop -> ok end.

run_server() ->
    ok = inets:start(),
    {ok, _} = inets:start(httpd,
			  [{port, 8000}, {server_name, "hello_erlang"},
			   {server_root, "./"}, {document_root, "./public"},
			   {bind_address, "localhost"}]).

start() -> run_server().
