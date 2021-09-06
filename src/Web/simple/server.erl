-module(server).

-export([start/1]).

start(Port) ->
    spawn(fun () ->
		  {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
		  loop(Sock)
	  end).

loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(fun () -> handle(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    loop(Sock).

handle(Conn) ->
    {ok, File} = file:read_file("index.html"),
    Content = unicode:characters_to_list(File),
    gen_tcp:send(Conn, response(Content)),
    gen_tcp:close(Conn).

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: text/html\nCon"
				   "tent-Length: ~p\n\n~s",
				   [size(B), B])).
