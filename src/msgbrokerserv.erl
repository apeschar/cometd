-module(msgbrokerserv).

-export([start_link/2]).
-export([server/2, request_loop/2, accept_loop/2]).

start_link(Port, Broker) ->
    Pid = spawn_link(?MODULE, server, [Port, Broker]),
    {ok, Pid}.

server(Port, Broker) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
    accept_loop(ListenSocket, Broker).

accept_loop(ListenSocket, Broker) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, request_loop, [Socket, Broker]),
    ?MODULE:accept_loop(ListenSocket, Broker).

request_loop(Socket, Broker) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Binary} ->
            Str = re:replace(binary_to_list(Binary), "\r\n$", "\n", [{return, list}]),
            Res = case Str of
                "MESSAGE " ++ Args ->
                    case re:run(Args, "^([A-Za-z0-9._-]+)\s+(.+?)\r?\n?$", [{capture, all_but_first, list}]) of
                        {match, [Ch, Msg]} ->
                            msgbroker:put_message(Broker, Ch, Msg),
                            io_lib:format("OK~n", []);
                        _ ->
                            io_lib:format("ERROR: syntax~n", [])
                    end;
                "PING\n" ->
                    io_lib:format("PONG~n", []);
                _ ->
                    io_lib:format("ERROR: no such command~n", [])
            end,
            gen_tcp:send(Socket, Res),
            request_loop(Socket, Broker);
        {error, closed} ->
            0
    end.

