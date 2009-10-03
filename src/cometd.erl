-module(cometd).

-export([start/0, start/2]).
-export([loop/3]).

start() ->
    start(8080, 6780).

start(Port, ControlPort) ->
    {ok, Broker} = msgbroker:start_link(),
    {ok, ReLongpoll} = re:compile("^longpoll/([A-Za-z0-9._-]+)/([0-9]+)$"),

    % Start Control server
    {ok, Control} = msgbrokerserv:start_link(ControlPort, Broker),

    % Start HTTP server
    L = fun(R) -> ?MODULE:loop(R, Broker, ReLongpoll) end,
    {ok, _} = mochiweb_http:start([{name, ?MODULE}, {loop, L}, {port, Port}]),

    {ok, Broker}.

loop(Req, Broker, ReLongpoll) ->
    "/" ++ Path = Req:get(path),
    Res = case Path of
        "longpoll/" ++ _ ->
            case re:run(Path, ReLongpoll, [{capture, all_but_first, list}]) of
                {match, [Ch, LastStr]} ->
                    Last = list_to_integer(LastStr),
                    Timeout = 5000,
                    case msgbroker:get_message(Broker, Ch, Last, Timeout) of
                        {N, Ch, M} ->
                            Enc = urlencode:urlencode(M),
                            io_lib:format("_msg(~w,'~s',unescape('~s'));", [N, Ch, Enc]);
                        timeout ->
                            "_msg_timeout();"
                    end;
                nomatch ->
                    "Usage: /longpoll/channel/last_id"
            end;
        _ ->
            "cometd (C) 2009 Albert Peschar"
    end,
    ok(Req, Res).

ok(Req, Res) ->
    Req:ok({_ContentType = "text/plain",
            _Headers = [],
            Res}).

