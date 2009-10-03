-module(msgbroker).
-behaviour(gen_server).

-export([start_link/0, test/0]).
-export([get_message/3, get_message/4, put_message/3]).
-export([init/1, handle_call/3]).

-record(state, {id=0, messages=dict:new(), wait=dict:new()}).
-record(msg, {id, ch, msg}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

get_message(P, Ch, N) ->
    ok = gen_server:call(P, {get_message, Ch, N}),
    M = receive {P, message, Ch, Msg} -> Msg end,
    {M#msg.id, M#msg.ch, M#msg.msg}.

get_message(P, Ch, N, Timeout) ->
    ok = gen_server:call(P, {get_message, Ch, N}),
    receive
        {P, message, Ch, M} -> {M#msg.id, M#msg.ch, M#msg.msg}
    after
        Timeout -> timeout
    end.

put_message(P, Ch, M) ->
    ok = gen_server:call(P, {put_message, Ch, M}).

init(_Args) ->
    {ok, #state{}}.

handle_call({get_message, Ch, N}, {From, _Tag}, S) ->
    Messages = lists:reverse(lists:filter(fun(M) -> M#msg.id > N end, s_get_messages(Ch, S))),
    if
        length(Messages) > 0 ->
            [M|_] = Messages,
            From ! {self(), message, Ch, M},
            {reply, ok, S};
        length(Messages) == 0 ->
            S2 = S#state{wait=dict:update(Ch, fun(O) -> [From|O] end, [From], S#state.wait)},
            {reply, ok, S2}
    end;
handle_call({put_message, Ch, Msg}, _From, S) ->
    {M, S2} = s_add_message(Ch, Msg, S),
    {Waiting, S3} = s_get_and_erase_waiting(Ch, S2),
    lists:map(fun(P) -> P ! {self(), message, Ch, M} end, Waiting),
    {reply, ok, S3}.

s_get_messages(Ch, S) ->
    case dict:find(Ch, S#state.messages) of
        {ok, Val} -> Val;
        error -> []
    end.

s_add_message(Ch, Msg, S) ->
    N = S#state.id + 1,
    M = #msg{id=N, ch=Ch, msg=Msg},
    Dict = dict:update(Ch, fun(O) -> first(150, [M|O]) end, [M], S#state.messages),
    S2 = S#state{id=N, messages=Dict},
    {M, S2}.

s_get_and_erase_waiting(Ch, S) ->
    case dict:find(Ch, S#state.wait) of
        {ok, Val} -> {Val, S#state{wait=dict:erase(Ch, S#state.wait)}};
        error -> {[], S}
    end.

first(_N, []) -> [];
first(N, [_H|_T]) when N < 1 -> [];
first(N, [H|T]) -> [H|first(N-1, T)].

test() ->
    {ok, P} = start_link(),
    ok = put_message(P, test, hello),
    io:format("~p~n", [get_message(P, test, 0)]).

