-module(urlencode).

-export([urlencode/1, zeropad/2]).

-define(BETWEEN(A,B,X), ((X>A-1) and (X<B+1))).

urlencode([]) -> [];
urlencode([H|T]) -> encode_byte(H) ++ urlencode(T).

encode_byte(Byte) when ?BETWEEN(48, 57, Byte)
                       or ?BETWEEN(65, 90, Byte)
                       or ?BETWEEN(97, 122, Byte) -> [Byte];
encode_byte(Byte) ->
    "%" ++ zeropad(2, lists:flatten(io_lib:format("~.16B", [Byte]))).

zeropad(Length, Str) ->
    L = length(Str),
    if
        L < Length -> zeropad(Length, "0" ++ Str);
        L >= Length -> Str
    end.

