-module(example).
-export([main/1]).

main(_Args) ->
    {struct, Props} = mochijson2:decode(<<"{\"who\":\"World\"}">>),
    io:format("Hello ~s!~n", [proplists:get_value(<<"who">>, Props)]).
