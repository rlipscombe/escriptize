-module(example).
-export([main/1]).

main(_Args) ->
    Map = jiffy:decode(<<"{\"who\":\"World\"}">>, [return_maps]),
    io:format("Hello ~s!~n", [maps:get(<<"who">>, Map)]).
