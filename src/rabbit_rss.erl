-module(rabbit_rss).

-export([start/0, stop/0, start/2, stop/1]).

start() ->
    rabbit_rss_sup:start_link(), ok.

stop() ->
    ok.

start(normal, []) ->
    rabbit_rss_sup:start_link().

stop(_State) ->
    ok.
