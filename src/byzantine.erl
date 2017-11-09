-module(byzantine).

-export([start/0]).

start() ->
    {ok, _Started} = application:ensure_all_started(byzantine),
    register(cl_monitor, spawn_link(cluster_monitor, start, [3])),
    register(clock, spawn_link(client_clock, start, [2])).
