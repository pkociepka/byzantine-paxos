-module(byzantine).

-export([start/0]).

start() ->
    {ok, Started} = application:ensure_all_started(byzantine),
    register(cl_monitor, spawn_link(cluster_monitor, start, [7])).
