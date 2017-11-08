-module(byzantine).

-export([start/0]).

start() ->
    {ok, Started} = application:ensure_all_started(byzantine).
