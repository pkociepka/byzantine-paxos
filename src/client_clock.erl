-module(client_clock).

-export([start/1]).

start(Value) ->
    loop(Value).

loop(Value) ->
    receive
        {get, Pid} ->
            Pid ! {seq, Value},
            loop(Value+1)
    end.
